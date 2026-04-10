/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkFastBilateralImageFilter_hxx
#define itkFastBilateralImageFilter_hxx

#include "itkMinimumMaximumImageCalculator.h"
#include "itkImageDuplicator.h"
#include <cmath>

namespace itk
{

template <class TInputImage, class TOutputImage>
void
FastBilateralImageFilter<TInputImage, TOutputImage>::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method. this should
  // copy the output requested region to the input requested region
  Superclass::GenerateInputRequestedRegion();

  // get pointers to the input and output
  typename Superclass::InputImagePointer inputPtr = const_cast<TInputImage *>(this->GetInput());

  if (!inputPtr)
  {
    return;
  }

  // Pad the image by 2*sigma (pixel units)
  // this is done to ensure that nearby pixels are still
  // included in calculations
  // When the filter does the downsampling pixels are placed into
  // bins based on their position/sigma. Therefore, assuming
  // that the input region includes a pixel that would be placed into a new bin
  // on its own, that bin and the bin beside it would need to be populated.
  // Each bin can contain at most ceil(sigma) pixels
  InputImageSizeType radius;
  for (size_t i = 0; i < ImageDimension; ++i)
  {
    radius[i] = 2 * std::ceil(m_DomainSigma[i] / (this->GetInput()->GetSpacing()[i]));
  }

  // get a copy of the input requested region (should equal the output
  // requested region)
  typename TInputImage::RegionType inputRequestedRegion;
  inputRequestedRegion = inputPtr->GetRequestedRegion();

  // pad the input requested region by the operator radius
  inputRequestedRegion.PadByRadius(radius);

  // crop the input requested region at the input's largest possible region
  if (inputRequestedRegion.Crop(inputPtr->GetLargestPossibleRegion()))
  {
    inputPtr->SetRequestedRegion(inputRequestedRegion);
    return;
  }
  else
  {
    // Couldn't crop the region (requested region is outside the largest
    // possible region).  Throw an exception.

    // store what we tried to request (prior to trying to crop)
    inputPtr->SetRequestedRegion(inputRequestedRegion);

    // build an exception
    InvalidRequestedRegionError e(__FILE__, __LINE__);
    e.SetLocation(ITK_LOCATION);
    e.SetDescription("Requested region is outside the largest possible region.");
    e.SetDataObject(inputPtr);
    throw e;
  }
}

template <class TInputImage, class TOutputImage>
void
FastBilateralImageFilter<TInputImage, TOutputImage>::GenerateData()
{
  this->AllocateOutputs();
  InputImageConstPointer input = this->GetInput();
  OutputImagePointer     output = this->GetOutput();

  // Array to store domain sigmas, used during down-sampling and reconstruction
  DomainSigmaArrayType domainSigmaInPixels;

  // Minimum intensity of the input image,
  // used during down-sampling and reconstruction
  InputPixelType intensityMin;

  // Define the GridType
  // These are pointers to the source and destination of the blurring filter
  // Data from the input will be sorted into the first two images while the
  // second two images will provide the output data.
  // The parameters are determined by the size of the input image and the
  // values of m_DomainSigma and m_RangeSigma
  typename GridType::Pointer gridImage = GridType::New();
  typename GridType::Pointer gridWeight;
  typename GridType::Pointer gridImageOut;
  typename GridType::Pointer gridWeightOut;

  // The amount of padding around the grid images, required so that
  // interpolation is not done outside of the grid during reconstruction
  int padding = 2;

  // Setup the higher dimensional grids (gridImage and gridWeight).
  {
    GridIndexType gridStartPos;
    GridSizeType  gridSize;

    // Convert domain sigmas from spacing units to pixel units
    // for the blurring in the grid.
    // The set method for m_DomainSigma uses spacing units to be
    // consistent with the itkBilateralImageFilter implementation.
    // When the data is placed into bins in the grid the
    // itkDiscreteGaussianImageFilter will be run on the grid using
    // imageSpacingOff.
    InputImageSizeType inputSize = input->GetRequestedRegion().GetSize();
    //  InputImageSizeType fullSize = input->GetLargestPossibleRegion().GetSize();

    const InputImageSpacingType & spacing = input->GetSpacing();
    for (size_t i = 0; i < ImageDimension; ++i)
    {
      domainSigmaInPixels[i] = m_DomainSigma[i] / spacing[i];
      gridSize[i] = std::floor((inputSize[i] - 1) / domainSigmaInPixels[i]) + 1 + 2 * padding;
    }

    // Determine min/max intensities to calculate grid size in the intensity axis
    using MinMaxCalculatorType = MinimumMaximumImageCalculator<InputImageType>;
    typename MinMaxCalculatorType::Pointer calculator = MinMaxCalculatorType::New();

    calculator->SetImage(input);
    calculator->Compute();
    intensityMin = calculator->GetMinimum();
    InputPixelType intensityMax = calculator->GetMaximum();
    InputPixelType intensityDelta = static_cast<InputPixelType>(intensityMax - intensityMin);
    gridSize[ImageDimension] = static_cast<GridSizeValueType>((intensityDelta / m_RangeSigma) + 1 + 2 * padding);

    for (size_t i = 0; i < ImageDimension + 1; ++i)
    {
      gridStartPos[i] = 0;
    }

    GridRegionType region;
    region.SetSize(gridSize);
    region.SetIndex(gridStartPos);
    gridImage->SetRegions(region);
  }

  // Allocate the memory for pixel data
  // We now have an empty grid. Two will be needed total,
  // one will be a container for the down-sampled data and one
  // will remember how many pixels were placed into each bin
  gridImage->Allocate();
  // Init values of image to 0
  gridImage->FillBuffer(0.0);

  // Duplicate our grid image for the weight image
  {
    using DuplicatorType = ImageDuplicator<GridType>;
    typename DuplicatorType::Pointer duplicator = DuplicatorType::New();
    duplicator->SetInputImage(gridImage);
    duplicator->Update();
    gridWeight = duplicator->GetOutput();
  }

  // Sort the input image in gridImage and keep track of weights in gridWeight
  {
    InputImageConstIteratorType iterInputImage(input, input->GetRequestedRegion());
    GridIndexType               gridIndices;
    size_t                      i;
    InputPixelType              current;
    InputPixelType              intensityDelta;
    InputImageIndexType         index;

    // For every pixel in the input image, place it into a bin in the grid
    // This is a scatter type operation and will be inefficient, as far as I
    // know, there is no way to place the pixels into the grid using iterators
    for (iterInputImage.GoToBegin(); !iterInputImage.IsAtEnd(); ++iterInputImage)
    {
      index = iterInputImage.GetIndex();
      current = iterInputImage.Get();
      // Determine the position in the grid to place the pixel
      for (i = 0; i < ImageDimension; ++i)
      {
        gridIndices[i] = static_cast<GridSizeValueType>(index[i] / domainSigmaInPixels[i] + 0.5 + padding);
      }
      intensityDelta = current - intensityMin;
      gridIndices[ImageDimension] = static_cast<GridSizeValueType>(intensityDelta / m_RangeSigma + 0.5 + padding);

      // Update the bin and the weight
      (gridImage->GetPixel(gridIndices)) += current;
      (gridWeight->GetPixel(gridIndices)) += 1.0;
    }
  }

  // Perform blurring on gridImage and gridWeight
  // outputs are pointed to by gridImageOut and gridWeightOut
  {

    // This variance approximately corresponds to a 1D filter of [1 2 1] which is
    // used in Paris and Durands C++ implementation to blur their down-sampled
    // data. With this variance a kernel width larger than 5 is not necessary.
    double variance = 1.59577;
    int    maxWidth = 5;

    // Setup the Gaussian filter
    typename BlurType::Pointer gridImageBlurFilter = BlurType::New();
    typename BlurType::Pointer gridWeightBlurFilter = BlurType::New();

    gridImageBlurFilter->SetVariance(variance);
    gridWeightBlurFilter->SetVariance(variance);

    gridImageBlurFilter->UseImageSpacingOff();
    gridWeightBlurFilter->UseImageSpacingOff();

    gridImageBlurFilter->SetMaximumKernelWidth(maxWidth);
    gridWeightBlurFilter->SetMaximumKernelWidth(maxWidth);

    gridImageBlurFilter->SetInput(gridImage);
    gridWeightBlurFilter->SetInput(gridWeight);

    gridImageOut = gridImageBlurFilter->GetOutput();
    gridWeightOut = gridWeightBlurFilter->GetOutput();

    gridImageBlurFilter->Update();
    gridWeightBlurFilter->Update();
  }

  // Early division; in Paris and Durand's implementation early division can
  // be done on the grid image, or interpolation on both the bin and the
  // weights can be done then the division. Interpolation is an expensive
  // operation so I've opted for the early division approach.
  {
    GridImageIteratorType      iterGridImage(gridImageOut, gridImageOut->GetLargestPossibleRegion());
    GridImageConstIteratorType iterGridWeight(gridWeightOut, gridWeightOut->GetLargestPossibleRegion());

    GridPixelType weight;

    for (iterGridImage.GoToBegin(), iterGridWeight.GoToBegin(); !iterGridImage.IsAtEnd();
         ++iterGridImage, ++iterGridWeight)
    {
      if ((weight = iterGridWeight.Get()) != 0.0)
      {
        iterGridImage.Value() /= weight;
      }
    }
  }

  // Perform interpolation in order to construct the output.
  // For every pixel in the input image, determine where in the grid the pixel
  // was placed and interpolate for the output pixel's value.
  {

    OutputImageIteratorType     iterOutputImage(output, output->GetRequestedRegion());
    InputImageConstIteratorType iterInputImage(input, output->GetRequestedRegion());

    InterpolatedIndexType              gridIndices;
    size_t                             i;
    InputPixelType                     current;
    InputPixelType                     intensityDelta;
    InputImageIndexType                index;
    typename InterpolatorType::Pointer interpolator = InterpolatorType::New();
    interpolator->SetInputImage(gridImageOut);

    for (iterOutputImage.GoToBegin(), iterInputImage.GoToBegin(); !iterOutputImage.IsAtEnd();
         ++iterOutputImage, ++iterInputImage)
    {
      index = iterInputImage.GetIndex();
      current = iterInputImage.Get();
      // Determine the position in the grid to get the data from
      for (i = 0; i < ImageDimension; ++i)
      {
        gridIndices[i] = index[i] / domainSigmaInPixels[i] + padding;
      }

      intensityDelta = current - intensityMin;

      gridIndices[ImageDimension] = intensityDelta / m_RangeSigma + padding;

      iterOutputImage.Set(static_cast<OutputPixelType>(interpolator->EvaluateAtContinuousIndex(gridIndices)));
    }
  }
}

template <class TInputImage, class TOutputImage>
void
FastBilateralImageFilter<TInputImage, TOutputImage>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "DomainSigma: " << m_DomainSigma << std::endl;
  os << indent << "RangeSigma: " << m_RangeSigma << std::endl;
}

} // namespace itk

#endif
