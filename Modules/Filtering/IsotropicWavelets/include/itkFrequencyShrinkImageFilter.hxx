/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkFrequencyShrinkImageFilter_hxx
#define itkFrequencyShrinkImageFilter_hxx

#include <itkFrequencyShrinkImageFilter.h>
#include <itkProgressReporter.h>
#include <numeric>
#include <functional>
#include "itkInd2Sub.h"
#include <itkPasteImageFilter.h>
#include <itkAddImageFilter.h>
#include <itkMultiplyImageFilter.h>
// #include <itkGaussianSpatialFunction.h>
// #include <itkFrequencyImageRegionIteratorWithIndex.h>

namespace itk
{
template <class TImageType>
FrequencyShrinkImageFilter<TImageType>::FrequencyShrinkImageFilter()

{
  for (unsigned int j = 0; j < ImageDimension; j++)
  {
    m_ShrinkFactors[j] = 2;
  }

  this->m_FrequencyBandFilter = FrequencyBandFilterType::New();
  // The band filter only let pass half of the frequencies.
  this->m_FrequencyBandFilter->SetFrequencyThresholdsInRadians(0.0, Math::pi_over_2);
  bool lowFreqThresholdPassing = true;
  bool highFreqThresholdPassing = true;
  this->m_FrequencyBandFilter->SetPassBand(lowFreqThresholdPassing, highFreqThresholdPassing);
  // The band is not radial, but square like.
  this->m_FrequencyBandFilter->SetRadialBand(false);
  // Pass high positive freqs but stop negative high ones, to avoid overlapping.
  this->m_FrequencyBandFilter->SetPassNegativeHighFrequencyThreshold(false);
}

template <class TImageType>
void
FrequencyShrinkImageFilter<TImageType>::SetShrinkFactors(unsigned int factor)
{
  unsigned int j;

  for (j = 0; j < ImageDimension; j++)
  {
    if (factor != m_ShrinkFactors[j])
    {
      break;
    }
  }
  if (j < ImageDimension)
  {
    for (j = 0; j < ImageDimension; j++)
    {
      m_ShrinkFactors[j] = factor;
      if (m_ShrinkFactors[j] < 1)
      {
        m_ShrinkFactors[j] = 1;
      }
    }
  }
  this->Modified();
}

template <class TImageType>
void
FrequencyShrinkImageFilter<TImageType>::SetShrinkFactor(unsigned int i, unsigned int factor)
{
  if (m_ShrinkFactors[i] == factor)
  {
    return;
  }

  m_ShrinkFactors[i] = factor;
  this->Modified();
}

/**
 * Implementation Detail:
 * The implementation calculate the number of different regions in an image,
 * depending on the dimension:
 * numberOfRegions = 2^dim (positive and negative frequencies per dim)
 * then uses function to convert a linear array of regions [0, ..., numberOfRegions - 1]
 * to binary subindices (only two options: positive or negative region)
 * In 3D: numberOfRegions = Nr = 2^3 = 8
 * sizeOfSubindices = [2,2,2]
 * Region = 0       -----> Ind2Sub(   0, [2,2,2]) = [0,0,0]
 * Region = 1       -----> Ind2Sub(   1, [2,2,2]) = [1,0,0]
 * Region = Nr - 1  -----> Ind2Sub(Nr-1, [2,2,2]) = [1,1,1]
 * So, if the result of Ind2Sub is 0 we paste the positive frequencies, if 1, negative freq
 */
template <class TImageType>
void
FrequencyShrinkImageFilter<TImageType>::GenerateData()
{
  // Get the input and output pointers
  const ImageType * inputPtr = this->GetInput();

  typename ImageType::Pointer outputPtr = this->GetOutput();
  this->AllocateOutputs();
  // outputPtr->SetBufferedRegion(outputPtr->GetLargestPossibleRegion());
  outputPtr->FillBuffer(0);

  // Output is the sum of the four(2D) or eight(3D) quadrants.
  // We can do it only because high freqs are removed.
  // This filter will remove it by default a BandPass Filter.
  if (this->m_ApplyBandFilter)
  {
    typename ImageType::SpacingType                  inputSpacing = this->GetInput()->GetSpacing();
    const typename ImageType::SpacingType::ValueType spacingValue = inputSpacing[0];
    // Check that the spacing is the same in all directions.
    {
      bool all_equal = true;
      for (unsigned int i = 1; i < ImageDimension; ++i)
      {
        if (itk::Math::NotAlmostEquals(inputSpacing[i], spacingValue))
        {
          all_equal = false;
        }
      }
      if (!all_equal)
      {
        itkExceptionMacro(<< "Spacing of input image is not the same in all directions " << inputSpacing);
      }
    }

    this->m_FrequencyBandFilter->SetInput(this->GetInput());
    this->m_FrequencyBandFilter->SetFrequencyThresholds(
      this->m_FrequencyBandFilter->GetLowFrequencyThreshold() * spacingValue,
      this->m_FrequencyBandFilter->GetHighFrequencyThreshold() * spacingValue);
    this->m_FrequencyBandFilter->Update();
    inputPtr = this->m_FrequencyBandFilter->GetOutput();
  }

  typename TImageType::SizeType        inputSize = inputPtr->GetLargestPossibleRegion().GetSize();
  typename TImageType::SizeType        outputSize = outputPtr->GetLargestPossibleRegion().GetSize();
  const typename TImageType::IndexType indexOrigOut = outputPtr->GetLargestPossibleRegion().GetIndex();
  // Manage ImageDimension array linearly:{{{
  FixedArray<unsigned int, ImageDimension> nsizes;
  unsigned int                             numberOfRegions = 1;
  for (unsigned int dim = 0; dim < ImageDimension; ++dim)
  {
    nsizes[dim] = 2;
    numberOfRegions *= nsizes[dim];
  }
  FixedArray<unsigned int, ImageDimension> subIndices;
  /// }}}

  // Prepare filter to paste the different regions into output.
  using PasteFilterType = itk::PasteImageFilter<ImageType>;
  typename PasteFilterType::Pointer pasteFilter = PasteFilterType::New();
  pasteFilter->SetSourceImage(inputPtr);
  pasteFilter->SetDestinationImage(outputPtr);
  // pasteFilter->InPlaceOn();

  using RegionType = typename ImageType::RegionType;
  ProgressReporter progress(this, 0, numberOfRegions);
  for (unsigned int n = 0; n < numberOfRegions; ++n)
  {
    subIndices = itk::Ind2Sub<ImageDimension>(n, nsizes);
    RegionType                     zoneRegion;
    typename ImageType::SizeType   zoneSize;
    typename TImageType::IndexType inputIndex = indexOrigOut;
    typename TImageType::IndexType outputIndex = indexOrigOut;
    // Note that lowFreqsOfInput is inputSize/2 if outputSize is even, (outputSize - 1)/2 if odd.
    for (unsigned int dim = 0; dim < ImageDimension; ++dim)
    {
      zoneSize[dim] = outputSize[dim];
      outputIndex[dim] = indexOrigOut[dim];
      if (subIndices[dim] == 0) // positive frequencies
      {
        inputIndex[dim] = indexOrigOut[dim];
      }
      else // negative frequencies
      {
        inputIndex[dim] = indexOrigOut[dim] + inputSize[dim] - zoneSize[dim];
      }
    }
    zoneRegion.SetIndex(inputIndex);
    zoneRegion.SetSize(zoneSize);
    itkDebugMacro(<< "n:" << n << " region: " << zoneRegion);

    pasteFilter->SetSourceRegion(zoneRegion);
    pasteFilter->SetDestinationIndex(outputIndex);
    pasteFilter->Update();
    // Sum the quadrants.
    using AddFilterType = itk::AddImageFilter<TImageType, TImageType>;
    typename AddFilterType::Pointer addFilter = AddFilterType::New();
    addFilter->SetInput1(outputPtr);
    addFilter->SetInput2(pasteFilter->GetOutput());
    addFilter->InPlaceOn();
    if (n == numberOfRegions - 1) // Graft the output.
    {
      addFilter->Update();
      outputPtr = addFilter->GetOutput();
      using MultiplyFilterType = itk::MultiplyImageFilter<TImageType, TImageType, TImageType>;
      typename MultiplyFilterType::Pointer multiplyFilter = MultiplyFilterType::New();
      multiplyFilter->SetInput(outputPtr);
      multiplyFilter->SetConstant(static_cast<typename TImageType::PixelType::value_type>(1.0 / numberOfRegions));

      multiplyFilter->GraftOutput(outputPtr);
      multiplyFilter->Update();
      this->GraftOutput(multiplyFilter->GetOutput());
      // addFilter->GraftOutput(outputPtr);
      // addFilter->Update();
      // this->GraftOutput(addFilter->GetOutput());
    }
    else // update
    {
      addFilter->Update();
      outputPtr = addFilter->GetOutput();
    }
    progress.CompletedPixel();
  }

  /** Ensure image is hermitian in the Nyquist bands (even)
   * Example: Image 2D size 8, index = [0,...,7]
   * Each quadrant is a region pasted from the original image. The index refers to the input image of size 8. The input
   * image is hermitian, so:
   *   0
   * 1 == 7
   * 2 == 6  <- 6 is Nyq in new image.
   * 3 == 5
   *   4  <- Nyq original
   * Hermitian table, using the equivalences above. (assuming imag part zero to avoid working with conjugates) . Note
   * that index 6 is the new Nyquist. 0    1  |  6    7 0 0,0  1,0 | 2,0  1,0 1 0,1  1,1 | 2,1  1,1
   * ---------------------
   * 6 0,2  1,2 | 2,2  1,2
   * 7 0,1  1,1 | 2,1  1,1
   * /
   */
  // Fix Nyquist band. Folding results for hermiticity.
  // {
  // typename TImageType::IndexType index = indexOrigOut + lowFreqsOfInput;
  // typename TImageType::IndexType modIndex = indexOrigOut + lowFreqsOfInput;
  // // typename TImageType::SizeType endSize = outputSize - lowFreqsOfInput;
  // for(unsigned int dim = 0; dim < ImageDimension; ++dim)
  //   {
  //   for(int i = 1; i < (int)lowFreqsOfInput[dim]; ++i)
  //     {
  //     index = indexOrigOut + lowFreqsOfInput;
  //     modIndex = indexOrigOut + lowFreqsOfInput;
  //     index.SetElement(dim, indexOrigOut[dim] + i );
  //     modIndex.SetElement(dim, indexOrigOut[dim] + outputSize[dim] - i );
  //     typename TImageType::PixelType value =
  //       std::conj(outputPtr->GetPixel(index));
  //     outputPtr->SetPixel(modIndex, value);
  //     // The stored nyquiist value corresponds to  the positive side.
  //     outputPtr->SetPixel(index, value);
  //     }
  //     // The stored nyquiist value corresponds to  the positive side.
  //     index.SetElement(dim, indexOrigOut[dim]);
  //     outputPtr->SetPixel(index, std::conj(outputPtr->GetPixel(index)));
  //   }
  // }

  // // Apply a gaussian window of the size of the output.
  // using FunctionType = itk::GaussianSpatialFunction<double, ImageDimension>;
  // typename FunctionType::Pointer gaussian = FunctionType::New();
  // using ArrayType = FixedArray< double, ImageDimension >;
  // ArrayType m_Mean;
  // ArrayType m_Sigma;
  // double m_Scale = 1.0;
  // bool m_Normalized = true;
  // for (unsigned int i = 0; i<ImageDimension; ++i)
  //   {
  //   m_Mean[i] = indexOrigOut[i];
  //   // 6.0 is equivalent to 3sigmas (border values are close to zero)
  //   // m_Sigma[i] = outputSize[i]/(2*3.0);
  //   m_Sigma[i] = outputSize[i]/(2.0*2.35);
  //   }
  // gaussian->SetSigma(m_Sigma);
  // gaussian->SetMean(m_Mean);
  // gaussian->SetScale(m_Scale);
  // gaussian->SetNormalized(m_Normalized);
  //
  // // Create an iterator that will walk the output region
  // using OutputIterator = itk::FrequencyImageRegionIteratorWithIndex< TImageType >;
  // OutputIterator outIt = OutputIterator( outputPtr,
  //     outputPtr->GetRequestedRegion() );
  // outIt.GoToBegin();
  // while( !outIt.IsAtEnd() )
  //   {
  //   typename TImageType::IndexType ind = outIt.GetFrequencyBin();
  //   typename FunctionType::InputType f;
  //   for (unsigned int i = 0; i<ImageDimension; ++i)
  //     {
  //     f[i] = static_cast<typename FunctionType::InputType::ValueType>(ind[i]);
  //     }
  //   const double value = gaussian->Evaluate(f);
  //   // Set the pixel value to the function value
  //   outIt.Set( outIt.Get() * static_cast<typename TImageType::PixelType::value_type>(value) );
  //   ++outIt;
  //   }
}

template <class TImageType>
void
FrequencyShrinkImageFilter<TImageType>::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();

  // get pointers to the input and output
  auto * inputPtr = const_cast<TImageType *>(this->GetInput());

  itkAssertInDebugAndIgnoreInReleaseMacro(inputPtr != nullptr);

  // The filter chops high frequencies [0 1...H,H-1 H-2...1].
  // We need the whole input image, independently of the RequestedRegion.
  inputPtr->SetRequestedRegion(inputPtr->GetLargestPossibleRegion());
}

template <class TImageType>
void
FrequencyShrinkImageFilter<TImageType>::GenerateOutputInformation()
{
  // Call the superclass' implementation of this method
  Superclass::GenerateOutputInformation();

  // Get pointers to the input and output
  const TImageType * inputPtr = this->GetInput();
  TImageType *       outputPtr = this->GetOutput();

  itkAssertInDebugAndIgnoreInReleaseMacro(inputPtr);
  itkAssertInDebugAndIgnoreInReleaseMacro(outputPtr != nullptr);

  // Compute the output spacing, the output image size, and the
  // output image start index
  const typename TImageType::SpacingType & inputSpacing = inputPtr->GetSpacing();
  const typename TImageType::SizeType &    inputSize = inputPtr->GetLargestPossibleRegion().GetSize();
  const typename TImageType::IndexType &   inputStartIndex = inputPtr->GetLargestPossibleRegion().GetIndex();
  const typename TImageType::PointType &   inputOrigin = inputPtr->GetOrigin();

  // ContinuousIndex<double,ImageDimension> inputIndexOutputOrigin;

  typename TImageType::SpacingType outputSpacing(inputSpacing);
  typename TImageType::SizeType    outputSize;
  typename TImageType::PointType   outputOrigin;
  typename TImageType::IndexType   outputStartIndex;
  // TODO Check if you want to modify metadata in this filter.
  // Reduce Spacing, a frequency shrinker deletes high frequency domain.
  // The spacing is taken into account by FrequencyIterators method GetFrequency().
  for (unsigned int i = 0; i < TImageType::ImageDimension; i++)
  {
    outputSpacing[i] = inputSpacing[i] * m_ShrinkFactors[i];
    // inputIndexOutputOrigin[i] = 0.5*(m_ShrinkFactors[i]-1);
    // outputStartIndex[i] =
    //   Math::Ceil<SizeValueType>(inputStartIndex[i]/static_cast<double>( m_ShrinkFactors[i]) );
    // outputSize[i] = Math::Floor<SizeValueType>(
    //     static_cast<double>( inputSize[i] -
    //       outputStartIndex[i]*m_ShrinkFactors[i]+inputStartIndex[i])
    //     / static_cast<double>(m_ShrinkFactors[i])
    //     );
    outputStartIndex[i] = inputStartIndex[i];
    outputSize[i] =
      Math::Floor<SizeValueType>(static_cast<double>(inputSize[i]) / static_cast<double>(m_ShrinkFactors[i]));

    if (outputSize[i] < 1)
    {
      itkExceptionMacro("InputImage is too small! An output pixel does not map to a whole input bin.");
    }
  }

  // inputPtr->TransformContinuousIndexToPhysicalPoint(inputIndexOutputOrigin, outputOrigin);
  outputOrigin = inputOrigin;

  outputPtr->SetSpacing(outputSpacing);
  outputPtr->SetOrigin(outputOrigin);

  // Set region
  typename TImageType::RegionType outputLargestPossibleRegion;
  outputLargestPossibleRegion.SetSize(outputSize);
  outputLargestPossibleRegion.SetIndex(outputStartIndex);

  outputPtr->SetLargestPossibleRegion(outputLargestPossibleRegion);
}

template <class TImageType>
void
FrequencyShrinkImageFilter<TImageType>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Shrink Factor: ";
  for (unsigned int j = 0; j < ImageDimension; j++)
  {
    os << m_ShrinkFactors[j] << " ";
  }
  os << std::endl;
  os << "ApplyBandFilter: " << this->m_ApplyBandFilter << std::endl;

  itkPrintSelfObjectMacro(FrequencyBandFilter);
}
} // end namespace itk

#endif
