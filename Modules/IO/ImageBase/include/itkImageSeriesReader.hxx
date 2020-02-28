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
#ifndef itkImageSeriesReader_hxx
#define itkImageSeriesReader_hxx

#include "itkImageSeriesReader.h"

#include "itkImageAlgorithm.h"
#include "itkArray.h"
#include "itkVector.h"
#include "itkMath.h"
#include "itkProgressReporter.h"
#include "itkMetaDataObject.h"
#include <iomanip>

namespace itk
{
// Destructor
template <typename TOutputImage>
ImageSeriesReader<TOutputImage>::~ImageSeriesReader()
{
  // Clear the eventual previous content of the MetaDictionary array
  if (!m_MetaDataDictionaryArray.empty())
  {
    for (auto & i : m_MetaDataDictionaryArray)
    {
      // each element is a raw pointer, delete them.
      delete i;
    }
  }
  m_MetaDataDictionaryArray.clear();
}

template <typename TOutputImage>
void
ImageSeriesReader<TOutputImage>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "ReverseOrder: " << m_ReverseOrder << std::endl;
  os << indent << "ForceOrthogonalDirection: " << m_ForceOrthogonalDirection << std::endl;
  os << indent << "UseStreaming: " << m_UseStreaming << std::endl;

  itkPrintSelfObjectMacro(ImageIO);

  os << indent << "MetaDataDictionaryArrayMTime: " << m_MetaDataDictionaryArrayMTime << std::endl;
  os << indent << "MetaDataDictionaryArrayUpdate: " << m_MetaDataDictionaryArrayUpdate << std::endl;
}

template <typename TOutputImage>
int
ImageSeriesReader<TOutputImage>::ComputeMovingDimensionIndex(ReaderType * reader)
{
  // This method computes the the dimension index which we are going
  // to be moving in for slices

  unsigned int movingDimension = reader->GetImageIO()->GetNumberOfDimensions();

  if (movingDimension > TOutputImage::ImageDimension - 1)
  {
    movingDimension = TOutputImage::ImageDimension - 1;
  }

  const TOutputImage * first = reader->GetOutput();
  SizeType             dimSize = first->GetLargestPossibleRegion().GetSize();

  // collapse the number of dimensions in image if any of the last
  // dimensions are one
  while (movingDimension > 0 && dimSize[movingDimension - 1] == 1)
  {
    --movingDimension;
  }

  return movingDimension;
}

template <typename TOutputImage>
void
ImageSeriesReader<TOutputImage>::GenerateOutputInformation()
{
  typename TOutputImage::Pointer output = this->GetOutput();

  using SpacingScalarType = typename TOutputImage::SpacingValueType;
  Array<SpacingScalarType> position1(TOutputImage::ImageDimension);
  position1.Fill(0.0f);
  Array<SpacingScalarType> positionN(TOutputImage::ImageDimension);
  positionN.Fill(0.0f);

  std::string key("ITK_ImageOrigin");

  // Clear the previous content of the MetaDictionary array
  if (!m_MetaDataDictionaryArray.empty())
  {
    for (auto & i : m_MetaDataDictionaryArray)
    {
      // each element is a raw pointer, delete them.
      delete i;
    }
  }
  m_MetaDataDictionaryArray.clear();

  const auto numberOfFiles = static_cast<int>(m_FileNames.size());
  if (numberOfFiles == 0)
  {
    itkExceptionMacro(<< "At least one filename is required.");
  }

  const int firstFileName = (m_ReverseOrder ? numberOfFiles - 1 : 0);
  const int lastFileName = (m_ReverseOrder ? 0 : numberOfFiles - 1);

  typename ReaderType::Pointer firstReader = ReaderType::New();
  typename ReaderType::Pointer lastReader = ReaderType::New();
  firstReader->SetFileName(m_FileNames[firstFileName].c_str());
  lastReader->SetFileName(m_FileNames[lastFileName].c_str());
  if (m_ImageIO)
  {
    firstReader->SetImageIO(m_ImageIO);
    lastReader->SetImageIO(m_ImageIO);
  }

  // update the MetaDataDictionary and output information
  firstReader->UpdateOutputInformation();
  const TOutputImage * first = firstReader->GetOutput();

  typename TOutputImage::SpacingType   spacing = first->GetSpacing();
  typename TOutputImage::PointType     origin = first->GetOrigin();
  typename TOutputImage::DirectionType direction = first->GetDirection();
  ImageRegionType                      largestRegion = first->GetLargestPossibleRegion();
  unsigned int                         numberOfComponents = first->GetNumberOfComponentsPerPixel();

  if (numberOfFiles == 1)
  {
    // just retain all of the file's meta data

    // the slice moving direction for a single image can be the
    // output image dimensions, since this will indicate that we can
    // not move in the slice moving direction
    this->m_NumberOfDimensionsInImage = firstReader->GetImageIO()->GetNumberOfDimensions();
    if (this->m_NumberOfDimensionsInImage > TOutputImage::ImageDimension)
    {
      this->m_NumberOfDimensionsInImage = TOutputImage::ImageDimension;
    }
  }
  else
  {
    // first of multiple slices
    spacing = first->GetSpacing();
    direction = first->GetDirection();
    numberOfComponents = first->GetNumberOfComponentsPerPixel();
    SizeType dimSize = largestRegion.GetSize();

    // compute the moving dimensions index, or the number of image
    // dimensions we are going to use
    this->m_NumberOfDimensionsInImage = ComputeMovingDimensionIndex(firstReader);
    dimSize[this->m_NumberOfDimensionsInImage] = static_cast<typename SizeType::SizeValueType>(numberOfFiles);
    IndexType start;
    start.Fill(0);
    largestRegion.SetSize(dimSize);
    largestRegion.SetIndex(start);

    // Initialize the position to the origin returned by the reader
    unsigned int j;
    for (j = 0; j < TOutputImage::ImageDimension; j++)
    {
      position1[j] = static_cast<SpacingScalarType>(origin[j]);
    }
    // Override the position if there is an ITK_ImageOrigin
    ExposeMetaData<Array<SpacingScalarType>>(firstReader->GetImageIO()->GetMetaDataDictionary(), key, position1);


    // last of multiple slices
    lastReader->UpdateOutputInformation();
    const TOutputImage * last = lastReader->GetOutput();

    // Initialize the position to the origin returned by the reader
    for (j = 0; j < TOutputImage::ImageDimension; j++)
    {
      positionN[j] = static_cast<SpacingScalarType>(last->GetOrigin()[j]);
    }
    // Override the position if there is an ITK_ImageOrigin
    ExposeMetaData<Array<SpacingScalarType>>(lastReader->GetImageIO()->GetMetaDataDictionary(), key, positionN);

    // Compute and set the inter slice spacing
    // and last (usually third) axis of direction
    Vector<SpacingScalarType, TOutputImage::ImageDimension> dirN;
    for (j = 0; j < TOutputImage::ImageDimension; ++j)
    {
      dirN[j] = positionN[j] - position1[j];
    }
    SpacingScalarType dirNnorm = dirN.GetNorm();
    if (Math::AlmostEquals(dirNnorm, 0.0))
    {
      spacing[this->m_NumberOfDimensionsInImage] = 1.0;
      this->m_SpacingDefined = false;
    }
    else
    {
      spacing[this->m_NumberOfDimensionsInImage] = dirNnorm / (numberOfFiles - 1);
      this->m_SpacingDefined = true;
      if (!m_ForceOrthogonalDirection)
      {
        for (j = 0; j < TOutputImage::ImageDimension; ++j)
        {
          direction[j][this->m_NumberOfDimensionsInImage] = dirN[j] / dirNnorm;
        }
      }
    }
  }

  output->SetOrigin(origin);
  output->SetSpacing(spacing);
  output->SetDirection(direction);
  output->SetLargestPossibleRegion(largestRegion);

  // If a VectorImage, this requires us to set the VectorLength before allocate
  if (strcmp(output->GetNameOfClass(), "VectorImage") == 0)
  {
    using AccessorFunctorType = typename TOutputImage::AccessorFunctorType;
    AccessorFunctorType::SetVectorLength(output, numberOfComponents);
  }
}

template <typename TOutputImage>
void
ImageSeriesReader<TOutputImage>::EnlargeOutputRequestedRegion(DataObject * output)
{
  typename TOutputImage::Pointer out = dynamic_cast<TOutputImage *>(output);
  ImageRegionType                requestedRegion = out->GetRequestedRegion();
  ImageRegionType                largestRegion = out->GetLargestPossibleRegion();

  if (m_UseStreaming)
  {
    out->SetRequestedRegion(requestedRegion);
  }
  else
  {
    out->SetRequestedRegion(largestRegion);
  }
}

template <typename TOutputImage>
void
ImageSeriesReader<TOutputImage>::GenerateData()
{
  TOutputImage * output = this->GetOutput();

  ImageRegionType requestedRegion = output->GetRequestedRegion();
  ImageRegionType largestRegion = output->GetLargestPossibleRegion();
  ImageRegionType sliceRegionToRequest = output->GetRequestedRegion();

  // Each file must have the same size.
  SizeType validSize = largestRegion.GetSize();

  // If more than one file is being read, then the input dimension
  // will be less than the output dimension.  In this case, set
  // the last dimension that is other than 1 of validSize to 1.  However, if the
  // input and output have the same number of dimensions, this should
  // not be done because it will lower the dimension of the output image.
  if (TOutputImage::ImageDimension != this->m_NumberOfDimensionsInImage)
  {
    validSize[this->m_NumberOfDimensionsInImage] = 1;
    sliceRegionToRequest.SetSize(this->m_NumberOfDimensionsInImage, 1);
    sliceRegionToRequest.SetIndex(this->m_NumberOfDimensionsInImage, 0);
  }

  // Allocate the output buffer
  output->SetBufferedRegion(requestedRegion);
  output->Allocate();

  // progress reported on a per slice basis
  ProgressReporter progress(this, 0, requestedRegion.GetSize(TOutputImage::ImageDimension - 1), 100);

  // We utilize the modified time of the output information to
  // know when the meta array needs to be updated, when the output
  // information is updated so should the meta array.
  // Each file can not be read in the UpdateOutputInformation methods
  // due to the poor performance of reading each file a second time there.
  bool needToUpdateMetaDataDictionaryArray =
    this->m_OutputInformationMTime > this->m_MetaDataDictionaryArrayMTime && m_MetaDataDictionaryArrayUpdate;

  typename TOutputImage::InternalPixelType * outputBuffer = output->GetBufferPointer();
  IndexType                                  sliceStartIndex = requestedRegion.GetIndex();
  const auto                                 numberOfFiles = static_cast<int>(m_FileNames.size());

  typename TOutputImage::PointType   prevSliceOrigin = output->GetOrigin();
  typename TOutputImage::SpacingType outputSpacing = output->GetSpacing();
  double                             maxSpacingDeviation = 0.0;
  bool                               prevSliceIsValid = false;

  for (int i = 0; i != numberOfFiles; ++i)
  {
    if (TOutputImage::ImageDimension != this->m_NumberOfDimensionsInImage)
    {
      sliceStartIndex[this->m_NumberOfDimensionsInImage] = i;
    }

    const bool insideRequestedRegion = requestedRegion.IsInside(sliceStartIndex);
    const int  iFileName = (m_ReverseOrder ? numberOfFiles - i - 1 : i);
    bool       nonUniformSampling = false;
    double     spacingDeviation = 0.0;

    // check if we need this slice
    if (!insideRequestedRegion && !needToUpdateMetaDataDictionaryArray)
    {
      continue;
    }

    // configure reader
    typename ReaderType::Pointer reader = ReaderType::New();
    reader->SetFileName(m_FileNames[iFileName].c_str());

    TOutputImage * readerOutput = reader->GetOutput();

    if (m_ImageIO)
    {
      reader->SetImageIO(m_ImageIO);
    }
    reader->SetUseStreaming(m_UseStreaming);
    readerOutput->SetRequestedRegion(sliceRegionToRequest);

    // update the data or info
    if (!insideRequestedRegion)
    {
      reader->UpdateOutputInformation();
    }
    else
    {
      // read the meta data information
      readerOutput->UpdateOutputInformation();

      // propagate the requested region to determin what the region
      // will actually be read
      readerOutput->PropagateRequestedRegion();

      // check that the size of each slice is the same
      if (readerOutput->GetLargestPossibleRegion().GetSize() != validSize)
      {
        itkExceptionMacro(<< "Size mismatch! The size of  " << m_FileNames[iFileName].c_str() << " is "
                          << readerOutput->GetLargestPossibleRegion().GetSize()
                          << " and does not match the required size " << validSize << " from file "
                          << m_FileNames[m_ReverseOrder ? numberOfFiles - 1 : 0].c_str());
      }

      // get the size of the region to be read
      SizeType readSize = readerOutput->GetRequestedRegion().GetSize();

      if (readSize == sliceRegionToRequest.GetSize())
      {
        // if the buffer of the ImageReader is going to match that of
        // ourselves, then set the ImageReader's buffer to a section
        // of ours

        const size_t numberOfPixelsInSlice = sliceRegionToRequest.GetNumberOfPixels();

        using AccessorFunctorType = typename TOutputImage::AccessorFunctorType;
        const size_t numberOfInternalComponentsPerPixel = AccessorFunctorType::GetVectorLength(output);


        const ptrdiff_t sliceOffset = (TOutputImage::ImageDimension != this->m_NumberOfDimensionsInImage)
                                        ? (i - requestedRegion.GetIndex(this->m_NumberOfDimensionsInImage))
                                        : 0;

        const ptrdiff_t numberOfPixelComponentsUpToSlice =
          numberOfPixelsInSlice * numberOfInternalComponentsPerPixel * sliceOffset;
        const bool bufferDelete = false;

        typename TOutputImage::InternalPixelType * outputSliceBuffer = outputBuffer + numberOfPixelComponentsUpToSlice;

        if (strcmp(output->GetNameOfClass(), "VectorImage") == 0)
        {
          // if the input image type is a vector image then the number
          // of components needs to be set for the size
          readerOutput->GetPixelContainer()->SetImportPointer(
            outputSliceBuffer,
            static_cast<unsigned long>(numberOfPixelsInSlice * numberOfInternalComponentsPerPixel),
            bufferDelete);
        }
        else
        {
          // otherwise the actual number of pixels needs to be passed
          readerOutput->GetPixelContainer()->SetImportPointer(
            outputSliceBuffer, static_cast<unsigned long>(numberOfPixelsInSlice), bufferDelete);
        }
        readerOutput->UpdateOutputData();
      }
      else
      {
        // the read region isn't going to match exactly what we need
        // to update to buffer created by the reader, then copy

        reader->Update();

        // output of buffer copy
        ImageRegionType outRegion = requestedRegion;
        outRegion.SetIndex(sliceStartIndex);

        // set the moving dimension to a size of 1
        if (TOutputImage::ImageDimension != this->m_NumberOfDimensionsInImage)
        {
          outRegion.SetSize(this->m_NumberOfDimensionsInImage, 1);
        }

        ImageAlgorithm::Copy(readerOutput, output, sliceRegionToRequest, outRegion);
      }

      // verify that slice spacing is the expected one
      // since we can be skipping some slices because they are outside of requested region
      // I am using additional variable
      if (prevSliceIsValid)
      {
        typename TOutputImage::PointType sliceOrigin = readerOutput->GetOrigin();
        using SpacingScalarType = typename TOutputImage::SpacingValueType;
        Vector<SpacingScalarType, TOutputImage::ImageDimension> dirN;
        for (size_t j = 0; j < TOutputImage::ImageDimension; ++j)
        {
          dirN[j] = static_cast<SpacingScalarType>(sliceOrigin[j]) - static_cast<SpacingScalarType>(prevSliceOrigin[j]);
        }
        SpacingScalarType dirNnorm = dirN.GetNorm();

        if (this->m_SpacingDefined &&
            !Math::AlmostEquals(
              dirNnorm,
              outputSpacing[this->m_NumberOfDimensionsInImage])) // either non-uniform sampling or missing slice
        {
          nonUniformSampling = true;
          spacingDeviation = Math::abs(outputSpacing[this->m_NumberOfDimensionsInImage] - dirNnorm);
          if (spacingDeviation > maxSpacingDeviation)
          {
            maxSpacingDeviation = spacingDeviation;
          }

          needToUpdateMetaDataDictionaryArray = true;
        }
        prevSliceOrigin = sliceOrigin;
      }
      else
      {
        prevSliceOrigin = readerOutput->GetOrigin();
        prevSliceIsValid = true;
      }

      // report progress for read slices
      progress.CompletedPixel();
    } // end !insidedRequestedRegion

    // Deep copy the MetaDataDictionary into the array
    if (reader->GetImageIO() && needToUpdateMetaDataDictionaryArray)
    {
      auto newDictionary = new DictionaryType;
      *newDictionary = reader->GetImageIO()->GetMetaDataDictionary();
      if (nonUniformSampling)
      {
        // slice-specific information
        EncapsulateMetaData<double>(*newDictionary, "ITK_non_uniform_sampling_deviation", spacingDeviation);
      }
      m_MetaDataDictionaryArray.push_back(newDictionary);
    }
  } // end per slice loop


  if (TOutputImage::ImageDimension != this->m_NumberOfDimensionsInImage &&
      maxSpacingDeviation > m_SpacingWarningRelThreshold * outputSpacing[this->m_NumberOfDimensionsInImage])
  {
    itkWarningMacro(<< "Non uniform sampling or missing slices detected,  maximum nonuniformity:"
                    << maxSpacingDeviation);
  }
  if (maxSpacingDeviation > 0.0)
  {
    EncapsulateMetaData<double>(output->GetMetaDataDictionary(),
                                "ITK_non_uniform_sampling_deviation",
                                maxSpacingDeviation); // maximum deviation
  }


  // update the time if we modified the meta array
  if (needToUpdateMetaDataDictionaryArray)
  {
    m_MetaDataDictionaryArrayMTime.Modified();
  }
}

template <typename TOutputImage>
typename ImageSeriesReader<TOutputImage>::DictionaryArrayRawPointer
ImageSeriesReader<TOutputImage>::GetMetaDataDictionaryArray() const
{
  // this warning has been introduced in 3.17 due to a change in
  // behavior. It may be removed in the future.
  if (this->m_OutputInformationMTime > this->m_MetaDataDictionaryArrayMTime)
  {
    itkWarningMacro("The MetaDataDictionaryArray is not up to date. This is no longer updated in the "
                    "UpdateOutputInformation method but in GenerateData.");
  }
  return &m_MetaDataDictionaryArray;
}
} // namespace itk

#endif
