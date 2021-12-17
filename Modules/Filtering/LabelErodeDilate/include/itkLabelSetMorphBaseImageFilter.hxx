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
#ifndef itkLabelSetMorphBaseImageFilter_hxx
#define itkLabelSetMorphBaseImageFilter_hxx

#include "itkImageRegionConstIterator.h"
#include "itkImageRegionIterator.h"

#include "itkImageLinearIteratorWithIndex.h"
#include "itkImageLinearConstIteratorWithIndex.h"

#include "itkLabelSetUtils.h"
#include "itkImageFileWriter.h"

namespace itk
{
template <typename TInputImage, bool doDilate, typename TOutputImage>
LabelSetMorphBaseImageFilter<TInputImage, doDilate, TOutputImage>::LabelSetMorphBaseImageFilter()
{
  this->SetNumberOfRequiredOutputs(1);
  this->SetNumberOfRequiredInputs(1);
  // needs to be selected according to erosion/dilation

  m_DistanceImage = DistanceImageType::New();

  if (doDilate)
  {
    m_Extreme = NumericTraits<RealType>::NonpositiveMin();
    m_MagnitudeSign = 1;
  }
  else
  {
    m_Extreme = NumericTraits<RealType>::max();
    m_MagnitudeSign = -1;
  }
  m_UseImageSpacing = false;

  this->SetRadius(1);

  this->DynamicMultiThreadingOff();
}

template <typename TInputImage, bool doDilate, typename TOutputImage>
void
LabelSetMorphBaseImageFilter<TInputImage, doDilate, TOutputImage>::ThreadedGenerateData(const OutputImageRegionType &,
                                                                                        ThreadIdType)
{}

template <typename TInputImage, bool doDilate, typename TOutputImage>
RegionIndexType
LabelSetMorphBaseImageFilter<TInputImage, doDilate, TOutputImage>::SplitRequestedRegion(
  RegionIndexType         i,
  RegionIndexType         num,
  OutputImageRegionType & splitRegion)
{
  // Get the output pointer
  OutputImageType * outputPtr = this->GetOutput();

  // Initialize the splitRegion to the output requested region
  splitRegion = outputPtr->GetRequestedRegion();

  const OutputSizeType & requestedRegionSize = splitRegion.GetSize();

  OutputIndexType splitIndex = splitRegion.GetIndex();
  OutputSizeType  splitSize = splitRegion.GetSize();

  // split on the outermost dimension available
  // and avoid the current dimension
  int splitAxis = static_cast<int>(outputPtr->GetImageDimension()) - 1;
  while ((requestedRegionSize[splitAxis] == 1) || (splitAxis == static_cast<int>(m_CurrentDimension)))
  {
    --splitAxis;
    if (splitAxis < 0)
    { // cannot split
      itkDebugMacro("Cannot Split");
      return 1;
    }
  }

  // determine the actual number of pieces that will be generated
  auto range = static_cast<double>(requestedRegionSize[splitAxis]);

  auto         valuesPerThread = static_cast<unsigned int>(std::ceil(range / static_cast<double>(num)));
  unsigned int maxThreadIdUsed = static_cast<unsigned int>(std::ceil(range / static_cast<double>(valuesPerThread))) - 1;

  // Split the region
  if (i < maxThreadIdUsed)
  {
    splitIndex[splitAxis] += i * valuesPerThread;
    splitSize[splitAxis] = valuesPerThread;
  }
  if (i == maxThreadIdUsed)
  {
    splitIndex[splitAxis] += i * valuesPerThread;
    // last thread needs to process the "rest" dimension being split
    splitSize[splitAxis] = splitSize[splitAxis] - i * valuesPerThread;
  }

  // set the split region ivars
  splitRegion.SetIndex(splitIndex);
  splitRegion.SetSize(splitSize);

  itkDebugMacro("Split Piece: " << splitRegion);

  return maxThreadIdUsed + 1;
}

template <typename TInputImage, bool doDilate, typename TOutputImage>
void
LabelSetMorphBaseImageFilter<TInputImage, doDilate, TOutputImage>::SetRadius(ScalarRealType radius)
{
  RadiusType s;

  s.Fill(radius);
  this->SetRadius(s);
}

template <typename TInputImage, bool doDilate, typename TOutputImage>
void
LabelSetMorphBaseImageFilter<TInputImage, doDilate, TOutputImage>::EnlargeOutputRequestedRegion(DataObject * output)
{
  auto * out = dynamic_cast<TOutputImage *>(output);

  if (out)
  {
    out->SetRequestedRegion(out->GetLargestPossibleRegion());
  }
}

template <typename TInputImage, bool doDilate, typename TOutputImage>
void
LabelSetMorphBaseImageFilter<TInputImage, doDilate, TOutputImage>::GenerateData()
{
  ThreadIdType nbthreads = this->GetNumberOfWorkUnits();

  typename TInputImage::ConstPointer inputImage(this->GetInput());
  typename TOutputImage::Pointer     outputImage(this->GetOutput());

  this->AllocateOutputs();

  m_DistanceImage->SetBufferedRegion(outputImage->GetRequestedRegion());
  m_DistanceImage->Allocate();
  m_DistanceImage->FillBuffer(0);
  m_DistanceImage->CopyInformation(inputImage);

  if (this->GetUseImageSpacing())
  {
    // radius is in mm
    for (unsigned P = 0; P < InputImageType::ImageDimension; P++)
    {
      m_Scale[P] = 0.5 * m_Radius[P] * m_Radius[P];
    }
  }
  else
  {
    // this gives us a little bit of a margin
    for (unsigned P = 0; P < InputImageType::ImageDimension; P++)
    {
      m_Scale[P] = (0.5 * m_Radius[P] * m_Radius[P] + 1);
    }
  }

  // set up the scaling parameter
  // first non zero element of m_Scale sets the value used the first
  // active pass over the image.
  // Subsequent non zero values are scaled by the first non zero
  // value to support elliptical operations.
  // The first value needs to be recorded for use by the erosion operation.
  unsigned firstval = 0;
  for (unsigned P = 0; P < InputImageType::ImageDimension; P++)
  {
    if (m_Radius[P] != 0)
    {
      firstval = P;
      break;
    }
  }
  m_BaseSigma = m_Scale[firstval];
  for (unsigned P = firstval + 1; P < InputImageType::ImageDimension; P++)
  {
    m_Scale[P] = m_Scale[P] / m_Scale[firstval];
  }

  m_FirstPassDone = false;

  // Set up the multithreaded processing
  typename ImageSource<TOutputImage>::ThreadStruct str;
  str.Filter = this;
  ProcessObject::MultiThreaderType * multithreader = this->GetMultiThreader();
  multithreader->SetNumberOfWorkUnits(nbthreads);
  multithreader->SetSingleMethod(this->ThreaderCallback, &str);

  // multithread the execution
  for (unsigned int d = 0; d < ImageDimension; d++)
  {
    m_CurrentDimension = d;
    multithreader->SingleMethodExecute();
    if (this->m_Scale[m_CurrentDimension] > 0)
    {
      // needs to be set outside the multithreaded code
      // first pass is completed as soon as we hit a structuring
      // element dimension that is non zero.
      m_FirstPassDone = true;
    }
  }
}

template <typename TInputImage, bool doDilate, typename TOutputImage>
void
LabelSetMorphBaseImageFilter<TInputImage, doDilate, TOutputImage>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  if (m_UseImageSpacing)
  {
    os << "Scale in world units: " << m_Radius << std::endl;
  }
  else
  {
    os << "Scale in voxels: " << m_Radius << std::endl;
  }
}

template <typename TInputImage, bool doDilate, typename TOutputImage>
void
LabelSetMorphBaseImageFilter<TInputImage, doDilate, TOutputImage>::writeDist(std::string fname)
{
  using WriterType = typename itk::ImageFileWriter<DistanceImageType>;
  typename WriterType::Pointer writer = WriterType::New();
  writer->SetInput(m_DistanceImage);
  writer->SetFileName(fname.c_str());
  writer->Update();
}
} // namespace itk
#endif
