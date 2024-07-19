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
#ifndef itkParabolicOpenCloseImageFilter_hxx
#define itkParabolicOpenCloseImageFilter_hxx

#include "itkImageRegionConstIterator.h"
#include "itkImageRegionIterator.h"

// #define NOINDEX
#ifndef NOINDEX
#  include "itkImageLinearIteratorWithIndex.h"
#  include "itkImageLinearConstIteratorWithIndex.h"
#else
#  include "itkImageLinearIterator.h"
#  include "itkImageLinearConstIterator.h"
#endif
#include "itkStatisticsImageFilter.h"
#include "itkParabolicMorphUtils.h"

namespace itk
{
template <typename TInputImage, bool DoOpen, typename TOutputImage>
ParabolicOpenCloseImageFilter<TInputImage, DoOpen, TOutputImage>::ParabolicOpenCloseImageFilter()
{
  this->SetNumberOfRequiredOutputs(1);
  this->SetNumberOfRequiredInputs(1);
  // needs to be selected according to erosion/dilation
  m_UseImageSpacing = false;
  m_ParabolicAlgorithm = INTERSECTION;
  m_Stage = 1; // indicate whether we are on the first pass or the
  // second

  this->DynamicMultiThreadingOff();
}

template <typename TInputImage, bool DoOpen, typename TOutputImage>
unsigned int
ParabolicOpenCloseImageFilter<TInputImage, DoOpen, TOutputImage>::SplitRequestedRegion(
  unsigned int            i,
  unsigned int            num,
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

template <typename TInputImage, bool DoOpen, typename TOutputImage>
void
ParabolicOpenCloseImageFilter<TInputImage, DoOpen, TOutputImage>::SetScale(ScalarRealType scale)
{
  RadiusType s;

  s.Fill(scale);
  this->SetScale(s);
}

#if 1
template <typename TInputImage, bool DoOpen, typename TOutputImage>
void
ParabolicOpenCloseImageFilter<TInputImage, DoOpen, TOutputImage>::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method. this should
  // copy the output requested region to the input requested region
  Superclass::GenerateInputRequestedRegion();

  // This filter needs all of the input
  InputImagePointer image = const_cast<InputImageType *>(this->GetInput());
  if (image)
  {
    image->SetRequestedRegion(this->GetInput()->GetLargestPossibleRegion());
  }
}

#endif

#if 1
template <typename TInputImage, bool DoOpen, typename TOutputImage>
void
ParabolicOpenCloseImageFilter<TInputImage, DoOpen, TOutputImage>::EnlargeOutputRequestedRegion(DataObject * output)
{
  auto * out = dynamic_cast<TOutputImage *>(output);

  if (out)
  {
    out->SetRequestedRegion(out->GetLargestPossibleRegion());
  }
}

#endif

template <typename TInputImage, bool DoOpen, typename TOutputImage>
void
ParabolicOpenCloseImageFilter<TInputImage, DoOpen, TOutputImage>::GenerateData()
{
  ThreadIdType nbthreads = this->GetNumberOfWorkUnits();

  //  using InputConstIteratorType = ImageLinearConstIteratorWithIndex< TInputImage  > ;
  //  using OutputIteratorType = ImageLinearIteratorWithIndex< TOutputImage >;

  // for stages after the first
  // using OutputConstIteratorType = ImageLinearConstIteratorWithIndex< TOutputImage  > ;

  //  using RegionType = ImageRegion< TInputImage::ImageDimension >;

  typename TInputImage::ConstPointer inputImage(this->GetInput());
  typename TOutputImage::Pointer     outputImage(this->GetOutput());

  // const unsigned int imageDimension = inputImage->GetImageDimension();

  outputImage->SetBufferedRegion(outputImage->GetRequestedRegion());
  outputImage->Allocate();

  typename ImageSource<OutputImageType>::ThreadStruct str;
  str.Filter = this;

  ProcessObject::MultiThreaderType * multithreader = this->GetMultiThreader();
  multithreader->SetNumberOfWorkUnits(nbthreads);
  multithreader->SetSingleMethod(this->ThreaderCallback, &str);

  // multithread the execution

  // multithread the execution - stage 1
  m_Stage = 1;

  for (unsigned int d = 0; d < ImageDimension; d++)
  {
    m_CurrentDimension = d;
    multithreader->SingleMethodExecute();
  }

  // multithread the execution - stage 2
  m_Stage = 2;
  for (unsigned int d = 0; d < ImageDimension; d++)
  {
    m_CurrentDimension = d;
    multithreader->SingleMethodExecute();
  }

  m_Stage = 1;

#if 0
  // Set up the multithreaded processing
  typename ImageSource< TOutputImage >::ThreadStruct str;
  str.Filter = this;
  this->GetMultiThreader()->SetNumberOfWorkUnits( this->GetNumberOfWorkUnits() );
  this->GetMultiThreader()->SetSingleMethod(this->ThreaderCallback, &str);

  // multithread the execution - stage 1
  m_Stage = 1;
  for ( unsigned int d = 0; d < ImageDimension; d++ )
    {
    m_CurrentDimension = d;
    this->GetMultiThreader()->SingleMethodExecute();
    }
  // swap over the parameters controlling erosion/dilation
  m_Extreme = m_Extreme2;
  m_MagnitudeSign = m_MagnitudeSign2;

  // multithread the execution - stage 2
  m_Stage = 2;
  for ( unsigned int d = 0; d < ImageDimension; d++ )
    {
    m_CurrentDimension = d;
    this->GetMultiThreader()->SingleMethodExecute();
    }
  // swap them back
  m_Extreme = m_Extreme1;
  m_MagnitudeSign = m_MagnitudeSign1;
  m_Stage = 1;
#endif
}

////////////////////////////////////////////////////////////

template <typename TInputImage, bool DoOpen, typename TOutputImage>
void
ParabolicOpenCloseImageFilter<TInputImage, DoOpen, TOutputImage>::ThreadedGenerateData(
  const OutputImageRegionType & outputRegionForThread,
  ThreadIdType                  threadId)
{
  // compute the number of rows first, so we can setup a progress reporter
  unsigned int  numberOfRows = 1;
  InputSizeType size = outputRegionForThread.GetSize();

  for (unsigned int d = 0; d < InputImageDimension; ++d)
  {
    if (d != m_CurrentDimension)
    {
      numberOfRows *= size[d];
    }
  }

  float progressPerDimension = 1.0 / ImageDimension;

  ProgressReporter progress(
    this, threadId, numberOfRows, 30, m_CurrentDimension * progressPerDimension, progressPerDimension);

  using InputConstIteratorType = ImageLinearConstIteratorWithIndex<TInputImage>;
  using OutputIteratorType = ImageLinearIteratorWithIndex<TOutputImage>;

  // for stages after the first
  using OutputConstIteratorType = ImageLinearConstIteratorWithIndex<TOutputImage>;

  using RegionType = ImageRegion<TInputImage::ImageDimension>;

  typename TInputImage::ConstPointer inputImage(this->GetInput());
  typename TOutputImage::Pointer     outputImage(this->GetOutput());

  // const unsigned int imageDimension = inputImage->GetImageDimension();

  // outputImage->SetBufferedRegion( outputImage->GetRequestedRegion() );
  // outputImage->Allocate();
  RegionType region = outputRegionForThread;

  InputConstIteratorType  inputIterator(inputImage, region);
  OutputIteratorType      outputIterator(outputImage, region);
  OutputConstIteratorType inputIteratorStage2(outputImage, region);

  if (m_Stage == 1)
  {
    // deal with the first dimension - this should be copied to the
    // output if the scale is 0
    if (m_CurrentDimension == 0)
    {
      if (m_Scale[0] > 0)
      {
        // Perform as normal
        //     RealType magnitude = 1.0/(2.0 * m_Scale[0]);
        unsigned long LineLength = region.GetSize()[0];
        RealType      image_scale = this->GetInput()->GetSpacing()[0];

        doOneDimension<InputConstIteratorType, OutputIteratorType, RealType, PixelType, OutputPixelType, !DoOpen>(
          inputIterator,
          outputIterator,
          progress,
          LineLength,
          0,
          this->m_UseImageSpacing,
          image_scale,
          this->m_Scale[0],
          m_ParabolicAlgorithm);
      }
      else
      {
        // copy to output
        using InItType = ImageRegionConstIterator<TInputImage>;
        using OutItType = ImageRegionIterator<TOutputImage>;

        InItType  InIt(inputImage, region);
        OutItType OutIt(outputImage, region);
        while (!InIt.IsAtEnd())
        {
          OutIt.Set(static_cast<OutputPixelType>(InIt.Get()));
          ++InIt;
          ++OutIt;
        }
      }
    }
    else
    {
      if (m_Scale[m_CurrentDimension] > 0)
      {
        // now deal with the other dimensions for first stage
        unsigned long LineLength = region.GetSize()[m_CurrentDimension];
        RealType      image_scale = this->GetInput()->GetSpacing()[m_CurrentDimension];

        doOneDimension<OutputConstIteratorType, OutputIteratorType, RealType, PixelType, OutputPixelType, !DoOpen>(
          inputIteratorStage2,
          outputIterator,
          progress,
          LineLength,
          m_CurrentDimension,
          this->m_UseImageSpacing,
          image_scale,
          this->m_Scale[m_CurrentDimension],
          m_ParabolicAlgorithm);
      }
    }
  }
  else
  {
    // deal with the other dimensions for second stage
    if (m_Scale[m_CurrentDimension] > 0)
    {
      // RealType magnitude = 1.0/(2.0 * m_Scale[dd]);
      unsigned long LineLength = region.GetSize()[m_CurrentDimension];
      RealType      image_scale = this->GetInput()->GetSpacing()[m_CurrentDimension];

      doOneDimension<OutputConstIteratorType, OutputIteratorType, RealType, PixelType, OutputPixelType, DoOpen>(
        inputIteratorStage2,
        outputIterator,
        progress,
        LineLength,
        m_CurrentDimension,
        this->m_UseImageSpacing,
        image_scale,
        this->m_Scale[m_CurrentDimension],
        m_ParabolicAlgorithm);
    }
  }
}

template <typename TInputImage, bool DoOpen, typename TOutputImage>
void
ParabolicOpenCloseImageFilter<TInputImage, DoOpen, TOutputImage>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  if (m_UseImageSpacing)
  {
    os << "Scale in world units: " << m_Scale << std::endl;
  }
  else
  {
    os << "Scale in voxels: " << m_Scale << std::endl;
  }
}
} // namespace itk
#endif
