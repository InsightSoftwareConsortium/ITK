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
#ifndef itkFFTWInverse1DFFTImageFilter_hxx
#define itkFFTWInverse1DFFTImageFilter_hxx

#include "itkInverse1DFFTImageFilter.hxx"

#include "itkFFTWCommonExtended.h"
#include "itkIndent.h"
#include "itkImageLinearConstIteratorWithIndex.h"
#include "itkImageLinearIteratorWithIndex.h"
#include "itkMetaDataObject.h"
#include "itkMultiThreaderBase.h"

#if defined(ITK_USE_FFTWF) || defined(ITK_USE_FFTWD)

namespace itk
{

template <typename TInputImage, typename TOutputImage>
FFTWInverse1DFFTImageFilter<TInputImage, TOutputImage>::FFTWInverse1DFFTImageFilter()

{
  // We cannot split over the FFT direction
  this->m_ImageRegionSplitter = ImageRegionSplitterDirection::New();
  this->DynamicMultiThreadingOff();
}


template <typename TInputImage, typename TOutputImage>
FFTWInverse1DFFTImageFilter<TInputImage, TOutputImage>::~FFTWInverse1DFFTImageFilter()
{
  if (m_PlanComputed)
  {
    this->DestroyPlans();
  }
}


template <typename TInputImage, typename TOutputImage>
void
FFTWInverse1DFFTImageFilter<TInputImage, TOutputImage>::DestroyPlans()
{
  for (unsigned int i = 0; i < m_PlanArray.size(); i++)
  {
    FFTW1DProxyType::DestroyPlan(m_PlanArray[i]);
    delete[] m_InputBufferArray[i];
    delete[] m_OutputBufferArray[i];
    this->m_PlanComputed = false;
  }
}


template <typename TInputImage, typename TOutputImage>
const ImageRegionSplitterBase *
FFTWInverse1DFFTImageFilter<TInputImage, TOutputImage>::GetImageRegionSplitter() const
{
  return this->m_ImageRegionSplitter.GetPointer();
}


template <typename TInputImage, typename TOutputImage>
void
FFTWInverse1DFFTImageFilter<TInputImage, TOutputImage>::BeforeThreadedGenerateData()
{
  Superclass::BeforeThreadedGenerateData();

  this->m_ImageRegionSplitter->SetDirection(this->GetDirection());

  OutputImageType * outputPtr = this->GetOutput();

  const typename OutputImageType::SizeType & outputSize = outputPtr->GetRequestedRegion().GetSize();
  const unsigned int                         lineSize = outputSize[this->m_Direction];

  if (this->m_PlanComputed)
  {
    // if the image sizes aren't the same,
    // we have to compute the plan again
    if (this->m_LastImageSize != lineSize)
    {
      this->DestroyPlans();
    }
  }
  if (!this->m_PlanComputed)
  {
    const int threads = this->GetNumberOfWorkUnits();
    m_PlanArray.resize(threads);
    m_InputBufferArray.resize(threads);
    m_OutputBufferArray.resize(threads);
    for (int i = 0; i < threads; i++)
    {
      try
      {
        m_InputBufferArray[i] = new typename FFTW1DProxyType::ComplexType[lineSize];
        m_OutputBufferArray[i] = new typename FFTW1DProxyType::ComplexType[lineSize];
      }
      catch (const std::bad_alloc &)
      {
        itkExceptionMacro("Problem allocating memory for internal computations");
      }
      m_PlanArray[i] = FFTW1DProxyType::Plan_dft_1d(
        lineSize, m_InputBufferArray[i], m_OutputBufferArray[i], FFTW_BACKWARD, FFTW_ESTIMATE, 1);
    }
    this->m_LastImageSize = lineSize;
    this->m_PlanComputed = true;
  }
}


template <typename TInputImage, typename TOutputImage>
void
FFTWInverse1DFFTImageFilter<TInputImage, TOutputImage>::ThreadedGenerateData(const OutputImageRegionType & outputRegion,
                                                                             ThreadIdType                  threadID)
{
  // get pointers to the input and output
  const InputImageType * inputPtr = this->GetInput();
  OutputImageType *      outputPtr = this->GetOutput();

  const typename OutputImageType::SizeType & outputSize = outputPtr->GetRequestedRegion().GetSize();
  const unsigned int                         lineSize = outputSize[this->m_Direction];

  using InputIteratorType = itk::ImageLinearConstIteratorWithIndex<InputImageType>;
  using OutputIteratorType = itk::ImageLinearIteratorWithIndex<OutputImageType>;
  InputIteratorType  inputIt(inputPtr, outputRegion);
  OutputIteratorType outputIt(outputPtr, outputRegion);

  inputIt.SetDirection(this->m_Direction);
  outputIt.SetDirection(this->m_Direction);

  typename InputIteratorType::PixelType * inputBufferIt;
  typename FFTW1DProxyType::ComplexType * outputBufferIt;

  // for every fft line
  for (inputIt.GoToBegin(), outputIt.GoToBegin(); !inputIt.IsAtEnd(); outputIt.NextLine(), inputIt.NextLine())
  {
    // copy the input line into our buffer
    inputIt.GoToBeginOfLine();
    inputBufferIt = reinterpret_cast<typename InputIteratorType::PixelType *>(m_InputBufferArray[threadID]);
    while (!inputIt.IsAtEndOfLine())
    {
      *inputBufferIt = inputIt.Get();
      ++inputIt;
      ++inputBufferIt;
    }

    // do the transform
    FFTW1DProxyType::Execute(m_PlanArray[threadID]);

    // copy the output from the buffer into our line
    outputBufferIt = m_OutputBufferArray[threadID];
    outputIt.GoToBeginOfLine();
    while (!outputIt.IsAtEndOfLine())
    {
      outputIt.Set((*outputBufferIt)[0] / lineSize);
      ++outputIt;
      ++outputBufferIt;
    }
  }
}

} // namespace itk

#endif // defined( ITK_USE_FFTWF ) || defined( ITK_USE_FFTWD )

#endif //_itkFFTWInverse1DFFTImageFilter_hxx
