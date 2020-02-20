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

#ifndef itkKrcahEigenToMeasureParameterEstimationFilter_hxx
#define itkKrcahEigenToMeasureParameterEstimationFilter_hxx

#include "itkKrcahEigenToMeasureParameterEstimationFilter.h"

namespace itk
{

template <typename TInputImage, typename TOutputImage>
KrcahEigenToMeasureParameterEstimationFilter<TInputImage, TOutputImage>::KrcahEigenToMeasureParameterEstimationFilter()
  : m_ParameterSet(KrcahImplementationEnum::UseImplementationParameters)
{
  /* Set parameter size to 3 */
  ParameterArrayType parameters = this->GetParametersOutput()->Get();
  parameters.SetSize(3);
  parameters[0] = 0.5;
  parameters[1] = 0.5;
  parameters[2] = 1;
  this->GetParametersOutput()->Set(parameters);
}

template <typename TInputImage, typename TOutputImage>
void
KrcahEigenToMeasureParameterEstimationFilter<TInputImage, TOutputImage>::BeforeThreadedGenerateData()
{
  m_ThreadAccumulatedTrace = NumericTraits<RealType>::ZeroValue();
  m_ThreadCount = NumericTraits<RealType>::ZeroValue();
}

template <typename TInputImage, typename TOutputImage>
void
KrcahEigenToMeasureParameterEstimationFilter<TInputImage, TOutputImage>::AfterThreadedGenerateData()
{
  /* Determine default parameters */
  RealType alpha, beta, gamma;
  switch (m_ParameterSet)
  {
    case KrcahImplementationEnum::UseImplementationParameters:
      alpha = Math::sqrt2 * 0.5f;
      beta = Math::sqrt2 * 0.5f;
      gamma = Math::sqrt2 * 0.5f;
      break;
    case KrcahImplementationEnum::UseJournalParameters:
      alpha = 0.5f;
      beta = 0.5f;
      gamma = 0.25f;
      break;
    default:
      itkExceptionMacro(<< "Have bad parameterset enumeration " << static_cast<char>(m_ParameterSet));
      break;
  }

  /* Do derived measures */
  const RealType accum(m_ThreadAccumulatedTrace);
  const RealType count(m_ThreadCount);
  if (count > 0)
  {
    RealType averageTrace = accum / count;
    gamma = gamma * averageTrace;
  }
  else
  {
    gamma = NumericTraits<RealType>::ZeroValue();
  }

  /* Assign outputs parameters */
  ParameterArrayType parameters;
  parameters.SetSize(3);
  parameters[0] = alpha;
  parameters[1] = beta;
  parameters[2] = gamma;
  this->GetParametersOutput()->Set(parameters);
}

template <typename TInputImage, typename TOutputImage>
void
KrcahEigenToMeasureParameterEstimationFilter<TInputImage, TOutputImage>::DynamicThreadedGenerateData(
  const OutputImageRegionType & outputRegionForThread)
{
  /* If size is zero, return */
  const SizeValueType size0 = outputRegionForThread.GetSize(0);
  if (size0 == 0)
  {
    return;
  }

  /* Determine which function to call */
  RealType (Self::*traceFunction)(InputImagePixelType);
  switch (m_ParameterSet)
  {
    case KrcahImplementationEnum::UseImplementationParameters:
      traceFunction = &Self::CalculateTraceAccordingToImplementation;
      break;
    case KrcahImplementationEnum::UseJournalParameters:
      traceFunction = &Self::CalculateTraceAccordingToJournalArticle;
      break;
    default:
      itkExceptionMacro(<< "Have bad parameterset enumeration " << static_cast<char>(m_ParameterSet));
      break;
  }

  /* Get input and mask pointer */
  InputImageConstPointer            inputPointer = this->GetInput();
  MaskSpatialObjectTypeConstPointer maskPointer = this->GetMask();

  OutputImageType * outputPtr = this->GetOutput(0);

  // Define the portion of the input to walk for this thread, using
  // the CallCopyOutputRegionToInputRegion method allows for the input
  // and output images to be different dimensions
  InputImageRegionType inputRegionForThread;

  this->CallCopyOutputRegionToInputRegion(inputRegionForThread, outputRegionForThread);

  MultiThreaderBase::Pointer mt = this->GetMultiThreader();

  mt->ParallelizeImageRegion<TInputImage::ImageDimension>(
    outputRegionForThread,
    [inputPointer, maskPointer, outputPtr, this, traceFunction](const OutputImageRegionType region) {
      /* Keep track of the current accumulation */
      RealType accum = NumericTraits<RealType>::ZeroValue();
      RealType count = NumericTraits<RealType>::ZeroValue();

      typename InputImageType::PointType point;

      /* Setup iterator */
      ImageRegionConstIteratorWithIndex<TInputImage> inputIt(inputPointer, region);
      ImageRegionIterator<OutputImageType>           outputIt(outputPtr, region);

      /* Iterate and count */
      while (!inputIt.IsAtEnd())
      {
        // Process point
        inputPointer->TransformIndexToPhysicalPoint(inputIt.GetIndex(), point);
        if ((!maskPointer) || (maskPointer->IsInsideInObjectSpace(point)))
        {
          /* Compute trace */
          count++;
          accum += (this->*traceFunction)(inputIt.Get());
        }

        // Set
        outputIt.Set(static_cast<OutputImagePixelType>(inputIt.Get()));

        // Increment
        ++inputIt;
        ++outputIt;
      }

      /* Block and store */
      std::lock_guard<std::mutex> mutexHolder(m_Mutex);
      m_ThreadCount += count;
      m_ThreadAccumulatedTrace += accum;
    },
    nullptr);
}

template <typename TInputImage, typename TOutputImage>
typename KrcahEigenToMeasureParameterEstimationFilter<TInputImage, TOutputImage>::RealType
KrcahEigenToMeasureParameterEstimationFilter<TInputImage, TOutputImage>::CalculateTraceAccordingToImplementation(
  InputImagePixelType pixel)
{
  /* Sum of the absolute value of the eigenvalues */
  RealType trace = 0;
  for (unsigned int i = 0; i < pixel.Length; ++i)
  {
    trace += Math::abs(pixel[i]);
  }
  return trace;
}

template <typename TInputImage, typename TOutputImage>
typename KrcahEigenToMeasureParameterEstimationFilter<TInputImage, TOutputImage>::RealType
KrcahEigenToMeasureParameterEstimationFilter<TInputImage, TOutputImage>::CalculateTraceAccordingToJournalArticle(
  InputImagePixelType pixel)
{
  /* Sum of the eigenvalues */
  RealType trace = 0;
  for (unsigned int i = 0; i < pixel.Length; ++i)
  {
    trace += pixel[i];
  }
  return trace;
}

template <typename TInputImage, typename TOutputImage>
void
KrcahEigenToMeasureParameterEstimationFilter<TInputImage, TOutputImage>::PrintSelf(std::ostream & os,
                                                                                   Indent         indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "ParameterSet: " << static_cast<char>(GetParameterSet()) << std::endl;
}

} // namespace itk

#endif /* itkKrcahEigenToMeasureParameterEstimationFilter_hxx */
