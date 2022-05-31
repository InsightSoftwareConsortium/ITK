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
#ifndef itkSinusoidImageSource_hxx
#define itkSinusoidImageSource_hxx

#include "itkSinusoidSpatialFunction.h"
#include "itkImageRegionIterator.h"
#include "itkProgressReporter.h"

namespace itk
{

template <typename TOutputImage>
SinusoidImageSource<TOutputImage>::SinusoidImageSource()

{
  m_Frequency.Fill(1.0);
}


template <typename TOutputImage>
void
SinusoidImageSource<TOutputImage>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Sinusoid frequency: [";
  for (unsigned int ii = 0; ii < ImageDimension; ++ii)
  {
    os << this->m_Frequency[ii];
    if (ii != ImageDimension - 1)
    {
      os << ", ";
    }
  }
  os << "]" << std::endl;

  os << indent << "Sinusoid phase shift: " << m_PhaseOffset << std::endl;
}


template <typename TOutputImage>
void
SinusoidImageSource<TOutputImage>::SetParameters(const ParametersType & parameters)
{
  ArrayType frequency;
  for (unsigned int ii = 0; ii < ArrayType::Length; ++ii)
  {
    frequency[ii] = parameters[ii];
  }
  this->SetFrequency(frequency);

  const double phaseOffset = parameters[ArrayType::Length];
  this->SetPhaseOffset(phaseOffset);
}


template <typename TOutputImage>
typename SinusoidImageSource<TOutputImage>::ParametersType
SinusoidImageSource<TOutputImage>::GetParameters() const
{
  ParametersType parameters(ArrayType::Length + 1);
  for (unsigned int ii = 0; ii < ArrayType::Length; ++ii)
  {
    parameters[ii] = m_Frequency[ii];
  }
  parameters[ArrayType::Length] = m_PhaseOffset;

  return parameters;
}


template <typename TOutputImage>
unsigned int
SinusoidImageSource<TOutputImage>::GetNumberOfParameters() const
{
  return ArrayType::Length + 1;
}


template <typename TOutputImage>
void
SinusoidImageSource<TOutputImage>::GenerateData()
{
  TOutputImage * outputPtr = this->GetOutput();

  // allocate the output buffer
  outputPtr->SetBufferedRegion(outputPtr->GetRequestedRegion());
  outputPtr->Allocate();

  // Create and initialize a new gaussian function
  using FunctionType = SinusoidSpatialFunction<double, ImageDimension>;
  typename FunctionType::Pointer sinusoid = FunctionType::New();

  sinusoid->SetFrequency(m_Frequency);
  sinusoid->SetPhaseOffset(m_PhaseOffset);

  // Create an iterator that will walk the output region
  using OutputIterator = ImageRegionIterator<TOutputImage>;
  const typename OutputImageType::RegionType requestedRegion = outputPtr->GetRequestedRegion();
  OutputIterator                             outIt = OutputIterator(outputPtr, outputPtr->GetRequestedRegion());

  ProgressReporter progress(this, 0, outputPtr->GetRequestedRegion().GetNumberOfPixels());
  // Walk the output image, evaluating the spatial function at each pixel
  outIt.GoToBegin();
  while (!outIt.IsAtEnd())
  {
    typename TOutputImage::IndexType index = outIt.GetIndex();
    // The position at which the function is evaluated
    typename FunctionType::InputType evalPoint;
    outputPtr->TransformIndexToPhysicalPoint(index, evalPoint);
    const double value = sinusoid->Evaluate(evalPoint);

    // Set the pixel value to the function value
    outIt.Set(static_cast<typename TOutputImage::PixelType>(value));
    progress.CompletedPixel();

    ++outIt;
  }
}

} // end namespace itk

#endif
