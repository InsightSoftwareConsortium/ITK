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
#ifndef itkParametricPath_hxx
#define itkParametricPath_hxx

#include "itkMath.h"

namespace itk
{

template <unsigned int VDimension>
auto
ParametricPath<VDimension>::EvaluateToIndex(const InputType & input) const -> IndexType
{
  IndexType index;

  const ContinuousIndexType continuousIndex = this->Evaluate(input);
  // Round each coordinate to the nearest integer value
  for (unsigned int i = 0; i < VDimension; ++i)
  {
    index[i] = static_cast<IndexValueType>(continuousIndex[i] + 0.5);
  }

  return index;
}

template <unsigned int VDimension>
auto
ParametricPath<VDimension>::IncrementInput(InputType & input) const -> OffsetType
{
  int       iterationCount = 0;
  InputType inputStepSize = m_DefaultInputStepSize;

  // Are we already at (or past) the end of the input?
  InputType  finalInputValue = this->EndOfInput();
  IndexType  currentImageIndex = this->EvaluateToIndex(input);
  IndexType  finalImageIndex = this->EvaluateToIndex(finalInputValue);
  OffsetType offset = finalImageIndex - currentImageIndex;
  if ((offset == OffsetType{} && Math::NotExactlyEquals(input, this->StartOfInput())) || (input >= finalInputValue))
  {
    return {};
  }

  bool tooSmall = false;
  bool tooBig = false;
  do
  {
    if (iterationCount++ > 10000)
    {
      itkExceptionStringMacro("Too many iterations");
    }

    IndexType nextImageIndex = this->EvaluateToIndex(input + inputStepSize);
    offset = nextImageIndex - currentImageIndex;

    tooBig = false;
    tooSmall = (offset == OffsetType{});
    if (tooSmall)
    {
      // double the input step size, but don't go past the end of the input
      inputStepSize *= 2;
      if ((input + inputStepSize) >= finalInputValue)
      {
        inputStepSize = finalInputValue - input;
      }
    }
    else
    {
      // Search for an offset dimension that is too big
      for (unsigned int i = 0; i < VDimension && !tooBig; ++i)
      {
        tooBig = (offset[i] >= 2 || offset[i] <= -2);
      }

      if (tooBig)
      {
        inputStepSize /= 1.5;
      }
    }
  } while (tooSmall || tooBig);

  input += inputStepSize;
  return offset;
}

template <unsigned int VDimension>
auto
ParametricPath<VDimension>::EvaluateDerivative(const InputType & input) const -> VectorType
{
  InputType inputStepSize = m_DefaultInputStepSize;
  if ((input + inputStepSize) >= this->EndOfInput())
  {
    inputStepSize = this->EndOfInput() - input;
  }

  return (this->Evaluate(input + inputStepSize) - this->Evaluate(input)) / inputStepSize;
}

template <unsigned int VDimension>
void
ParametricPath<VDimension>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent
     << "DefaultInputStepSize: " << static_cast<typename NumericTraits<InputType>::PrintType>(m_DefaultInputStepSize)
     << std::endl;
}
} // namespace itk

#endif
