/*=========================================================================
 *
 *  Copyright Insight Software Consortium
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
#ifndef itkParametricPath_hxx
#define itkParametricPath_hxx

#include "itkParametricPath.h"
#include "itkMath.h"

namespace itk
{
/**
 * Constructor
 */
template< unsigned int VDimension >
ParametricPath< VDimension >
::ParametricPath()
{
  m_DefaultInputStepSize = 0.3;
}

template< unsigned int VDimension >
typename ParametricPath< VDimension >::IndexType
ParametricPath< VDimension >
::EvaluateToIndex(const InputType & input) const
{
  ContinuousIndexType continuousIndex;
  IndexType           index;

  continuousIndex = this->Evaluate(input);

  // Round each coordinate to the nearest integer value
  for ( unsigned int i = 0; i < VDimension; i++ )
    {
    index[i] = static_cast< IndexValueType >( continuousIndex[i] + 0.5 );
    }

  return index;
}

template< unsigned int VDimension >
typename ParametricPath< VDimension >::OffsetType
ParametricPath< VDimension >
::IncrementInput(InputType & input) const
{
  int        iterationCount;
  bool       tooSmall;
  bool       tooBig;
  InputType  inputStepSize;
  InputType  finalInputValue;
  OffsetType offset;
  IndexType  currentImageIndex;
  IndexType  nextImageIndex;
  IndexType  finalImageIndex;

  iterationCount    = 0;
  inputStepSize     = m_DefaultInputStepSize;

  // Are we already at (or past) the end of the input?
  finalInputValue   = this->EndOfInput();
  currentImageIndex = this->EvaluateToIndex(input);
  finalImageIndex   = this->EvaluateToIndex(finalInputValue);
  offset            = finalImageIndex - currentImageIndex;
  if ( ( offset == this->GetZeroOffset() && Math::NotExactlyEquals(input, this->StartOfInput()) )
       || ( input >= finalInputValue ) )
    {
    return this->GetZeroOffset();
    }

  do
    {
    if ( iterationCount++ > 10000 ) { itkExceptionMacro(<< "Too many iterations"); }

    nextImageIndex    = this->EvaluateToIndex(input + inputStepSize);
    offset            = nextImageIndex - currentImageIndex;

    tooBig = false;
    tooSmall = ( offset == this->GetZeroOffset() );
    if ( tooSmall )
      {
      // double the input step size, but don't go past the end of the input
      inputStepSize *= 2;
      if ( ( input + inputStepSize ) >= finalInputValue )
        {
        inputStepSize = finalInputValue - input;
        }
      }
    else
      {
      // Search for an offset dimension that is too big
      for ( unsigned int i = 0; i < VDimension && !tooBig; i++ )
        {
        tooBig = ( offset[i] >= 2 || offset[i] <= -2 );
        }

      if ( tooBig )
        {
        inputStepSize /= 1.5;
        }
      }
    }
  while ( tooSmall || tooBig );

  input += inputStepSize;
  return offset;
}

template< unsigned int VDimension >
typename ParametricPath< VDimension >::VectorType
ParametricPath< VDimension >
::EvaluateDerivative(const InputType & input) const
{
  InputType inputStepSize;

  inputStepSize = m_DefaultInputStepSize;
  if ( ( input + inputStepSize ) >= this->EndOfInput() )
    {
    inputStepSize = this->EndOfInput() - input;
    }

  return ( this->Evaluate(input + inputStepSize) - this->Evaluate(input) ) / inputStepSize;
}

template< unsigned int VDimension >
void
ParametricPath< VDimension >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "DefaultInputSize: " << m_DefaultInputStepSize << std::endl;
}
} // end namespaceitk

#endif
