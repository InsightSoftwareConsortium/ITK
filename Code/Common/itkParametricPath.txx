/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkParametricPath.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkParametricPath_txx
#define __itkParametricPath_txx

#include "itkParametricPath.h"

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

  continuousIndex = Evaluate(input);

  // Round each coordinate to the nearest integer value
  for ( unsigned int i = 0; i < VDimension; i++ )
    {
    index[i] = ( typename IndexType::IndexValueType )(continuousIndex[i] + 0.5);
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
  if ( ( offset == this->GetZeroOffset() && input != this->StartOfInput() )
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

  return ( Evaluate(input + inputStepSize) - Evaluate(input) ) / inputStepSize;
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
