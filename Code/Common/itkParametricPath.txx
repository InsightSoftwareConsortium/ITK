/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkParametricPath.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/


#ifndef _itkParametricPath_txx
#define _itkParametricPath_txx

#include "itkParametricPath.h"

namespace itk
{

template<unsigned int VDimension>
typename ParametricPath<VDimension>::IndexType
ParametricPath<VDimension>
::EvaluateToIndex( const InputType & input ) const
{
  ContinuousIndexType continuousIndex;
  IndexType           index;
  
  continuousIndex = Evaluate( input );
  
  // Round each coordinate to the nearest integer value
  for( unsigned int i=0; i<VDimension; i++ )
    {
    index[i] = (typename IndexType::IndexValueType)( continuousIndex[i] + 0.5 );
    }
  
  return index;
}



template<unsigned int VDimension>
typename ParametricPath<VDimension>::OffsetType
ParametricPath<VDimension>
::IncrementInput(InputType & input) const
{
  int         iterationCount;
  bool        tooSmall;
  bool        tooBig;
  InputType   inputStepSize;
  InputType   finalInputValue;
  OffsetType  offset;
  IndexType   currentImageIndex;
  IndexType   nextImageIndex;
  IndexType   finalImageIndex;
  
  iterationCount    = 0;
  inputStepSize     = m_DefaultInputStepSize;

  // Are we already at (or past) the end of the input?
  finalInputValue   = EndOfInput();
  currentImageIndex = EvaluateToIndex( input );
  finalImageIndex   = EvaluateToIndex( finalInputValue );
  offset            = finalImageIndex - currentImageIndex;
  if(  ( offset == m_ZeroOffset && input != StartOfInput() )  ||
       ( input >=finalInputValue )  )
    {
    return m_ZeroOffset;
    }
  
  do
    {
    if( iterationCount++ > 10000 ) {itkExceptionMacro(<<"Too many iterations");}
    
    nextImageIndex    = EvaluateToIndex( input + inputStepSize );
    offset            = nextImageIndex - currentImageIndex;
    
    tooBig = false;
    tooSmall = ( offset == m_ZeroOffset );
    if( tooSmall )
      {
      // double the input step size, but don't go past the end of the input
      inputStepSize *= 2;
      if(  (input + inputStepSize) >= finalInputValue  )
        {
        inputStepSize = finalInputValue - input;
        }
      }
    else
      {
      // Search for an offset dimension that is too big
      for( unsigned int i=0; i<VDimension && !tooBig; i++ )
        {
        tooBig = ( offset[i] >= 2 || offset[i] <= -2 );
        }
      
      if( tooBig )
        {
        inputStepSize /= 1.5;
        }
      }
    }
  while( tooSmall || tooBig );
  
  input += inputStepSize;
  return offset;
}

template<unsigned int VDimension>
typename ParametricPath<VDimension>::VectorType
ParametricPath<VDimension>
::EvaluateDerivative(const InputType & input) const
{
  InputType   inputStepSize;
  
  inputStepSize = m_DefaultInputStepSize;
  if(  (input + inputStepSize) >= EndOfInput()  )
    {
    inputStepSize = EndOfInput() - input;
    }
  
  return ( Evaluate(input + inputStepSize) - Evaluate(input) ) / inputStepSize;
}

} // end namespaceitk

#endif
