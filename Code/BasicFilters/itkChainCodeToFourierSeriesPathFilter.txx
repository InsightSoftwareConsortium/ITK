/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkChainCodeToFourierSeriesPathFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef _itkChainCodeToFourierSeriesPathFilter_txx
#define _itkChainCodeToFourierSeriesPathFilter_txx

#include "itkChainCodeToFourierSeriesPathFilter.h"
#include "itkOffset.h"
#include <math.h>

// not all versions of math.h seem to define M_PI:
#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

namespace itk
{

/**
 * Constructor
 */
template <class TInputChainCodePath, class TOutputFourierSeriesPath>
ChainCodeToFourierSeriesPathFilter<TInputChainCodePath,TOutputFourierSeriesPath>
::ChainCodeToFourierSeriesPathFilter()
{
  this->SetNumberOfRequiredInputs( 1 );
  m_NumberOfHarmonics = 8;
}


/**
 * GenerateData Performs the reflection
 */
template <class TInputChainCodePath, class TOutputFourierSeriesPath>
void
ChainCodeToFourierSeriesPathFilter<TInputChainCodePath,TOutputFourierSeriesPath>
::GenerateData( void )
{
  IndexType           index;
  VectorType          indexVector;
  VectorType          cosCoefficient;
  VectorType          sinCoefficient;
  OutputPathInputType theta;

  unsigned int        numSteps;
  unsigned int        numHarmonics = m_NumberOfHarmonics; // private copy
  int                 dimension = OffsetType::GetOffsetDimension();

  typename Superclass::InputPathConstPointer  inputPtr  = this->GetInput();
  typename Superclass::OutputPathPointer      outputPtr = this->GetOutput(0);

  //outputPtr->SetRequestedRegion( inputPtr->GetRequestedRegion() );
  //outputPtr->SetBufferedRegion( inputPtr->GetBufferedRegion() );
  //outputPtr->SetLargestPossibleRegion( inputPtr->GetLargestPossibleRegion() );
  //outputPtr->Allocate();  // Allocate() is an Image function
  
  numSteps = inputPtr->NumberOfSteps();
  outputPtr->Clear();
  
  // Adjust our private copy of numHarmonics if necessary
  if( numHarmonics <= 1 )
    numHarmonics = 2;
  else if( numHarmonics*2 > numSteps )
    numHarmonics = numSteps / 2;
  
  for( unsigned n=0; n<numHarmonics; n++ )
    {
    index = inputPtr->GetStart();
    cosCoefficient.Fill(0.0);
    sinCoefficient.Fill(0.0);
    
    for( InputPathInputType step=0; step<numSteps; step++ )
      {
      index += inputPtr->Evaluate( step );
      theta = 2*n*M_PI*(double(step+1))/numSteps;
      
      // turn the current index into a vector
      for( int d=0; d<dimension; d++ )
        indexVector[d] = index[d];
      
      cosCoefficient += indexVector * (cos(theta)/numSteps);
      sinCoefficient += indexVector * (sin(theta)/numSteps);
      }
    
    outputPtr->AddHarmonic( cosCoefficient, sinCoefficient );
    }
}



template <class TInputChainCodePath, class TOutputFourierSeriesPath>
void
ChainCodeToFourierSeriesPathFilter<TInputChainCodePath,TOutputFourierSeriesPath>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  
  os << indent << "NumberOfHarmonics: " << m_NumberOfHarmonics << std::endl;
}

} // end namespace itk

#endif
