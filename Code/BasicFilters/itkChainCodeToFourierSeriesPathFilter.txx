/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkChainCodeToFourierSeriesPathFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkChainCodeToFourierSeriesPathFilter_txx
#define __itkChainCodeToFourierSeriesPathFilter_txx

#include "itkChainCodeToFourierSeriesPathFilter.h"
#include "itkOffset.h"
#include <math.h>

namespace itk
{
/**
 * Constructor
 */
template< class TInputChainCodePath, class TOutputFourierSeriesPath >
ChainCodeToFourierSeriesPathFilter< TInputChainCodePath, TOutputFourierSeriesPath >
::ChainCodeToFourierSeriesPathFilter()
{
  this->SetNumberOfRequiredInputs(1);
  m_NumberOfHarmonics = 8;
}

/**
 * GenerateData Performs the reflection
 */
template< class TInputChainCodePath, class TOutputFourierSeriesPath >
void
ChainCodeToFourierSeriesPathFilter< TInputChainCodePath, TOutputFourierSeriesPath >
::GenerateData(void)
{
  IndexType           index;
  VectorType          indexVector;
  VectorType          cosCoefficient;
  VectorType          sinCoefficient;
  OutputPathInputType theta;

  unsigned int numSteps;
  unsigned int numHarmonics = m_NumberOfHarmonics;        // private copy
  int          dimension = OffsetType::GetOffsetDimension();

  typename Superclass::InputPathConstPointer inputPtr  = this->GetInput();
  typename Superclass::OutputPathPointer outputPtr = this->GetOutput(0);

  //outputPtr->SetRequestedRegion( inputPtr->GetRequestedRegion() );
  //outputPtr->SetBufferedRegion( inputPtr->GetBufferedRegion() );
  //outputPtr->SetLargestPossibleRegion( inputPtr->GetLargestPossibleRegion() );
  //outputPtr->Allocate();  // Allocate() is an Image function

  numSteps = inputPtr->NumberOfSteps();
  outputPtr->Clear();

  const double nPI = 4.0 * vcl_atan(1.0);

  // Adjust our private copy of numHarmonics if necessary
  if ( numHarmonics <= 1 )
    {
    numHarmonics = 2;
    }
  else if ( numHarmonics * 2 > numSteps )
    {
    numHarmonics = numSteps / 2;
    }

  for ( unsigned n = 0; n < numHarmonics; n++ )
    {
    index = inputPtr->GetStart();
    cosCoefficient.Fill(0.0);
    sinCoefficient.Fill(0.0);

    for ( InputPathInputType step = 0; step < numSteps; step++ )
      {
      index += inputPtr->Evaluate(step);
      theta = 2 * n * nPI * ( double(step + 1) ) / numSteps;

      // turn the current index into a vector
      for ( int d = 0; d < dimension; d++ )
        {
        indexVector[d] = index[d];
        }

      cosCoefficient += indexVector * ( vcl_cos(theta) / numSteps );
      sinCoefficient += indexVector * ( vcl_sin(theta) / numSteps );
      }

    outputPtr->AddHarmonic(cosCoefficient, sinCoefficient);
    }
}

template< class TInputChainCodePath, class TOutputFourierSeriesPath >
void
ChainCodeToFourierSeriesPathFilter< TInputChainCodePath, TOutputFourierSeriesPath >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "NumberOfHarmonics: " << m_NumberOfHarmonics << std::endl;
}
} // end namespace itk

#endif
