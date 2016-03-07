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
#ifndef itkChainCodeToFourierSeriesPathFilter_hxx
#define itkChainCodeToFourierSeriesPathFilter_hxx

#include "itkChainCodeToFourierSeriesPathFilter.h"
#include <cmath>

namespace itk
{
/**
 * Constructor
 */
template< typename TInputChainCodePath, typename TOutputFourierSeriesPath >
ChainCodeToFourierSeriesPathFilter< TInputChainCodePath, TOutputFourierSeriesPath >
::ChainCodeToFourierSeriesPathFilter()
{
  this->SetNumberOfRequiredInputs(1);
  m_NumberOfHarmonics = 8;
}

/**
 * GenerateData Performs the reflection
 */
template< typename TInputChainCodePath, typename TOutputFourierSeriesPath >
void
ChainCodeToFourierSeriesPathFilter< TInputChainCodePath, TOutputFourierSeriesPath >
::GenerateData(void)
{
  IndexType           index;
  VectorType          indexVector;
  VectorType          cosCoefficient;
  VectorType          sinCoefficient;
  OutputPathInputType theta;

  size_t               numSteps;
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

  const double nPI = 4.0 * std::atan(1.0);

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

      cosCoefficient += indexVector * ( std::cos(theta) / numSteps );
      sinCoefficient += indexVector * ( std::sin(theta) / numSteps );
      }

    outputPtr->AddHarmonic(cosCoefficient, sinCoefficient);
    }
}

template< typename TInputChainCodePath, typename TOutputFourierSeriesPath >
void
ChainCodeToFourierSeriesPathFilter< TInputChainCodePath, TOutputFourierSeriesPath >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "NumberOfHarmonics: " << m_NumberOfHarmonics << std::endl;
}
} // end namespace itk

#endif
