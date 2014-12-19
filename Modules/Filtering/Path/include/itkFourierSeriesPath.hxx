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
#ifndef itkFourierSeriesPath_hxx
#define itkFourierSeriesPath_hxx

#include "itkFourierSeriesPath.h"
#include <cmath>

namespace itk
{
template< unsigned int VDimension >
typename FourierSeriesPath< VDimension >::OutputType
FourierSeriesPath< VDimension >
::Evaluate(const InputType & input) const
{
  InputType  theta;
  OutputType output;
  int        numHarmonics;

  numHarmonics = m_CosCoefficients->Size();
  output.Fill(0);

  const double PI = 4.0 * std::atan(1.0);

  if ( numHarmonics > 0 ) { output += m_CosCoefficients->ElementAt(0); }

  for ( int n = 1; n < numHarmonics; n++ )
    {
    // input defined over [0,1] maps to theta defined over [0,2pi * n]
    theta = PI * 2.0 * n * input;
    output += ( m_CosCoefficients->ElementAt(n) * std::cos(theta)
                + m_SinCoefficients->ElementAt(n) * std::sin(theta) ) * 2.0;
    }

  return output;
}

template< unsigned int VDimension >
typename FourierSeriesPath< VDimension >::VectorType
FourierSeriesPath< VDimension >
::EvaluateDerivative(const InputType & input) const
{
  InputType  theta;
  VectorType output;
  int        numHarmonics;

  numHarmonics = m_CosCoefficients->Size();
  output.Fill(0);

  const double PI = 4.0 * std::atan(1.0);

  for ( int n = 1; n < numHarmonics; n++ )
    {
    // input defined over [0,1] maps to theta defined over [0,2pi * n]
    theta = PI * 2.0 * n * input;
    output += ( m_SinCoefficients->ElementAt(n) * std::cos(theta)
                - m_CosCoefficients->ElementAt(n) * std::sin(theta) ) * ( 2.0 * n );
    }

  return output;
}

template< unsigned int VDimension >
void
FourierSeriesPath< VDimension >
::AddHarmonic(const VectorType & CosCoefficients,
              const VectorType & SinCoefficients)
{
  unsigned int numHarmonics = m_CosCoefficients->Size();

  m_CosCoefficients->InsertElement(numHarmonics, CosCoefficients);
  m_SinCoefficients->InsertElement(numHarmonics, SinCoefficients);
  this->Modified();
}

/**
 * Constructor
 */
template< unsigned int VDimension >
FourierSeriesPath< VDimension >
::FourierSeriesPath()
{
  this->SetDefaultInputStepSize(1.0 / 50.0);
  m_CosCoefficients = CoefficientsType::New();
  m_SinCoefficients = CoefficientsType::New();
}

/**
 * Standard "PrintSelf" method
 */
template< unsigned int VDimension >
void
FourierSeriesPath< VDimension >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Cos Coefficients:  " << m_CosCoefficients << std::endl;
  os << indent << "Sin Coefficients:  " << m_SinCoefficients << std::endl;
}
} // end namespaceitk

#endif
