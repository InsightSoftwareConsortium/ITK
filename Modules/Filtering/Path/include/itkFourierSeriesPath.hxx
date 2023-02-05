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
#ifndef itkFourierSeriesPath_hxx
#define itkFourierSeriesPath_hxx

#include <cmath>

namespace itk
{
template <unsigned int VDimension>
auto
FourierSeriesPath<VDimension>::Evaluate(const InputType & input) const -> OutputType
{
  InputType  theta;
  OutputType output;
  int        numHarmonics;

  numHarmonics = m_CosCoefficients->Size();
  output.Fill(0);

  const double PI = 4.0 * std::atan(1.0);

  if (numHarmonics > 0)
  {
    output += m_CosCoefficients->ElementAt(0);
  }

  for (int n = 1; n < numHarmonics; ++n)
  {
    // input defined over [0,1] maps to theta defined over [0,2pi * n]
    theta = PI * 2.0 * n * input;
    output +=
      (m_CosCoefficients->ElementAt(n) * std::cos(theta) + m_SinCoefficients->ElementAt(n) * std::sin(theta)) * 2.0;
  }

  return output;
}

template <unsigned int VDimension>
auto
FourierSeriesPath<VDimension>::EvaluateDerivative(const InputType & input) const -> VectorType
{
  InputType  theta;
  VectorType output;
  int        numHarmonics;

  numHarmonics = m_CosCoefficients->Size();
  output.Fill(0);

  const double PI = 4.0 * std::atan(1.0);

  for (int n = 1; n < numHarmonics; ++n)
  {
    // input defined over [0,1] maps to theta defined over [0,2pi * n]
    theta = PI * 2.0 * n * input;
    output += (m_SinCoefficients->ElementAt(n) * std::cos(theta) - m_CosCoefficients->ElementAt(n) * std::sin(theta)) *
              (2.0 * n);
  }

  return output;
}

template <unsigned int VDimension>
void
FourierSeriesPath<VDimension>::AddHarmonic(const VectorType & CosCoefficients, const VectorType & SinCoefficients)
{
  unsigned int numHarmonics = m_CosCoefficients->Size();

  m_CosCoefficients->InsertElement(numHarmonics, CosCoefficients);
  m_SinCoefficients->InsertElement(numHarmonics, SinCoefficients);
  this->Modified();
}

template <unsigned int VDimension>
FourierSeriesPath<VDimension>::FourierSeriesPath()
{
  this->SetDefaultInputStepSize(1.0 / 50.0);
  m_CosCoefficients = CoefficientsType::New();
  m_SinCoefficients = CoefficientsType::New();
}

template <unsigned int VDimension>
void
FourierSeriesPath<VDimension>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  itkPrintSelfObjectMacro(CosCoefficients);
  itkPrintSelfObjectMacro(SinCoefficients);
}
} // namespace itk

#endif
