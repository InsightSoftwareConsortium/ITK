/*=========================================================================
 *
 *  Copyright NumFOCUS
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

#ifndef itkKrcahEigenToMeasureImageFilter_hxx
#define itkKrcahEigenToMeasureImageFilter_hxx

#include "itkKrcahEigenToMeasureImageFilter.h"
#include "itkImageRegionConstIteratorWithIndex.h"
#include "itkImageRegionIterator.h"

namespace itk
{
template <typename TInputImage, typename TOutputImage>
KrcahEigenToMeasureImageFilter<TInputImage, TOutputImage>::KrcahEigenToMeasureImageFilter()
  : Superclass()
  , m_EnhanceType(-1.0f)
{}

template <typename TInputImage, typename TOutputImage>
void
KrcahEigenToMeasureImageFilter<TInputImage, TOutputImage>::BeforeThreadedGenerateData()
{
  ParameterArrayType parameters = this->GetParametersInput()->Get();
  if (parameters.GetSize() != 3)
  {
    itkExceptionMacro(<< "Parameters must have size 3. Given array of size " << parameters.GetSize());
  }
}

template <typename TInputImage, typename TOutputImage>
typename KrcahEigenToMeasureImageFilter<TInputImage, TOutputImage>::OutputImagePixelType
KrcahEigenToMeasureImageFilter<TInputImage, TOutputImage>::ProcessPixel(const InputImagePixelType & pixel)
{
  /* Grab parameters */
  ParameterArrayType parameters = this->GetParametersInput()->Get();
  RealType           alpha = parameters[0];
  RealType           beta = parameters[1];
  RealType           gamma = parameters[2];

  /* Grab pixel values */
  double sheetness = 0.0;
  auto   a1 = static_cast<double>(pixel[0]);
  auto   a2 = static_cast<double>(pixel[1]);
  auto   a3 = static_cast<double>(pixel[2]);
  double l1 = Math::abs(a1);
  double l2 = Math::abs(a2);
  double l3 = Math::abs(a3);

  /* Avoid divisions by zero (or close to zero) */
  if (static_cast<double>(l3) < Math::eps || static_cast<double>(l2) < Math::eps)
  {
    return static_cast<OutputImagePixelType>(sheetness);
  }

  /**
   * Compute sheet, noise, and tube like measures. Note that the average trace of the
   * Hessian matrix is implicitly included in \f$ \gamma \f$ here.
   */
  const double Rsheet = l2 / l3;
  const double Rnoise = (l1 + l2 + l3); // T implicite in m_Gamma
  const double Rtube = l1 / (l2 * l3);

  /* Multiply together to get sheetness */
  sheetness = (m_EnhanceType * a3 / l3);
  sheetness *= std::exp(-(Rsheet * Rsheet) / (alpha * alpha));
  sheetness *= std::exp(-(Rtube * Rtube) / (beta * beta));
  sheetness *= (1.0 - std::exp(-(Rnoise * Rnoise) / (gamma * gamma)));

  return static_cast<OutputImagePixelType>(sheetness);
}

template <typename TInputImage, typename TOutputImage>
void
KrcahEigenToMeasureImageFilter<TInputImage, TOutputImage>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Direction: " << GetEnhanceType() << std::endl;
}

} // namespace itk

#endif /* itkKrcahEigenToMeasureImageFilter_hxx */
