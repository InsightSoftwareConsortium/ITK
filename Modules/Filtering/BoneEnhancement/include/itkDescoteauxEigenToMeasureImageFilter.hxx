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

#ifndef itkDescoteauxEigenToMeasureImageFilter_hxx
#define itkDescoteauxEigenToMeasureImageFilter_hxx

#include "itkDescoteauxEigenToMeasureImageFilter.h"
#include "itkImageRegionConstIteratorWithIndex.h"
#include "itkImageRegionIterator.h"

namespace itk
{
template <typename TInputImage, typename TOutputImage>
DescoteauxEigenToMeasureImageFilter<TInputImage, TOutputImage>::DescoteauxEigenToMeasureImageFilter()
  : m_EnhanceType(-1.0)
{}

template <typename TInputImage, typename TOutputImage>
void
DescoteauxEigenToMeasureImageFilter<TInputImage, TOutputImage>::BeforeThreadedGenerateData()
{
  ParameterArrayType parameters = this->GetParametersInput()->Get();
  if (parameters.GetSize() != 3)
  {
    itkExceptionMacro(<< "Parameters must have size 3. Given array of size " << parameters.GetSize());
  }
}

template <typename TInputImage, typename TOutputImage>
typename DescoteauxEigenToMeasureImageFilter<TInputImage, TOutputImage>::OutputImagePixelType
DescoteauxEigenToMeasureImageFilter<TInputImage, TOutputImage>::ProcessPixel(const InputImagePixelType & pixel)
{
  /* Grab parameters */
  ParameterArrayType parameters = this->GetParametersInput()->Get();
  RealType           alpha = parameters[0];
  RealType           beta = parameters[1];
  RealType           c = parameters[2];

  /* Grab pixel values */
  double sheetness = 0.0;
  auto   a1 = static_cast<double>(pixel[0]);
  auto   a2 = static_cast<double>(pixel[1]);
  auto   a3 = static_cast<double>(pixel[2]);
  double l1 = Math::abs(a1);
  double l2 = Math::abs(a2);
  double l3 = Math::abs(a3);

  /* Deal with l3 > 0 */
  if (m_EnhanceType * a3 < 0)
  {
    return static_cast<OutputImagePixelType>(0.0);
  }

  /* Avoid divisions by zero (or close to zero) */
  if (l3 < Math::eps)
  {
    return static_cast<OutputImagePixelType>(0.0);
  }

  /* Compute measures */
  const double Rsheet = l2 / l3;
  const double Rblob = Math::abs(2 * l3 - l2 - l1) / l3;
  const double Rnoise = sqrt(l1 * l1 + l2 * l2 + l3 * l3);

  /* Multiply together to get sheetness */
  sheetness = 1.0;
  sheetness *= std::exp(-(Rsheet * Rsheet) / (2 * alpha * alpha));
  sheetness *= (1.0 - std::exp(-(Rblob * Rblob) / (2 * beta * beta)));
  sheetness *= (1.0 - std::exp(-(Rnoise * Rnoise) / (2 * c * c)));

  return static_cast<OutputImagePixelType>(sheetness);
}

template <typename TInputImage, typename TOutputImage>
void
DescoteauxEigenToMeasureImageFilter<TInputImage, TOutputImage>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Direction: " << GetEnhanceType() << std::endl;
}

} // namespace itk

#endif /* itkDescoteauxEigenToMeasureImageFilter_hxx */
