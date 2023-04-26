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
#ifndef itkComplexBSplineInterpolateImageFunction_hxx
#define itkComplexBSplineInterpolateImageFunction_hxx


namespace itk
{

template <typename TImageType, typename TCoordRep, typename TCoefficientType>
ComplexBSplineInterpolateImageFunction<TImageType, TCoordRep, TCoefficientType>::
  ComplexBSplineInterpolateImageFunction()
{
  m_RealInterpolator = InterpolatorType::New();
  m_ImaginaryInterpolator = InterpolatorType::New();

  m_RealFilter = RealFilterType::New();
  m_ImaginaryFilter = ImaginaryFilterType::New();

  this->SetSplineOrder(3);
}

template <typename TImageType, typename TCoordRep, typename TCoefficientType>
void
ComplexBSplineInterpolateImageFunction<TImageType, TCoordRep, TCoefficientType>::PrintSelf(std::ostream & os,
                                                                                           Indent         indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "SplineOrder: " << m_SplineOrder << std::endl;

  itkPrintSelfObjectMacro(RealInterpolator);
  itkPrintSelfObjectMacro(ImaginaryInterpolator);
  itkPrintSelfObjectMacro(RealFilter);
  itkPrintSelfObjectMacro(ImaginaryFilter);
}

template <typename TImageType, typename TCoordRep, typename TCoefficientType>
void
ComplexBSplineInterpolateImageFunction<TImageType, TCoordRep, TCoefficientType>::SetInputImage(
  const TImageType * inputData)
{
  if (inputData)
  {
    m_RealFilter->SetInput(inputData);
    m_ImaginaryFilter->SetInput(inputData);

    m_RealInterpolator->SetInputImage(m_RealFilter->GetOutput());
    m_ImaginaryInterpolator->SetInputImage(m_ImaginaryFilter->GetOutput());

    Superclass::SetInputImage(inputData);
  }
}

template <typename TImageType, typename TCoordRep, typename TCoefficientType>
void
ComplexBSplineInterpolateImageFunction<TImageType, TCoordRep, TCoefficientType>::SetSplineOrder(
  unsigned int SplineOrder)
{
  m_SplineOrder = SplineOrder;
  m_RealInterpolator->SetSplineOrder(SplineOrder);
  m_ImaginaryInterpolator->SetSplineOrder(SplineOrder);
}

template <typename TImageType, typename TCoordRep, typename TCoefficientType>
auto
ComplexBSplineInterpolateImageFunction<TImageType, TCoordRep, TCoefficientType>::EvaluateAtContinuousIndex(
  const ContinuousIndexType & x) const -> OutputType
{
  typename InterpolatorType::OutputType realPart = m_RealInterpolator->EvaluateAtContinuousIndex(x);
  typename InterpolatorType::OutputType imagPart = m_ImaginaryInterpolator->EvaluateAtContinuousIndex(x);
  using ReturnType =
    typename ComplexBSplineInterpolateImageFunction<TImageType, TCoordRep, TCoefficientType>::OutputType;

  return ReturnType(realPart, imagPart);
}
} // namespace itk

#endif
