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
#ifndef itkShapePriorMAPCostFunctionBase_hxx
#define itkShapePriorMAPCostFunctionBase_hxx


namespace itk
{

template <typename TFeatureImage, typename TOutputPixel>
ShapePriorMAPCostFunctionBase<TFeatureImage, TOutputPixel>::ShapePriorMAPCostFunctionBase()
{
  m_ShapeFunction = nullptr;
  m_ActiveRegion = nullptr;
  m_FeatureImage = nullptr;
}

template <typename TFeatureImage, typename TOutputPixel>
void
ShapePriorMAPCostFunctionBase<TFeatureImage, TOutputPixel>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  itkPrintSelfObjectMacro(ShapeFunction);
  itkPrintSelfObjectMacro(ActiveRegion);
  itkPrintSelfObjectMacro(FeatureImage);
}

template <typename TFeatureImage, typename TOutputPixel>
auto
ShapePriorMAPCostFunctionBase<TFeatureImage, TOutputPixel>::GetValue(const ParametersType & parameters) const
  -> MeasureType
{
  return (this->ComputeLogInsideTerm(parameters) + this->ComputeLogGradientTerm(parameters) +
          this->ComputeLogShapePriorTerm(parameters) + this->ComputeLogPosePriorTerm(parameters));
}

template <typename TFeatureImage, typename TOutputPixel>
void
ShapePriorMAPCostFunctionBase<TFeatureImage, TOutputPixel>::Initialize()
{
  if (!m_ShapeFunction)
  {
    itkExceptionMacro("ShapeFunction is not present.");
  }

  if (!m_ActiveRegion)
  {
    itkExceptionMacro("ActiveRegion is not present.");
  }

  if (!m_FeatureImage)
  {
    itkExceptionMacro("FeatureImage is not present.");
  }
}
} // end namespace itk

#endif
