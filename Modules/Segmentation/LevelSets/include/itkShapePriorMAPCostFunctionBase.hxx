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
#ifndef itkShapePriorMAPCostFunctionBase_hxx
#define itkShapePriorMAPCostFunctionBase_hxx

#include "itkShapePriorMAPCostFunctionBase.h"

namespace itk
{
/**
 * Constructor
 */
template< typename TFeatureImage, typename TOutputPixel >
ShapePriorMAPCostFunctionBase< TFeatureImage, TOutputPixel >
::ShapePriorMAPCostFunctionBase()
{
  m_ShapeFunction = ITK_NULLPTR;
  m_ActiveRegion  = ITK_NULLPTR;
  m_FeatureImage  = ITK_NULLPTR;
}

/**
 * PrintSelf
 */
template< typename TFeatureImage, typename TOutputPixel >
void
ShapePriorMAPCostFunctionBase< TFeatureImage, TOutputPixel >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "ShapeFunction: " << m_ShapeFunction.GetPointer() << std::endl;
  os << indent << "ActiveRegion:  " << m_ActiveRegion.GetPointer()  << std::endl;
  os << indent << "FeatureImage:  " << m_FeatureImage.GetPointer()  << std::endl;
}

/**
 *
 */
template< typename TFeatureImage, typename TOutputPixel >
typename ShapePriorMAPCostFunctionBase< TFeatureImage, TOutputPixel >
::MeasureType
ShapePriorMAPCostFunctionBase< TFeatureImage, TOutputPixel >
::GetValue(const ParametersType & parameters) const
{
  return ( this->ComputeLogInsideTerm(parameters)
           + this->ComputeLogGradientTerm(parameters)
           + this->ComputeLogShapePriorTerm(parameters)
           + this->ComputeLogPosePriorTerm(parameters) );
}

/**
 *
 */
template< typename TFeatureImage, typename TOutputPixel >
void
ShapePriorMAPCostFunctionBase< TFeatureImage, TOutputPixel >
::Initialize(void)
{
  if ( !m_ShapeFunction )
    {
    itkExceptionMacro(<< "ShapeFunction is not present.");
    }

  if ( !m_ActiveRegion )
    {
    itkExceptionMacro(<< "ActiveRegion is not present.");
    }

  if ( !m_FeatureImage )
    {
    itkExceptionMacro(<< "FeatureImage is not present.");
    }
}
} // end namespace itk

#endif
