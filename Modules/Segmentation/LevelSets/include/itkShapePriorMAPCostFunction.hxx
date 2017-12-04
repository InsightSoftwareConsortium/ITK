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
#ifndef itkShapePriorMAPCostFunction_hxx
#define itkShapePriorMAPCostFunction_hxx

#include "itkShapePriorMAPCostFunction.h"

namespace itk
{
/**
 * Constructor
 */
template< typename TFeatureImage, typename TOutputPixel >
ShapePriorMAPCostFunction< TFeatureImage, TOutputPixel >
::ShapePriorMAPCostFunction()
{
  m_GaussianFunction = GaussianKernelFunction<double>::New();
  m_ShapeParameterMeans = ArrayType(1);
  m_ShapeParameterMeans.Fill(0.0);
  m_ShapeParameterStandardDeviations = ArrayType(1);
  m_ShapeParameterStandardDeviations.Fill(0.0);
  m_Weights.Fill(1.0);
}

/**
 * PrintSelf
 */
template< typename TFeatureImage, typename TOutputPixel >
void
ShapePriorMAPCostFunction< TFeatureImage, TOutputPixel >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "ShapeParameterMeans: " << m_ShapeParameterMeans << std::endl;
  os << indent << "ShapeParameterStandardDeviations:  ";
  os << m_ShapeParameterStandardDeviations  << std::endl;
  os << indent << "Weights: " << m_Weights << std::endl;
}

/**
 *
 */
template< typename TFeatureImage, typename TOutputPixel >
typename ShapePriorMAPCostFunction< TFeatureImage, TOutputPixel >
::MeasureType
ShapePriorMAPCostFunction< TFeatureImage, TOutputPixel >
::ComputeLogInsideTerm(const ParametersType & parameters) const
{
  this->m_ShapeFunction->SetParameters(parameters);

  typename NodeContainerType::ConstIterator iter = this->GetActiveRegion()->Begin();
  typename NodeContainerType::ConstIterator end  = this->GetActiveRegion()->End();

  MeasureType counter = 0.0;

  // count the number of pixels inside the current contour but outside the
  // current shape
  while ( iter != end )
    {
    NodeType node = iter.Value();
    typename ShapeFunctionType::PointType point;

    this->GetFeatureImage()->TransformIndexToPhysicalPoint(node.GetIndex(), point);

    if ( node.GetValue() <= 0.0 )
      {
      double value = this->m_ShapeFunction->Evaluate(point);
      if ( value > 0.0 )
        {
        counter += 1.0;
        }
      else if ( value > -1.0 )
        {
        counter += ( 1.0 + value );
        }
      }

    ++iter;
    }

  MeasureType output = counter * m_Weights[0];
  return output;
}

/**
 *
 */
template< typename TFeatureImage, typename TOutputPixel >
typename ShapePriorMAPCostFunction< TFeatureImage, TOutputPixel >
::MeasureType
ShapePriorMAPCostFunction< TFeatureImage, TOutputPixel >
::ComputeLogShapePriorTerm(const ParametersType & parameters) const
{
  // assume the shape parameters is from a independent gaussian distributions
  MeasureType measure = 0.0;

  for ( unsigned int j = 0; j < this->m_ShapeFunction->GetNumberOfShapeParameters(); j++ )
    {
    measure += itk::Math::sqr( ( parameters[j] - m_ShapeParameterMeans[j] )
                             / m_ShapeParameterStandardDeviations[j] );
    }
  measure *= m_Weights[2];
  return measure;
}

/**
 *
 */
template< typename TFeatureImage, typename TOutputPixel >
typename ShapePriorMAPCostFunction< TFeatureImage, TOutputPixel >
::MeasureType
ShapePriorMAPCostFunction< TFeatureImage, TOutputPixel >
::ComputeLogGradientTerm(const ParametersType & parameters) const
{
  this->m_ShapeFunction->SetParameters(parameters);

  typename NodeContainerType::ConstIterator iter = this->GetActiveRegion()->Begin();
  typename NodeContainerType::ConstIterator end  = this->GetActiveRegion()->End();
  MeasureType sum = 0.0;

  // Assume that ( 1 - FeatureImage ) approximates a Gaussian (zero mean, unit
  // variance)
  // along the normal of the evolving contour.
  // The GradientTerm is then given by a Laplacian of the goodness of fit of
  // the Gaussian.
  while ( iter != end )
    {
    NodeType node = iter.Value();
    typename ShapeFunctionType::PointType point;

    this->GetFeatureImage()->TransformIndexToPhysicalPoint(node.GetIndex(), point);

    sum += itk::Math::sqr( m_GaussianFunction->Evaluate( this->m_ShapeFunction->Evaluate(point) )
                         - 1.0 + this->GetFeatureImage()->GetPixel( node.GetIndex() ) );

    ++iter;
    }

  sum *= m_Weights[1];
//  std::cout << sum << " ";
  return sum;
}

/**
 *
 */
template< typename TFeatureImage, typename TOutputPixel >
typename ShapePriorMAPCostFunction< TFeatureImage, TOutputPixel >
::MeasureType
ShapePriorMAPCostFunction< TFeatureImage, TOutputPixel >
::ComputeLogPosePriorTerm( const ParametersType & itkNotUsed(parameters) ) const
{
  return 0.0;
}

/**
 *
 */
template< typename TFeatureImage, typename TOutputPixel >
void
ShapePriorMAPCostFunction< TFeatureImage, TOutputPixel >
::Initialize()
{
  this->Superclass::Initialize();

  // check if the mean and variances array are of the right size
  if ( m_ShapeParameterMeans.Size() <
       this->m_ShapeFunction->GetNumberOfShapeParameters() )
    {
    itkExceptionMacro(<< "ShapeParameterMeans does not have at least "
                      << this->m_ShapeFunction->GetNumberOfShapeParameters()
                      << " number of elements.");
    }

  if ( m_ShapeParameterStandardDeviations.Size() <
       this->m_ShapeFunction->GetNumberOfShapeParameters() )
    {
    itkExceptionMacro(<< "ShapeParameterStandardDeviations does not have at least "
                      << this->m_ShapeFunction->GetNumberOfShapeParameters()
                      << " number of elements.");
    }
}
} // end namespace itk

#endif
