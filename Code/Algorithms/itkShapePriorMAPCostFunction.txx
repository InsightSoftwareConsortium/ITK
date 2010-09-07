/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkShapePriorMAPCostFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkShapePriorMAPCostFunction_txx
#define __itkShapePriorMAPCostFunction_txx

#include "itkShapePriorMAPCostFunction.h"

namespace itk
{
/**
 * Constructor
 */
template< class TFeatureImage, class TOutputPixel >
ShapePriorMAPCostFunction< TFeatureImage, TOutputPixel >
::ShapePriorMAPCostFunction()
{
  m_GaussianFunction = GaussianKernelFunction::New();
  m_ShapeParameterMeans = ArrayType(1);
  m_ShapeParameterMeans.Fill(0.0);
  m_ShapeParameterStandardDeviations = ArrayType(1);
  m_ShapeParameterStandardDeviations.Fill(0.0);
  m_Weights.Fill(1.0);
}

/**
 * PrintSelf
 */
template< class TFeatureImage, class TOutputPixel >
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
template< class TFeatureImage, class TOutputPixel >
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
template< class TFeatureImage, class TOutputPixel >
typename ShapePriorMAPCostFunction< TFeatureImage, TOutputPixel >
::MeasureType
ShapePriorMAPCostFunction< TFeatureImage, TOutputPixel >
::ComputeLogShapePriorTerm(const ParametersType & parameters) const
{
  // assume the shape parameters is from a independent gaussian distributions
  MeasureType measure = 0.0;

  for ( unsigned int j = 0; j < this->m_ShapeFunction->GetNumberOfShapeParameters(); j++ )
    {
    measure += vnl_math_sqr( ( parameters[j] - m_ShapeParameterMeans[j] )
                             / m_ShapeParameterStandardDeviations[j] );
    }
  measure *= m_Weights[2];
  return measure;
}

/**
 *
 */
template< class TFeatureImage, class TOutputPixel >
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

    sum += vnl_math_sqr( m_GaussianFunction->Evaluate( this->m_ShapeFunction->Evaluate(point) )
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
template< class TFeatureImage, class TOutputPixel >
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
template< class TFeatureImage, class TOutputPixel >
void
ShapePriorMAPCostFunction< TFeatureImage, TOutputPixel >
::Initialize(void)
throw ( ExceptionObject )
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
