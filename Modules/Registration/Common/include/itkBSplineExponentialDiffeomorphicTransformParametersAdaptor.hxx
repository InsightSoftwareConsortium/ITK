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
#ifndef itkBSplineExponentialDiffeomorphicTransformParametersAdaptor_hxx
#define itkBSplineExponentialDiffeomorphicTransformParametersAdaptor_hxx

#include "itkBSplineExponentialDiffeomorphicTransformParametersAdaptor.h"

namespace itk
{

template<typename TTransform>
BSplineExponentialDiffeomorphicTransformParametersAdaptor<TTransform>
::BSplineExponentialDiffeomorphicTransformParametersAdaptor()
{
  this->m_NumberOfControlPointsForTheConstantVelocityField.Fill( 4 );
  this->m_NumberOfControlPointsForTheConstantVelocityFieldSetTime = 0;
  this->m_NumberOfControlPointsForTheUpdateField.Fill( 4 );
  this->m_NumberOfControlPointsForTheUpdateFieldSetTime = 0;
}

template<typename TTransform>
BSplineExponentialDiffeomorphicTransformParametersAdaptor<TTransform>
::~BSplineExponentialDiffeomorphicTransformParametersAdaptor()
{
}

/**
 * set mesh size for velocity field
 */
template<typename TTransform>
void
BSplineExponentialDiffeomorphicTransformParametersAdaptor<TTransform>
::SetMeshSizeForTheConstantVelocityField( const ArrayType &meshSize )
{
  ArrayType numberOfControlPoints;
  numberOfControlPoints.Fill( 0 );
  for( unsigned int d = 0; d < SpaceDimension; d++ )
    {
    if( meshSize[d] > 0 )
      {
      numberOfControlPoints[d] = meshSize[d] + this->m_Transform->GetSplineOrder();
      }
    }
  this->SetNumberOfControlPointsForTheConstantVelocityField( numberOfControlPoints );
}

/**
 * set mesh size for update field
 */
template<typename TTransform>
void
BSplineExponentialDiffeomorphicTransformParametersAdaptor<TTransform>
::SetMeshSizeForTheUpdateField( const ArrayType &meshSize )
{
  ArrayType numberOfControlPoints;
  numberOfControlPoints.Fill( 0 );
  for( unsigned int d = 0; d < SpaceDimension; d++ )
    {
    if( meshSize[d] > 0 )
      {
      numberOfControlPoints[d] = meshSize[d] + this->m_Transform->GetSplineOrder();
      }
    }
  this->SetNumberOfControlPointsForTheUpdateField( numberOfControlPoints );
}

/**
 * set number of control points for velocity field
 */
template<typename TTransform>
void
BSplineExponentialDiffeomorphicTransformParametersAdaptor<TTransform>
::SetNumberOfControlPointsForTheConstantVelocityField( const ArrayType &controlPoints )
{
  this->m_NumberOfControlPointsForTheConstantVelocityFieldSetTime = this->GetMTime();
  if( controlPoints != this->m_NumberOfControlPointsForTheConstantVelocityField )
    {
    this->m_NumberOfControlPointsForTheConstantVelocityField = controlPoints;
    this->Modified();
    }
}

/**
 * set number of control points for update field
 */
template<typename TTransform>
void
BSplineExponentialDiffeomorphicTransformParametersAdaptor<TTransform>
::SetNumberOfControlPointsForTheUpdateField( const ArrayType &controlPoints )
{
  this->m_NumberOfControlPointsForTheUpdateFieldSetTime = this->GetMTime();
  if( controlPoints != this->m_NumberOfControlPointsForTheUpdateField )
    {
    this->m_NumberOfControlPointsForTheUpdateField = controlPoints;
    this->Modified();
    }
}

template<typename TTransform>
void
BSplineExponentialDiffeomorphicTransformParametersAdaptor<TTransform>
::AdaptTransformParameters()
{
  Superclass::AdaptTransformParameters();

  if( this->m_NumberOfControlPointsForTheUpdateFieldSetTime > 0 )
    {
    this->m_Transform->SetNumberOfControlPointsForTheUpdateField(
      this->m_NumberOfControlPointsForTheUpdateField );
    }
  if( this->m_NumberOfControlPointsForTheConstantVelocityFieldSetTime > 0 )
    {
    this->m_Transform->SetNumberOfControlPointsForTheConstantVelocityField(
      this->m_NumberOfControlPointsForTheConstantVelocityField );
    }
}

template <typename TTransform>
void
BSplineExponentialDiffeomorphicTransformParametersAdaptor<TTransform>
::PrintSelf( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf( os,indent );

  os << indent << "B-spline parameters: " << std::endl;
  os << indent << "  number of control points for the velocity field = "
    << this->m_NumberOfControlPointsForTheConstantVelocityField << std::endl;
}

}  // namespace itk

#endif
