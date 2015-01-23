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
#ifndef itkBSplineSmoothingOnUpdateDisplacementFieldTransformParametersAdaptor_hxx
#define itkBSplineSmoothingOnUpdateDisplacementFieldTransformParametersAdaptor_hxx

#include "itkBSplineSmoothingOnUpdateDisplacementFieldTransformParametersAdaptor.h"

namespace itk
{

template<typename TTransform>
BSplineSmoothingOnUpdateDisplacementFieldTransformParametersAdaptor<TTransform>
::BSplineSmoothingOnUpdateDisplacementFieldTransformParametersAdaptor()
{
  this->m_NumberOfControlPointsForTheUpdateField.Fill( 4 );
  this->m_NumberOfControlPointsForTheTotalField.Fill( 0 );
  this->m_NumberOfControlPointsForTheUpdateFieldSetTime = 0;
  this->m_NumberOfControlPointsForTheTotalFieldSetTime = 0;
}

template<typename TTransform>
BSplineSmoothingOnUpdateDisplacementFieldTransformParametersAdaptor<TTransform>
::~BSplineSmoothingOnUpdateDisplacementFieldTransformParametersAdaptor()
{
}

/**
 * set mesh size for update field
 */
template<typename TTransform>
void
BSplineSmoothingOnUpdateDisplacementFieldTransformParametersAdaptor<TTransform>
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
 * set mesh size for total field
 */
template<typename TTransform>
void
BSplineSmoothingOnUpdateDisplacementFieldTransformParametersAdaptor<TTransform>
::SetMeshSizeForTheTotalField( const ArrayType &meshSize )
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
  this->SetNumberOfControlPointsForTheTotalField( numberOfControlPoints );
}

/**
 * set number of control points for update field
 */
template<typename TTransform>
void
BSplineSmoothingOnUpdateDisplacementFieldTransformParametersAdaptor<TTransform>
::SetNumberOfControlPointsForTheUpdateField( const ArrayType &controlPoints )
{
  this->m_NumberOfControlPointsForTheUpdateFieldSetTime = this->GetMTime();
  if( controlPoints != this->m_NumberOfControlPointsForTheUpdateField )
    {
    this->m_NumberOfControlPointsForTheUpdateField = controlPoints;
    this->Modified();
    }
}

/**
 * set number of control points for total field
 */
template<typename TTransform>
void
BSplineSmoothingOnUpdateDisplacementFieldTransformParametersAdaptor<TTransform>
::SetNumberOfControlPointsForTheTotalField( const ArrayType &controlPoints )
{
  this->m_NumberOfControlPointsForTheTotalFieldSetTime = this->GetMTime();
  if( controlPoints != this->m_NumberOfControlPointsForTheTotalField )
    {
    this->m_NumberOfControlPointsForTheTotalField = controlPoints;
    this->Modified();
    }
}

template<typename TTransform>
void
BSplineSmoothingOnUpdateDisplacementFieldTransformParametersAdaptor<TTransform>
::AdaptTransformParameters()
{
  Superclass::AdaptTransformParameters();

  if( this->m_NumberOfControlPointsForTheUpdateFieldSetTime > 0 )
    {
    this->m_Transform->SetNumberOfControlPointsForTheUpdateField(
      this->m_NumberOfControlPointsForTheUpdateField );
    }
  if( this->m_NumberOfControlPointsForTheTotalFieldSetTime > 0 )
    {
    this->m_Transform->SetNumberOfControlPointsForTheTotalField(
      this->m_NumberOfControlPointsForTheTotalField );
    }
}

template <typename TTransform>
void
BSplineSmoothingOnUpdateDisplacementFieldTransformParametersAdaptor<TTransform>
::PrintSelf( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf( os,indent );

  os << indent << "B-spline parameters: " << std::endl;
  os << indent << "  number of control points for the update field = "
    << this->m_NumberOfControlPointsForTheUpdateField << std::endl;
  os << indent << "  number of control points for the total field = "
    << this->m_NumberOfControlPointsForTheTotalField << std::endl;
}

}  // namespace itk

#endif
