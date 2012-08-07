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
#ifndef __itkBSplineExponentialDiffeomorphicTransformParametersAdaptor_hxx
#define __itkBSplineExponentialDiffeomorphicTransformParametersAdaptor_hxx

#include "itkBSplineExponentialDiffeomorphicTransformParametersAdaptor.h"

namespace itk
{

template<class TTransform>
BSplineExponentialDiffeomorphicTransformParametersAdaptor<TTransform>
::BSplineExponentialDiffeomorphicTransformParametersAdaptor()
{
  this->m_NumberOfControlPointsForTheVelocityField.Fill( 4 );
  this->m_NumberOfControlPointsForTheVelocityFieldSetTime = 0;
}

template<class TTransform>
BSplineExponentialDiffeomorphicTransformParametersAdaptor<TTransform>
::~BSplineExponentialDiffeomorphicTransformParametersAdaptor()
{
}

/**
 * set mesh size for velocity field
 */
template<class TTransform>
void
BSplineExponentialDiffeomorphicTransformParametersAdaptor<TTransform>
::SetMeshSizeForTheVelocityField( const ArrayType &meshSize )
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
  this->SetNumberOfControlPointsForTheVelocityField( numberOfControlPoints );
}

/**
 * set number of control points for velocity field
 */
template<class TTransform>
void
BSplineExponentialDiffeomorphicTransformParametersAdaptor<TTransform>
::SetNumberOfControlPointsForTheVelocityField( const ArrayType &controlPoints )
{
  this->m_NumberOfControlPointsForTheVelocityFieldSetTime = this->GetMTime();
  if( controlPoints != this->m_NumberOfControlPointsForTheVelocityField )
    {
    this->m_NumberOfControlPointsForTheVelocityField = controlPoints;
    this->Modified();
    }
}

template<class TTransform>
void
BSplineExponentialDiffeomorphicTransformParametersAdaptor<TTransform>
::AdaptTransformParameters()
{
  Superclass::AdaptTransformParameters();

  if( this->m_NumberOfControlPointsForTheVelocityFieldSetTime > 0 )
    {
    this->m_Transform->SetNumberOfControlPointsForTheVelocityField(
      this->m_NumberOfControlPointsForTheVelocityField );
    }
}

template <class TTransform>
void
BSplineExponentialDiffeomorphicTransformParametersAdaptor<TTransform>
::PrintSelf( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf( os,indent );

  os << indent << "B-spline parameters: " << std::endl;
  os << indent << "  number of control points for the velocity field = "
    << this->m_NumberOfControlPointsForTheVelocityField << std::endl;
}

}  // namespace itk

#endif
