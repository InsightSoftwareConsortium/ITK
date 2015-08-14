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
#ifndef itkGaussianExponentialDiffeomorphicTransformParametersAdaptor_hxx
#define itkGaussianExponentialDiffeomorphicTransformParametersAdaptor_hxx

#include "itkGaussianExponentialDiffeomorphicTransformParametersAdaptor.h"
#include "itkMath.h"

namespace itk
{

template<typename TTransform>
GaussianExponentialDiffeomorphicTransformParametersAdaptor<TTransform>
::GaussianExponentialDiffeomorphicTransformParametersAdaptor() :
  m_GaussianSmoothingVarianceForTheConstantVelocityField( 0.5 ),
  m_GaussianSmoothingVarianceForTheUpdateField( 1.75 ),
  m_GaussianSmoothingVarianceForTheConstantVelocityFieldSetTime( 0 ),
  m_GaussianSmoothingVarianceForTheUpdateFieldSetTime( 0 )
{
}

template<typename TTransform>
GaussianExponentialDiffeomorphicTransformParametersAdaptor<TTransform>
::~GaussianExponentialDiffeomorphicTransformParametersAdaptor()
{
}

template<typename TTransform>
void
GaussianExponentialDiffeomorphicTransformParametersAdaptor<TTransform>
::SetGaussianSmoothingVarianceForTheConstantVelocityField( ScalarType variance )
{
  this->m_GaussianSmoothingVarianceForTheConstantVelocityFieldSetTime = this->GetMTime();
  if( Math::NotExactlyEquals(this->m_GaussianSmoothingVarianceForTheConstantVelocityField, variance) )
    {
    itkDebugMacro( "Setting GaussianSmoothingVarianceForTheConstantVelocityField to " << variance );
    this->m_GaussianSmoothingVarianceForTheConstantVelocityField = variance;
    this->Modified();
    }
}

template<typename TTransform>
void
GaussianExponentialDiffeomorphicTransformParametersAdaptor<TTransform>
::SetGaussianSmoothingVarianceForTheUpdateField( ScalarType variance )
{
  this->m_GaussianSmoothingVarianceForTheUpdateFieldSetTime = this->GetMTime();
  if( Math::NotExactlyEquals(this->m_GaussianSmoothingVarianceForTheUpdateField, variance) )
    {
    itkDebugMacro( "Setting GaussianSmoothingVarianceForTheUpdateField to " << variance );
    this->m_GaussianSmoothingVarianceForTheUpdateField = variance;
    this->Modified();
    }
}

template<typename TTransform>
void
GaussianExponentialDiffeomorphicTransformParametersAdaptor<TTransform>
::AdaptTransformParameters()
{
  Superclass::AdaptTransformParameters();

  if( this->m_GaussianSmoothingVarianceForTheConstantVelocityFieldSetTime > 0 )
    {
    this->m_Transform->SetGaussianSmoothingVarianceForTheConstantVelocityField(
      this->m_GaussianSmoothingVarianceForTheConstantVelocityField );
    }
  if( this->m_GaussianSmoothingVarianceForTheUpdateFieldSetTime > 0 )
    {
    this->m_Transform->SetGaussianSmoothingVarianceForTheUpdateField(
      this->m_GaussianSmoothingVarianceForTheUpdateField );
    }
}

template <typename TTransform>
void
GaussianExponentialDiffeomorphicTransformParametersAdaptor<TTransform>
::PrintSelf( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf( os,indent );

  if( this->m_GaussianSmoothingVarianceForTheConstantVelocityFieldSetTime > 0 )
    {
    os << indent << "Gaussian smoothing parameters: " << std::endl;
    if( this->m_GaussianSmoothingVarianceForTheConstantVelocityFieldSetTime > 0 )
      {
      os << indent << "m_GaussianSmoothingVarianceForTheConstantVelocityField: "
         << this->m_GaussianSmoothingVarianceForTheConstantVelocityField
         << std::endl;
      }
    if( this->m_GaussianSmoothingVarianceForTheUpdateFieldSetTime > 0 )
      {
      os << indent << "m_GaussianSmoothingVarianceForTheUpdateField: "
         << this->m_GaussianSmoothingVarianceForTheUpdateField
         << std::endl;
      }
    }
}

}  // namespace itk

#endif
