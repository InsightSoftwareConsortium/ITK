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
#ifndef itkGaussianSmoothingOnUpdateDisplacementFieldTransformParametersAdaptor_hxx
#define itkGaussianSmoothingOnUpdateDisplacementFieldTransformParametersAdaptor_hxx

#include "itkGaussianSmoothingOnUpdateDisplacementFieldTransformParametersAdaptor.h"
#include "itkMath.h"

namespace itk
{

template<typename TTransform>
GaussianSmoothingOnUpdateDisplacementFieldTransformParametersAdaptor<TTransform>
::GaussianSmoothingOnUpdateDisplacementFieldTransformParametersAdaptor() :
  m_GaussianSmoothingVarianceForTheUpdateField( 1.75 ),
  m_GaussianSmoothingVarianceForTheTotalField( 0.5 ),
  m_GaussianSmoothingVarianceForTheUpdateFieldSetTime( 0 ),
  m_GaussianSmoothingVarianceForTheTotalFieldSetTime( 0 )
{
}

template<typename TTransform>
GaussianSmoothingOnUpdateDisplacementFieldTransformParametersAdaptor<TTransform>
::~GaussianSmoothingOnUpdateDisplacementFieldTransformParametersAdaptor()
{
}

template<typename TTransform>
void
GaussianSmoothingOnUpdateDisplacementFieldTransformParametersAdaptor<TTransform>
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
GaussianSmoothingOnUpdateDisplacementFieldTransformParametersAdaptor<TTransform>
::SetGaussianSmoothingVarianceForTheTotalField( const ScalarType variance )
{
  this->m_GaussianSmoothingVarianceForTheTotalFieldSetTime = this->GetMTime();
  if( Math::NotExactlyEquals(this->m_GaussianSmoothingVarianceForTheTotalField, variance) )
    {
    itkDebugMacro( "Setting GaussianSmoothingVarianceForTheTotalField to " << variance );
    this->m_GaussianSmoothingVarianceForTheTotalField = variance;
    this->Modified();
    }
}

template<typename TTransform>
void
GaussianSmoothingOnUpdateDisplacementFieldTransformParametersAdaptor<TTransform>
::AdaptTransformParameters()
{
  Superclass::AdaptTransformParameters();

  if( this->m_GaussianSmoothingVarianceForTheUpdateFieldSetTime > 0 )
    {
    this->m_Transform->SetGaussianSmoothingVarianceForTheUpdateField(
      this->m_GaussianSmoothingVarianceForTheUpdateField );
    }
  if( this->m_GaussianSmoothingVarianceForTheTotalFieldSetTime > 0 )
    {
    this->m_Transform->SetGaussianSmoothingVarianceForTheTotalField(
      this->m_GaussianSmoothingVarianceForTheTotalField );
    }
}

template <typename TTransform>
void
GaussianSmoothingOnUpdateDisplacementFieldTransformParametersAdaptor<TTransform>
::PrintSelf( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf( os,indent );

  if( this->m_GaussianSmoothingVarianceForTheUpdateFieldSetTime > 0 ||
      this->m_GaussianSmoothingVarianceForTheTotalFieldSetTime > 0 )
    {
    os << indent << "Gaussian smoothing parameters: " << std::endl;
    if( this->m_GaussianSmoothingVarianceForTheUpdateFieldSetTime > 0 )
      {
      os << indent << "m_GaussianSmoothingVarianceForTheUpdateField: "
         << this->m_GaussianSmoothingVarianceForTheUpdateField
         << std::endl;
      }
    if( this->m_GaussianSmoothingVarianceForTheTotalFieldSetTime > 0 )
      {
      os << indent << "m_GaussianSmoothingVarianceForTheTotalField: "
         << this->m_GaussianSmoothingVarianceForTheTotalField
         << std::endl;
      }
    }
}

}  // namespace itk

#endif
