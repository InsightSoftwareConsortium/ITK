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
#ifndef __itkDeformationFieldTransform_hxx
#define __itkDeformationFieldTransform_hxx

#include "itkDeformationFieldTransform.h"

#include "itkVectorLinearInterpolateImageFunction.h"

namespace itk
{

/**
 * Constructor
 */
template<class TScalar, unsigned int NDimensions>
DeformationFieldTransform<TScalar, NDimensions>::
DeformationFieldTransform() : Superclass( NDimensions, 0 )
{
  this->m_DeformationField = NULL;
  this->m_InverseDeformationField = NULL;

  typedef VectorLinearInterpolateImageFunction<DeformationFieldType, ScalarType>
    DefaultInterpolatorType;
  typename DefaultInterpolatorType::Pointer interpolator
    = DefaultInterpolatorType::New();
  this->m_Interpolator = interpolator;
}

/**
 * Destructor
 */
template<class TScalar, unsigned int NDimensions>
DeformationFieldTransform<TScalar, NDimensions>::
~DeformationFieldTransform()
{
}

/**
 * Transform point
 */
template<class TScalar, unsigned int NDimensions>
typename DeformationFieldTransform<TScalar, NDimensions>::OutputPointType
DeformationFieldTransform<TScalar, NDimensions>
::TransformPoint( const InputPointType& inputPoint ) const
{
  if( !this->m_DeformationField )
    {
    itkExceptionMacro( "No deformation field is specified." );
    }
  if( !this->m_Interpolator )
    {
    itkExceptionMacro( "No interpolator is specified." );
    }

  typename InterpolatorType::ContinuousIndexType cidx;
  typename InterpolatorType::PointType point;
  point.CastFrom( inputPoint );

  OutputPointType outputPoint;
  outputPoint.CastFrom( inputPoint );

  if( this->m_Interpolator->IsInsideBuffer( point ) )
    {
    this->m_DeformationField->
      TransformPhysicalPointToContinuousIndex( point, cidx );
    typename InterpolatorType::OutputType displacement =
      this->m_Interpolator->EvaluateAtContinuousIndex( cidx );
    outputPoint += displacement;
    }
  else
    {
    ScalarType infinity;
    if( vcl_numeric_limits<ScalarType>::has_infinity )
      {
      infinity = vcl_numeric_limits<ScalarType>::infinity();
      }
    else
      {
      infinity = NumericTraits<ScalarType>::max();
      }
    outputPoint.Fill( infinity );
    }

  return outputPoint;
}

/**
 * return an inverse transformation
 */
template<class TScalar, unsigned int NDimensions>
bool DeformationFieldTransform<TScalar, NDimensions>
::GetInverse( Self *inverse ) const
{
  if ( !inverse || !this->m_InverseDeformationField )
    {
    return false;
    }
  else
    {
    inverse->SetDeformationField( this->m_InverseDeformationField );
    inverse->SetInverseDeformationField( this->m_DeformationField );
    inverse->SetInterpolator( this->m_Interpolator );

    return true;
    }
}

// Return an inverse of this transform
template<class TScalar, unsigned int NDimensions>
typename DeformationFieldTransform<TScalar, NDimensions>::InverseTransformBasePointer
DeformationFieldTransform<TScalar, NDimensions>
::GetInverseTransform() const
{
  Pointer inverseTransform = New();
  if( this->GetInverse( inverseTransform ) )
    {
    return inverseTransform.GetPointer();
    }
  else
    {
    return NULL;
    }
}

template<class TScalar, unsigned int NDimensions>
typename DeformationFieldTransform<TScalar, NDimensions>::JacobianType &
DeformationFieldTransform<TScalar, NDimensions>
::GetJacobian( const InputPointType & ) const
{
  this->m_Jacobian.Fill( 0.0 );
  itkExceptionMacro( "GetJacobian is not implemented since"
    << "the DeformationFieldTransform uses no parameters.");
  return this->m_Jacobian;
}

template<class TScalar, unsigned int NDimensions>
void DeformationFieldTransform<TScalar, NDimensions>
::SetDeformationField( DeformationFieldType* field )
{
  itkDebugMacro("setting DeformationField to " << field);
  if ( this->m_DeformationField != field )
    {
    this->m_DeformationField = field;
    this->Modified();
    if( ! this->m_Interpolator.IsNull() )
      {
      this->m_Interpolator->SetInputImage( this->m_DeformationField );
      }
    }
}

template<class TScalar, unsigned int NDimensions>
void DeformationFieldTransform<TScalar, NDimensions>
::SetInterpolator( InterpolatorType* interpolator )
{
  itkDebugMacro("setting Interpolator to " << interpolator);
  if ( this->m_Interpolator != interpolator )
    {
    this->m_Interpolator = interpolator;
    this->Modified();
    if( ! this->m_DeformationField.IsNull() )
      {
      this->m_Interpolator->SetInputImage( this->m_DeformationField );
      }
    }
}

template <class TScalar, unsigned int NDimensions>
void
DeformationFieldTransform<TScalar, NDimensions>::
PrintSelf( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf( os,indent );

  std::cout << indent << "Interpolator: " << std::endl;
  std::cout << indent << indent << this->m_Interpolator << std::endl;

  if( this->m_DeformationField )
    {
    std::cout << indent << "Deformation Field: " << std::endl;
    std::cout << indent << indent << this->m_DeformationField << std::endl;
    }

  if( this->m_InverseDeformationField )
    {
    std::cout << indent << "Inverse Deformation Field: " << std::endl;
    std::cout << indent << indent << this->m_InverseDeformationField << std::endl;
    }
}
} // namespace itk

#endif
