/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    $RCSfile: itkDeformationFieldTransform.txx,v $
  Language:  C++
  Date:      $Date: $
  Version:   $Revision: $

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkDeformationFieldTransform_txx
#define __itkDeformationFieldTransform_txx

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
  this->m_PreviousDeformationFieldMTime = 0;
  this->m_PreviousInterpolatorMTime = 0;

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
  /* Check if either the deformation field or iterpolatr have changed since
   * we were last in here. */
  if( this->m_DeformationField->GetMTime() >
        this->m_PreviousDeformationFieldMTime ||
      this->m_Interpolator->GetMTime() > this->m_PreviousInterpolatorMTime )
    {
    this->m_Interpolator->SetInputImage( this->m_DeformationField );
    }
  this->m_PreviousDeformationFieldMTime = this->GetMTime();
  this->m_PreviousInterpolatorMTime = this->GetMTime();

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
