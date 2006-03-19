/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSimilarity2DTransform.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkSimilarity2DTransform_txx
#define _itkSimilarity2DTransform_txx

#include "itkSimilarity2DTransform.h"


namespace itk
{

// Constructor with default arguments
template <class TScalarType>
Similarity2DTransform<TScalarType>
::Similarity2DTransform():Superclass(OutputSpaceDimension, ParametersDimension)
{
  m_Scale = 1.0f; 
}

// Constructor with arguments
template<class TScalarType>
Similarity2DTransform<TScalarType>::
Similarity2DTransform( unsigned int spaceDimension, 
                  unsigned int parametersDimension):
  Superclass(spaceDimension,parametersDimension)
{
  m_Scale = 1.0f; 

}
 
// Set Parameters
template <class TScalarType>
void
Similarity2DTransform<TScalarType>
::SetParameters( const ParametersType & parameters )
{
  itkDebugMacro( << "Setting parameters " << parameters );

  // Set scale
  this->SetVarScale( parameters[0] );
 
  // Set angle
  this->SetVarAngle( parameters[1] );

  // Set translation
  OffsetType translation;
  for(unsigned int i=0; i < SpaceDimension; i++) 
    {
    translation[i] = parameters[i+2];
    }
  this->SetVarTranslation( translation );

  this->ComputeMatrix();
  this->ComputeOffset();

  // Modified is always called since we just have a pointer to the
  // parameters and cannot know if the parameters have changed.
  this->Modified();

  itkDebugMacro(<<"After setting parameters ");
}


// Get Parameters
template <class TScalarType>
const typename Similarity2DTransform<TScalarType>::ParametersType &
Similarity2DTransform<TScalarType>
::GetParameters( void ) const
{
  itkDebugMacro( << "Getting parameters ");

  this->m_Parameters[0] = this->GetScale();
  this->m_Parameters[1] = this->GetAngle();
 
  // Get the translation
  OffsetType translation = this->GetTranslation();
  for(unsigned int i=0; i < SpaceDimension; i++) 
    {
    this->m_Parameters[i+2] = translation[i];
    }

  itkDebugMacro(<<"After getting parameters " << this->m_Parameters );

  return this->m_Parameters;
}



// Set Scale Part
template <class TScalarType>
void
Similarity2DTransform<TScalarType>
::SetScale( ScaleType scale )
{
  m_Scale = scale;
  this->ComputeMatrix();
  this->ComputeOffset();
}


// Compute the matrix
template <class TScalarType>
void
Similarity2DTransform<TScalarType>
::ComputeMatrix( void )
{
  const double angle = this->GetAngle();

  const double cc = vcl_cos(angle );
  const double ss = vcl_sin(angle );

  const double ca = cc * m_Scale;
  const double sa = ss * m_Scale;

  MatrixType matrix;
  matrix[0][0]= ca; matrix[0][1]=-sa;
  matrix[1][0]= sa; matrix[1][1]= ca;

  this->SetVarMatrix( matrix );

}

/** Compute the Angle from the Rotation Matrix */
template <class TScalarType>
void
Similarity2DTransform<TScalarType>
::ComputeMatrixParameters( void )
{
  m_Scale = vcl_sqrt(vnl_math_sqr( this->GetMatrix()[0][0] ) +
                  vnl_math_sqr( this->GetMatrix()[0][1] ) );

  this->SetVarAngle( vcl_acos(this->GetMatrix()[0][0] / m_Scale ) ); 

  if(this->GetMatrix()[1][0]<0.0)
    {
    this->SetVarAngle( this->GetAngle() );
    }

  if( ( this->GetMatrix()[1][0] / m_Scale ) - vcl_sin(this->GetAngle()) > 0.000001)
    {
    std::cout << "Bad Rotation Matrix" << std::endl;
    }
}


// Compute the transformation Jacobian
template<class TScalarType>
const typename Similarity2DTransform<TScalarType>::JacobianType &
Similarity2DTransform<TScalarType>::
GetJacobian( const InputPointType & p ) const
{
  const double angle = this->GetAngle();
  const double ca = vcl_cos(angle );
  const double sa = vcl_sin(angle );

  this->m_Jacobian.Fill(0.0);

  const InputPointType center = this->GetCenter();  
  const double cx = center[0];
  const double cy = center[1];

  const OutputVectorType translation = this->GetTranslation();

  // derivatives with respect to the scale
  this->m_Jacobian[0][0] =    ca * ( p[0] - cx ) - sa * ( p[1] - cy );
  this->m_Jacobian[1][0] =    sa * ( p[0] - cx ) + ca * ( p[1] - cy ); 

  // derivatives with respect to the angle
  this->m_Jacobian[0][1] = ( -sa * ( p[0] - cx ) - ca * ( p[1] - cy ) ) * m_Scale;
  this->m_Jacobian[1][1] = (  ca * ( p[0] - cx ) - sa * ( p[1] - cy ) ) * m_Scale; 

  // compute derivatives with respect to the translation part
  // first with respect to tx
  this->m_Jacobian[0][2] = 1.0;
  this->m_Jacobian[1][2] = 0.0;
  // first with respect to ty
  this->m_Jacobian[0][3] = 0.0;
  this->m_Jacobian[1][3] = 1.0;

  return this->m_Jacobian;

}

 
// Set Identity
template <class TScalarType>
void
Similarity2DTransform<TScalarType>
::SetIdentity(void)
{
  this->Superclass::SetIdentity();
  m_Scale = static_cast< TScalarType >( 1.0f );
}
 
// Print self
template<class TScalarType>
void
Similarity2DTransform<TScalarType>::
PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  os << indent << "Scale =" << m_Scale  << std::endl;
}

// Create and return an inverse transformation
template<class TScalarType>
void
Similarity2DTransform<TScalarType>::
CloneInverseTo( Pointer & result ) const
{
  result = New();
  result->SetCenter( this->GetCenter() );  // inverse have the same center
  result->SetScale( 1.0 / this->GetScale() );
  result->SetAngle( -this->GetAngle() );
  result->SetTranslation( -( this->GetInverseMatrix() * this->GetTranslation() ) );
}

// Create and return a clone of the transformation
template<class TScalarType>
void
Similarity2DTransform<TScalarType>::
CloneTo( Pointer & result ) const
{
  result = New();
  result->SetCenter( this->GetCenter() );
  result->SetScale( this->GetScale() );
  result->SetAngle( this->GetAngle() );
  result->SetTranslation( this->GetTranslation() );
}

// Set the similarity matrix
template<class TScalarType>
void
Similarity2DTransform<TScalarType>::
SetMatrix(const MatrixType & matrix )
{
  itkDebugMacro("setting  m_Matrix  to " << matrix ); 
 
  typename MatrixType::InternalMatrixType test = 
    matrix.GetVnlMatrix() * matrix.GetTranspose();

  test /= test[0][0]; // factor out the scale

  const double tolerance = 1e-10;
  if( !test.is_identity( tolerance ) ) 
    {
    itk::ExceptionObject ex(__FILE__,__LINE__,"Attempt to set a Non-Orthogonal matrix",ITK_LOCATION);
    throw ex;
    }

  this->SetVarMatrix( matrix );
  this->ComputeOffset();
  this->ComputeMatrixParameters();
  this->Modified();

}

} // namespace

#endif
