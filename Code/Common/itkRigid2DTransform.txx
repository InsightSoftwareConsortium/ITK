/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRigid2DTransform.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkRigid2DTransform_txx
#define _itkRigid2DTransform_txx

#include "itkRigid2DTransform.h"


namespace itk
{

// Constructor with default arguments
template<class TScalarType>
Rigid2DTransform<TScalarType>::
Rigid2DTransform():Superclass(OutputSpaceDimension,ParametersDimension)
{
  m_Offset.Fill( 0 );
  m_RotationMatrix.SetIdentity();
  m_InverseMatrix.SetIdentity();
}
 

// Constructor with arguments
template<class TScalarType>
Rigid2DTransform<TScalarType>::
Rigid2DTransform( unsigned int spaceDimension, 
                  unsigned int parametersDimension):
  Superclass(spaceDimension,parametersDimension)
{
  m_Offset.Fill( 0 );
  m_RotationMatrix.SetIdentity();
  m_InverseMatrix.SetIdentity();
  m_Angle = NumericTraits< TScalarType >::Zero;
  m_Center.Fill( 0.0 );
  m_Translation.Fill( 0.0 );
}
 

// Destructor
template<class TScalarType>
Rigid2DTransform<TScalarType>::
~Rigid2DTransform()
{
}


// Print self
template<class TScalarType>
void
Rigid2DTransform<TScalarType>::
PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  os << indent << "Offset: " << m_Offset   << std::endl;
  os << indent << "RotationMatrix: " << m_RotationMatrix   << std::endl;
  os << indent << "Angle       = " << m_Angle        << std::endl;
  os << indent << "Center      = " << m_Center       << std::endl;
  os << indent << "Translation = " << m_Translation  << std::endl;
}


//
template<class TScalarType>
const typename Rigid2DTransform< TScalarType >::MatrixType &
Rigid2DTransform<TScalarType>::
GetInverseMatrix() const
{
  // If the transform has been modified we recompute the inverse
  if(m_InverseMatrixMTime != m_RotationMatrixMTime)
    {
    m_InverseMatrix = m_RotationMatrix.GetTranspose();
    m_InverseMatrixMTime = m_RotationMatrixMTime;
    }
  return m_InverseMatrix; 
}

// Compose with another affine transformation
template<class TScalarType>
void
Rigid2DTransform<TScalarType>::
SetRotationMatrix(const MatrixType & matrix )
{
  itkDebugMacro("setting  m_RotationMatrix  to " << matrix ); 
  // The matrix must be orthogonal otherwise it is not
  // representing a valid rotaion in 2D space
  typename MatrixType::InternalMatrixType test = 
    matrix.GetVnlMatrix() * matrix.GetTranspose();

  const double tolerance = 1e-10;
  if( !test.is_identity( tolerance ) ) 
    {
    itk::ExceptionObject ex;
    ex.SetDescription("Attempt to set a Non-Orthogonal matrix");
    ex.SetLocation(__FILE__);
    throw ex;
    }

  m_RotationMatrix = matrix;
  m_RotationMatrixMTime.Modified();
  this->Modified(); 
}


// Compose with another affine transformation
template<class TScalarType>
void
Rigid2DTransform<TScalarType>::
Compose(const Self * other, bool pre )
{
  if (pre) 
    {
    m_Offset         = m_RotationMatrix * other->m_Offset + m_Offset;
    m_RotationMatrix = m_RotationMatrix * other->m_RotationMatrix;
    }
  else 
    {
    m_Offset         = other->m_RotationMatrix * m_Offset + other->m_Offset;
    m_RotationMatrix = other->m_RotationMatrix * m_RotationMatrix;
    }
  m_RotationMatrixMTime.Modified();
  this->Modified();
}


// Compose with a translation
template<class TScalarType>
void
Rigid2DTransform<TScalarType>::
Translate(const OffsetType &offset, bool)
{
  m_Offset += offset;
  return;
}




// Transform a point
template<class TScalarType>
typename Rigid2DTransform<TScalarType>::OutputPointType
Rigid2DTransform<TScalarType>::
TransformPoint(const InputPointType &point) const 
{
  return m_RotationMatrix * point + m_Offset; 
}


// Transform a vector
template<class TScalarType>
typename Rigid2DTransform<TScalarType>::OutputVectorType
Rigid2DTransform<TScalarType>::
TransformVector(const InputVectorType &vect) const 
{
  return  m_RotationMatrix * vect;
}


// Transform a vnl_vector_fixed
template<class TScalarType>
typename Rigid2DTransform<TScalarType>::OutputVnlVectorType
Rigid2DTransform<TScalarType>::
TransformVector(const InputVnlVectorType &vect) const 
{
  return  m_RotationMatrix * vect;
}


// Transform a CovariantVector
template<class TScalarType>
typename Rigid2DTransform<TScalarType>::OutputCovariantVectorType
Rigid2DTransform<TScalarType>::
TransformCovariantVector(const InputCovariantVectorType &vect) const 
{
  // Covariant vectors are transformed like contravariant
  // vectors under orthogonal transformations.
  return  m_RotationMatrix * vect;
}



// Back transform a point
template<class TScalarType>
typename Rigid2DTransform<TScalarType>::InputPointType
Rigid2DTransform<TScalarType>::
BackTransform(const OutputPointType &point) const 
{
  return m_InverseMatrix * (point - m_Offset);
}

// Back transform a vector
template<class TScalarType>
typename Rigid2DTransform<TScalarType>::InputVectorType
Rigid2DTransform<TScalarType>::
BackTransform(const OutputVectorType &vect ) const 
{
  return  m_InverseMatrix * vect;
}

// Back transform a vnl_vector
template<class TScalarType>
typename Rigid2DTransform<TScalarType>::InputVnlVectorType
Rigid2DTransform<TScalarType>::
BackTransform(const OutputVnlVectorType &vect ) const 
{
  return  m_InverseMatrix * vect;
}


// Back Transform a CovariantVector
template<class TScalarType>
typename Rigid2DTransform<TScalarType>::InputCovariantVectorType
Rigid2DTransform<TScalarType>::
BackTransform(const OutputCovariantVectorType &vect) const 
{
  return m_RotationMatrix * vect;
}

// Create and return an inverse transformation
template<class TScalarType>
bool 
Rigid2DTransform<TScalarType>::
GetInverse(Self* inverse) const
{
  if(!inverse)
    {
    return false;
    }

  inverse->m_RotationMatrix   =   this->GetInverseMatrix();
  inverse->m_InverseMatrix    =   m_RotationMatrix;
  inverse->m_Offset           = -(this->GetInverseMatrix() * m_Offset);

  return true;
}

  
// Compute the Jacobian in one position 
template<class TScalarType >
void
Rigid2DTransform< TScalarType >::
SetIdentity( void ) 
{
  m_Offset.Fill( NumericTraits< TScalarType >::Zero );
  m_RotationMatrix.SetIdentity();
  m_InverseMatrix.SetIdentity();

  m_Angle = NumericTraits< TScalarType >::Zero;
  m_Center.Fill( 0.0 );
  m_Translation.Fill( 0.0 );

}

template <class TScalarType>
void
Rigid2DTransform<TScalarType>
::SetCenter( const InputPointType & center )
{
  m_Center = center;
  this->ComputeMatrixAndOffset();
}

template <class TScalarType>
void
Rigid2DTransform<TScalarType>
::SetTranslation( const OutputVectorType & translation )
{
  m_Translation = translation;
  this->ComputeMatrixAndOffset();
}



// Set Rotational Part
template <class TScalarType>
void
Rigid2DTransform<TScalarType>
::SetAngle(TScalarType angle)
{
  m_Angle = angle;
  this->ComputeMatrixAndOffset();
}


// Set Rotational Part
template <class TScalarType>
void
Rigid2DTransform<TScalarType>
::SetAngleInDegrees(TScalarType angle)
{
  const TScalarType angleInRadians = angle * atan(1.0) / 45.0;
  this->SetAngle( angleInRadians );
}

// Compute the matrix
template <class TScalarType>
void
Rigid2DTransform<TScalarType>
::ComputeMatrixAndOffset( void )
{
  const double ca = cos(this->GetAngle());
  const double sa = sin(this->GetAngle());

  const double cx = this->GetCenter()[0];
  const double cy = this->GetCenter()[1];

  const double tx = this->GetTranslation()[0];
  const double ty = this->GetTranslation()[1];

  this->m_RotationMatrix[0][0]= ca; this->m_RotationMatrix[0][1]=-sa;
  this->m_RotationMatrix[1][0]= sa; this->m_RotationMatrix[1][1]= ca;

  this->m_RotationMatrixMTime.Modified();

  OffsetType offset;

  offset[0] = tx + sa * cy + ( 1.0 - ca ) * cx;
  offset[1] = ty - sa * cx + ( 1.0 - ca ) * cy;

  this->SetOffset( offset );

  this->Modified();
}

// Compute the Jacobian in one position 
template<class TScalarType >
const typename Rigid2DTransform<TScalarType>::JacobianType & 
Rigid2DTransform< TScalarType >::
GetJacobian( const InputPointType & ) const
{
  this->m_Jacobian.Fill( NumericTraits< ITK_TYPENAME JacobianType::ValueType >::Zero );
  return this->m_Jacobian;
}


 
} // namespace

#endif
