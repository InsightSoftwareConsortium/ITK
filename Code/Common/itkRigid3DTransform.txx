/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRigid3DTransform.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkRigid3DTransform_txx
#define _itkRigid3DTransform_txx

#include "itkRigid3DTransform.h"


namespace itk
{

// Constructor with default arguments
template<class TScalarType>
Rigid3DTransform<TScalarType>::
Rigid3DTransform():Superclass(OutputSpaceDimension,ParametersDimension)
{
  m_Offset.Fill( 0 );
  m_RotationMatrix.SetIdentity();
  m_RotationMatrixMTime.Modified();
}
 

// Constructor with default arguments
template<class TScalarType>
Rigid3DTransform<TScalarType>::
Rigid3DTransform(unsigned int spaceDimension, 
                 unsigned int parametersDimension):
  Superclass(spaceDimension,parametersDimension)
{
  m_Offset.Fill( 0 );
  m_RotationMatrix.SetIdentity();
  m_RotationMatrixMTime.Modified();
}
 
// Destructor
template<class TScalarType>
Rigid3DTransform<TScalarType>::
~Rigid3DTransform()
{
}


// Print self
template<class TScalarType>
void
Rigid3DTransform<TScalarType>::
PrintSelf(std::ostream &os, Indent indent) const
{

  Superclass::PrintSelf(os,indent);
  
  os << indent << "Offset: " << m_Offset   << std::endl;
  os << indent << "RotationMatrix: " << m_RotationMatrix   << std::endl;
}


// Compose with another affine transformation
template<class TScalarType>
void
Rigid3DTransform<TScalarType>::
SetRotationMatrix(const MatrixType & matrix )
{
  itkDebugMacro("setting  m_RotationMatrix  to " << matrix ); 
  // The matrix must be orthogonal otherwise it is not
  // representing a valid rotaion in 3D space
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
Rigid3DTransform<TScalarType>::
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
}


// Compose with a translation
template<class TScalarType>
void
Rigid3DTransform<TScalarType>::
Translate(const OffsetType &offset, bool)
{
  m_Offset += offset;
  return;
}




// Transform a point
template<class TScalarType>
typename Rigid3DTransform<TScalarType>::OutputPointType
Rigid3DTransform<TScalarType>::
TransformPoint(const InputPointType &point) const 
{
  return m_RotationMatrix * point + m_Offset; 
}


// Transform a vector
template<class TScalarType>
typename Rigid3DTransform<TScalarType>::OutputVectorType
Rigid3DTransform<TScalarType>::
TransformVector(const InputVectorType &vect) const 
{
  return  m_RotationMatrix * vect;
}


// Transform a vnl_vector_fixed
template<class TScalarType>
typename Rigid3DTransform<TScalarType>::OutputVnlVectorType
Rigid3DTransform<TScalarType>::
TransformVector(const InputVnlVectorType &vect) const 
{
  return  m_RotationMatrix * vect;
}


// Transform a CovariantVector
template<class TScalarType>
typename Rigid3DTransform<TScalarType>::OutputCovariantVectorType
Rigid3DTransform<TScalarType>::
TransformCovariantVector(const InputCovariantVectorType &vect) const 
{
  // Covariant vectors are transformed like contravariant
  // vectors under orthogonal transformations.
  return  m_RotationMatrix * vect;
}



// Back transform a point
template<class TScalarType>
typename Rigid3DTransform<TScalarType>::InputPointType
Rigid3DTransform<TScalarType>::
BackTransform(const OutputPointType &point) const 
{
  return this->GetInverseMatrix() * (point - m_Offset);
}

// Back transform a vector
template<class TScalarType>
typename Rigid3DTransform<TScalarType>::InputVectorType
Rigid3DTransform<TScalarType>::
BackTransform(const OutputVectorType &vect ) const 
{
  return  this->GetInverseMatrix() * vect;
}

// Back transform a vnl_vector
template<class TScalarType>
typename Rigid3DTransform<TScalarType>::InputVnlVectorType
Rigid3DTransform<TScalarType>::
BackTransform(const OutputVnlVectorType &vect ) const 
{
  return  this->GetInverseMatrix() * vect;
}


// Back Transform a CovariantVector
template<class TScalarType>
typename Rigid3DTransform<TScalarType>::InputCovariantVectorType
Rigid3DTransform<TScalarType>::
BackTransform(const OutputCovariantVectorType &vect) const 
{
  return m_RotationMatrix * vect;
}


//
template<class TScalarType>
typename Rigid3DTransform< TScalarType >::MatrixType
Rigid3DTransform<TScalarType>::
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


// Create and return an inverse transformation
template<class TScalarType>
bool 
Rigid3DTransform<TScalarType>::
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
Rigid3DTransform< TScalarType >::
SetIdentity( void ) 
{
  m_Offset.Fill( NumericTraits< TScalarType >::Zero );
  m_RotationMatrix.SetIdentity();
  m_RotationMatrixMTime.Modified();
}


  
// Compute the Jacobian in one position 
template<class TScalarType >
const typename Rigid3DTransform<TScalarType>::JacobianType & 
Rigid3DTransform< TScalarType >::
GetJacobian( const InputPointType & ) const
{
  

  this->m_Jacobian.Fill( NumericTraits< ITK_TYPENAME JacobianType::ValueType >::Zero );

  // TODO

  return this->m_Jacobian;

}


 
} // namespace

#endif
