/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRigid3DTransform.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
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
  m_InverseMatrix.SetIdentity();
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
  m_InverseMatrix = m_RotationMatrix.GetTranspose();
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
  m_InverseMatrix = m_RotationMatrix.GetTranspose();
}


// Compose with a translation
template<class TScalarType>
void
Rigid3DTransform<TScalarType>::
Translate(const OffsetType &offset, bool pre)
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
    return m_InverseMatrix * (point - m_Offset);
}

// Back transform a vector
template<class TScalarType>
typename Rigid3DTransform<TScalarType>::InputVectorType
Rigid3DTransform<TScalarType>::
BackTransform(const OutputVectorType &vect ) const 
{
    return  m_InverseMatrix * vect;
}

// Back transform a vnl_vector
template<class TScalarType>
typename Rigid3DTransform<TScalarType>::InputVnlVectorType
Rigid3DTransform<TScalarType>::
BackTransform(const OutputVnlVectorType &vect ) const 
{
    return  m_InverseMatrix * vect;
}


// Back Transform a CovariantVector
template<class TScalarType>
typename Rigid3DTransform<TScalarType>::InputCovariantVectorType
Rigid3DTransform<TScalarType>::
BackTransform(const OutputCovariantVectorType &vect) const 
{
  return m_RotationMatrix * vect;
}



// Create and return an inverse transformation
template<class TScalarType>
typename Rigid3DTransform<TScalarType>::Pointer
Rigid3DTransform<TScalarType>::
Inverse( void ) const
{
  Pointer result = New();
  result->m_RotationMatrix   =   m_InverseMatrix;
  result->m_InverseMatrix    =   m_RotationMatrix;
  result->m_Offset           = -(m_InverseMatrix * m_Offset);
  return result;
}


  
// Compute the Jacobian in one position 
template<class TScalarType >
void
Rigid3DTransform< TScalarType >::
SetIdentity( void ) 
{
  m_Offset.Fill( NumericTraits< TScalarType >::Zero );
  m_RotationMatrix.SetIdentity();
  m_InverseMatrix.SetIdentity();
}


  
// Compute the Jacobian in one position 
template<class TScalarType >
const typename Rigid3DTransform<TScalarType>::JacobianType & 
Rigid3DTransform< TScalarType >::
GetJacobian( const InputPointType & p ) const
{
  

  m_Jacobian.Fill( NumericTraits< ITK_TYPENAME JacobianType::ValueType >::Zero );

  // TODO

  return m_Jacobian;

}


 
} // namespace

#endif
