/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAffineTransform.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkAffineTransform_txx
#define _itkAffineTransform_txx

#include "itkNumericTraits.h"
#include "itkAffineTransform.h"
#include "vnl/algo/vnl_matrix_inverse.h"


namespace itk
{

// Constructor with default arguments
template<class TScalarType, unsigned int NDimensions>
AffineTransform<TScalarType, NDimensions>::
AffineTransform():Superclass(SpaceDimension,ParametersDimension)
{
  m_Matrix.SetIdentity();
  m_Inverse.SetIdentity();
  m_Offset.Fill( 0 );
  m_Singular = false;
}


// Constructor with default arguments
template<class TScalarType, unsigned int NDimensions>
AffineTransform<TScalarType, NDimensions>::
AffineTransform( unsigned int outputSpaceDimension, 
                 unsigned int parametersDimension   ):
       Superclass(outputSpaceDimension,parametersDimension)
{
  m_Matrix.SetIdentity();
  m_Inverse.SetIdentity();
  m_Offset.Fill( 0 );
  m_Singular = false;
}



// Constructor with explicit arguments
template<class TScalarType, unsigned int NDimensions>
AffineTransform<TScalarType, NDimensions>::
AffineTransform(const MatrixType &matrix, const OutputVectorType &offset)
{
  m_Matrix = matrix;
  m_Offset = offset;
  RecomputeInverse();
}




// Destructor
template<class TScalarType, unsigned int NDimensions>
AffineTransform<TScalarType, NDimensions>::
~AffineTransform()
{
  return;
}



// Print self
template<class TScalarType, unsigned int NDimensions>
void
AffineTransform<TScalarType, NDimensions>::
PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  unsigned int i, j;
  
  os << indent << "Matrix: " << std::endl;
  for (i = 0; i < NDimensions; i++) 
    {
    os << indent.GetNextIndent();
    for (j = 0; j < NDimensions; j++)
      {
      os << m_Matrix[i][j] << " ";
      }
    os << m_Offset[i] << std::endl;
    }
}


// Compose with another affine transformation
template<class TScalarType, unsigned int NDimensions>
void
AffineTransform<TScalarType, NDimensions>::
Compose(const Self * other, bool pre)
{
  if (pre) 
    {
    m_Offset = m_Matrix * other->m_Offset + m_Offset;
    m_Matrix = m_Matrix * other->m_Matrix;
    }
  else 
    {
    m_Offset = other->m_Matrix * m_Offset + other->m_Offset;
    m_Matrix = other->m_Matrix * m_Matrix;
    }
  RecomputeInverse();

  return;
}


// Compose with a translation
template<class TScalarType, unsigned int NDimensions>
void
AffineTransform<TScalarType, NDimensions>::
Translate(const OutputVectorType &offset, bool pre)
{
  if (pre) 
    {
    m_Offset += m_Matrix * offset;
    }
  else 
    {
    m_Offset += offset;
    }
  RecomputeInverse();

  return;
}


// Compose with isotropic scaling
template<class TScalarType, unsigned int NDimensions>
void
AffineTransform<TScalarType, NDimensions>::
Scale(const TScalarType &factor, bool pre) 
{
  if (pre) 
    {
    m_Matrix *= factor;
    }
  else 
    {
    m_Matrix *= factor;
    m_Offset *= factor;
    }
  RecomputeInverse();
  return;
}



// Compose with anisotropic scaling
template<class TScalarType, unsigned int NDimensions>
void
AffineTransform<TScalarType, NDimensions>::
Scale(const OutputVectorType &factor, bool pre) 
{
  MatrixType trans;
  unsigned int i, j;

  for (i = 0; i < NDimensions; i++) 
    {
    for (j = 0; j < NDimensions; j++) 
      {
      trans[i][j] = 0.0;
      }
    trans[i][i] = factor[i];
    }
  if (pre) 
    {
    m_Matrix = m_Matrix * trans;
    }
  else 
    {
    m_Matrix = trans * m_Matrix;
    m_Offset = trans * m_Offset;
    }
  RecomputeInverse();
  return;
}



// Compose with elementary rotation
template<class TScalarType, unsigned int NDimensions>
void
AffineTransform<TScalarType, NDimensions>::
Rotate(int axis1, int axis2, TScalarType angle, bool pre) 
{
  MatrixType trans;
  unsigned int i, j;

  for (i = 0; i < NDimensions; i++) 
    {
    for (j = 0; j < NDimensions; j++) 
      {
      trans[i][j] = 0.0;
      }
    trans[i][i] = 1.0;
    }
  trans[axis1][axis1] =  cos(angle);
  trans[axis1][axis2] =  sin(angle);
  trans[axis2][axis1] = -sin(angle);
  trans[axis2][axis2] =  cos(angle);
  if (pre) 
    {
    m_Matrix = m_Matrix * trans;
    }
  else 
    {
    m_Matrix = trans * m_Matrix;
    m_Offset = trans * m_Offset;
    }
  RecomputeInverse();
  return;
}


// Compose with 2D rotation
// \todo Find a way to generate a compile-time error
// is this is used with NDimensions != 2.
template<class TScalarType, unsigned int NDimensions>
void
AffineTransform<TScalarType, NDimensions>::
Rotate2D(TScalarType angle, bool pre)
{
  MatrixType trans;

  trans[0][0] =  cos(angle);
  trans[0][1] =  sin(angle);
  trans[1][0] = -sin(angle);
  trans[1][1] =  cos(angle);
  if (pre) 
    {
    m_Matrix = m_Matrix * trans;
    }
  else 
    {
    m_Matrix = trans * m_Matrix;
    m_Offset = trans * m_Offset;
    }
  RecomputeInverse();
  return;
}



// Compose with 3D rotation
// \todo Find a way to generate a compile-time error
// is this is used with NDimensions != 3.
template<class TScalarType, unsigned int NDimensions>
void
AffineTransform<TScalarType, NDimensions>::
Rotate3D(const OutputVectorType &axis, TScalarType angle, bool pre)
{
  MatrixType trans;
  ScalarType r, x1, x2, x3;
  ScalarType q0, q1, q2, q3;

  // Convert the axis to a unit vector
  r = sqrt(axis[0]*axis[0] + axis[1]*axis[1] + axis[2]*axis[2]);
  x1 = axis[0] / r;
  x2 = axis[1] / r;
  x3 = axis[2] / r;

  // Compute quaternion elements
  q0 = cos(angle/2.0);
  q1 = x1 * sin(angle/2.0);
  q2 = x2 * sin(angle/2.0);
  q3 = x3 * sin(angle/2.0);

  // Compute elements of the rotation matrix
  trans[0][0] = q0*q0 + q1*q1 - q2*q2 - q3*q3;
  trans[0][1] = 2.0*(q1*q2 - q0*q3);
  trans[0][2] = 2.0*(q1*q3 + q0*q2);
  trans[1][0] = 2.0*(q1*q2 + q0*q3);
  trans[1][1] = q0*q0 + q2*q2 - q1*q1 - q3*q3;
  trans[1][2] = 2.0*(q2*q3 - q0*q1);
  trans[2][0] = 2.0*(q1*q3 - q0*q2);
  trans[2][1] = 2.0*(q2*q3 + q0*q1);
  trans[2][2] = q0*q0 + q3*q3 - q1*q1 - q2*q2;

  // Compose rotation matrix with the existing matrix
  if (pre) 
    {
    m_Matrix = m_Matrix * trans;
    }
  else 
    {
    m_Matrix = trans * m_Matrix;
    m_Offset = trans * m_Offset;
    }
  RecomputeInverse();
  return;
}


// Compose with elementary rotation
template<class TScalarType, unsigned int NDimensions>
void
AffineTransform<TScalarType, NDimensions>::
Shear(int axis1, int axis2, TScalarType coef, bool pre)
{
  MatrixType trans;
  unsigned int i, j;

  for (i = 0; i < NDimensions; i++) 
    {
    for (j = 0; j < NDimensions; j++) 
      {
      trans[i][j] = 0.0;
      }
    trans[i][i] = 1.0;
    }
  trans[axis1][axis2] =  coef;
  if (pre) 
    {
    m_Matrix = m_Matrix * trans;
    }
  else 
    {
    m_Matrix = trans * m_Matrix;
    m_Offset = trans * m_Offset;
    }
  RecomputeInverse();
  return;
}


// Transform a point
template<class TScalarType, unsigned int NDimensions>
typename AffineTransform<TScalarType, NDimensions>::OutputPointType
AffineTransform<TScalarType, NDimensions>::
TransformPoint(const InputPointType &point) const 
{
  return m_Matrix * point + m_Offset;
}


// Transform a vector
template<class TScalarType, unsigned int NDimensions>
typename AffineTransform<TScalarType, NDimensions>::OutputVectorType
AffineTransform<TScalarType, NDimensions>::
TransformVector(const InputVectorType &vect) const 
{
  return m_Matrix * vect;
}


// Transform a vnl_vector_fixed
template<class TScalarType, unsigned int NDimensions>
typename AffineTransform<TScalarType, NDimensions>::OutputVnlVectorType
AffineTransform<TScalarType, NDimensions>::
TransformVector(const InputVnlVectorType &vect) const {
  return m_Matrix * vect;
}


// Transform a CovariantVector
template<class TScalarType, unsigned int NDimensions>
typename AffineTransform<TScalarType, NDimensions>::OutputCovariantVectorType
AffineTransform<TScalarType, NDimensions>::
TransformCovariantVector(const InputCovariantVectorType &vec) const 
{
  OutputCovariantVectorType  result;    // Converted vector

  for (unsigned int i = 0; i < NDimensions; i++) 
    {
    result[i] = NumericTraits<ScalarType>::Zero;
    for (unsigned int j = 0; j < NDimensions; j++) 
      {
      result[i] += m_Inverse[j][i]*vec[j]; // Inverse transposed
      }
    }
  return result;
}


// Back transform a point
template<class TScalarType, unsigned int NDimensions>
typename AffineTransform<TScalarType, NDimensions>::InputPointType
AffineTransform<TScalarType, NDimensions>::
BackTransform(const OutputPointType &point) const 
{
  InputPointType result;       // Converted point
  ScalarType temp[NDimensions];
  unsigned int i, j;

  for (j = 0; j < NDimensions; j++) 
    {
    temp[j] = point[j] - m_Offset[j];
    }

  for (i = 0; i < NDimensions; i++) 
    {
    result[i] = 0.0;
    for (j = 0; j < NDimensions; j++) 
      {
      result[i] += m_Inverse[i][j]*temp[j];
      }
    }
  return result;
}




// Back transform a vector
template<class TScalarType, unsigned int NDimensions>
typename AffineTransform<TScalarType, NDimensions>::InputVectorType
AffineTransform<TScalarType, NDimensions>::
BackTransform(const OutputVectorType &vect ) const 
{
  return m_Inverse * vect;
}




// Back transform a vnl_vector
template<class TScalarType, unsigned int NDimensions>
typename AffineTransform<TScalarType, NDimensions>::InputVnlVectorType
AffineTransform<TScalarType, NDimensions>::
BackTransform(const OutputVnlVectorType &vect ) const 
{
  return m_Inverse * vect;
}



// Back Transform a CovariantVector
template<class TScalarType, unsigned int NDimensions>
typename AffineTransform<TScalarType, NDimensions>::InputCovariantVectorType
AffineTransform<TScalarType, NDimensions>::
BackTransform(const OutputCovariantVectorType &vec) const 
{

  InputCovariantVectorType result;    // Converted vector

  for (unsigned int i = 0; i < NDimensions; i++) 
    {
    result[i] = NumericTraits<ScalarType>::Zero;
    for (unsigned int j = 0; j < NDimensions; j++) 
      {
      result[i] += m_Matrix[j][i]*vec[j]; // Direct matrix transposed
      }
    }
  return result;
}




// Back transform a given point which is represented as type PointType
template<class TScalarType, unsigned int NDimensions>
typename AffineTransform<TScalarType, NDimensions>::InputPointType
AffineTransform<TScalarType, NDimensions>::
BackTransformPoint(const OutputPointType &point) const
{
  InputPointType result;       // Converted point
  ScalarType temp[NDimensions];
  unsigned int i, j;
  
  for (j = 0; j < NDimensions; j++) 
    {
    temp[j] = point[j] - m_Offset[j];
    }

  for (i = 0; i < NDimensions; i++) 
    {
    result[i] = 0.0;
    for (j = 0; j < NDimensions; j++) 
      {
      result[i] += m_Inverse[i][j]*temp[j];
      }
    }
  return result;
}





// Create and return an inverse transformation
template<class TScalarType, unsigned int NDimensions>
typename AffineTransform<TScalarType, NDimensions>::Pointer
AffineTransform<TScalarType, NDimensions>::
Inverse( void ) const
{
  Pointer result = New();
  result->m_Matrix   =   m_Inverse;
  result->m_Inverse  =   m_Matrix;
  result->m_Offset   = -(m_Inverse * m_Offset);
  result->m_Singular =   false;
  return result;
}




// Compute a distance between two affine transforms
template<class TScalarType, unsigned int NDimensions>
typename AffineTransform<TScalarType, NDimensions>::ScalarType
AffineTransform<TScalarType, NDimensions>::
Metric(const Self * other) const
{
  ScalarType result = 0.0, term;

  for (unsigned int i = 0; i < NDimensions; i++) 
    {
    for (unsigned int j = 0; j < NDimensions; j++) 
      {
      term = m_Matrix[i][j] - other->m_Matrix[i][j];
      result += term * term;
      }
    term = m_Offset[i] - other->m_Offset[i];
    result += term * term;
    }
  return sqrt(result);
}



// Compute a distance between self and the identity transform
template<class TScalarType, unsigned int NDimensions>
typename AffineTransform<TScalarType, NDimensions>::ScalarType
AffineTransform<TScalarType, NDimensions>::
Metric(void) const
{
  ScalarType result = 0.0, term;

  for (unsigned int i = 0; i < NDimensions; i++) 
    {
    for (unsigned int j = 0; j < NDimensions; j++) 
      {
      if (i == j)
        {
        term = m_Matrix[i][j] - 1.0;
        }
      else
        {
        term = m_Matrix[i][j];
        }
      result += term * term;
      }
    term = m_Offset[i];
    result += term * term;
    }

  return sqrt(result);
}



// Recompute the inverse matrix (internal)
template<class TScalarType, unsigned int NDimensions>
void
AffineTransform<TScalarType, NDimensions>::
RecomputeInverse( void )
{
  m_Singular = false;
  try 
    {
    m_Inverse  = m_Matrix.GetInverse();
    }
  catch(...) 
    {
    m_Singular = true;
    }

  this->Modified();
  return;
}




// Get parameters
template<class TScalarType, unsigned int NDimensions>
const typename AffineTransform<TScalarType, NDimensions>::ParametersType &
AffineTransform<TScalarType, NDimensions>::
GetParameters( void ) const
{

  // Transfer the linear part
  unsigned int par = 0;

  for(unsigned int row=0; row<NDimensions; row++) 
  {
    for(unsigned int col=0; col<NDimensions; col++) 
    {
      m_Parameters[par] = m_Matrix[row][col];
      ++par;
    }
  }

  // Transfer the constant part
  for(unsigned int i=0; i<NDimensions; i++) 
  {
    m_Parameters[par] = m_Offset[i];
    ++par;
  }

  return m_Parameters;

}




// Set parameters
template<class TScalarType, unsigned int NDimensions>
void
AffineTransform<TScalarType, NDimensions>::
SetParameters( const ParametersType & parameters )
{

  // Transfer the linear part
  unsigned int par = 0;

  m_Parameters = parameters;

  for(unsigned int row=0; row<NDimensions; row++) 
  {
    for(unsigned int col=0; col<NDimensions; col++) 
    {
      m_Matrix[row][col] = m_Parameters[par];
      ++par;
    }
  }

  // Transfer the constant part
  for(unsigned int i=0; i<NDimensions; i++) 
  {
    m_Offset[i] = m_Parameters[par];
    ++par;
  }

  this->Modified();
}


// Compute the Jacobian in one position 
template<class TScalarType, unsigned int NDimensions>
const typename AffineTransform<TScalarType, NDimensions>::JacobianType & 
AffineTransform<TScalarType, NDimensions>::
GetJacobian( const InputPointType & p ) const
{
  
  // The Jacobian of the affine transform is composed of
  // subblocks of diagonal matrices, each one of them having
  // a constant value in the diagonal.

  m_Jacobian.Fill( 0.0 );

  unsigned int blockOffset = 0;
  
  for(unsigned int block=0; block < SpaceDimension; block++) 
  {
    for(unsigned int dim=0; dim < SpaceDimension; dim++ ) 
    {
       m_Jacobian( block , blockOffset + dim ) = p[dim];
    }

    blockOffset += SpaceDimension;

  }

  for(unsigned int dim=0; dim < SpaceDimension; dim++ ) 
  {
// Should a "translation scale be provided ?
//     m_Jacobian[ dim ][ blockOffset + dim ] = m_TranslationScale;
     m_Jacobian( dim , blockOffset + dim ) = 1.0;
  }

  return m_Jacobian;

}


 

} // namespace

#endif
