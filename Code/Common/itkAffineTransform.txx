/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAffineTransform.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef _itkAffineTransform_txx
#define _itkAffineTransform_txx

#include "itkNumericTraits.h"
#include "itkAffineTransform.h"
#include "vnl/algo/vnl_matrix_inverse.h"


namespace itk
{

// Constructor with default arguments
template<class TScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType >
AffineTransform<TScalarType, NDimensions,TParameters,TJacobianType>::
AffineTransform()
{
  m_Matrix.SetIdentity();
  m_Inverse.SetIdentity();
  m_Offset.Fill( 0 );
  m_Singular = false;
}



// Constructor with explicit arguments
template<class TScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType >
AffineTransform<TScalarType, NDimensions,TParameters,TJacobianType>::
AffineTransform(const MatrixType &matrix, const OutputVectorType &offset)
{
  m_Matrix = matrix;
  m_Offset = offset;
  RecomputeInverse();
}




// Copy Constructor
template <class TScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType >
AffineTransform<TScalarType, NDimensions,TParameters,TJacobianType>::
AffineTransform( const AffineTransform<TScalarType, NDimensions,TParameters,TJacobianType> & other )
{
  m_Matrix    = other.m_Matrix;
  m_Offset    = other.m_Offset;
  m_Inverse   = other.m_Inverse;
  m_Singular  = other.m_Singular;
}



// Destructor
template<class TScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType >
AffineTransform<TScalarType, NDimensions,TParameters,TJacobianType>::
~AffineTransform()
{
  return;
}


// Assignment Operator
template <class TScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType >
const AffineTransform<TScalarType, NDimensions,TParameters,TJacobianType> &
AffineTransform<TScalarType, NDimensions,TParameters,TJacobianType>::
operator=( const Self & other )
{
  m_Matrix   = other.m_Matrix;
  m_Offset   = other.m_Offset;
  m_Inverse  = other.m_Inverse;
  m_Singular = other.m_Singular;
  return *this;
}



// Print self
template<class TScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType >
void
AffineTransform<TScalarType, NDimensions,TParameters,TJacobianType>::
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
template<class TScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType >
void
AffineTransform<TScalarType, NDimensions,TParameters,TJacobianType>::
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
template<class TScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType >
void
AffineTransform<TScalarType, NDimensions,TParameters,TJacobianType>::
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
template<class TScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType >
void
AffineTransform<TScalarType, NDimensions,TParameters,TJacobianType>::
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
template<class TScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType >
void
AffineTransform<TScalarType, NDimensions,TParameters,TJacobianType>::
Scale(const InputVectorType &factor, bool pre) 
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
template<class TScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType >
void
AffineTransform<TScalarType, NDimensions,TParameters,TJacobianType>::
Rotate(int axis1, int axis2, double angle, bool pre) 
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
template<class TScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType >
void
AffineTransform<TScalarType, NDimensions,TParameters,TJacobianType>::
Rotate2D(double angle, bool pre)
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
template<class TScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType >
void
AffineTransform<TScalarType, NDimensions,TParameters,TJacobianType>::
Rotate3D(const InputVectorType &axis, double angle, bool pre)
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
template<class TScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType >
void
AffineTransform<TScalarType, NDimensions,TParameters,TJacobianType>::
Shear(int axis1, int axis2, double coef, bool pre)
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
template<class TScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType >
AffineTransform<TScalarType, NDimensions,TParameters,TJacobianType>::OutputPointType
AffineTransform<TScalarType, NDimensions,TParameters,TJacobianType>::
TransformPoint(const InputPointType &point) const 
{
  return m_Matrix * point + m_Offset;
}


// Transform a vector
template<class TScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType >
AffineTransform<TScalarType, NDimensions,TParameters,TJacobianType>::OutputVectorType
AffineTransform<TScalarType, NDimensions,TParameters,TJacobianType>::
TransformVector(const InputVectorType &vect) const 
{
  return m_Matrix * vect;
}


// Transform a vnl_vector_fixed
template<class TScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType >
AffineTransform<TScalarType, NDimensions,TParameters,TJacobianType>::OutputVnlVectorType
AffineTransform<TScalarType, NDimensions,TParameters,TJacobianType>::
TransformVector(const InputVnlVectorType &vect) const {
  return m_Matrix * vect;
}


// Transform a CovariantVector
template<class TScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType >
AffineTransform<TScalarType, NDimensions,TParameters,TJacobianType>::OutputCovariantVectorType
AffineTransform<TScalarType, NDimensions,TParameters,TJacobianType>::
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
template<class TScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType >
AffineTransform<TScalarType, NDimensions,TParameters,TJacobianType>::InputPointType
AffineTransform<TScalarType, NDimensions,TParameters,TJacobianType>::
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
template<class TScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType >
AffineTransform<TScalarType, NDimensions,TParameters,TJacobianType>::InputVectorType
AffineTransform<TScalarType, NDimensions,TParameters,TJacobianType>::
BackTransform(const OutputVectorType &vect ) const 
{
  return m_Inverse * vect;
}




// Back transform a vnl_vector
template<class TScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType >
AffineTransform<TScalarType, NDimensions,TParameters,TJacobianType>::InputVnlVectorType
AffineTransform<TScalarType, NDimensions,TParameters,TJacobianType>::
BackTransform(const OutputVnlVectorType &vect ) const 
{
  return m_Inverse * vect;
}



// Back Transform a CovariantVector
template<class TScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType >
AffineTransform<TScalarType, NDimensions,TParameters,TJacobianType>::InputCovariantVectorType
AffineTransform<TScalarType, NDimensions,TParameters,TJacobianType>::
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
template<class TScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType >
AffineTransform<TScalarType, NDimensions,TParameters,TJacobianType>::InputPointType
AffineTransform<TScalarType, NDimensions,TParameters,TJacobianType>::
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
template<class TScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType >
AffineTransform<TScalarType, NDimensions,TParameters,TJacobianType>::Pointer
AffineTransform<TScalarType, NDimensions,TParameters,TJacobianType>::
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
template<class TScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType >
double
AffineTransform<TScalarType, NDimensions,TParameters,TJacobianType>::
Metric(const Self * other) const
{
  double result = 0.0, term;

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
template<class TScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType >
double
AffineTransform<TScalarType, NDimensions,TParameters,TJacobianType>::
Metric(void) const
{
  double result = 0.0, term;

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
template<class TScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType >
void
AffineTransform<TScalarType, NDimensions,TParameters,TJacobianType>::
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
  return;
}



// Set parameters
template<class TScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType >
void
AffineTransform<TScalarType, NDimensions,TParameters,TJacobianType>::
SetParameters( const ParametersType & parameters )
{

  // Transfer the linear part
  unsigned int par = 0;

  for(unsigned int row=0; row<NDimensions; row++) 
  {
    for(unsigned int col=0; col<NDimensions; col++) 
    {
      m_Matrix[row][col] = parameters[par];
      ++par;
    }
  }

  // Transfer the constant part
  for(unsigned int i=0; i<NDimensions; i++) 
  {
    m_Offset[i] = parameters[par];
    ++par;
  }


}


// Set parameters
template<class TScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType >
const AffineTransform<TScalarType, NDimensions,TParameters,TJacobianType>::JacobianType &
AffineTransform<TScalarType, NDimensions,TParameters,TJacobianType>::
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
       m_Jacobian[ block ][ blockOffset + dim ] = p[dim];
    }

    blockOffset += SpaceDimension;

  }

  for(unsigned int dim=0; dim < SpaceDimension; dim++ ) 
  {
// Should a "translation scale be provided ?
//     m_Jacobian[ dim ][ blockOffset + dim ] = m_TranslationScale;
     m_Jacobian[ dim ][ blockOffset + dim ] = 1.0;
  }

  return m_Jacobian;

}


 

} // namespace

#endif
