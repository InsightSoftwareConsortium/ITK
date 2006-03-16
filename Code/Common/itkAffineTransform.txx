/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAffineTransform.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkAffineTransform_txx
#define __itkAffineTransform_txx

#include "itkNumericTraits.h"
#include "itkAffineTransform.h"
#include "vnl/algo/vnl_matrix_inverse.h"


namespace itk
{

/** Constructor with default arguments */
template<class TScalarType, unsigned int NDimensions>
AffineTransform<TScalarType, NDimensions>::
AffineTransform(): Superclass(SpaceDimension,ParametersDimension)
{
}


/** Constructor with default arguments */
template<class TScalarType, unsigned int NDimensions>
AffineTransform<TScalarType, NDimensions>::
AffineTransform( unsigned int outputSpaceDimension, 
                 unsigned int parametersDimension   ):
  Superclass(outputSpaceDimension,parametersDimension)
{
}


/** Constructor with explicit arguments */
template<class TScalarType, unsigned int NDimensions>
AffineTransform<TScalarType, NDimensions>::
AffineTransform(const MatrixType & matrix,
               const OutputVectorType & offset):
  Superclass(matrix, offset)
{
}


/**  Destructor */
template<class TScalarType, unsigned int NDimensions>
AffineTransform<TScalarType, NDimensions>::
~AffineTransform()
{
  return;
}


/** Print self */
template<class TScalarType, unsigned int NDimensions>
void
AffineTransform<TScalarType, NDimensions>::
PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
}


/** Compose with a translation */
template<class TScalarType, unsigned int NDimensions>
void
AffineTransform<TScalarType, NDimensions>::
Translate(const OutputVectorType &trans, bool pre)
{
  OutputVectorType newTranslation = this->GetTranslation();
  if (pre) 
    {
    newTranslation += this->GetMatrix() * trans;
    }
  else 
    {
    newTranslation += trans;
    }
  this->SetVarTranslation(newTranslation);
  this->ComputeOffset();
  this->Modified();
  return;
}


/** Compose with isotropic scaling */
template<class TScalarType, unsigned int NDimensions>
void
AffineTransform<TScalarType, NDimensions>
::Scale(const TScalarType &factor, bool pre) 
{
  if (pre) 
    {
    MatrixType newMatrix = this->GetMatrix();
    newMatrix *= factor;
    this->SetVarMatrix(newMatrix);
    }
  else 
    {
    MatrixType newMatrix = this->GetMatrix();
    newMatrix *= factor;
    this->SetVarMatrix(newMatrix);

    OutputVectorType newTranslation = this->GetTranslation();
    newTranslation *= factor;
    this->SetVarTranslation(newTranslation);
    }
  this->ComputeMatrixParameters();
  this->ComputeOffset();
  this->Modified();
  return;
}


/** Compose with anisotropic scaling */
template<class TScalarType, unsigned int NDimensions>
void
AffineTransform<TScalarType, NDimensions>
::Scale(const OutputVectorType &factor, bool pre) 
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
    this->SetVarMatrix( this->GetMatrix() * trans );
    }
  else 
    {
    this->SetVarMatrix( trans * this->GetMatrix() );
    this->SetVarTranslation( trans * this->GetTranslation() );
    }
  this->ComputeMatrixParameters();
  this->ComputeOffset();
  this->Modified();
  return;
}


/** Compose with elementary rotation */
template<class TScalarType, unsigned int NDimensions>
void
AffineTransform<TScalarType, NDimensions>
::Rotate(int axis1, int axis2, TScalarType angle, bool pre) 
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
    this->SetVarMatrix( this->GetMatrix() * trans );
    }
  else 
    {
    this->SetVarMatrix( trans * this->GetMatrix() );
    this->SetVarTranslation( trans * this->GetTranslation() );
    }
  this->ComputeMatrixParameters();
  this->ComputeOffset();
  this->Modified();
  return;
}


/** Compose with 2D rotation
 * \todo Find a way to generate a compile-time error
 * is this is used with NDimensions != 2. */
template<class TScalarType, unsigned int NDimensions>
void
AffineTransform<TScalarType, NDimensions>
::Rotate2D(TScalarType angle, bool pre)
{
  MatrixType trans;

  trans[0][0] =  cos(angle);
  trans[0][1] = -sin(angle);
  trans[1][0] = sin(angle);
  trans[1][1] =  cos(angle);
  if (pre) 
    {
    this->SetVarMatrix( this->GetMatrix() * trans );
    }
  else 
    {
    this->SetVarMatrix( trans * this->GetMatrix() );
    this->SetVarTranslation( trans * this->GetTranslation() );
    }
  this->ComputeMatrixParameters();
  this->ComputeOffset();
  this->Modified();
  return;
}


/** Compose with 3D rotation
 *  \todo Find a way to generate a compile-time error
 *  is this is used with NDimensions != 3. */
template<class TScalarType, unsigned int NDimensions>
void
AffineTransform<TScalarType, NDimensions>
::Rotate3D(const OutputVectorType &axis, TScalarType angle, bool pre)
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
    this->SetVarMatrix( this->GetMatrix() * trans );
    }
  else 
    {
    this->SetVarMatrix( trans * this->GetMatrix() );
    this->SetVarTranslation( trans * this->GetTranslation() );
    }
  this->ComputeMatrixParameters();
  this->ComputeOffset();
  this->Modified();
  return;
}


/** Compose with elementary rotation */
template<class TScalarType, unsigned int NDimensions>
void
AffineTransform<TScalarType, NDimensions>
::Shear(int axis1, int axis2, TScalarType coef, bool pre)
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
    this->SetVarMatrix( this->GetMatrix() * trans );
    }
  else 
    {
    this->SetVarMatrix( trans * this->GetMatrix() );
    this->SetVarTranslation( trans * this->GetTranslation() );
    }
  this->ComputeMatrixParameters();
  this->ComputeOffset();
  this->Modified();
  return;
}


/** Compute a distance between two affine transforms */
template<class TScalarType, unsigned int NDimensions>
typename AffineTransform<TScalarType, NDimensions>::ScalarType
AffineTransform<TScalarType, NDimensions>
::Metric(const Self * other) const
{
  ScalarType result = 0.0, term;

  for (unsigned int i = 0; i < NDimensions; i++) 
    {
    for (unsigned int j = 0; j < NDimensions; j++) 
      {
      term = this->GetMatrix()[i][j] - other->GetMatrix()[i][j];
      result += term * term;
      }
    term = this->GetOffset()[i] - other->GetOffset()[i];
    result += term * term;
    }
  return sqrt(result);
}


/** Compute a distance between self and the identity transform */
template<class TScalarType, unsigned int NDimensions>
typename AffineTransform<TScalarType, NDimensions>::ScalarType
AffineTransform<TScalarType, NDimensions>
::Metric(void) const
{
  ScalarType result = 0.0, term;

  for (unsigned int i = 0; i < NDimensions; i++) 
    {
    for (unsigned int j = 0; j < NDimensions; j++) 
      {
      if (i == j)
        {
        term = this->GetMatrix()[i][j] - 1.0;
        }
      else
        {
        term = this->GetMatrix()[i][j];
        }
      result += term * term;
      }
    term = this->GetOffset()[i];
    result += term * term;
    }

  return sqrt(result);
}

/** Back transform a point */
template<class TScalarType, unsigned int NDimensions>
typename AffineTransform<TScalarType, NDimensions>::InputPointType
AffineTransform<TScalarType, NDimensions>::
BackTransform(const OutputPointType &point) const 
{
  itkWarningMacro(<<"BackTransform(): This method is slated to be removed\
   from ITK.  Instead, please use GetInverse() to generate an inverse\
   transform and then perform the transform using that inverted transform.");
  InputPointType result;       // Converted point
  ScalarType temp[NDimensions];
  unsigned int i, j;

  for (j = 0; j < NDimensions; j++) 
    {
    temp[j] = point[j] - this->GetOffset()[j];
    }

  for (i = 0; i < NDimensions; i++) 
    {
    result[i] = 0.0;
    for (j = 0; j < NDimensions; j++) 
      {
      result[i] += this->GetInverseMatrix()[i][j]*temp[j];
      }
    }
  return result;
}


/** Back transform a vector */
template<class TScalarType, unsigned int NDimensions>
typename AffineTransform<TScalarType, NDimensions>::InputVectorType
AffineTransform<TScalarType, NDimensions>::
BackTransform(const OutputVectorType &vect ) const 
{
  itkWarningMacro(<<"BackTransform(): This method is slated to be removed\
   from ITK. Instead, please use GetInverse() to generate an inverse\
   transform and then perform the transform using that inverted transform.");
  return this->GetInverseMatrix() * vect;
}


/** Back transform a vnl_vector */
template<class TScalarType, unsigned int NDimensions>
typename AffineTransform<TScalarType, NDimensions>::InputVnlVectorType
AffineTransform<TScalarType, NDimensions>::
BackTransform(const OutputVnlVectorType &vect ) const 
{
  itkWarningMacro(<<"BackTransform(): This method is slated to be removed\
   from ITK. Instead, please use GetInverse() to generate an inverse\
    transform and then perform the transform using that inverted transform.");
  return this->GetInverseMatrix() * vect;
}


/** Back Transform a CovariantVector */
template<class TScalarType, unsigned int NDimensions>
typename AffineTransform<TScalarType, NDimensions>::InputCovariantVectorType
AffineTransform<TScalarType, NDimensions>::
BackTransform(const OutputCovariantVectorType &vec) const 
{
  itkWarningMacro(<<"BackTransform(): This method is slated to be removed\
   from ITK. Instead, please use GetInverse() to generate an inverse\
   transform and then perform the transform using that inverted transform.");

  InputCovariantVectorType result;    // Converted vector

  for (unsigned int i = 0; i < NDimensions; i++) 
    {
    result[i] = NumericTraits<ScalarType>::Zero;
    for (unsigned int j = 0; j < NDimensions; j++) 
      {
      result[i] += this->GetMatrix()[j][i]*vec[j]; // Direct matrix transposed
      }
    }
  return result;
}


/** Back transform a given point which is represented as type PointType */
template<class TScalarType, unsigned int NDimensions>
typename AffineTransform<TScalarType, NDimensions>::InputPointType
AffineTransform<TScalarType, NDimensions>::
BackTransformPoint(const OutputPointType &point) const
{
  return this->BackTransform(point);
}

} // namespace

#endif
