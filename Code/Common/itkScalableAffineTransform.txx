/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkScalableAffineTransform.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkScalableAffineTransform_txx
#define __itkScalableAffineTransform_txx

#include "itkNumericTraits.h"
#include "itkScalableAffineTransform.h"
#include "itkAffineTransform.h"
#include "vnl/algo/vnl_matrix_inverse.h"

namespace itk
{
/** Constructor with default arguments */
template< class TScalarType, unsigned int NDimensions >
ScalableAffineTransform< TScalarType, NDimensions >
::ScalableAffineTransform():
  Superclass(Self::OutputSpaceDimension, Self::ParametersDimension)
{
  for ( unsigned int i = 0; i < NDimensions; i++ )
    {
    m_Scale[i] = 1;
    m_MatrixScale[i] = 1;
    }
}

/** Constructor with default arguments */
template< class TScalarType, unsigned int NDimensions >
ScalableAffineTransform< TScalarType, NDimensions >
::ScalableAffineTransform(unsigned int outputSpaceDimension,
                          unsigned int parametersDimension):
  Superclass(outputSpaceDimension, parametersDimension)
{
  for ( unsigned int i = 0; i < NDimensions; i++ )
    {
    m_Scale[i] = 1;
    m_MatrixScale[i] = 1;
    }
}

/** Constructor with default arguments */
template< class TScalarType, unsigned int NDimensions >
ScalableAffineTransform< TScalarType, NDimensions >
::ScalableAffineTransform(const MatrixType & matrix,
                          const OutputVectorType & offset):
  Superclass(matrix, offset)
{
  for ( unsigned int i = 0; i < NDimensions; i++ )
    {
    m_Scale[i] = 1;
    m_MatrixScale[i] = 1;
    }
}

/** Destructor */
template< class TScalarType, unsigned int NDimensions >
ScalableAffineTransform< TScalarType, NDimensions >
::~ScalableAffineTransform()
{
  return;
}

/** Print self */
template< class TScalarType, unsigned int NDimensions >
void
ScalableAffineTransform< TScalarType, NDimensions >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  unsigned int i;

  os << indent << "Scale : ";
  for ( i = 0; i < NDimensions; i++ )
    {
    os << m_Scale[i] << " ";
    }
  os << std::endl;
  os << indent << "MatrixScale : ";
  for ( i = 0; i < NDimensions; i++ )
    {
    os << m_MatrixScale[i] << " ";
    }
  os << std::endl;
}

// Set the parameters in order to fit an Identity transform
template< class TScalarType, unsigned int NDimensions >
void
ScalableAffineTransform< TScalarType, NDimensions >
::SetIdentity(void)
{
  for ( unsigned int i = 0; i < NDimensions; i++ )
    {
    m_Scale[i] = 1;
    m_MatrixScale[i] = 1;
    }
  this->Superclass::SetIdentity();
}

/** Set the scale of the transformation */
template< class TScalarType, unsigned int NDimensions >
void
ScalableAffineTransform< TScalarType, NDimensions >
::SetScale(const InputVectorType & scale)
{
  unsigned int i;

  for ( i = 0; i < NDimensions; i++ )
    {
    m_Scale[i] = scale[i];
    }
  this->ComputeMatrix();
  this->Modified();
}

template< class TScalarType, unsigned int NDimensions >
void
ScalableAffineTransform< TScalarType, NDimensions >
::SetScale(const double scale[NDimensions])
{
  unsigned int i;

  for ( i = 0; i < NDimensions; i++ )
    {
    m_Scale[i] = scale[i];
    }
  this->ComputeMatrix();
  this->Modified();
}

// Get an inverse of this transform
template< class TScalarType, unsigned int NDimensions >
bool
ScalableAffineTransform< TScalarType, NDimensions >
::GetInverse(Self *inverse) const
{
  return this->Superclass::GetInverse(inverse);
}

// Return an inverse of this transform
template< class TScalarType, unsigned int NDimensions >
typename ScalableAffineTransform< TScalarType, NDimensions >
::InverseTransformBasePointer
ScalableAffineTransform< TScalarType, NDimensions >
::GetInverseTransform() const
{
  Pointer inv = New();

  return this->GetInverse(inv) ? inv.GetPointer() : NULL;
}

/** Set the scale of the transformation */
template< class TScalarType, unsigned int NDimensions >
void
ScalableAffineTransform< TScalarType, NDimensions >
::ComputeMatrix()
{
  unsigned int i;

  for ( i = 0; i < NDimensions; i++ )
    {
    if ( m_Scale[i] != m_MatrixScale[i] )
      {
      break;
      }
    }
  if ( i < NDimensions )
    {
    MatrixType mat;
    typename MatrixType::InternalMatrixType & imat = mat.GetVnlMatrix();
    for ( i = 0; i < NDimensions; i++ )
      {
      if ( m_MatrixScale[i] != 0 && m_Scale[i] != 0 )
        {
        imat.put(i, i, m_Scale[i] / m_MatrixScale[i] * this->GetMatrix()[i][i]);
        m_MatrixScale[i] = m_Scale[i];
        }
      else
        {
        m_Scale[i] = 1;
        m_MatrixScale[i] = 1;
        imat.put(i, i, this->GetMatrix()[i][i]);
        }
      }
    Superclass::SetVarMatrix(mat);
    }
}
} // namespace

#endif
