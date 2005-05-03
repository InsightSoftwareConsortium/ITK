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
#ifndef _itkScalableAffineTransform_txx
#define _itkScalableAffineTransform_txx

#include "itkNumericTraits.h"
#include "itkScalableAffineTransform.h"
#include "itkAffineTransform.h"
#include "vnl/algo/vnl_matrix_inverse.h"


namespace itk
{

/** Constructor with default arguments */
template<class TScalarType, unsigned int NDimensions>
ScalableAffineTransform<TScalarType, NDimensions>
::ScalableAffineTransform()
  : Superclass(Self::OutputSpaceDimension, Self::ParametersDimension)
{
  for (unsigned int i=0; i<NDimensions; i++)
    {
    m_Scale[i] = 1;
    m_MatrixScale[i] = 1;
    }
}

/** Constructor with default arguments */
template<class TScalarType, unsigned int NDimensions>
ScalableAffineTransform<TScalarType, NDimensions>
::ScalableAffineTransform( unsigned int outputSpaceDimension,
                           unsigned int parametersDimension )
  : Superclass(outputSpaceDimension, parametersDimension)
{
  for (unsigned int i=0; i<NDimensions; i++)
    {
    m_Scale[i] = 1;
    m_MatrixScale[i] = 1;
    }
}

/** Constructor with default arguments */
template<class TScalarType, unsigned int NDimensions>
ScalableAffineTransform<TScalarType, NDimensions>
::ScalableAffineTransform( const MatrixType & matrix,
                           const OutputVectorType & offset )
  : Superclass(matrix, offset)
{
  for (unsigned int i=0; i<NDimensions; i++)
    {
    m_Scale[i] = 1;
    m_MatrixScale[i] = 1;
    }
}

/** Destructor */
template<class TScalarType, unsigned int NDimensions>
ScalableAffineTransform<TScalarType, NDimensions>
::~ScalableAffineTransform()
{
  return;
}

/** Print self */
template<class TScalarType, unsigned int NDimensions>
void
ScalableAffineTransform<TScalarType, NDimensions>
::PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  unsigned int i;

  os << indent << "Scale : ";
  for (i = 0; i < NDimensions; i++) 
    {
    os << m_Scale[i] << " ";
    }
  os << std::endl;
  os << indent << "MatrixScale : ";
  for (i = 0; i < NDimensions; i++) 
    {
    os << m_MatrixScale[i] << " ";
    }
  os << std::endl;
}


// Set the parameters in order to fit an Identity transform
template<class TScalarType, unsigned int NDimensions>
void
ScalableAffineTransform<TScalarType, NDimensions>
::SetIdentity( void ) 
{ 
  for (unsigned int i=0; i<NDimensions; i++)
    {
    m_Scale[i] = 1;
    m_MatrixScale[i] = 1;
    }
  this->Superclass::SetIdentity();
}


/** Set the scale of the transformation */
template<class TScalarType, unsigned int NDimensions>
void
ScalableAffineTransform<TScalarType, NDimensions>
::SetScale(const InputVectorType & scale )
{
  unsigned int i; 
  for (i=0; i<NDimensions; i++)
    {
    m_Scale[i] = scale[i];
    }
  this->ComputeMatrix();
  this->Modified();
}

template<class TScalarType, unsigned int NDimensions>
void
ScalableAffineTransform<TScalarType, NDimensions>
::SetScale(const double scale[NDimensions] )
{
  unsigned int i; 
  for (i=0; i<NDimensions; i++)
    {
    m_Scale[i] = scale[i];
    }
  this->ComputeMatrix();
  this->Modified();
}


/** Set the scale of the transformation */
template<class TScalarType, unsigned int NDimensions>
void
ScalableAffineTransform<TScalarType, NDimensions>
::ComputeMatrix()
{
  unsigned int i; 
  for (i=0; i<NDimensions; i++)
    {
    if(m_Scale[i] != m_MatrixScale[i])
      {
      break;
      }
    }
  if ( i < NDimensions ) 
    { 
    MatrixType mat;
    for (i=0; i<NDimensions; i++)
      {
      if(m_MatrixScale[i] != 0 && m_Scale[i] != 0)
        {
        mat.GetVnlMatrix().put(i, i, m_Scale[i]/m_MatrixScale[i]
                                     * this->GetMatrix()[i][i]);
        m_MatrixScale[i] = m_Scale[i];
        }
      else
        {
        m_Scale[i] = 1;
        m_MatrixScale[i] = 1;
        mat.GetVnlMatrix().put(i, i, this->GetMatrix()[i][i]);
        }
      }
    Superclass::SetVarMatrix(mat);
    }
}


} // namespace

#endif
