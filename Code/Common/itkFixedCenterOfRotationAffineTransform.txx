/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFixedCenterOfRotationAffineTransform.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkFixedCenterOfRotationAffineTransform_txx
#define _itkFixedCenterOfRotationAffineTransform_txx

#include "itkNumericTraits.h"
#include "itkFixedCenterOfRotationAffineTransform.h"
#include "itkAffineTransform.h"
#include "vnl/algo/vnl_matrix_inverse.h"


namespace itk
{

/** Constructor with default arguments */
template<class TScalarType, unsigned int NDimensions>
FixedCenterOfRotationAffineTransform<TScalarType, NDimensions>::
FixedCenterOfRotationAffineTransform()
  :Superclass(Self::SpaceDimension,Self::ParametersDimension)
{
  m_MatrixComponent.SetIdentity();
  
  for (unsigned int i=0; i<NDimensions; i++)
    {
    m_ScaleComponent[i] = 1;
    }
  m_ScaleMatrixComponent.SetIdentity();
}




/** Destructor */
template<class TScalarType, unsigned int NDimensions>
FixedCenterOfRotationAffineTransform<TScalarType, NDimensions>::
~FixedCenterOfRotationAffineTransform()
{
  return;
}



/** Print self */
template<class TScalarType, unsigned int NDimensions>
void
FixedCenterOfRotationAffineTransform<TScalarType, NDimensions>::
PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  unsigned int i;

  os << indent << "Matrix Component:";
  os << m_MatrixComponent << std::endl;

  os << indent << "ScaleMatrix Component:";
  os << m_ScaleMatrixComponent << std::endl;
  
  os << indent << "Scale Component : ";
  for (i = 0; i < NDimensions; i++) 
    {
    os << m_ScaleComponent[i] << " ";
    }
  os << std::endl;
}


/** Set the matrix without scale the scale is added using SetScale */
template<class TScalarType, unsigned int NDimensions>
void
FixedCenterOfRotationAffineTransform<TScalarType, NDimensions>       
::SetMatrixComponent(const MatrixType &matrix)
{ 
  m_MatrixComponent = matrix;
  this->RecomputeMatrix();
}

/** Set the scale of the transformation */
template<class TScalarType, unsigned int NDimensions>
void
FixedCenterOfRotationAffineTransform<TScalarType, NDimensions>
::SetScaleComponent(const double scale[NDimensions] )
{
  unsigned int i; 
  for (i=0; i<NDimensions; i++)
    {
    if ( scale[i] != m_ScaleComponent[i] )
      {
      break;
      }
    } 
  if ( i < NDimensions ) 
    { 
    for (i=0; i<NDimensions; i++)
      {
      m_ScaleComponent[i] = scale[i];
      m_ScaleMatrixComponent.GetVnlMatrix().put(i,i,m_ScaleComponent[i]);  
      }
    }
  this->RecomputeMatrix();
}


/** Set the center of rotation */
template<class TScalarType, unsigned int NDimensions>
void
FixedCenterOfRotationAffineTransform<TScalarType, NDimensions>
::SetCenterOfRotationComponent(const InputPointType &cor)
{
  this->SetCenter(cor);
}

   

/** Set the offset */
template<class TScalarType, unsigned int NDimensions>
void
FixedCenterOfRotationAffineTransform<TScalarType, NDimensions>
::SetOffsetComponent(const OffsetType &offset)
{
  this->SetTranslation(offset);
}

/** Recompute the affine matrix 
 *  R(Sx+CoR)+O */
template<class TScalarType, unsigned int NDimensions>
void
FixedCenterOfRotationAffineTransform<TScalarType, NDimensions>::
RecomputeMatrix(void)
{
  Superclass::SetMatrix(m_MatrixComponent*m_ScaleMatrixComponent);
  this->Modified();
}
  
// Set the parameters in order to fit an Identity transform
template<class TScalarType, unsigned int NDimensions>
void
FixedCenterOfRotationAffineTransform<TScalarType, NDimensions>::
SetIdentity( void ) 
{ 
  this->Superclass::SetIdentity();
  m_MatrixComponent.SetIdentity();
  
  for (unsigned int i=0; i<NDimensions; i++)
    {
    m_ScaleComponent[i] = 1;
    }
  m_ScaleMatrixComponent.SetIdentity();
  this->RecomputeMatrix();
  this->Modified();  
}


/** Set matrix of the AffineTransform
 *  This reinitializes the different components, i.e ScaleComponent,
 *  OffsetComponent, etc ...*/
template<class TScalarType, unsigned int NDimensions>
void
FixedCenterOfRotationAffineTransform<TScalarType, NDimensions>::
SetMatrix(const MatrixType &matrix)
{
  Superclass::SetMatrix(matrix); 
  m_MatrixComponent = matrix;
  for (unsigned int i=0; i<NDimensions; i++)
    {
    m_ScaleComponent[i] = 1;
    }
  m_ScaleMatrixComponent.SetIdentity();
  this->m_MatrixMTime.Modified();
  this->Modified();
}


} // namespace

#endif
