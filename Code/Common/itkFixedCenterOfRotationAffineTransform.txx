/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFixedCenterOfRotationAffineTransform.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
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
  :Superclass(SpaceDimension,ParametersDimension)
{
  m_CenterOfRotationComponent.Fill( 0 );
  m_MatrixComponent.SetIdentity();
  m_OffsetComponent.Fill( 0 );
  
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

  os << indent << "CenterOfRotationComponent : ";
  for (i = 0; i < NDimensions; i++) 
    {
    os << m_CenterOfRotationComponent[i] << " ";
    }
  os << std::endl;

  os << indent << "Matrix Component:";
  os << m_MatrixComponent << std::endl;

  os << indent << "Offset Component:";
  os << m_OffsetComponent << std::endl;
  
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
  this->RecomputeOffset();
  this->Superclass::RecomputeInverse(); 
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
  this->Superclass::RecomputeInverse(); 
}


/** Set the center of rotation */
template<class TScalarType, unsigned int NDimensions>
void
FixedCenterOfRotationAffineTransform<TScalarType, NDimensions>
::SetCenterOfRotationComponent(const InputPointType &cor)
{
  
  unsigned int i; 
  for (i=0; i<NDimensions; i++)
    {
    if ( cor[i] != m_CenterOfRotationComponent[i] )
      {
      break;
      }
    } 
  if ( i < NDimensions ) 
    { 
    for (i=0; i<NDimensions; i++)
      {
      m_CenterOfRotationComponent[i] = cor[i];
      }
    }
 
  this->RecomputeOffset();
  this->Superclass::RecomputeInverse(); 
}

   

/** Set the offset */
template<class TScalarType, unsigned int NDimensions>
void
FixedCenterOfRotationAffineTransform<TScalarType, NDimensions>
::SetOffsetComponent(const OffsetType &offset)
{
  m_OffsetComponent = offset;
  this->RecomputeOffset();
  this->Superclass::RecomputeInverse(); 
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
  
/** Recompute the offset
 *  R(Sx+CoR)+O */
template<class TScalarType, unsigned int NDimensions>
void
FixedCenterOfRotationAffineTransform<TScalarType, NDimensions>::
RecomputeOffset(void)
{
  InputPointType pt = m_MatrixComponent*m_CenterOfRotationComponent;
  OffsetType offset;
  for (unsigned int i=0; i<NDimensions; i++)
    {
    offset[i] = -pt[i] + m_CenterOfRotationComponent[i] + m_OffsetComponent[i];
    } 
  Superclass::SetOffset(offset);
  this->Modified();
}

// Set the parameters in order to fit an Identity transform
template<class TScalarType, unsigned int NDimensions>
void
FixedCenterOfRotationAffineTransform<TScalarType, NDimensions>::
SetIdentity( void ) 
{ 
  this->Superclass::SetIdentity();
  m_CenterOfRotationComponent.Fill( 0 );
  m_MatrixComponent.SetIdentity();
  m_OffsetComponent.Fill( 0 );
  
  for (unsigned int i=0; i<NDimensions; i++)
    {
    m_ScaleComponent[i] = 1;
    }
  m_ScaleMatrixComponent.SetIdentity();
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
  Superclass::RecomputeInverse(); 
  this->Modified();
}


/** Set offset (origin) of the Affine Transform.
 *  This reinitializes the different components, i.e ScaleComponent,
 *  OffsetComponent, etc ...*/
template<class TScalarType, unsigned int NDimensions>
void
FixedCenterOfRotationAffineTransform<TScalarType, NDimensions>::
SetOffset(const OffsetType &offset)
{ 
  m_CenterOfRotationComponent.Fill(0);
  m_OffsetComponent = offset;
  Superclass::SetOffset(offset); 
  this->Modified(); 
}

// Get parameters
template<class TScalarType, unsigned int NDimensions>
const typename FixedCenterOfRotationAffineTransform<TScalarType, NDimensions>::ParametersType &
FixedCenterOfRotationAffineTransform<TScalarType, NDimensions>::
GetParameters( void ) const
{
  // Transfer the linear part
  unsigned int par = 0;

  MatrixType matrix = this->GetMatrix();
  OffsetType offset = this->GetOffset();

  for(unsigned int row=0; row<NDimensions; row++) 
    {
    for(unsigned int col=0; col<NDimensions; col++) 
      {
      m_Parameters[par] = matrix[row][col];
      ++par;
      }
    }

  // Transfer the constant part without the center of rotation
  InputPointType pt = matrix*m_CenterOfRotationComponent;
  for(unsigned int i=0; i<NDimensions; i++) 
    {
    m_Parameters[par] = offset[i]+pt[i]-m_CenterOfRotationComponent[i];
    ++par;
    }

  return m_Parameters;
}

/** Set parameters of the transformation 
 *  The center of rotation should be set before calling
 *  SetParameters() */
template<class TScalarType, unsigned int NDimensions>
void
FixedCenterOfRotationAffineTransform<TScalarType, NDimensions>::
SetParameters( const ParametersType & parameters )
{
  // Transfer the linear part
  unsigned int par = 0;

  m_Parameters = parameters;
  MatrixType matrix;
  OffsetType offset;

  for(unsigned int row=0; row<NDimensions; row++) 
    {
    for(unsigned int col=0; col<NDimensions; col++) 
      {
      matrix[row][col] = m_Parameters[par];
      ++par;
      }
    }

  Superclass::SetMatrix(matrix);

  // Transfer the constant part
  InputPointType pt = matrix*m_CenterOfRotationComponent;
  for (unsigned int i=0; i<NDimensions; i++)
    {
    offset[i] = -pt[i] + m_CenterOfRotationComponent[i] + m_Parameters[par];
    par++;
    } 

  Superclass::SetOffset(offset);
 
  // Recompute the inverse
  Superclass::RecomputeInverse();
  this->Modified();
}


} // namespace

#endif
