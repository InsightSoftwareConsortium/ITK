/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSimilarity2DTransform.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkSimilarity2DTransform_txx
#define _itkSimilarity2DTransform_txx

#include "itkSimilarity2DTransform.h"


namespace itk
{

// Constructor with default arguments
template <class TScalarType>
Similarity2DTransform<TScalarType>
::Similarity2DTransform()
{
 
}

// Copy Constructor
template <class TScalarType>
Similarity2DTransform<TScalarType>
::Similarity2DTransform( const Self & other )
{
  // call the superclass copy constructor
}

// Set Parameters
template <class TScalarType>
void
Similarity2DTransform<TScalarType>
::SetParameters( const ParametersType & parameters )
{
  itkDebugMacro( << "Setting paramaters " << parameters );

  // Set angles with parameters
  m_Angle = parameters[0];
 
  // Set angles with parameters
  m_Scale = parameters[1];
 
  // Transfer the translation part
  OffsetType offset;
  for(unsigned int i=0; i < SpaceDimension; i++) 
  {
    offset[i] = parameters[i+2];
  }

  this->SetOffset( offset );

  ComputeMatrix();

  itkDebugMacro(<<"After setting paramaters ");
}


// Get Parameters
template <class TScalarType>
const typename Similarity2DTransform<TScalarType>::ParametersType &
Similarity2DTransform<TScalarType>
::GetParameters( void ) const
{
  itkDebugMacro( << "Getting parameters ");

  m_Parameters[0] = m_Angle;
  m_Parameters[1] = m_Scale;
 
  // Transfer the translation part
  OffsetType offset = this->GetOffset();
  for(unsigned int i=0; i < SpaceDimension; i++) 
  {
    m_Parameters[i+2] = offset[i];
  }

  itkDebugMacro(<<"After getting parameters " << m_Parameters );

  return m_Parameters;
}


// Set Rotational Part
template <class TScalarType>
void
Similarity2DTransform<TScalarType>
::SetRotation(TScalarType angle)
{
  m_Angle = angle;
  this->ComputeMatrix();
}


// Set Scale Part
template <class TScalarType>
void
Similarity2DTransform<TScalarType>
::SetScale(TScalarType scale)
{
  m_Scale = scale;
  this->ComputeMatrix();
}


// Compute the matrix
template <class TScalarType>
void
Similarity2DTransform<TScalarType>
::ComputeMatrix( void )
{
  const double cx = cos(m_Angle);
  const double sx = sin(m_Angle);

  const double cxz = cx * m_Scale;
  const double sxz = sx * m_Scale;

  m_RotationMatrix[0][0] =  cxz;  m_RotationMatrix[0][1] = sxz;
  m_RotationMatrix[1][0] = -sxz;  m_RotationMatrix[1][1] = cxz;

  m_InverseMatrix = m_RotationMatrix.GetTranspose();

}


// Set parameters
template<class TScalarType>
const typename Similarity2DTransform<TScalarType>::JacobianType &
Similarity2DTransform<TScalarType>::
GetJacobian( const InputPointType & p ) const
{
  // need to check if angles are in the right order
  const double cx = cos(m_Angle);
  const double sx = sin(m_Angle);

  const double cxz = cx * m_Scale;
  const double sxz = sx * m_Scale;

  m_Jacobian.Fill(0.0);

  // derivatives with respect to the angle
  m_Jacobian[0][0] = -sxz * p[0] + cxz * p[1]; 
  m_Jacobian[1][0] = -cxz * p[0] - sxz * p[1];

  // derivatives with respect to the scale
  m_Jacobian[0][1] =  cx  * p[0] + sx  * p[1]; 
  m_Jacobian[1][1] = -sx  * p[0] + cx  * p[1];

  // compute derivatives for the translation part
  unsigned int blockOffset = 2;  
  for(unsigned int dim=0; dim < SpaceDimension; dim++ ) 
  {
    m_Jacobian[ dim ][ blockOffset + dim ] = 1.0;
  }

  return m_Jacobian;

}

 
// Set Identity
template <class TScalarType>
void
Similarity2DTransform<TScalarType>
::SetIdentity(void)
{
  m_Angle = NumericTraits< TScalarType >::Zero;
  m_Scale = static_cast< TScalarType >( 1.0f );
  ComputeMatrix();
  OffsetType offset;
  offset.Fill( NumericTraits<TScalarType>::Zero );
  this->SetOffset( offset );
}


 
// Print self
template<class TScalarType>
void
Similarity2DTransform<TScalarType>::
PrintSelf(std::ostream &os, Indent indent) const
{

  Superclass::PrintSelf(os,indent);
  
  os << indent << "Euler's angle: Angle=" << m_Angle  
     << std::endl;
}

} // namespace

#endif
