/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkEuler2DTransform.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkEuler2DTransform_txx
#define _itkEuler2DTransform_txx

#include "itkEuler2DTransform.h"


namespace itk
{

// Constructor with default arguments
template <class TScalarType>
Euler2DTransform<TScalarType>
::Euler2DTransform()
{
  this->m_Parameters.set_size(ParametersDimension);
  m_Angle = 0.0;
}

// Set Parameters
template <class TScalarType>
void
Euler2DTransform<TScalarType>
::SetParameters( const ParametersType & parameters )
{
  itkDebugMacro( << "Setting paramaters " << parameters );

  this->m_Parameters = parameters;

  // Set angles with parameters
  m_Angle = parameters[0];
 
  // Transfer the translation part
  OffsetType offset;
  for(unsigned int i=0; i < SpaceDimension; i++) 
    {
    offset[i] = parameters[i+1];
    }

  this->SetOffset( offset );

  this->ComputeMatrix();

  itkDebugMacro(<<"After setting parameters ");
}

// Get Parameters
template <class TScalarType>
const typename Euler2DTransform<TScalarType>::ParametersType &
Euler2DTransform<TScalarType>
::GetParameters( void ) const
{
  itkDebugMacro( << "Getting parameters ");

  // Set angles with parameters
  this->m_Parameters[0] = m_Angle;
 
  // Transfer the translation part
  OffsetType offset = this->GetOffset();
  for(unsigned int i=0; i < SpaceDimension; i++) 
    {
    this->m_Parameters[i+1] = offset[i];
    }

  itkDebugMacro(<<"After getting parameters " << this->m_Parameters );

  return this->m_Parameters;
}



// Set Identity
template <class TScalarType>
void
Euler2DTransform<TScalarType>
::SetIdentity(void)
{
  m_Angle = NumericTraits< TScalarType >::Zero;
  ComputeMatrix();
  OffsetType offset;
  offset.Fill( NumericTraits<TScalarType>::Zero );
  this->SetOffset( offset );
}

  

/** */
template <class TScalarType>
void
Euler2DTransform<TScalarType>
::SetRotationMatrix(const MatrixType &matrix)
{
  Superclass::SetRotationMatrix(matrix);
  this->ComputeAngleFromMatrix();
}
 

/** Compose with another Euler transform */
template <class TScalarType>
void
Euler2DTransform<TScalarType>
::Compose(const Superclass *other, bool pre)
{
  Superclass::Compose(other,pre);
  this->ComputeAngleFromMatrix();
}

// Set Rotational Part
template <class TScalarType>
void
Euler2DTransform<TScalarType>
::SetRotation(TScalarType angle)
{
  m_Angle = angle;
  this->ComputeMatrix();
}


// Compute the matrix
template <class TScalarType>
void
Euler2DTransform<TScalarType>
::ComputeMatrix( void )
{
  const double cx = cos(m_Angle);
  const double sx = sin(m_Angle);

  this->m_RotationMatrix[0][0] =  cx; this->m_RotationMatrix[0][1] = -sx;
  this->m_RotationMatrix[1][0] =  sx; this->m_RotationMatrix[1][1] =  cx;

  this->m_RotationMatrixMTime.Modified();
}

/** Compute the Angle from the Rotation Matrix */
template <class TScalarType>
void
Euler2DTransform<TScalarType>
::ComputeAngleFromMatrix( void )
{
  m_Angle = acos(this->m_RotationMatrix[0][0]); 

  if(this->m_RotationMatrix[1][0]<0.0)
    {
    m_Angle = -m_Angle;
    }

  if(this->m_RotationMatrix[1][0]-sin(m_Angle) > 0.000001)
    {
    std::cout << "Bad Rotation Matrix" << std::endl;
    }
}

// Set parameters
template<class TScalarType>
const typename Euler2DTransform<TScalarType>::JacobianType &
Euler2DTransform<TScalarType>::
GetJacobian( const InputPointType & p ) const
{
  // need to check if angles are in the right order
  const double cx = cos(m_Angle);
  const double sx = sin(m_Angle);

  this->m_Jacobian.Fill(0.0);

  // derivatives with respect to the angle
  this->m_Jacobian[0][0] = -sx * p[0] - cx * p[1]; 
  this->m_Jacobian[1][0] =  cx * p[0] - sx * p[1];

  // compute derivatives for the translation part
  unsigned int blockOffset = 1;  
  for(unsigned int dim=0; dim < SpaceDimension; dim++ ) 
    {
    this->m_Jacobian[ dim ][ blockOffset + dim ] = 1.0;
    }

  return this->m_Jacobian;

}
  
// Print self
template<class TScalarType>
void
Euler2DTransform<TScalarType>::
PrintSelf(std::ostream &os, Indent indent) const
{

  Superclass::PrintSelf(os,indent);
  
  os << indent << "Euler's angle: Angle=" << m_Angle  
     << std::endl;
}

} // namespace

#endif
