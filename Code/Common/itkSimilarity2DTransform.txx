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
::Similarity2DTransform():Superclass(OutputSpaceDimension, ParametersDimension)
{
  m_Scale = 1.0; 

  // note: this virtual function will only
  // call the one defined in this class because 
  // we are in a constructor
  this->ComputeMatrixAndOffset();
}



// Copy Constructor
template <class TScalarType>
Similarity2DTransform<TScalarType>
::Similarity2DTransform( const Self & other ):Superclass( other )
{
  m_Scale = other.m_Scale;

  // note: this virtual function will only
  // call the one defined in this class because 
  // we are in a constructor
  this->ComputeMatrixAndOffset();
}


// Set Parameters
template <class TScalarType>
void
Similarity2DTransform<TScalarType>
::SetParameters( const ParametersType & parameters )
{
  itkDebugMacro( << "Setting paramaters " << parameters );

  // Set angles with parameters
  m_Scale = parameters[0];
 
  // Set angles with parameters
  this->SetAngle( parameters[1] );

  InputPointType center;
  for(unsigned int j=0; j < SpaceDimension; j++) 
  {
    center[j] = parameters[j+2];
  }
  this->SetCenterOfRotation( center );


  // Transfer the translation part
  OffsetType offset;
  for(unsigned int i=0; i < SpaceDimension; i++) 
  {
    offset[i] = parameters[i+4];
  }

  this->SetOffset( offset );

  this->ComputeMatrixAndOffset();

  itkDebugMacro(<<"After setting paramaters ");
}


// Get Parameters
template <class TScalarType>
const typename Similarity2DTransform<TScalarType>::ParametersType &
Similarity2DTransform<TScalarType>
::GetParameters( void ) const
{
  itkDebugMacro( << "Getting parameters ");

  m_Parameters[0] = m_Scale;
  m_Parameters[1] = this->GetAngle();
 
  InputPointType center = this->GetCenter();
  for(unsigned int j=0; j < SpaceDimension; j++) 
  {
    m_Parameters[j+2] = center[j];
  }

  // Transfer the translation part
  OffsetType offset = this->GetOffset();
  for(unsigned int i=0; i < SpaceDimension; i++) 
  {
    m_Parameters[i+4] = offset[i];
  }

  itkDebugMacro(<<"After getting parameters " << m_Parameters );

  return m_Parameters;
}



// Set Scale Part
template <class TScalarType>
void
Similarity2DTransform<TScalarType>
::SetScale(TScalarType scale)
{
  m_Scale = scale;
  this->ComputeMatrixAndOffset();
}




// Compute the matrix
template <class TScalarType>
void
Similarity2DTransform<TScalarType>
::ComputeMatrixAndOffset( void )
{
  const double angle = this->GetAngle();
  const double cx = cos(angle);
  const double sx = sin(angle);

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
  const double angle = this->GetAngle();
  const double cx = cos( angle );
  const double sx = sin( angle );

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
  this->Superclass::SetIdentity();
  m_Scale = static_cast< TScalarType >( 1.0f );
  this->ComputeMatrixAndOffset();
}


 
// Print self
template<class TScalarType>
void
Similarity2DTransform<TScalarType>::
PrintSelf(std::ostream &os, Indent indent) const
{

  Superclass::PrintSelf(os,indent);
  
  os << indent << "Scale =" << m_Scale  << std::endl;
}

} // namespace

#endif
