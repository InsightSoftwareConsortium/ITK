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
  this->SetCenter( center );


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

  const double cc = cos( angle );
  const double ss = sin( angle );

  const double ca = cc * m_Scale;
  const double sa = ss * m_Scale;

  const InputPointType center = this->GetCenter();  
  const double cx = center[0];
  const double cy = center[1];

  const OutputVectorType translation = this->GetTranslation();
  const double tx = translation[0];
  const double ty = translation[1];

  m_RotationMatrix[0][0]= ca; m_RotationMatrix[0][1]=-sa;
  m_RotationMatrix[1][0]= sa; m_RotationMatrix[1][1]= ca;

  OffsetType offset;

  offset[0] = tx + sa * cy + ( 1.0 - ca ) * cx;
  offset[1] = ty - sa * cx + ( 1.0 - ca ) * cy;

  this->SetOffset( offset );

  const double ci = cc / m_Scale;
  const double si = ss / m_Scale;

  m_InverseMatrix[0][0]= ci; m_InverseMatrix[0][1]= si;
  m_InverseMatrix[1][0]=-si; m_InverseMatrix[1][1]= ci;

  this->Modified();


}


// Set parameters
template<class TScalarType>
const typename Similarity2DTransform<TScalarType>::JacobianType &
Similarity2DTransform<TScalarType>::
GetJacobian( const InputPointType & p ) const
{

  // need to check if angles are in the right order
  const double angle = this->GetAngle();
  const double ca = cos( angle );
  const double sa = sin( angle );

  m_Jacobian.Fill(0.0);

  const InputPointType center = this->GetCenter();  
  const double cx = center[0];
  const double cy = center[1];

  const OutputVectorType translation = this->GetTranslation();

  // derivatives with respect to the scale
  m_Jacobian[0][0] =    ca * ( p[0] - cx ) - sa * ( p[1] - cy );
  m_Jacobian[1][0] =    sa * ( p[0] - cx ) + ca * ( p[1] - cy ); 

  // derivatives with respect to the angle
  m_Jacobian[0][1] = ( -sa * ( p[0] - cx ) - ca * ( p[1] - cy ) ) * m_Scale;
  m_Jacobian[1][1] = (  ca * ( p[0] - cx ) - sa * ( p[1] - cy ) ) * m_Scale; 

  // compute derivatives with respect to the center part
  // first with respect to cx
  m_Jacobian[0][2] = 1.0 - ca * m_Scale;
  m_Jacobian[1][2] =     - sa * m_Scale;  
  // then with respect to cy
  m_Jacobian[0][3] =       sa * m_Scale;
  m_Jacobian[1][3] = 1.0 - ca * m_Scale;


  // compute derivatives with respect to the translation part
  // first with respect to tx
  m_Jacobian[0][4] = 1.0;
  m_Jacobian[1][4] = 0.0;
  // first with respect to ty
  m_Jacobian[0][5] = 0.0;
  m_Jacobian[1][5] = 1.0;

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
