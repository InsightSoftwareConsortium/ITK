/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCenteredRigid2DTransform.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkCenteredRigid2DTransform_txx
#define _itkCenteredRigid2DTransform_txx

#include "itkCenteredRigid2DTransform.h"


namespace itk
{

// Constructor with default arguments
template <class TScalarType>
CenteredRigid2DTransform<TScalarType>
::CenteredRigid2DTransform():Superclass(OutputSpaceDimension, ParametersDimension)
{
  // note: this virtual function will only
  // call the one defined in this class because 
  // we are in a constructor
  this->ComputeMatrixAndOffset();
}




// Copy Constructor
template <class TScalarType>
CenteredRigid2DTransform<TScalarType>
::CenteredRigid2DTransform( const Self & other )
{
  this->m_Parameters    = other.m_Parameter;

  // note: this virtual function will only
  // call the one defined in this class because 
  // we are in a constructor
  this->ComputeMatrixAndOffset(); 
}

// Constructor with arguments
template<class TScalarType>
CenteredRigid2DTransform<TScalarType>::
CenteredRigid2DTransform( unsigned int spaceDimension, 
                          unsigned int parametersDimension):
  Superclass(spaceDimension,parametersDimension)
{
  // note: this virtual function will only
  // call the one defined in this class because 
  // we are in a constructor
  this->ComputeMatrixAndOffset(); 
}
 

//
// Set Parameters
// 
// Parameters are ordered as:
//
// p[0]   = angle
// p[1:2} = center of rotation coordinates
// p[3:4} = translation components
//
//
template <class TScalarType>
void
CenteredRigid2DTransform<TScalarType>
::SetParameters( const ParametersType & parameters )
{
  itkDebugMacro( << "Setting paramaters " << parameters );

  this->m_Parameters = parameters;

  // Take the angle
  this->SetAngle(parameters[0]);
 
  // Transfer the center
  InputPointType newCenter;
  for(unsigned int i=0; i < SpaceDimension; i++) 
    {
    newCenter[i] = parameters[i+1];
    }

  this->SetCenter(newCenter);

  OutputVectorType newTranslation;
  // Transfer the translation
  for(unsigned int j=0; j < SpaceDimension; j++) 
    {
    newTranslation[j] = parameters[j+1+SpaceDimension];
    }

  this->SetTranslation(newTranslation);

  this->ComputeMatrixAndOffset();

  itkDebugMacro(<<"After setting parameters ");
}

//
// Get Parameters
// 
// Parameters are ordered as:
//
// p[0]   = angle
// p[1:2} = center of rotation coordinates
// p[3:4} = translation components
//
template <class TScalarType>
const typename CenteredRigid2DTransform<TScalarType>::ParametersType &
CenteredRigid2DTransform<TScalarType>
::GetParameters( void ) const
{
  itkDebugMacro( << "Getting parameters ");

  // Set angles with parameters
  this->m_Parameters[0] = this->GetAngle();
 
  // Transfer the center of rotation 
  for(unsigned int i=0; i < SpaceDimension; i++) 
    {
    this->m_Parameters[i+1] = this->GetCenter()[i];
    }

  // Transfer the translation
  for(unsigned int j=0; j < SpaceDimension; j++) 
    {
    this->m_Parameters[j+1+SpaceDimension] =this->GetTranslation()[j];
    }

  itkDebugMacro(<<"After getting parameters " << this->m_Parameters );

  return this->m_Parameters;
}



// Set parameters
template<class TScalarType>
const typename CenteredRigid2DTransform<TScalarType>::JacobianType &
CenteredRigid2DTransform<TScalarType>::
GetJacobian( const InputPointType & p ) const
{
  // need to check if angles are in the right order
  const double ca = cos(this->GetAngle());
  const double sa = sin(this->GetAngle());

  this->m_Jacobian.Fill(0.0);

  const double cx = this->GetCenter()[0];
  const double cy = this->GetCenter()[1];

  // derivatives with respect to the angle
  this->m_Jacobian[0][0] = -sa * ( p[0] - cx ) - ca * ( p[1] - cy );
  this->m_Jacobian[1][0] =  ca * ( p[0] - cx ) - sa * ( p[1] - cy ); 

  // compute derivatives with respect to the center part
  // first with respect to cx
  this->m_Jacobian[0][1] = 1.0 - ca;
  this->m_Jacobian[1][1] =     - sa;  
  // then with respect to cy
  this->m_Jacobian[0][2] =       sa;
  this->m_Jacobian[1][2] = 1.0 - ca;


  // compute derivatives with respect to the translation part
  // first with respect to tx
  this->m_Jacobian[0][3] = 1.0;
  this->m_Jacobian[1][3] = 0.0;
  // first with respect to ty
  this->m_Jacobian[0][4] = 0.0;
  this->m_Jacobian[1][4] = 1.0;

  return this->m_Jacobian;

}
  
// Create and return an inverse transformation
template<class TScalarType>
void
CenteredRigid2DTransform<TScalarType>::
CloneInverseTo( Pointer & result ) const
{
  result = New();
  result->SetCenter( this->GetCenter() );  // inverse have the same center
  result->SetAngle( -this->GetAngle() );
  result->SetTranslation( -( this->GetInverseMatrix() * this->GetTranslation() ) );
}

// Print self
template<class TScalarType>
void
CenteredRigid2DTransform<TScalarType>::
PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
}

} // namespace

#endif
