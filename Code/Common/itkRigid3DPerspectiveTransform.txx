/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRigid3DPerspectiveTransform.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkRigid3DPerspectiveTransform_txx
#define _itkRigid3DPerspectiveTransform_txx

#include "itkRigid3DPerspectiveTransform.h"


namespace itk
{

// Constructor with default arguments
template<class TScalarType>
Rigid3DPerspectiveTransform<TScalarType>::
Rigid3DPerspectiveTransform():Superclass(SpaceDimension,ParametersDimension)
{
  m_Offset.Fill( 0 );
  m_RotationMatrix = m_Versor.GetMatrix();
  m_FocalDistance = 1.0;
  m_Height = 1.0;
  m_Width  = 1.0;
}
 

// Destructor
template<class TScalarType>
Rigid3DPerspectiveTransform<TScalarType>::
~Rigid3DPerspectiveTransform()
{
}


// Print self
template<class TScalarType>
void
Rigid3DPerspectiveTransform<TScalarType>::
PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Offset: "       << m_Offset   << std::endl;
  os << indent << "Rotation: "     << m_Versor << std::endl;
  os << indent << "FocalDistance: "<< m_FocalDistance << std::endl;
  os << indent << "Height: "       << m_Height << std::endl;
  os << indent << "Width: "        << m_Width << std::endl;
  os << indent << "DirectMatrix: " << m_RotationMatrix   << std::endl;
}


// Set Parameters
template <class TScalarType>
void
Rigid3DPerspectiveTransform<TScalarType>
::SetParameters( const ParametersType & parameters )
{


  // Transfer the versor part
  
  AxisType axis;

  axis[0] = parameters[0];
  axis[1] = parameters[1];
  axis[2] = parameters[2];

  const TScalarType angle  = parameters[3];

  m_Versor.Set( axis, angle );


  
  
  // Transfer the translation part
  
  OffsetType offset;
  for(unsigned int i=0; i < SpaceDimension; i++) 
    {
    offset[i] = parameters[i+3];
    }

  
  this->SetOffset( offset );

  ComputeMatrix();

}


// Set rotation
template<class TScalarType>
void
Rigid3DPerspectiveTransform<TScalarType>::
SetRotation(const VersorType &rotation )
{
  m_Versor          = rotation;
  m_RotationMatrix  = m_Versor.GetMatrix();
  return;
}


// Set rotation
template<class TScalarType>
void
Rigid3DPerspectiveTransform<TScalarType>::
SetRotation(const Vector<TScalarType,3> & axis, double angle )
{
  const double sinus   = sin(angle/2.0);
  const double cosinus = cos(angle/2.0);
  Vector<TScalarType,3> norm;
  norm = axis;
  norm.Normalize();
  norm *= sinus;
  VnlQuaternionType q;
  q[0] = cosinus;
  q[1] = norm[0];
  q[2] = norm[1];
  q[3] = norm[2];
  VersorType v;
  v.Set(q);
  this->SetRotation(v);
}


// Transform a point
template<class TScalarType>
typename Rigid3DPerspectiveTransform<TScalarType>::OutputPointType
Rigid3DPerspectiveTransform<TScalarType>::
TransformPoint(const InputPointType &point) const 
{
  
  InputPointType rigid =  m_RotationMatrix * point + m_Offset;
  
  OutputPointType result;
  
  TScalarType factor = m_Height / (rigid[2]+m_FocalDistance);
  
  result[0] = rigid[0] * factor + m_Width  / 2.0;
  result[1] = rigid[1] * factor + m_Height / 2.0;
  
  return result;
}



// Transform a point
template<class TScalarType>
void
Rigid3DPerspectiveTransform<TScalarType>::
ComputeMatrix(void) 
{
  m_RotationMatrix = m_Versor.GetMatrix();
}


 
// Compute the Jacobian in one position 
template<class TScalarType >
const typename Rigid3DPerspectiveTransform<TScalarType>::JacobianType & 
Rigid3DPerspectiveTransform< TScalarType >::
GetJacobian( const InputPointType & p ) const
{
  

  m_Jacobian.Fill( 0.0 );

  // TODO

  return m_Jacobian;

}


 

} // namespace

#endif
