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
  m_Versor.SetIdentity();
  m_RotationMatrix = m_Versor.GetMatrix();
  m_FocalDistance = 1.0;
  m_FixedOffset.Fill(0);
  m_CenterOfRotation.Fill(0);
  m_Parameters.Fill(0);
  m_Parameters[3]=1; // identity versor
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

  os << indent << "Parameters: "   << m_Parameters  << std::endl;
  os << indent << "Offset: "       << m_Offset   << std::endl;
  os << indent << "Rotation: "     << m_Versor << std::endl;
  os << indent << "FocalDistance: "<< m_FocalDistance << std::endl;
  os << indent << "RotationMatrix: " << m_RotationMatrix   << std::endl;
  os << indent << "FixedOffset: " << m_FixedOffset   << std::endl;
  os << indent << "CenterOfRotation: " << m_CenterOfRotation   << std::endl;
}


// Set Parameters
template <class TScalarType>
void
Rigid3DPerspectiveTransform<TScalarType>
::SetParameters( const ParametersType & parameters )
{
  // Transfer the rotation part (quaternion)
  m_Versor.Set(parameters[0],parameters[1],parameters[2],parameters[3]);

  // Transfer the translation part
  OffsetType offset;
  for(unsigned int i=0; i < SpaceDimension; i++) 
    { 
    offset[i] = parameters[i+4];
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
  unsigned int i;
  InputPointType centered;
  for(i=0;i<3;i++)
    { 
    centered[i] = point[i] - m_CenterOfRotation[i];
    }

  InputPointType rotated =  m_RotationMatrix * centered;

  InputPointType rigided;
  for(i=0;i<3;i++)
    { 
    rigided[i] = rotated[i] + m_Offset[i] + m_CenterOfRotation[i] 
      + m_FixedOffset[i];
    }

  OutputPointType result;
  
  TScalarType factor = m_FocalDistance / rigided[2];
  
  result[0] = rigided[0] * factor;
  result[1] = rigided[1] * factor;

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
GetJacobian( const InputPointType &) const
{
  

  m_Jacobian.Fill( 0.0 );

  // TODO

  return m_Jacobian;

}


 

} // namespace

#endif
