/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRigid3DPerspectiveTransform.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef _itkRigid3DPerspectiveTransform_txx
#define _itkRigid3DPerspectiveTransform_txx

#include "itkRigid3DPerspectiveTransform.h"


namespace itk
{

// Constructor with default arguments
template<class TScalarType>
Rigid3DPerspectiveTransform<TScalarType>::
Rigid3DPerspectiveTransform()
{
  m_Offset.Fill( 0 );
  m_Rotation = VnlQuaternionType(0,0,0,1); // axis * sin(t/2), cos(t/2)
  m_DirectMatrix = m_Rotation.rotation_matrix();
  m_FocalDistance = 1.0;
  m_Height = 1.0;
  m_Width  = 1.0;
}
 

    
// Copy Constructor
template <class TScalarType>
Rigid3DPerspectiveTransform<TScalarType>
::Rigid3DPerspectiveTransform( const Rigid3DPerspectiveTransform<TScalarType> & other )
{
  m_Offset        = other.m_Offset;
  m_Rotation      = other.m_Rotation;
  m_FocalDistance = other.m_FocalDistance;
  m_Width         = other.m_Width;
  m_Height        = other.m_Height;
  m_DirectMatrix    = m_Rotation.rotation_matrix();
}

// Destructor
template<class TScalarType>
Rigid3DPerspectiveTransform<TScalarType>::
~Rigid3DPerspectiveTransform()
{
}


// Assignment Operator
template <class TScalarType>
const Rigid3DPerspectiveTransform<TScalarType> &
Rigid3DPerspectiveTransform<TScalarType>
::operator=( const Self & other )
{
  m_Offset        = other.m_Offset;
  m_Rotation      = other.m_Rotation;
  m_FocalDistance = other.m_FocalDistance;
  m_Width         = other.m_Width;
  m_Height        = other.m_Height;
  m_DirectMatrix    = m_Rotation.rotation_matrix();
  return *this;
}


// Print self
template<class TScalarType>
std::ostream &
Rigid3DPerspectiveTransform<TScalarType>::
PrintSelf(std::ostream &s) const
{
  s << m_Offset   << std::endl;
  s << m_Rotation << std::endl;
  s << m_Focal << std::endl;
  s << m_Height << std::endl;
  s << m_Width << std::endl;
  s << m_DirectMatrix   << std::endl;
  return s;
}


// Set rotation
template<class TScalarType>
void
Rigid3DPerspectiveTransform<TScalarType>::
SetRotation(const VnlQuaternionType &rotation )
{
  m_Rotation      = rotation;
  m_DirectMatrix  = m_Rotation.rotation_matrix();
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
  SetRotation( q );
}

// Transform a point
template<class TScalarType>
Rigid3DPerspectiveTransform<TScalarType>::OutputPointType
Rigid3DPerspectiveTransform<TScalarType>::
TransformPoint(const InputPointType &point) const 
{
  /*
  std::cout << "Rigid3DPerspectiveTransform = " << (void *)this << std::endl;
  std::cout << "Width " << m_Width          << std::endl;
  std::cout << "Height" << m_Height         << std::endl;
  std::cout << "Focal " << m_FocalDistance  << std::endl;
  std::cout << "Input point = " << point << std::endl;
  */
  
  InputPointType rigid =  m_DirectMatrix * point + m_Offset;
  
  //  std::cout << "Rigid point = " << rigid << std::endl;
  
  OutputPointType result;
  TScalarType factor = m_Height / (rigid[0]-m_FocalDistance);
  result[0] = rigid[0] * factor + m_Width  / 2.0;
  result[1] = rigid[1] * factor + m_Height / 2.0;
  // std::cout << "Mapped point = " << result << std::endl;
  // std::cout << std::endl;
  return result;
}

// Transform a vector
template<class TScalarType>
Rigid3DPerspectiveTransform<TScalarType>::OutputVectorType
Rigid3DPerspectiveTransform<TScalarType>::
TransformVector(const InputVectorType &point) const 
{
  OutputVectorType result;
  return result;
}

 
// Transform a vnl_vector
template<class TScalarType>
Rigid3DPerspectiveTransform<TScalarType>::OutputVnlVectorType
Rigid3DPerspectiveTransform<TScalarType>::
TransformVnlVector(const InputVnlVectorType &point) const 
{
  OutputVnlVectorType result;
  return result;
}


} // namespace

#endif
