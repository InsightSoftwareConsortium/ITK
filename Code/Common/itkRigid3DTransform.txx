/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRigid3DTransform.txx
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
#ifndef _itkRigid3DTransform_txx
#define _itkRigid3DTransform_txx

#include "itkRigid3DTransform.h"


namespace itk
{

// Constructor with default arguments
template<class ScalarType, unsigned int NDimensions>
Rigid3DTransform<ScalarType, NDimensions>::
Rigid3DTransform()
{
  m_Offset.Fill( 0 );
  m_Rotation = VnlQuaternionType(0,0,0,1); // axis * sin(t/2), cos(t/2)
  m_DirectMatrix = m_Rotation.rotation_matrix();
  m_InverseMatrix = m_DirectMatrix.GetTranspose();
}
 
// Constructor with explicit arguments
template<class ScalarType, unsigned int NDimensions>
Rigid3DTransform<ScalarType, NDimensions>::
Rigid3DTransform(const VectorType &offset, const VnlQuaternionType & rotation)
{
  m_Offset   = offset;
  m_Rotation = rotation;
  m_DirectMatrix   = m_Rotation.rotation_matrix();
  m_InverseMatrix = m_DirectMatrix.GetTranspose();
}

    
// Copy Constructor
template <class ScalarType, unsigned int NDimensions>
Rigid3DTransform<ScalarType, NDimensions>
::Rigid3DTransform( const Rigid3DTransform<ScalarType, NDimensions> & other )
{
  m_Offset    = other.m_Offset;
  m_Rotation  = other.m_Rotation;
  m_DirectMatrix    = m_Rotation.rotation_matrix();
  m_InverseMatrix = m_DirectMatrix.GetTranspose();
}

// Destructor
template<class ScalarType, unsigned int NDimensions>
Rigid3DTransform<ScalarType, NDimensions>::
~Rigid3DTransform()
{
}


// Assignment Operator
template <class ScalarType, unsigned int NDimensions>
const Rigid3DTransform<ScalarType, NDimensions> &
Rigid3DTransform<ScalarType, NDimensions>
::operator=( const Self & other )
{
  m_Offset    = other.m_Offset;
  m_Rotation  = other.m_Rotation;
  m_DirectMatrix    = m_Rotation.rotation_matrix();
  m_InverseMatrix = m_DirectMatrix.GetTranspose();
  return *this;
}


// Print self
template<class ScalarType, unsigned int NDimensions>
std::ostream &
Rigid3DTransform<ScalarType, NDimensions>::
PrintSelf(std::ostream &s) const
{
  s << m_Offset   << std::endl;
  s << m_Rotation << std::endl;
  s << m_DirectMatrix   << std::endl;
  return s;
}

// Set rotation
template<class ScalarType, unsigned int NDimensions>
void
Rigid3DTransform<ScalarType, NDimensions>::
SetRotation(const VnlQuaternionType &rotation )
{
  m_Rotation      = rotation;
  m_DirectMatrix  = m_Rotation.rotation_matrix();
  m_InverseMatrix = m_DirectMatrix.GetTranspose();
  return;
}

// Compose with another affine transformation
template<class ScalarType, unsigned int NDimensions>
void
Rigid3DTransform<ScalarType, NDimensions>::
Compose(const Self &other, bool )
{
  m_Offset  += other.m_Offset;
  m_Rotation = m_Rotation * other.m_Rotation;
  m_DirectMatrix   = m_Rotation.rotation_matrix();
  m_InverseMatrix = m_DirectMatrix.GetTranspose();
  return;
}


// Compose with a translation
template<class ScalarType, unsigned int NDimensions>
void
Rigid3DTransform<ScalarType, NDimensions>::
Translate(const VectorType &offset, bool pre)
{
  m_Offset += offset;
  return;
}


// Compose with a rotation
template<class ScalarType, unsigned int NDimensions>
void
Rigid3DTransform<ScalarType, NDimensions>::
Rotate(const VnlQuaternionType &rotation, bool pre)
{
  m_Rotation      = m_Rotation * rotation;
  m_DirectMatrix  = m_Rotation.rotation_matrix();
  m_InverseMatrix = m_DirectMatrix.GetTranspose();
  return;
}


// Compose with a rotation
template<class ScalarType, unsigned int NDimensions>
void
Rigid3DTransform<ScalarType, NDimensions>::
Rotate(const VectorType & axis, ScalarType angle )
{
  m_Rotation      = VnlQuaternionType( axis.Get_vnl_vector(), angle );
  m_DirectMatrix  = m_Rotation.rotation_matrix();
  m_InverseMatrix = m_DirectMatrix.GetTranspose();
  return;
}


// Transform a point
template<class ScalarType, unsigned int NDimensions>
Rigid3DTransform<ScalarType, NDimensions>::OutputPointType
Rigid3DTransform<ScalarType, NDimensions>::
TransformPoint(const InputPointType &point) const 
{
  return m_DirectMatrix * point + m_Offset;
}


// Transform a vector
template<class ScalarType, unsigned int NDimensions>
Rigid3DTransform<ScalarType, NDimensions>::VectorType
Rigid3DTransform<ScalarType, NDimensions>::
TransformVector(const VectorType &vect) const 
{
  return  m_DirectMatrix * vect;
}


// Transform a vnl_vector_fixed
template<class ScalarType, unsigned int NDimensions>
Rigid3DTransform<ScalarType, NDimensions>::VnlVectorType
Rigid3DTransform<ScalarType, NDimensions>::
TransformVector(const VnlVectorType &vect) const 
{
  return  m_DirectMatrix * vect;
}


// Transform a CovariantVector
template<class ScalarType, unsigned int NDimensions>
Rigid3DTransform<ScalarType, NDimensions>::CovariantVectorType
Rigid3DTransform<ScalarType, NDimensions>::
TransformVector(const CovariantVectorType &vect) const 
{
  // Covariant vectors are transformed like contravariant
  // vectors under orthogonal transformations.
  return  m_DirectMatrix * vect;
}



// Back transform a point
template<class ScalarType, unsigned int NDimensions>
Rigid3DTransform<ScalarType, NDimensions>::InputPointType
Rigid3DTransform<ScalarType, NDimensions>::
BackTransform(const OutputPointType &point) const {
  return m_InverseMatrix * (point - m_Offset);
}

// Back transform a vector
template<class ScalarType, unsigned int NDimensions>
Rigid3DTransform<ScalarType, NDimensions>::VectorType
Rigid3DTransform<ScalarType, NDimensions>::
BackTransform(const VectorType &vect ) const 
{
    return  m_InverseMatrix * vect;
}

// Back transform a vnl_vector
template<class ScalarType, unsigned int NDimensions>
Rigid3DTransform<ScalarType, NDimensions>::VnlVectorType
Rigid3DTransform<ScalarType, NDimensions>::
BackTransform(const VnlVectorType &vect ) const 
{
    return  m_InverseMatrix * vect;
}


// Back Transform a CovariantVector
template<class ScalarType, unsigned int NDimensions>
Rigid3DTransform<ScalarType, NDimensions>::CovariantVectorType
Rigid3DTransform<ScalarType, NDimensions>::
BackTransform(const CovariantVectorType &vect) const 
{
  return m_DirectMatrix * vect;
}



// Create and return an inverse transformation
template<class ScalarType, unsigned int NDimensions>
Rigid3DTransform<ScalarType, NDimensions>
Rigid3DTransform<ScalarType, NDimensions>::
Inverse()
{
  Self result;
  result.m_Offset         = - m_Offset;
  result.m_Rotation       =   m_Rotation.inverse();
  result.m_DirectMatrix   =   m_Rotation.rotation_matrix();
  result.m_InverseMatrix  =   m_DirectMatrix.GetTranspose();
  return result;
}


  
} // namespace

#endif
