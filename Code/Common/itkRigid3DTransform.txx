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
template<class TScalarType>
Rigid3DTransform<TScalarType>::
Rigid3DTransform()
{
  m_Offset.Fill( 0 );
  m_Rotation = VnlQuaternionType(0,0,0,1); // axis * sin(t/2), cos(t/2)
  m_DirectMatrix = m_Rotation.rotation_matrix();
  m_InverseMatrix = m_DirectMatrix.GetTranspose();
  m_CenterOfRotation = NULL;
}
 
    
// Copy Constructor
template <class TScalarType>
Rigid3DTransform<TScalarType>
::Rigid3DTransform( const Self & other )
{
  m_Offset    = other.m_Offset;
  m_Rotation  = other.m_Rotation;
  m_DirectMatrix    = m_Rotation.rotation_matrix();
  m_InverseMatrix = m_DirectMatrix.GetTranspose();
  m_CenterOfRotation = other.m_CenterOfRotation;
}

// Destructor
template<class TScalarType>
Rigid3DTransform<TScalarType>::
~Rigid3DTransform()
{
}


// Assignment Operator
template <class TScalarType>
const Rigid3DTransform<TScalarType> &
Rigid3DTransform<TScalarType>
::operator=( const Self & other )
{
  m_Offset    = other.m_Offset;
  m_Rotation  = other.m_Rotation;
  m_DirectMatrix    = m_Rotation.rotation_matrix();
  m_InverseMatrix = m_DirectMatrix.GetTranspose();
  m_CenterOfRotation = other.m_CenterOfRotation;
  return *this;
}

// Set Center of Rotation
template<class TScalarType>
void
Rigid3DTransform<TScalarType>::
SetCenterOfRotation(const double* center) 
{
  m_CenterOfRotation = center;
}



// Get Center of Rotation
template<class TScalarType>
const double*
Rigid3DTransform<TScalarType>::
GetCenterOfRotation(void) 
{
  return m_CenterOfRotation;
}

// Print self
template<class TScalarType>
std::ostream &
Rigid3DTransform<TScalarType>::
PrintSelf(std::ostream &s) const
{
  s << m_Offset   << std::endl;
  s << m_Rotation << std::endl;
  s << m_DirectMatrix   << std::endl;
  return s;
}

// Set rotation
template<class TScalarType>
void
Rigid3DTransform<TScalarType>::
SetRotation(const VnlQuaternionType &rotation )
{
  m_Rotation      = rotation;
  m_DirectMatrix  = m_Rotation.rotation_matrix();
  m_InverseMatrix = m_DirectMatrix.GetTranspose();
  return;
}

// Set rotation
template<class TScalarType>
void
Rigid3DTransform<TScalarType>::
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
 
// Compose with another affine transformation
template<class TScalarType>
void
Rigid3DTransform<TScalarType>::
Compose(const Self &other, bool )
{
  m_Offset  += other.m_Offset;
  m_Rotation = m_Rotation * other.m_Rotation;
  m_DirectMatrix   = m_Rotation.rotation_matrix();
  m_InverseMatrix = m_DirectMatrix.GetTranspose();
  return;
}


// Compose with a translation
template<class TScalarType>
void
Rigid3DTransform<TScalarType>::
Translate(const OffsetType &offset, bool pre)
{
  m_Offset += offset;
  return;
}


// Compose with a rotation
template<class TScalarType>
void
Rigid3DTransform<TScalarType>::
Rotate(const VnlQuaternionType &rotation, bool pre)
{
  m_Rotation      = m_Rotation * rotation;
  m_DirectMatrix  = m_Rotation.rotation_matrix();
  m_InverseMatrix = m_DirectMatrix.GetTranspose();
  return;
}


// Compose with a rotation
template<class TScalarType>
void
Rigid3DTransform<TScalarType>::
Rotate(const InputVectorType & axis, TScalarType angle )
{
  m_Rotation      = VnlQuaternionType( axis.Get_vnl_vector(), angle );
  m_DirectMatrix  = m_Rotation.rotation_matrix();
  m_InverseMatrix = m_DirectMatrix.GetTranspose();
  return;
}

// set Rotation Matrix using Euler's angle
template<class TScalarType>
void
Rigid3DTransform<TScalarType>::
SetEulerAngles(double alpha, double beta, double gamma)
{
  const double ca = cos(alpha);
  const double sa = sin(alpha);
  const double cb = cos(beta);
  const double sb = sin(beta); 
  const double cg = cos(gamma);
  const double sg = sin(gamma);

  m_DirectMatrix[0][0] = ca * cb;
  m_DirectMatrix[0][1] = ca * sb *sg - sa * cg;
  m_DirectMatrix[0][2] = ca * sb *cg + sa * sg;
  m_DirectMatrix[1][0] = sa * cb;
  m_DirectMatrix[1][1] = sa * sb * sg + ca * cg;
  m_DirectMatrix[1][2] = sa * sb * cg - ca * sg;
  m_DirectMatrix[2][0] = -sb;
  m_DirectMatrix[2][1] = cb * sg;
  m_DirectMatrix[2][2] = cb * cg;

}




// Transform a point
template<class TScalarType>
Rigid3DTransform<TScalarType>::OutputPointType
Rigid3DTransform<TScalarType>::
TransformPoint(const InputPointType &point) const 
{
  if(m_CenterOfRotation)
  {
    InputPointType intermediate;
    for(unsigned int i=0;i<3;i++)
    {
      intermediate[i] = point[i] - m_CenterOfRotation[i];
    }
    intermediate = m_DirectMatrix * intermediate + m_Offset;
    for(unsigned int i=0;i<3;i++)
    {
      intermediate[i] +=  m_CenterOfRotation[i];
    }
    return intermediate;
  }
  else
  {
    return m_DirectMatrix * point + m_Offset;
  }
}


// Transform a vector
template<class TScalarType>
Rigid3DTransform<TScalarType>::OutputVectorType
Rigid3DTransform<TScalarType>::
TransformVector(const InputVectorType &vect) const 
{
  return  m_DirectMatrix * vect;
}


// Transform a vnl_vector_fixed
template<class TScalarType>
Rigid3DTransform<TScalarType>::OutputVnlVectorType
Rigid3DTransform<TScalarType>::
TransformVnlVector(const InputVnlVectorType &vect) const 
{
  return  m_DirectMatrix * vect;
}


// Transform a CovariantVector
template<class TScalarType>
Rigid3DTransform<TScalarType>::OutputCovariantVectorType
Rigid3DTransform<TScalarType>::
TransformCovariantVector(const InputCovariantVectorType &vect) const 
{
  // Covariant vectors are transformed like contravariant
  // vectors under orthogonal transformations.
  return  m_DirectMatrix * vect;
}



// Back transform a point
template<class TScalarType>
Rigid3DTransform<TScalarType>::InputPointType
Rigid3DTransform<TScalarType>::
BackTransform(const OutputPointType &point) const {

  if(m_CenterOfRotation)
  {
    OutputPointType centered;
    for(unsigned int i=0;i<3;i++)
    {
      centered[i] = point[i] - m_CenterOfRotation[i];
    }
    centered = m_InverseMatrix * centered - m_Offset;
    for(unsigned int i=0;i<3;i++)
    {
      centered[i] +=  m_CenterOfRotation[i];
    }
    return centered;
  }
  else
  {
    return m_InverseMatrix * (point - m_Offset);
  }

  
}

// Back transform a vector
template<class TScalarType>
Rigid3DTransform<TScalarType>::InputVectorType
Rigid3DTransform<TScalarType>::
BackTransform(const OutputVectorType &vect ) const 
{
    return  m_InverseMatrix * vect;
}

// Back transform a vnl_vector
template<class TScalarType>
Rigid3DTransform<TScalarType>::InputVnlVectorType
Rigid3DTransform<TScalarType>::
BackTransform(const OutputVnlVectorType &vect ) const 
{
    return  m_InverseMatrix * vect;
}


// Back Transform a CovariantVector
template<class TScalarType>
Rigid3DTransform<TScalarType>::InputCovariantVectorType
Rigid3DTransform<TScalarType>::
BackTransform(const OutputCovariantVectorType &vect) const 
{
  return m_DirectMatrix * vect;
}



// Create and return an inverse transformation
template<class TScalarType>
Rigid3DTransform<TScalarType>
Rigid3DTransform<TScalarType>::
Inverse( void ) const
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
