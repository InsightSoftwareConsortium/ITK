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
  m_DirectMatrix.SetIdentity();
  m_InverseMatrix.SetIdentity();
}
 
    
// Copy Constructor
template <class TScalarType>
Rigid3DTransform<TScalarType>
::Rigid3DTransform( const Self & other )
{
  m_Offset        = other.m_Offset;
  m_DirectMatrix  = other.m_DirectMatrix;
  m_InverseMatrix = other.m_InverseMatrix;
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
  m_Offset        = other.m_Offset;
  m_DirectMatrix  = other.m_DirectMatrix;
  m_InverseMatrix = other.m_InverseMatrix;
  return *this;
}



// Print self
template<class TScalarType>
std::ostream &
Rigid3DTransform<TScalarType>::
PrintSelf(std::ostream &s) const
{
  s << m_Offset   << std::endl;
  s << m_DirectMatrix   << std::endl;
  return s;
}


 
// Compose with another affine transformation
template<class TScalarType>
void
Rigid3DTransform<TScalarType>::
Compose(const Self * other, bool pre )
{
  if (pre) 
    {
    m_Offset       = m_DirectMatrix * other->m_Offset + m_Offset;
    m_DirectMatrix = m_DirectMatrix * other->m_DirectMatrix;
    }
  else 
    {
    m_Offset       = other->m_DirectMatrix * m_Offset + other->m_Offset;
    m_DirectMatrix = other->m_DirectMatrix * m_DirectMatrix;
    }
  m_InverseMatrix = m_DirectMatrix.GetTranspose();
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




// Transform a point
template<class TScalarType>
Rigid3DTransform<TScalarType>::OutputPointType
Rigid3DTransform<TScalarType>::
TransformPoint(const InputPointType &point) const 
{
  return m_DirectMatrix * point + m_Offset; 
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
BackTransform(const OutputPointType &point) const 
{
    return m_InverseMatrix * (point - m_Offset);
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
Rigid3DTransform<TScalarType>::Pointer
Rigid3DTransform<TScalarType>::
Inverse( void ) const
{
  Pointer result = New();
  result->m_Matrix   =   m_Inverse;
  result->m_Inverse  =   m_Matrix;
  result->m_Offset   = -(m_Inverse * m_Offset);
  return result;
}


  
} // namespace

#endif
