/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTranslationTransform.txx
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
#ifndef _itkTranslationTransform_txx
#define _itkTranslationTransform_txx

#include "itkTranslationTransform.h"


namespace itk
{

// Constructor with default arguments
template<class ScalarType, unsigned int NDimensions>
TranslationTransform<ScalarType, NDimensions>::
TranslationTransform()
{
  m_Offset.Fill( 0 );
}
 
// Constructor with explicit arguments
template<class ScalarType, unsigned int NDimensions>
TranslationTransform<ScalarType, NDimensions>::
TranslationTransform(const VectorType &offset)
{
  m_Offset = offset;
}

    
// Copy Constructor
template <class ScalarType, unsigned int NDimensions>
TranslationTransform<ScalarType, NDimensions>
::TranslationTransform( const TranslationTransform<ScalarType, NDimensions> & other )
{
  m_Offset    = other.m_Offset;
}

// Destructor
template<class ScalarType, unsigned int NDimensions>
TranslationTransform<ScalarType, NDimensions>::
~TranslationTransform()
{
 return;
}


// Assignment Operator
template <class ScalarType, unsigned int NDimensions>
const TranslationTransform<ScalarType, NDimensions> &
TranslationTransform<ScalarType, NDimensions>
::operator=( const Self & other )
{
  m_Offset   = other.m_Offset;
  return *this;
}


// Print self
template<class ScalarType, unsigned int NDimensions>
std::ostream &
TranslationTransform<ScalarType, NDimensions>::
PrintSelf(std::ostream &s) const
{
  s << m_Offset << std::endl;
  return s;
}


// Compose with another affine transformation
template<class ScalarType, unsigned int NDimensions>
void
TranslationTransform<ScalarType, NDimensions>::
Compose(const Self &other, bool )
{
  m_Offset += other.m_Offset;
  return;
}


// Compose with a translation
template<class ScalarType, unsigned int NDimensions>
void
TranslationTransform<ScalarType, NDimensions>::
Translate(const VectorType &offset, bool )
{
  m_Offset += offset;
  return;
}



// Transform a point
template<class ScalarType, unsigned int NDimensions>
TranslationTransform<ScalarType, NDimensions>::OutputPointType
TranslationTransform<ScalarType, NDimensions>::
TransformPoint(const InputPointType &point) const 
{
  return point + m_Offset;
}


// Transform a vector
template<class ScalarType, unsigned int NDimensions>
TranslationTransform<ScalarType, NDimensions>::VectorType
TranslationTransform<ScalarType, NDimensions>::
TransformVector(const VectorType &vect) const 
{
  return  vect;
}


// Transform a vnl_vector_fixed
template<class ScalarType, unsigned int NDimensions>
TranslationTransform<ScalarType, NDimensions>::VnlVectorType
TranslationTransform<ScalarType, NDimensions>::
TransformVector(const VnlVectorType &vect) const 
{
  return  vect;
}


// Transform a CovariantVector
template<class ScalarType, unsigned int NDimensions>
TranslationTransform<ScalarType, NDimensions>::CovariantVectorType
TranslationTransform<ScalarType, NDimensions>::
TransformVector(const CovariantVectorType &vect) const 
{
  return  vect;
}



// Back transform a point
template<class ScalarType, unsigned int NDimensions>
TranslationTransform<ScalarType, NDimensions>::InputPointType
TranslationTransform<ScalarType, NDimensions>::
BackTransform(const OutputPointType &point) const {
  return point - m_Offset;
}

// Back transform a vector
template<class ScalarType, unsigned int NDimensions>
TranslationTransform<ScalarType, NDimensions>::VectorType
TranslationTransform<ScalarType, NDimensions>::
BackTransform(const VectorType &vect ) const 
{
    return  vect;
}

// Back transform a vnl_vector
template<class ScalarType, unsigned int NDimensions>
TranslationTransform<ScalarType, NDimensions>::VnlVectorType
TranslationTransform<ScalarType, NDimensions>::
BackTransform(const VnlVectorType &vect ) const 
{
    return  vect;
}


// Back Transform a CovariantVector
template<class ScalarType, unsigned int NDimensions>
TranslationTransform<ScalarType, NDimensions>::CovariantVectorType
TranslationTransform<ScalarType, NDimensions>::
BackTransform(const CovariantVectorType &vect) const 
{
  return vect;
}



// Create and return an inverse transformation
template<class ScalarType, unsigned int NDimensions>
TranslationTransform<ScalarType, NDimensions>
TranslationTransform<ScalarType, NDimensions>::
Inverse()
{
  Self result;
  result.m_Offset   = - m_Offset;
  return result;
}


  
} // namespace

#endif
