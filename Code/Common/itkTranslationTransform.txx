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
template<class TScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType>
TranslationTransform<TScalarType, NDimensions,TParameters,TJacobianType>::
TranslationTransform()
{
  m_Offset.Fill( 0 );
}
    


// Copy Constructor
template <class TScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType>
TranslationTransform<TScalarType, NDimensions,TParameters,TJacobianType>
::TranslationTransform( const 
    TranslationTransform<TScalarType, NDimensions, 
                         TParameters, TJacobianType > & other )
{
  m_Offset    = other.m_Offset;
}



// Destructor
template<class TScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType>
TranslationTransform<TScalarType, NDimensions,TParameters,TJacobianType>::
~TranslationTransform()
{
 return;
}


// Assignment Operator
template <class TScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType>
const TranslationTransform<TScalarType, NDimensions,
                           TParameters,TJacobianType> &
TranslationTransform<TScalarType, NDimensions,TParameters,TJacobianType>
::operator=( const Self & other )
{
  m_Offset   = other.m_Offset;
  return *this;
}



// Set the parameters
template <class TScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType>
void
TranslationTransform<TScalarType, NDimensions,TParameters,TJacobianType>
::SetParameters( const ParametersType & parameters )
{
  for( unsigned int i=0; i<SpaceDimension; i++ )
  {
    m_Offset[i] = parameters[i];
  }
}



// Print self
template<class TScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType>
void
TranslationTransform<TScalarType, NDimensions,TParameters,TJacobianType>::
PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  
  os << indent << "Offset: " << m_Offset << std::endl;
}


// Compose with another affine transformation
template<class TScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType>
void
TranslationTransform<TScalarType, NDimensions,TParameters,TJacobianType>::
Compose(const Self * other, bool )
{
  m_Offset += other->m_Offset;
  return;
}


// Compose with a translation
template<class TScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType>
void
TranslationTransform<TScalarType, NDimensions,TParameters,TJacobianType>::
Translate(const OutputVectorType &offset, bool )
{
  m_Offset += offset;
  return;
}



// Transform a point
template<class TScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType>
TranslationTransform<TScalarType, NDimensions,
              TParameters,TJacobianType>::OutputPointType
TranslationTransform<TScalarType, NDimensions,TParameters,TJacobianType>::
TransformPoint(const InputPointType &point) const 
{
  return point + m_Offset;
}


// Transform a vector
template<class TScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType>
TranslationTransform<TScalarType, NDimensions,
                     TParameters,TJacobianType>::OutputVectorType
TranslationTransform<TScalarType, NDimensions,TParameters,TJacobianType>::
TransformVector(const InputVectorType &vect) const 
{
  return  vect;
}


// Transform a vnl_vector_fixed
template<class TScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType>
TranslationTransform<TScalarType, NDimensions,
                     TParameters,TJacobianType>::OutputVnlVectorType
TranslationTransform<TScalarType, NDimensions,TParameters,TJacobianType>::
TransformVector(const InputVnlVectorType &vect) const 
{
  return  vect;
}


// Transform a CovariantVector
template<class TScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType>
TranslationTransform<TScalarType, NDimensions,
                      TParameters,TJacobianType>::OutputCovariantVectorType
TranslationTransform<TScalarType, NDimensions,TParameters,TJacobianType>::
TransformCovariantVector(const InputCovariantVectorType &vect) const 
{
  return  vect;
}



// Back transform a point
template<class TScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType>
TranslationTransform<TScalarType, NDimensions,
                     TParameters,TJacobianType>::InputPointType
TranslationTransform<TScalarType, NDimensions,TParameters,TJacobianType>::
BackTransform(const OutputPointType &point) const {
  return point - m_Offset;
}




// Back transform a vector
template<class TScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType>
TranslationTransform<TScalarType, NDimensions,
                      TParameters,TJacobianType>::InputVectorType
TranslationTransform<TScalarType, NDimensions,TParameters,TJacobianType>::
BackTransform(const OutputVectorType &vect ) const 
{
    return  vect;
}




// Back transform a vnl_vector
template<class TScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType>
TranslationTransform<TScalarType, NDimensions,
                     TParameters,TJacobianType>::InputVnlVectorType
TranslationTransform<TScalarType, NDimensions,TParameters,TJacobianType>::
BackTransform(const OutputVnlVectorType &vect ) const 
{
    return  vect;
}


// Back Transform a CovariantVector
template<class TScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType>
TranslationTransform<TScalarType, NDimensions,
                    TParameters,TJacobianType>::InputCovariantVectorType
TranslationTransform<TScalarType, NDimensions,TParameters,TJacobianType>::
BackTransform(const OutputCovariantVectorType &vect) const 
{
  return vect;
}



// Create and return an inverse transformation
template<class TScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType>
TranslationTransform<TScalarType, NDimensions,TParameters,TJacobianType>::Pointer
TranslationTransform<TScalarType, NDimensions,TParameters,TJacobianType>::
Inverse(void) const
{
  Pointer result = New();
  result->m_Offset   = - m_Offset;
  return result;
}


  
} // namespace

#endif
