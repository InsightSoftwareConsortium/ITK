/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkScaleTransform.txx
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
#ifndef _itkScaleTransform_txx
#define _itkScaleTransform_txx

#include "itkScaleTransform.h"


namespace itk
{

// Constructor with default arguments
template<class ScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType>
ScaleTransform<ScalarType, NDimensions,TParameters,TJacobianType>::
ScaleTransform()
{
  m_Scale.Fill( 1.0 );
}
    


// Copy Constructor
template <class ScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType>
ScaleTransform<ScalarType, NDimensions,TParameters,TJacobianType>
::ScaleTransform( const Self & other )
{
  m_Scale    = other.m_Scale;
}



// Destructor
template<class ScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType>
ScaleTransform<ScalarType, NDimensions,TParameters,TJacobianType>::
~ScaleTransform()
{
 return;
}


// Assignment Operator
template <class ScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType>
const ScaleTransform<ScalarType, NDimensions,
                           TParameters,TJacobianType> &
ScaleTransform<ScalarType, NDimensions,TParameters,TJacobianType>
::operator=( const Self & other )
{
  m_Scale   = other.m_Scale;
  return *this;
}



// Set the parameters
template <class ScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType>
void
ScaleTransform<ScalarType, NDimensions,TParameters,TJacobianType>
::SetParameters( const ParametersType & parameters )
{
  for( unsigned int i=0; i<SpaceDimension; i++ )
  {
    m_Scale[i] = parameters[i];
  }
}



// Print self
template<class ScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType>
std::ostream &
ScaleTransform<ScalarType, NDimensions,TParameters,TJacobianType>::
PrintSelf(std::ostream &s) const
{
  s << m_Scale << std::endl;
  return s;
}


// Compose with another affine transformation
template<class ScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType>
void
ScaleTransform<ScalarType, NDimensions,TParameters,TJacobianType>::
Compose(const Self * other, bool )
{
  for( unsigned int i=0; i<SpaceDimension; i++ )
  {
    m_Scale[i] *= other->m_Scale[i];
  }
  return;
}


// Compose with a scale
template<class ScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType>
void
ScaleTransform<ScalarType, NDimensions,TParameters,TJacobianType>::
Scale(const ScaleType & scale, bool )
{
  for( unsigned int i=0; i<SpaceDimension; i++ )
  {
    m_Scale[i] *= scale[i];
  }
  return;
}



// Transform a point
template<class ScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType>
ScaleTransform<ScalarType, NDimensions,
              TParameters,TJacobianType>::OutputPointType
ScaleTransform<ScalarType, NDimensions,TParameters,TJacobianType>::
TransformPoint(const InputPointType &point) const 
{
  OutputPointType result;
  for( unsigned int i=0; i<SpaceDimension; i++ )
  {
    result[i] = point[i] * m_Scale[i];
  }
  return result;
}


// Transform a vector
template<class ScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType>
ScaleTransform<ScalarType, NDimensions,
                     TParameters,TJacobianType>::OutputVectorType
ScaleTransform<ScalarType, NDimensions,TParameters,TJacobianType>::
TransformVector(const InputVectorType &vect) const 
{
  OutputVectorType result;
  for( unsigned int i=0; i<SpaceDimension; i++ )
  {
    result[i] = vect[i] * m_Scale[i];
  }
  return result;
}


// Transform a vnl_vector_fixed
template<class ScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType>
ScaleTransform<ScalarType, NDimensions,
                     TParameters,TJacobianType>::OutputVnlVectorType
ScaleTransform<ScalarType, NDimensions,TParameters,TJacobianType>::
TransformVnlVector(const InputVnlVectorType &vect) const 
{
  OutputVnlVectorType result;
  for( unsigned int i=0; i<SpaceDimension; i++ )
  {
    result[i] = vect[i] * m_Scale[i];
  }
  return result;
}


// Transform a CovariantVector
template<class ScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType>
ScaleTransform<ScalarType, NDimensions,
                      TParameters,TJacobianType>::OutputCovariantVectorType
ScaleTransform<ScalarType, NDimensions,TParameters,TJacobianType>::
TransformCovariantVector(const InputCovariantVectorType &vect) const 
{
  // Covariant Vectors are scaled by the inverse
  OutputCovariantVectorType result;
  for( unsigned int i=0; i<SpaceDimension; i++ )
  {
    result[i] = vect[i] / m_Scale[i];
  }
  return result;
}



// Back transform a point
template<class ScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType>
ScaleTransform<ScalarType, NDimensions,
                     TParameters,TJacobianType>::InputPointType
ScaleTransform<ScalarType, NDimensions,TParameters,TJacobianType>::
BackTransform(const OutputPointType &point) const {
  InputPointType result;
  for( unsigned int i=0; i<SpaceDimension; i++ )
  {
    result[i] = point[i] / m_Scale[i];
  }
  return result;
}




// Back transform a vector
template<class ScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType>
ScaleTransform<ScalarType, NDimensions,
                      TParameters,TJacobianType>::InputVectorType
ScaleTransform<ScalarType, NDimensions,TParameters,TJacobianType>::
BackTransform(const OutputVectorType &vect ) const 
{
  InputVectorType result;
  for( unsigned int i=0; i<SpaceDimension; i++ )
  {
    result[i] = vect[i] / m_Scale[i];
  }
  return result;
}




// Back transform a vnl_vector
template<class ScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType>
ScaleTransform<ScalarType, NDimensions,
                     TParameters,TJacobianType>::InputVnlVectorType
ScaleTransform<ScalarType, NDimensions,TParameters,TJacobianType>::
BackTransform(const OutputVnlVectorType &vect ) const 
{
  InputVnlVectorType result;
  for( unsigned int i=0; i<SpaceDimension; i++ )
  {
    result[i] = vect[i] / m_Scale[i];
  }
  return result;
}


// Back Transform a CovariantVector
template<class ScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType>
ScaleTransform<ScalarType, NDimensions,
                    TParameters,TJacobianType>::InputCovariantVectorType
ScaleTransform<ScalarType, NDimensions,TParameters,TJacobianType>::
BackTransform(const OutputCovariantVectorType &vect) const 
{
  // Covariant Vectors are scaled by the inverse
  InputCovariantVectorType result;
  for( unsigned int i=0; i<SpaceDimension; i++ )
  {
    result[i] = vect[i] * m_Scale[i];
  }
  return result;
}



// Create and return an inverse transformation
template<class ScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType>
ScaleTransform<ScalarType, NDimensions,TParameters,TJacobianType>::Pointer
ScaleTransform<ScalarType, NDimensions,TParameters,TJacobianType>::
Inverse(void) const
{
  Pointer result = New();
  result->m_Scale   = 1.0 / m_Scale;
  return result;
}


  
} // namespace

#endif
