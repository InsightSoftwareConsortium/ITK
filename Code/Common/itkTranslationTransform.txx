/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTranslationTransform.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkTranslationTransform_txx
#define _itkTranslationTransform_txx

#include "itkTranslationTransform.h"


namespace itk
{

// Constructor with default arguments
template<class TScalarType, unsigned int NDimensions>
TranslationTransform<TScalarType, NDimensions>::
TranslationTransform():Superclass(SpaceDimension,ParametersDimension)
{
  m_Offset.Fill( 0 );
}
    

// Destructor
template<class TScalarType, unsigned int NDimensions>
TranslationTransform<TScalarType, NDimensions>::
~TranslationTransform()
{
  return;
}


// Set the parameters
template <class TScalarType, unsigned int NDimensions>
void
TranslationTransform<TScalarType, NDimensions>
::SetParameters( const ParametersType & parameters )
{
  for( unsigned int i=0; i<SpaceDimension; i++ )
    {
    m_Offset[i] = parameters[i];
    }
}




// Get the parameters
template <class TScalarType, unsigned int NDimensions>
const typename TranslationTransform<TScalarType, NDimensions>::ParametersType &
TranslationTransform<TScalarType, NDimensions>
::GetParameters( void ) const
{
  for( unsigned int i=0; i<SpaceDimension; i++ )
    {
    m_Parameters[i] = m_Offset[i];
    }  
  return m_Parameters;
}



// Print self
template<class TScalarType, unsigned int NDimensions>
void
TranslationTransform<TScalarType, NDimensions>::
PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  
  os << indent << "Offset: " << m_Offset << std::endl;
}


// Compose with another affine transformation
template<class TScalarType, unsigned int NDimensions>
void
TranslationTransform<TScalarType, NDimensions>::
Compose(const Self * other, bool )
{
  m_Offset += other->m_Offset;
  return;
}


// Compose with a translation
template<class TScalarType, unsigned int NDimensions>
void
TranslationTransform<TScalarType, NDimensions>::
Translate(const OutputVectorType &offset, bool )
{
  m_Offset += offset;
  return;
}



// Transform a point
template<class TScalarType, unsigned int NDimensions>
typename TranslationTransform<TScalarType, NDimensions>::OutputPointType
TranslationTransform<TScalarType, NDimensions>::
TransformPoint(const InputPointType &point) const 
{
  return point + m_Offset;
}


// Transform a vector
template<class TScalarType, unsigned int NDimensions>
typename TranslationTransform<TScalarType, NDimensions>::OutputVectorType
TranslationTransform<TScalarType, NDimensions>::
TransformVector(const InputVectorType &vect) const 
{
  return  vect;
}


// Transform a vnl_vector_fixed
template<class TScalarType, unsigned int NDimensions>
typename TranslationTransform<TScalarType, NDimensions>::OutputVnlVectorType
TranslationTransform<TScalarType, NDimensions>::
TransformVector(const InputVnlVectorType &vect) const 
{
  return  vect;
}


// Transform a CovariantVector
template<class TScalarType, unsigned int NDimensions>
typename TranslationTransform<TScalarType, NDimensions>::OutputCovariantVectorType
TranslationTransform<TScalarType, NDimensions>::
TransformCovariantVector(const InputCovariantVectorType &vect) const 
{
  return  vect;
}



// Back transform a point
template<class TScalarType, unsigned int NDimensions>
typename TranslationTransform<TScalarType, NDimensions>::InputPointType
TranslationTransform<TScalarType, NDimensions>::
BackTransform(const OutputPointType &point) const {
  return point - m_Offset;
}




// Back transform a vector
template<class TScalarType, unsigned int NDimensions>
typename TranslationTransform<TScalarType, NDimensions>::InputVectorType
TranslationTransform<TScalarType, NDimensions>::
BackTransform(const OutputVectorType &vect ) const 
{
  return  vect;
}




// Back transform a vnl_vector
template<class TScalarType, unsigned int NDimensions>
typename TranslationTransform<TScalarType, NDimensions>::InputVnlVectorType
TranslationTransform<TScalarType, NDimensions>::
BackTransform(const OutputVnlVectorType &vect ) const 
{
  return  vect;
}


// Back Transform a CovariantVector
template<class TScalarType, unsigned int NDimensions>
typename TranslationTransform<TScalarType, NDimensions>::InputCovariantVectorType
TranslationTransform<TScalarType, NDimensions>::
BackTransform(const OutputCovariantVectorType &vect) const 
{
  return vect;
}



// Create and return an inverse transformation
template<class TScalarType, unsigned int NDimensions>
typename TranslationTransform<TScalarType, NDimensions>::Pointer
TranslationTransform<TScalarType, NDimensions>::
Inverse(void) const
{
  Pointer result = New();
  result->m_Offset   = - m_Offset;
  return result;
}

  

// Compute the Jacobian in one position 
template<class TScalarType, unsigned int NDimensions>
const typename TranslationTransform<TScalarType, NDimensions>::JacobianType & 
TranslationTransform< TScalarType, NDimensions >::
GetJacobian( const InputPointType & ) const
{

  m_Jacobian.Fill( 0.0 );

  for(unsigned int i=0; i<NDimensions; i++)
    {
    m_Jacobian(i,i) = 1.0f;
    }

  return m_Jacobian;

}



// Set the parameters for an Identity transform of this class
template<class TScalarType, unsigned int NDimensions>
void
TranslationTransform<TScalarType, NDimensions>::
SetIdentity()
{
  m_Offset.Fill( 0.0 );
}
 
  
} // namespace

#endif
