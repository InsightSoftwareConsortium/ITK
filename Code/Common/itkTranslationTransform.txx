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
template<class TScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType>
TranslationTransform<TScalarType, NDimensions,TParameters,TJacobianType>::
TranslationTransform()
{
  m_Offset.Fill( 0 );
}
    

// Destructor
template<class TScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType>
TranslationTransform<TScalarType, NDimensions,TParameters,TJacobianType>::
~TranslationTransform()
{
 return;
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
