/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkScaleTransform.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkScaleTransform_txx
#define _itkScaleTransform_txx

#include "itkScaleTransform.h"


namespace itk
{

// Constructor with default arguments
template<class ScalarType, unsigned int NDimensions>
ScaleTransform<ScalarType, NDimensions>::
ScaleTransform():Superclass(SpaceDimension,ParametersDimension)
{
  m_Scale.Fill( 1.0 );
}
    


// Destructor
template<class ScalarType, unsigned int NDimensions>
ScaleTransform<ScalarType, NDimensions>::
~ScaleTransform()
{
 return;
}


// Set the parameters
template <class ScalarType, unsigned int NDimensions>
void
ScaleTransform<ScalarType, NDimensions>
::SetParameters( const ParametersType & parameters )
{
  for( unsigned int i=0; i<SpaceDimension; i++ )
    {
    m_Scale[i] = parameters[i];
    }
  m_Parameters = parameters;
}


// Get Parameters
template <class TScalarType,unsigned int NDimensions>
const typename ScaleTransform<TScalarType,NDimensions>::ParametersType &
ScaleTransform<TScalarType,NDimensions>
::GetParameters( void ) const
{
  itkDebugMacro( << "Getting parameters ");

  // Transfer the translation part
  for(unsigned int i=0; i < SpaceDimension; i++) 
  {
    m_Parameters[i] = m_Scale[i];
  }

  itkDebugMacro(<<"After getting parameters " << m_Parameters );

  return m_Parameters;
}



// Print self
template<class ScalarType, unsigned int NDimensions>
void
ScaleTransform<ScalarType, NDimensions>::
PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  
  os << indent << "Scale: " << m_Scale << std::endl;
}


// Compose with another affine transformation
template<class ScalarType, unsigned int NDimensions>
void
ScaleTransform<ScalarType, NDimensions>::
Compose(const Self * other, bool )
{
  for( unsigned int i=0; i<SpaceDimension; i++ )
  {
    m_Scale[i] *= other->m_Scale[i];
  }
  return;
}


// Compose with a scale
template<class ScalarType, unsigned int NDimensions>
void
ScaleTransform<ScalarType, NDimensions>::
Scale(const ScaleType & scale, bool )
{
  for( unsigned int i=0; i<SpaceDimension; i++ )
  {
    m_Scale[i] *= scale[i];
  }
  return;
}



// Transform a point
template<class ScalarType, unsigned int NDimensions>
typename ScaleTransform<ScalarType, NDimensions>::OutputPointType
ScaleTransform<ScalarType, NDimensions>::
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
template<class ScalarType, unsigned int NDimensions>
typename ScaleTransform<ScalarType, NDimensions>::OutputVectorType
ScaleTransform<ScalarType, NDimensions>::
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
template<class ScalarType, unsigned int NDimensions>
typename ScaleTransform<ScalarType, NDimensions>::OutputVnlVectorType
ScaleTransform<ScalarType, NDimensions>::
TransformVector(const InputVnlVectorType &vect) const 
{
  OutputVnlVectorType result;
  for( unsigned int i=0; i<SpaceDimension; i++ )
  {
    result[i] = vect[i] * m_Scale[i];
  }
  return result;
}


// Transform a CovariantVector
template<class ScalarType, unsigned int NDimensions>
typename ScaleTransform<ScalarType, NDimensions>::OutputCovariantVectorType
ScaleTransform<ScalarType, NDimensions>::
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
template<class ScalarType, unsigned int NDimensions>
typename ScaleTransform<ScalarType, NDimensions>::InputPointType
ScaleTransform<ScalarType, NDimensions>::
BackTransform(const OutputPointType &point) const {
  InputPointType result;
  for( unsigned int i=0; i<SpaceDimension; i++ )
  {
    result[i] = point[i] / m_Scale[i];
  }
  return result;
}




// Back transform a vector
template<class ScalarType, unsigned int NDimensions>
typename ScaleTransform<ScalarType, NDimensions>::InputVectorType
ScaleTransform<ScalarType, NDimensions>::
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
template<class ScalarType, unsigned int NDimensions>
typename ScaleTransform<ScalarType, NDimensions>::InputVnlVectorType
ScaleTransform<ScalarType, NDimensions>::
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
template<class ScalarType, unsigned int NDimensions>
typename ScaleTransform<ScalarType, NDimensions>::InputCovariantVectorType
ScaleTransform<ScalarType, NDimensions>::
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
template<class ScalarType, unsigned int NDimensions>
typename ScaleTransform<ScalarType, NDimensions>::Pointer
ScaleTransform<ScalarType, NDimensions>::
Inverse(void) const
{
  Pointer result = New();
  for( unsigned int i=0; i<SpaceDimension; i++ )
  {
    result->m_Scale[i] = 1.0 / m_Scale[i];
  }
  return result;
}

// Compute the Jacobian of the transformation
// It follows the same order of Parameters vector 
template<class ScalarType, unsigned int NDimensions>
const typename ScaleTransform<ScalarType, NDimensions>::JacobianType &
ScaleTransform<ScalarType, NDimensions>
::GetJacobian( const InputPointType & p ) const
{
  
  m_Jacobian.Fill(0);
  for(unsigned int dim=0; dim<SpaceDimension; dim++)
    {
    m_Jacobian(dim,dim) = p[dim];
    }
  return m_Jacobian;
}


} // namespace

#endif
