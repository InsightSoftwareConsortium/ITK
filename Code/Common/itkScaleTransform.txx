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
template<class ScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType>
ScaleTransform<ScalarType, NDimensions,TParameters,TJacobianType>::
ScaleTransform()
{
  m_Scale.Fill( 1.0 );
}
    


// Destructor
template<class ScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType>
ScaleTransform<ScalarType, NDimensions,TParameters,TJacobianType>::
~ScaleTransform()
{
 return;
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
void
ScaleTransform<ScalarType, NDimensions,TParameters,TJacobianType>::
PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  
  os << indent << "Scale: " << m_Scale << std::endl;
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
  for( unsigned int i=0; i<SpaceDimension; i++ )
  {
    result->m_Scale[i] = 1.0 / m_Scale[i];
  }
  return result;
}

// Compute the Jacobian of the transformation
// It follows the same order of Parameters vector 
template<class ScalarType,
   unsigned int NDimensions,
         class TParameters,
   class TJacobianType>
const ScaleTransform<ScalarType, NDimensions,TParameters,TJacobianType>::JacobianType &
ScaleTransform<ScalarType, NDimensions,TParameters,TJacobianType>
::GetJacobian( const InputPointType & p ) const
{
  
  m_Jacobian.Fill(0);
  m_Jacobian[0][0] = p[0];
  m_Jacobian[1][1] = p[1];
  m_Jacobian[2][2] = p[2];
  return m_Jacobian;

 }

} // namespace

#endif
