/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkScaleLogarithmicTransform.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkScaleLogarithmicTransform_txx
#define _itkScaleLogarithmicTransform_txx

#include "itkScaleLogarithmicTransform.h"


namespace itk
{

// Constructor with default arguments
template<class ScalarType, unsigned int NDimensions>
ScaleLogarithmicTransform<ScalarType, NDimensions>::
ScaleLogarithmicTransform()
{
  return;
}
    


// Destructor
template<class ScalarType, unsigned int NDimensions>
ScaleLogarithmicTransform<ScalarType, NDimensions>::
~ScaleLogarithmicTransform()
{
  return;
}


// Set the parameters
template <class ScalarType, unsigned int NDimensions>
void
ScaleLogarithmicTransform<ScalarType, NDimensions>
::SetParameters( const ParametersType & parameters )
{
  ScaleType scales;
  for( unsigned int i=0; i<SpaceDimension; i++ )
    {
    scales[i] = exp( parameters[i] );
    }
  this->m_Parameters = parameters;
  this->SetScale( scales );

  // Modified is always called since we just have a pointer to the
  // parameters and cannot know if the parameters have changed.
  this->Modified();

}


// Get Parameters
template <class TScalarType,unsigned int NDimensions>
const typename ScaleLogarithmicTransform<TScalarType,NDimensions>::ParametersType &
ScaleLogarithmicTransform<TScalarType,NDimensions>
::GetParameters( void ) const
{
  itkDebugMacro( << "Getting parameters ");

  const ScaleType & scales = this->GetScale(); 
  // Transfer the translation part
  for(unsigned int i=0; i < SpaceDimension; i++) 
    {
    this->m_Parameters[i] = log( scales[i] );
    }

  itkDebugMacro(<<"After getting parameters " << this->m_Parameters );

  return this->m_Parameters;
}



// Print self
template<class ScalarType, unsigned int NDimensions>
void
ScaleLogarithmicTransform<ScalarType, NDimensions>::
PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}


// Compute the Jacobian of the transformation
// It follows the same order of Parameters vector 
template<class ScalarType, unsigned int NDimensions>
const typename ScaleLogarithmicTransform<ScalarType, NDimensions>::JacobianType &
ScaleLogarithmicTransform<ScalarType, NDimensions>
::GetJacobian( const InputPointType & p ) const
{
  const ScaleType & scales = this->GetScale(); 
  this->m_Jacobian.Fill(0);
  for(unsigned int dim=0; dim<SpaceDimension; dim++)
    {
    // the derivative with respect to Log(scale) = scale * derivative with respect to scale.  
    this->m_Jacobian(dim,dim) = scales[dim] * p[dim];
    }
  return this->m_Jacobian;
}


} // namespace

#endif
