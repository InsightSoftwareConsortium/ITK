/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkScaleLogarithmicTransform.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
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
  m_Parameters = parameters;
  this->SetScale( scales );
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
    m_Parameters[i] = log( scales[i] );
    }

  itkDebugMacro(<<"After getting parameters " << m_Parameters );

  return m_Parameters;
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
  m_Jacobian.Fill(0);
  for(unsigned int dim=0; dim<SpaceDimension; dim++)
    {
    // the derivative with respect to Log(scale) = scale * derivative with respect to scale.  
    m_Jacobian(dim,dim) = scales[dim] * p[dim];
    }
  return m_Jacobian;
}


} // namespace

#endif
