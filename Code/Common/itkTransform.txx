/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTransform.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef _itkTransform_txx
#define _itkTransform_txx

#include "itkTransform.h"

namespace itk
{

/**
 * Constructor
 */
template < class TScalarType,
           unsigned int NInputDimensions,
           unsigned int NOutputDimensions,
           class TParameters,
           class TJacobianType>
Transform< TScalarType,NInputDimensions,NOutputDimensions,
           TParameters,TJacobianType>
::Transform()
{
 
}


// Compute the Jacobian of the transformation
// It follows the same order of Parameters vector 
template < class TScalarType,
           unsigned int NInputDimensions,
           unsigned int NOutputDimensions,
           class TParameters,
           class TJacobianType>
const Transform< TScalarType,NInputDimensions,NOutputDimensions,
           TParameters,TJacobianType>::JacobianType &
Transform< TScalarType,NInputDimensions,NOutputDimensions,
           TParameters,TJacobianType>
::GetJacobian( const InputPointType & p ) const
{
  
  // Each transform should redefine this method.
  // the following is just a default action
  return m_Jacobian;

}


} // end namespace itk


#endif

