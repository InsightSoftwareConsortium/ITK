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
           unsigned int NOutputDimensions >
Transform< TScalarType,NInputDimensions,NOutputDimensions>
::Transform():
  m_Parameters(1),
  m_Jacobian(NOutputDimensions,1)
{
}



/**
 * Constructor
 */
template < class TScalarType,
           unsigned int NInputDimensions,
           unsigned int NOutputDimensions >
Transform< TScalarType,NInputDimensions,NOutputDimensions>
::Transform(unsigned int dimension,unsigned int numberOfParameters):
  m_Parameters(numberOfParameters),
  m_Jacobian(dimension,numberOfParameters)
{
}



} // end namespace itk


#endif

