/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkArray2D.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkArray2D_txx
#define _itkArray2D_txx

#include "itkArray2D.h"

namespace itk
{


/**
 * Default constructor 
 */
template < typename TValueType >
Array2D<TValueType >
::Array2D():vnl_matrix<TValueType>()
{
}


/**
 * Constructor with number of rows and columns as arguments 
 */
template < typename TValueType >
Array2D<TValueType >
::Array2D(unsigned int rows,unsigned int cols):vnl_matrix<TValueType>(rows,cols)
{
}




} // namespace itk

#endif
