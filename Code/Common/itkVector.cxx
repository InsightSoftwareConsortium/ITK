/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVector.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkVector.h" 
#include "itkNumericTraits.h" 

namespace itk
{


/**
 *  Methods specialized by dimension
 *
 *  Limitations of VC++ on partial specialization 
 *  force us to define the following method for
 *  a set of types.
 */


Vector<double,3>
ITK_EXPORT CrossProduct( const Vector<double,3> & a, const Vector<double,3> & b ) 
{
  Vector<double,3> c;
  c[0] = a[1] * b[2] - a[2] * b[1];
  c[1] = a[2] * b[0] - a[0] * b[2];
  c[2] = a[0] * b[1] - a[1] * b[0];
  return c;  
}


Vector<float,3>
ITK_EXPORT CrossProduct( const Vector<float,3> & a, const Vector<float,3> & b ) 
{
  Vector<float,3> c;
  c[0] = a[1] * b[2] - a[2] * b[1];
  c[1] = a[2] * b[0] - a[0] * b[2];
  c[2] = a[0] * b[1] - a[1] * b[0];
  return c;  
}


Vector<int,3>
ITK_EXPORT CrossProduct( const Vector<int,3> & a, const Vector<int,3> & b ) 
{
  Vector<int,3> c;
  c[0] = a[1] * b[2] - a[2] * b[1];
  c[1] = a[2] * b[0] - a[0] * b[2];
  c[2] = a[0] * b[1] - a[1] * b[0];
  return c;  
}





} // end namespace itk


