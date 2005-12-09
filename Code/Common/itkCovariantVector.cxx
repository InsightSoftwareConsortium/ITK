/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCovariantVector.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkVector.h" 
#include "itkCovariantVector.h" 
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


void
ITKCommon_EXPORT CrossProduct( CovariantVector<double,3> & c, const Vector<double,3> & a, const Vector<double,3> & b ) 
{
  c[0] = a[1] * b[2] - a[2] * b[1];
  c[1] = a[2] * b[0] - a[0] * b[2];
  c[2] = a[0] * b[1] - a[1] * b[0];
}


void
ITKCommon_EXPORT CrossProduct( CovariantVector<float,3> & c, const Vector<float,3> & a, const Vector<float,3> & b ) 
{
  c[0] = a[1] * b[2] - a[2] * b[1];
  c[1] = a[2] * b[0] - a[0] * b[2];
  c[2] = a[0] * b[1] - a[1] * b[0];
}


void
ITKCommon_EXPORT CrossProduct( CovariantVector<int,3> & c, const Vector<int,3> & a, const Vector<int,3> & b ) 
{
  c[0] = a[1] * b[2] - a[2] * b[1];
  c[1] = a[2] * b[0] - a[0] * b[2];
  c[2] = a[0] * b[1] - a[1] * b[0];
}





} // end namespace itk


