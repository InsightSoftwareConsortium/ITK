/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkStatDistance.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkStatDistance_h
#define __itkStatDistance_h

#include <cmath>

namespace itk 
{
  template<class Vector>
  double
  EuclideanDistance(Vector* A, Vector* B) ;
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkStatDistance.txx"
#endif

#endif
