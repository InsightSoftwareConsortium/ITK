/*=========================================================================
  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkStatDistanceFunctions.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkStatDistanceFunctions_txx
#define __itkStatDistanceFunctions_txx

#include "itkStatDistanceFunctions.h"

namespace itk 
{
  template<class Vector>
  double
  EuclideanDistance(Vector* A, Vector* B)
  {
    double sumOfPartialDistance = 0;
    Vector::iterator iterA = A->begin() ;
    Vector::iterator iterB = B->begin() ;
    double temp ;
    while ( iterA != A->end() )
      {
        temp = (double) *iterA - (double) *iterB ;
        sumOfPartialDistance += temp * temp ;
        iterA++ ;
        iterB++ ;
        }
    return sqrt(sumOfPartialDistance) ;
  }
} // end of namespace itk

#endif
