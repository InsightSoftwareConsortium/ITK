/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkEuclideanDistance.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkEuclideanDistance_h
#define __itkEuclideanDistance_h

#include "itkNumericTraits.h"
#include "itkFixedArray.h"
#include "itkDistanceMetric.h"

namespace itk{ 
namespace Statistics{

template< class TVector >
class ITK_EXPORT EuclideanDistance : 
    public DistanceMetric< TVector >
{
public:
  /** Standard "Self" typedef. */
  typedef EuclideanDistance Self;
  typedef DistanceMetric< TVector > Superclass;
  typedef SmartPointer< Self > Pointer ; 

  itkStaticConstMacro(VectorLength, unsigned int, TVector::Length); 

  /** Run-time type information (and related methods). */
  itkTypeMacro(EuclideanDistance, DistanceMetric);

  /** Method for creation through the object factory. */
  itkNewMacro(Self) ;

  typedef typename TVector::ValueType ValueType ;

  double Evaluate(const TVector &x) const ;
  double Evaluate(const TVector &x1, const TVector &x2) const ;
  double Evaluate(const ValueType &a, const ValueType &b) const ;
  bool IsWithinRange(const TVector &x, const double radius) const ;

protected:
  EuclideanDistance() {}
  virtual ~EuclideanDistance() {} 
  virtual void PrintSelf(std::ostream& os, Indent indent) const
  { Superclass::PrintSelf(os, indent) ; }

} ; // end of class

} // end of namespace Statistics 
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkEuclideanDistance.txx"
#endif

#endif







