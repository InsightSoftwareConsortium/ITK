/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkEuclideanDistance.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
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

/** \class EuclideanDistance
 * \brief Euclidean distance function.
 *
 * This class is derived from DistanceMetric class. In addition to the
 * two Evaluate methods in the base class, this class has a method
 * (Evaluate) to compute the coordinate distance between two vector
 * component (not vectors), and a method to tell if a measurement
 * vector is whithin the range (defined by a radius value) from the
 * origin (set by SetOrigin mehtod).
 */
template< class TVector >
class ITK_EXPORT EuclideanDistance : 
    public DistanceMetric< TVector >
{
public:
  /** Standard "Self" typedef. */
  typedef EuclideanDistance Self;
  typedef DistanceMetric< TVector > Superclass;
  typedef SmartPointer< Self > Pointer ; 
  typedef SmartPointer<const Self> ConstPointer;

  /** Length of the vector */
  itkStaticConstMacro(VectorLength, unsigned int, TVector::Length); 

  /** Run-time type information (and related methods). */
  itkTypeMacro(EuclideanDistance, DistanceMetric);

  /** Method for creation through the object factory. */
  itkNewMacro(Self) ;

  /** Type of the component of a vector */
  typedef typename TVector::ValueType ValueType ;

  /** Gets the distance between the origin and x */
  double Evaluate(const TVector &x) const ;

  /** Gets the distance between x1 and x2 */
  double Evaluate(const TVector &x1, const TVector &x2) const ;

  /** Gets the cooridnate distance between a and b. NOTE: a and b
   * should be type of component */ 
  double Evaluate(const ValueType &a, const ValueType &b) const ;

  /** Returns true if the distance between x and the origin is less
   * than radius */
  bool IsWithinRange(const TVector &x, const double radius) const ;

protected:
  EuclideanDistance() {}
  virtual ~EuclideanDistance() {} 
} ; // end of class

} // end of namespace Statistics 
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkEuclideanDistance.txx"
#endif

#endif







