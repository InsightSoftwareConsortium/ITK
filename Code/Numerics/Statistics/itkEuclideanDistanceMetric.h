/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkEuclideanDistanceMetric.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkEuclideanDistanceMetric_h
#define __itkEuclideanDistanceMetric_h

#include "itkNumericTraits.h"
#include "itkDistanceMetric.h"
#include "itkMeasurementVectorTraits.h"

namespace itk
{
namespace Statistics
{
/** \class EuclideanDistanceMetric
 * \brief Euclidean distance function.
 *
 *
 */
template< class TVector >
class ITK_EXPORT EuclideanDistanceMetric:
  public DistanceMetric< TVector >
{
public:
  /** Standard "Self" typedef. */
  typedef EuclideanDistanceMetric    Self;
  typedef DistanceMetric< TVector >  Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  typedef typename Superclass::MeasurementVectorType                                MeasurementVectorType;
  typedef typename MeasurementVectorTraitsTypes< MeasurementVectorType >::ValueType ValueType;
  typedef typename Superclass::MeasurementVectorSizeType                            MeasurementVectorSizeType;

  typedef typename Superclass::OriginType OriginType;

  /** Run-time type information (and related methods). */
  itkTypeMacro(EuclideanDistanceMetric, DistanceMetric);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Gets the distance between the origin and x */
  double Evaluate(const MeasurementVectorType & x) const;

  /** Gets the distance between x1 and x2 */
  double Evaluate(const MeasurementVectorType & x1, const MeasurementVectorType & x2) const;

  /** Gets the cooridnate distance between a and b. NOTE: a and b
   * should be type of component. This method is used by
    * KdTreeKMeans estimators. When the estimator is refactored,
    * this method should be removed. */
  double Evaluate(const ValueType & a, const ValueType & b) const;

protected:
  EuclideanDistanceMetric() {}
  virtual ~EuclideanDistanceMetric() {}
};  // end of class
} // end of namespace Statistics
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkEuclideanDistanceMetric.txx"
#endif

#endif
