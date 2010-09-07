/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkEuclideanSquareDistanceMetric.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkEuclideanSquareDistanceMetric_h
#define __itkEuclideanSquareDistanceMetric_h

#include "itkNumericTraits.h"
#include "itkDistanceMetric.h"
#include "itkMeasurementVectorTraits.h"

namespace itk
{
namespace Statistics
{
/** \class EuclideanSquareDistanceMetric
 * \brief Computes Euclidean distance between origin and given measurement vector.
 *
 * \sa DistanceMetric
 * \sa EuclideanDistanceMetric
 * \sa ManhattanDistanceMetric
 */
template< class TVector >
class ITK_EXPORT EuclideanSquareDistanceMetric:
  public DistanceMetric< TVector >
{
public:
  /** Standard "Self" typedef. */
  typedef EuclideanSquareDistanceMetric Self;
  typedef DistanceMetric< TVector >     Superclass;
  typedef SmartPointer< Self >          Pointer;
  typedef SmartPointer< const Self >    ConstPointer;

  typedef typename Superclass::MeasurementVectorSizeType MeasurementVectorSizeType;
  typedef typename Superclass::MeasurementVectorType     MeasurementVectorType;

  /** Run-time type information (and related methods). */
  itkTypeMacro(EuclideanSquareDistanceMetric, DistanceMetric);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Gets the distance between the origin and x */
  double Evaluate(const MeasurementVectorType & x) const;

  /** Gets the distance between x1 and x2 */
  double Evaluate(const MeasurementVectorType & x1, const MeasurementVectorType & x2) const;

protected:
  EuclideanSquareDistanceMetric() {}
  virtual ~EuclideanSquareDistanceMetric() {}
};  // end of class
} // end of namespace Statistics
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkEuclideanSquareDistanceMetric.txx"
#endif

#endif
