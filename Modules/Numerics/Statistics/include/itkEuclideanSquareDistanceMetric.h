/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkEuclideanSquareDistanceMetric_h
#define itkEuclideanSquareDistanceMetric_h

#include "itkDistanceMetric.h"

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
 * \ingroup ITKStatistics
 */
template< typename TVector >
class ITK_TEMPLATE_EXPORT EuclideanSquareDistanceMetric:
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
  double Evaluate(const MeasurementVectorType & x) const ITK_OVERRIDE;

  /** Gets the distance between x1 and x2 */
  double Evaluate(const MeasurementVectorType & x1, const MeasurementVectorType & x2) const ITK_OVERRIDE;

protected:
  EuclideanSquareDistanceMetric() {}
  virtual ~EuclideanSquareDistanceMetric() ITK_OVERRIDE {}
};  // end of class
} // end of namespace Statistics
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkEuclideanSquareDistanceMetric.hxx"
#endif

#endif
