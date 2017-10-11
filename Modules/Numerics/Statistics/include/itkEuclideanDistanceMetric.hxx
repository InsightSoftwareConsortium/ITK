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
#ifndef itkEuclideanDistanceMetric_hxx
#define itkEuclideanDistanceMetric_hxx

#include "itkEuclideanDistanceMetric.h"

namespace itk
{
namespace Statistics
{
template< typename TVector >
inline double
EuclideanDistanceMetric< TVector >
::Evaluate(const MeasurementVectorType & x) const
{
  MeasurementVectorSizeType
    measurementVectorSize = this->GetMeasurementVectorSize();

  if ( measurementVectorSize == 0 )
    {
    itkExceptionMacro(<< "Please set the MeasurementVectorSize first");
    }
  MeasurementVectorTraits::Assert(this->GetOrigin(), measurementVectorSize,
                                  "EuclideanDistanceMetric::Evaluate Origin and input vector have different lengths");

  double sumOfSquares = NumericTraits< double >::ZeroValue();

  for ( unsigned int i = 0; i < measurementVectorSize; ++i )
    {
    const double temp = this->GetOrigin()[i] - x[i];
    sumOfSquares += temp * temp;
    }

  const double distance = std::sqrt(sumOfSquares);

  return distance;
}

template< typename TVector >
inline double
EuclideanDistanceMetric< TVector >
::Evaluate(const MeasurementVectorType & x1, const MeasurementVectorType & x2) const
{
  MeasurementVectorSizeType measurementVectorSize = NumericTraits<MeasurementVectorType>::GetLength(x1);

  if ( measurementVectorSize != NumericTraits<MeasurementVectorType>::GetLength(x2) )
    {
    itkExceptionMacro(<< "The two measurement vectors have unequal size ("
                      << NumericTraits<MeasurementVectorType>::GetLength(x1) << " and "
                      << NumericTraits<MeasurementVectorType>::GetLength(x2) << ")");
    }

  double sumOfSquares = NumericTraits< double >::ZeroValue();

  for ( unsigned int i = 0; i < measurementVectorSize; ++i )
    {
    const double temp = x1[i] - x2[i];
    sumOfSquares += temp * temp;
    }

  const double distance = std::sqrt(sumOfSquares);

  return distance;
}

template< typename TVector >
inline double
EuclideanDistanceMetric< TVector >
::Evaluate(const ValueType & a, const ValueType & b) const
{
  const double temp = a - b;

  return std::abs(temp);
}
} // end of namespace Statistics
} // end of namespace itk

#endif
