/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkManhattanDistanceMetric_hxx
#define itkManhattanDistanceMetric_hxx

namespace itk::Statistics
{
template <typename TVector>
inline double
ManhattanDistanceMetric<TVector>::Evaluate(const MeasurementVectorType & x) const
{
  const MeasurementVectorSizeType measurementVectorSize = this->GetMeasurementVectorSize();

  if (measurementVectorSize == 0)
  {
    itkExceptionMacro("Please set the MeasurementVectorSize first");
  }
  MeasurementVectorTraits::Assert(this->GetOrigin(),
                                  measurementVectorSize,
                                  "ManhattanDistanceMetric::Evaluate Origin and input vector have different lengths");

  double distance = 0.0;
  for (unsigned int i = 0; i < measurementVectorSize; ++i)
  {
    const double temp = itk::Math::abs(this->GetOrigin()[i] - x[i]);
    distance += temp;
  }
  return distance;
}

template <typename TVector>
inline double
ManhattanDistanceMetric<TVector>::Evaluate(const MeasurementVectorType & x1, const MeasurementVectorType & x2) const
{
  const MeasurementVectorSizeType measurementVectorSize = NumericTraits<MeasurementVectorType>::GetLength(x1);

  if (measurementVectorSize != NumericTraits<MeasurementVectorType>::GetLength(x2))
  {
    itkExceptionMacro("ManhattanDistanceMetric:: The two measurement vectors have unequal size");
  }

  double distance = 0.0;
  for (unsigned int i = 0; i < measurementVectorSize; ++i)
  {
    const double temp = itk::Math::abs(x1[i] - x2[i]);
    distance += temp;
  }
  return distance;
}
} // namespace itk::Statistics

#endif
