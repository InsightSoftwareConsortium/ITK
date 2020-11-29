/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkStatisticsLabelObjectAccessors_h
#define itkStatisticsLabelObjectAccessors_h
#include "itkShapeLabelObjectAccessors.h"

/*
 *
 * This code was contributed in the Insight Journal paper:
 * "Label object representation and manipulation with ITK"
 * by Lehmann G.
 * https://www.insight-journal.org/browse/publication/176
 *
 */

namespace itk
{
namespace Functor
{
template <typename TLabelObject>
class MinimumLabelObjectAccessor
{
public:
  using LabelObjectType = TLabelObject;
  using AttributeValueType = double;

  inline AttributeValueType
  operator()(const LabelObjectType * labelObject) const
  {
    return labelObject->GetMinimum();
  }
};

template <typename TLabelObject>
class MaximumLabelObjectAccessor
{
public:
  using LabelObjectType = TLabelObject;
  using AttributeValueType = double;

  inline AttributeValueType
  operator()(const LabelObjectType * labelObject) const
  {
    return labelObject->GetMaximum();
  }
};

template <typename TLabelObject>
class MeanLabelObjectAccessor
{
public:
  using LabelObjectType = TLabelObject;
  using AttributeValueType = double;

  inline AttributeValueType
  operator()(const LabelObjectType * labelObject) const
  {
    return labelObject->GetMean();
  }
};

template <typename TLabelObject>
class SumLabelObjectAccessor
{
public:
  using LabelObjectType = TLabelObject;
  using AttributeValueType = double;

  inline AttributeValueType
  operator()(const LabelObjectType * labelObject) const
  {
    return labelObject->GetSum();
  }
};

template <typename TLabelObject>
class StandardDeviationLabelObjectAccessor
{
public:
  using LabelObjectType = TLabelObject;
  using AttributeValueType = double;

  inline AttributeValueType
  operator()(const LabelObjectType * labelObject) const
  {
    return labelObject->GetStandardDeviation();
  }
};

template <typename TLabelObject>
class VarianceLabelObjectAccessor
{
public:
  using LabelObjectType = TLabelObject;
  using AttributeValueType = double;

  inline AttributeValueType
  operator()(const LabelObjectType * labelObject) const
  {
    return labelObject->GetVariance();
  }
};

template <typename TLabelObject>
class MedianLabelObjectAccessor
{
public:
  using LabelObjectType = TLabelObject;
  using AttributeValueType = double;

  inline AttributeValueType
  operator()(const LabelObjectType * labelObject) const
  {
    return labelObject->GetMedian();
  }
};

template <typename TLabelObject>
class MaximumIndexLabelObjectAccessor
{
public:
  using LabelObjectType = TLabelObject;
  using AttributeValueType = typename LabelObjectType::IndexType;

  inline AttributeValueType
  operator()(const LabelObjectType * labelObject) const
  {
    return labelObject->GetMaximumIndex();
  }
};

template <typename TLabelObject>
class MinimumIndexLabelObjectAccessor
{
public:
  using LabelObjectType = TLabelObject;
  using AttributeValueType = typename LabelObjectType::IndexType;

  inline AttributeValueType
  operator()(const LabelObjectType * labelObject) const
  {
    return labelObject->GetMinimumIndex();
  }
};

template <typename TLabelObject>
class CenterOfGravityLabelObjectAccessor
{
public:
  using LabelObjectType = TLabelObject;
  using AttributeValueType = typename LabelObjectType::PointType;

  inline AttributeValueType
  operator()(const LabelObjectType * labelObject) const
  {
    return labelObject->GetCenterOfGravity();
  }
};

/*
template< typename TLabelObject >
class CentralMomentsLabelObjectAccessor
{
public:
  using LabelObjectType = TLabelObject;
  using AttributeValueType = typename LabelObjectType::MatrixType;

  inline AttributeValueType operator()( const LabelObjectType * labelObject ) const
    {
    return labelObject->GetCentralMoments();
    }
  };
*/

template <typename TLabelObject>
class WeightedPrincipalMomentsLabelObjectAccessor
{
public:
  using LabelObjectType = TLabelObject;
  using AttributeValueType = typename LabelObjectType::VectorType;

  inline AttributeValueType
  operator()(const LabelObjectType * labelObject) const
  {
    return labelObject->GetWeightedPrincipalMoments();
  }
};

template <typename TLabelObject>
class WeightedPrincipalAxesLabelObjectAccessor
{
public:
  using LabelObjectType = TLabelObject;
  using AttributeValueType = typename LabelObjectType::MatrixType;

  inline AttributeValueType
  operator()(const LabelObjectType * labelObject) const
  {
    return labelObject->GetWeightedPrincipalAxes();
  }
};

template <typename TLabelObject>
class KurtosisLabelObjectAccessor
{
public:
  using LabelObjectType = TLabelObject;
  using AttributeValueType = double;

  inline AttributeValueType
  operator()(const LabelObjectType * labelObject) const
  {
    return labelObject->GetKurtosis();
  }
};

template <typename TLabelObject>
class SkewnessLabelObjectAccessor
{
public:
  using LabelObjectType = TLabelObject;
  using AttributeValueType = double;

  inline AttributeValueType
  operator()(const LabelObjectType * labelObject) const
  {
    return labelObject->GetSkewness();
  }
};

template <typename TLabelObject>
class WeightedElongationLabelObjectAccessor
{
public:
  using LabelObjectType = TLabelObject;
  using AttributeValueType = double;

  inline AttributeValueType
  operator()(const LabelObjectType * labelObject) const
  {
    return labelObject->GetWeightedElongation();
  }
};

template <typename TLabelObject>
class HistogramLabelObjectAccessor
{
public:
  using LabelObjectType = TLabelObject;

  using AttributeValueType = typename LabelObjectType::HistogramType *;

  inline AttributeValueType
  operator()(const LabelObjectType * labelObject) const
  {
    return labelObject->GetHistogram();
  }
};

template <typename TLabelObject>
class WeightedFlatnessLabelObjectAccessor
{
public:
  using LabelObjectType = TLabelObject;
  using AttributeValueType = double;

  inline AttributeValueType
  operator()(const LabelObjectType * labelObject) const
  {
    return labelObject->GetWeightedFlatness();
  }
};
} // namespace Functor
} // end namespace itk

#endif
