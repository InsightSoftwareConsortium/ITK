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
#ifndef __itkStatisticsLabelObjectAccessors_h
#define __itkStatisticsLabelObjectAccessors_h
#include "itkShapeLabelObjectAccessors.h"

/*
 *
 * This code was contributed in the Insight Journal paper:
 * "Label object representation and manipulation with ITK"
 * by Lehmann G.
 * http://hdl.handle.net/1926/584
 * http://www.insight-journal.org/browse/publication/176
 *
 */

namespace itk
{
namespace Functor
{
template< class TLabelObject >
class ITK_EXPORT MinimumLabelObjectAccessor
{
public:
  typedef TLabelObject LabelObjectType;
  typedef double       AttributeValueType;

  inline AttributeValueType operator()(const LabelObjectType *labelObject) const
  {
    return labelObject->GetMinimum();
  }
};

template< class TLabelObject >
class ITK_EXPORT MaximumLabelObjectAccessor
{
public:
  typedef TLabelObject LabelObjectType;
  typedef double       AttributeValueType;

  inline AttributeValueType operator()(const LabelObjectType *labelObject) const
  {
    return labelObject->GetMaximum();
  }
};

template< class TLabelObject >
class ITK_EXPORT MeanLabelObjectAccessor
{
public:
  typedef TLabelObject LabelObjectType;
  typedef double       AttributeValueType;

  inline AttributeValueType operator()(const LabelObjectType *labelObject) const
  {
    return labelObject->GetMean();
  }
};

template< class TLabelObject >
class ITK_EXPORT SumLabelObjectAccessor
{
public:
  typedef TLabelObject LabelObjectType;
  typedef double       AttributeValueType;

  inline AttributeValueType operator()(const LabelObjectType *labelObject) const
  {
    return labelObject->GetSum();
  }
};

template< class TLabelObject >
class ITK_EXPORT StandardDeviationLabelObjectAccessor
{
public:
  typedef TLabelObject LabelObjectType;
  typedef double       AttributeValueType;

  inline AttributeValueType operator()(const LabelObjectType *labelObject) const
  {
    return labelObject->GetStandardDeviation();
  }
};

template< class TLabelObject >
class ITK_EXPORT VarianceLabelObjectAccessor
{
public:
  typedef TLabelObject LabelObjectType;
  typedef double       AttributeValueType;

  inline AttributeValueType operator()(const LabelObjectType *labelObject) const
  {
    return labelObject->GetVariance();
  }
};

template< class TLabelObject >
class ITK_EXPORT MedianLabelObjectAccessor
{
public:
  typedef TLabelObject LabelObjectType;
  typedef double       AttributeValueType;

  inline AttributeValueType operator()(const LabelObjectType *labelObject) const
  {
    return labelObject->GetMedian();
  }
};

template< class TLabelObject >
class ITK_EXPORT MaximumIndexLabelObjectAccessor
{
public:
  typedef TLabelObject                        LabelObjectType;
  typedef typename LabelObjectType::IndexType AttributeValueType;

  inline AttributeValueType operator()(const LabelObjectType *labelObject) const
  {
    return labelObject->GetMaximumIndex();
  }
};

template< class TLabelObject >
class ITK_EXPORT MinimumIndexLabelObjectAccessor
{
public:
  typedef TLabelObject                        LabelObjectType;
  typedef typename LabelObjectType::IndexType AttributeValueType;

  inline AttributeValueType operator()(const LabelObjectType *labelObject) const
  {
    return labelObject->GetMinimumIndex();
  }
};

template< class TLabelObject >
class ITK_EXPORT CenterOfGravityLabelObjectAccessor
{
public:
  typedef TLabelObject                        LabelObjectType;
  typedef typename LabelObjectType::PointType AttributeValueType;

  inline AttributeValueType operator()(const LabelObjectType *labelObject) const
  {
    return labelObject->GetCenterOfGravity();
  }
};

/*
template< class TLabelObject >
class ITK_EXPORT CentralMomentsLabelObjectAccessor
{
public:
  typedef TLabelObject LabelObjectType;
  typedef typename LabelObjectType::MatrixType AttributeValueType;

  inline AttributeValueType operator()( const LabelObjectType * labelObject ) const
    {
    return labelObject->GetCentralMoments();
    }
  };
*/

template< class TLabelObject >
class ITK_EXPORT WeightedPrincipalMomentsLabelObjectAccessor
{
public:
  typedef TLabelObject                         LabelObjectType;
  typedef typename LabelObjectType::VectorType AttributeValueType;

  inline AttributeValueType operator()(const LabelObjectType *labelObject) const
  {
    return labelObject->GetWeightedPrincipalMoments();
  }
};

template< class TLabelObject >
class ITK_EXPORT WeightedPrincipalAxesLabelObjectAccessor
{
public:
  typedef TLabelObject                         LabelObjectType;
  typedef typename LabelObjectType::MatrixType AttributeValueType;

  inline AttributeValueType operator()(const LabelObjectType *labelObject) const
  {
    return labelObject->GetWeightedPrincipalAxes();
  }
};

template< class TLabelObject >
class ITK_EXPORT KurtosisLabelObjectAccessor
{
public:
  typedef TLabelObject LabelObjectType;
  typedef double       AttributeValueType;

  inline AttributeValueType operator()(const LabelObjectType *labelObject) const
  {
    return labelObject->GetKurtosis();
  }
};

template< class TLabelObject >
class ITK_EXPORT SkewnessLabelObjectAccessor
{
public:
  typedef TLabelObject LabelObjectType;
  typedef double       AttributeValueType;

  inline AttributeValueType operator()(const LabelObjectType *labelObject) const
  {
    return labelObject->GetSkewness();
  }
};

template< class TLabelObject >
class ITK_EXPORT WeightedElongationLabelObjectAccessor
{
public:
  typedef TLabelObject LabelObjectType;
  typedef double       AttributeValueType;

  inline AttributeValueType operator()(const LabelObjectType *labelObject) const
  {
    return labelObject->GetWeightedElongation();
  }
};

template< class TLabelObject >
class ITK_EXPORT HistogramLabelObjectAccessor
{
public:
  typedef TLabelObject LabelObjectType;

  typedef typename LabelObjectType::HistogramType *AttributeValueType;

  inline AttributeValueType operator()(const LabelObjectType *labelObject) const
  {
    return labelObject->GetHistogram();
  }
};

template< class TLabelObject >
class ITK_EXPORT WeightedFlatnessLabelObjectAccessor
{
public:
  typedef TLabelObject LabelObjectType;
  typedef double       AttributeValueType;

  inline AttributeValueType operator()(const LabelObjectType *labelObject) const
  {
    return labelObject->GetWeightedFlatness();
  }
};
}
} // end namespace itk

#endif
