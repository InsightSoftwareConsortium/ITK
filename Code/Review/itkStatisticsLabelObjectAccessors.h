/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkStatisticsLabelObjectAccessors.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkStatisticsLabelObjectAccessors_h
#define __itkStatisticsLabelObjectAccessors_h
#include "itkShapeLabelObjectAccessors.h"

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
class ITK_EXPORT SigmaLabelObjectAccessor
{
public:
  typedef TLabelObject LabelObjectType;
  typedef double       AttributeValueType;

  inline AttributeValueType operator()(const LabelObjectType *labelObject) const
  {
    return labelObject->GetSigma();
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
class ITK_EXPORT PrincipalMomentsLabelObjectAccessor
{
public:
  typedef TLabelObject                         LabelObjectType;
  typedef typename LabelObjectType::VectorType AttributeValueType;

  inline AttributeValueType operator()(const LabelObjectType *labelObject) const
  {
    return labelObject->GetPrincipalMoments();
  }
};

template< class TLabelObject >
class ITK_EXPORT PrincipalAxesLabelObjectAccessor
{
public:
  typedef TLabelObject                         LabelObjectType;
  typedef typename LabelObjectType::MatrixType AttributeValueType;

  inline AttributeValueType operator()(const LabelObjectType *labelObject) const
  {
    return labelObject->GetPrincipalAxes();
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
class ITK_EXPORT ElongationLabelObjectAccessor
{
public:
  typedef TLabelObject LabelObjectType;
  typedef double       AttributeValueType;

  inline AttributeValueType operator()(const LabelObjectType *labelObject) const
  {
    return labelObject->GetElongation();
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
class ITK_EXPORT FlatnessLabelObjectAccessor
{
public:
  typedef TLabelObject LabelObjectType;
  typedef double       AttributeValueType;

  inline AttributeValueType operator()(const LabelObjectType *labelObject) const
  {
    return labelObject->GetFlatness();
  }
};
}
} // end namespace itk

#endif
