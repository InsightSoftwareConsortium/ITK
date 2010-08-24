/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkShapeLabelObjectAccessors.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkShapeLabelObjectAccessors_h
#define __itkShapeLabelObjectAccessors_h
#include "itkLabelObjectAccessors.h"

namespace itk
{
namespace Functor
{
template< class TLabelObject >
class ITK_EXPORT SizeLabelObjectAccessor
{
public:
  typedef TLabelObject  LabelObjectType;
  typedef unsigned long AttributeValueType;

  inline AttributeValueType operator()(const LabelObjectType *labelObject) const
  {
    return labelObject->GetSize();
  }
};

template< class TLabelObject >
class ITK_EXPORT RegionLabelObjectAccessor
{
public:
  typedef TLabelObject                         LabelObjectType;
  typedef typename LabelObjectType::RegionType AttributeValueType;

  inline AttributeValueType operator()(const LabelObjectType *labelObject) const
  {
    return labelObject->GetRegion();
  }
};

template< class TLabelObject >
class ITK_EXPORT PhysicalSizeLabelObjectAccessor
{
public:
  typedef TLabelObject LabelObjectType;
  typedef double       AttributeValueType;

  inline AttributeValueType operator()(const LabelObjectType *labelObject) const
  {
    return labelObject->GetPhysicalSize();
  }
};

template< class TLabelObject >
class ITK_EXPORT RegionElongationLabelObjectAccessor
{
public:
  typedef TLabelObject LabelObjectType;
  typedef double       AttributeValueType;

  inline AttributeValueType operator()(const LabelObjectType *labelObject) const
  {
    return labelObject->GetRegionElongation();
  }
};

template< class TLabelObject >
class ITK_EXPORT SizeRegionRatioLabelObjectAccessor
{
public:
  typedef TLabelObject LabelObjectType;
  typedef double       AttributeValueType;

  inline AttributeValueType operator()(const LabelObjectType *labelObject) const
  {
    return labelObject->GetSizeRegionRatio();
  }
};

template< class TLabelObject >
class ITK_EXPORT SizeOnBorderLabelObjectAccessor
{
public:
  typedef TLabelObject  LabelObjectType;
  typedef unsigned long AttributeValueType;

  inline AttributeValueType operator()(const LabelObjectType *labelObject) const
  {
    return labelObject->GetSizeOnBorder();
  }
};

template< class TLabelObject >
class ITK_EXPORT PhysicalSizeOnBorderLabelObjectAccessor
{
public:
  typedef TLabelObject LabelObjectType;
  typedef double       AttributeValueType;

  inline AttributeValueType operator()(const LabelObjectType *labelObject) const
  {
    return labelObject->GetPhysicalSizeOnBorder();
  }
};

template< class TLabelObject >
class ITK_EXPORT CentroidLabelObjectAccessor
{
public:
  typedef TLabelObject                           LabelObjectType;
  typedef typename LabelObjectType::CentroidType AttributeValueType;

  inline AttributeValueType operator()(const LabelObjectType *labelObject) const
  {
    return labelObject->GetCentroid();
  }
};

template< class TLabelObject >
class ITK_EXPORT FeretDiameterLabelObjectAccessor
{
public:
  typedef TLabelObject LabelObjectType;
  typedef double       AttributeValueType;

  inline AttributeValueType operator()(const LabelObjectType *labelObject) const
  {
    return labelObject->GetFeretDiameter();
  }
};

template< class TLabelObject >
class ITK_EXPORT BinaryPrincipalMomentsLabelObjectAccessor
{
public:
  typedef TLabelObject                         LabelObjectType;
  typedef typename LabelObjectType::VectorType AttributeValueType;

  inline AttributeValueType operator()(const LabelObjectType *labelObject) const
  {
    return labelObject->GetBinaryPrincipalMoments();
  }
};

template< class TLabelObject >
class ITK_EXPORT BinaryPrincipalAxesLabelObjectAccessor
{
public:
  typedef TLabelObject                         LabelObjectType;
  typedef typename LabelObjectType::MatrixType AttributeValueType;

  inline AttributeValueType operator()(const LabelObjectType *labelObject) const
  {
    return labelObject->GetBinaryPrincipalAxes();
  }
};

template< class TLabelObject >
class ITK_EXPORT BinaryElongationLabelObjectAccessor
{
public:
  typedef TLabelObject LabelObjectType;
  typedef double       AttributeValueType;

  inline AttributeValueType operator()(const LabelObjectType *labelObject) const
  {
    return labelObject->GetBinaryElongation();
  }
};

template< class TLabelObject >
class ITK_EXPORT PerimeterLabelObjectAccessor
{
public:
  typedef TLabelObject LabelObjectType;
  typedef double       AttributeValueType;

  inline AttributeValueType operator()(const LabelObjectType *labelObject) const
  {
    return labelObject->GetPerimeter();
  }
};

template< class TLabelObject >
class ITK_EXPORT RoundnessLabelObjectAccessor
{
public:
  typedef TLabelObject LabelObjectType;
  typedef double       AttributeValueType;

  inline AttributeValueType operator()(const LabelObjectType *labelObject) const
  {
    return labelObject->GetRoundness();
  }
};

template< class TLabelObject >
class ITK_EXPORT EquivalentRadiusLabelObjectAccessor
{
public:
  typedef TLabelObject LabelObjectType;
  typedef double       AttributeValueType;

  inline AttributeValueType operator()(const LabelObjectType *labelObject) const
  {
    return labelObject->GetEquivalentRadius();
  }
};

template< class TLabelObject >
class ITK_EXPORT EquivalentPerimeterLabelObjectAccessor
{
public:
  typedef TLabelObject LabelObjectType;
  typedef double       AttributeValueType;

  inline AttributeValueType operator()(const LabelObjectType *labelObject) const
  {
    return labelObject->GetEquivalentPerimeter();
  }
};

template< class TLabelObject >
class ITK_EXPORT EquivalentEllipsoidSizeLabelObjectAccessor
{
public:
  typedef TLabelObject                         LabelObjectType;
  typedef typename LabelObjectType::VectorType AttributeValueType;

  inline AttributeValueType operator()(const LabelObjectType *labelObject) const
  {
    return labelObject->GetEquivalentEllipsoidSize();
  }
};

template< class TLabelObject >
class ITK_EXPORT BinaryFlatnessLabelObjectAccessor
{
public:
  typedef TLabelObject LabelObjectType;
  typedef double       AttributeValueType;

  inline AttributeValueType operator()(const LabelObjectType *labelObject) const
  {
    return labelObject->GetBinaryFlatness();
  }
};
}
} // end namespace itk

#endif
