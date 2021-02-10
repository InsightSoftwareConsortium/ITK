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
#ifndef itkShapeLabelObjectAccessors_h
#define itkShapeLabelObjectAccessors_h

#include "itkLabelObjectAccessors.h"
#include "itkIntTypes.h"

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
class NumberOfPixelsLabelObjectAccessor
{
public:
  using LabelObjectType = TLabelObject;
  using AttributeValueType = SizeValueType;

  inline AttributeValueType
  operator()(const LabelObjectType * labelObject) const
  {
    return labelObject->GetNumberOfPixels();
  }
};

template <typename TLabelObject>
class BoundingBoxLabelObjectAccessor
{
public:
  using LabelObjectType = TLabelObject;
  using AttributeValueType = typename LabelObjectType::RegionType;

  inline AttributeValueType
  operator()(const LabelObjectType * labelObject) const
  {
    return labelObject->GetBoundingBox();
  }
};

template <typename TLabelObject>
class PhysicalSizeLabelObjectAccessor
{
public:
  using LabelObjectType = TLabelObject;
  using AttributeValueType = double;

  inline AttributeValueType
  operator()(const LabelObjectType * labelObject) const
  {
    return labelObject->GetPhysicalSize();
  }
};

template <typename TLabelObject>
class NumberOfPixelsOnBorderLabelObjectAccessor
{
public:
  using LabelObjectType = TLabelObject;
  using AttributeValueType = SizeValueType;

  inline AttributeValueType
  operator()(const LabelObjectType * labelObject) const
  {
    return labelObject->GetNumberOfPixelsOnBorder();
  }
};

template <typename TLabelObject>
class PerimeterOnBorderLabelObjectAccessor
{
public:
  using LabelObjectType = TLabelObject;
  using AttributeValueType = double;

  inline AttributeValueType
  operator()(const LabelObjectType * labelObject) const
  {
    return labelObject->GetPerimeterOnBorder();
  }
};

template <typename TLabelObject>
class CentroidLabelObjectAccessor
{
public:
  using LabelObjectType = TLabelObject;
  using AttributeValueType = typename LabelObjectType::CentroidType;

  inline AttributeValueType
  operator()(const LabelObjectType * labelObject) const
  {
    return labelObject->GetCentroid();
  }
};

template <typename TLabelObject>
class FeretDiameterLabelObjectAccessor
{
public:
  using LabelObjectType = TLabelObject;
  using AttributeValueType = double;

  inline AttributeValueType
  operator()(const LabelObjectType * labelObject) const
  {
    return labelObject->GetFeretDiameter();
  }
};

template <typename TLabelObject>
class PrincipalMomentsLabelObjectAccessor
{
public:
  using LabelObjectType = TLabelObject;
  using AttributeValueType = typename LabelObjectType::VectorType;

  inline AttributeValueType
  operator()(const LabelObjectType * labelObject) const
  {
    return labelObject->GetPrincipalMoments();
  }
};

template <typename TLabelObject>
class PrincipalAxesLabelObjectAccessor
{
public:
  using LabelObjectType = TLabelObject;
  using AttributeValueType = typename LabelObjectType::MatrixType;

  inline AttributeValueType
  operator()(const LabelObjectType * labelObject) const
  {
    return labelObject->GetPrincipalAxes();
  }
};

template <typename TLabelObject>
class ElongationLabelObjectAccessor
{
public:
  using LabelObjectType = TLabelObject;
  using AttributeValueType = double;

  inline AttributeValueType
  operator()(const LabelObjectType * labelObject) const
  {
    return labelObject->GetElongation();
  }
};

template <typename TLabelObject>
class PerimeterLabelObjectAccessor
{
public:
  using LabelObjectType = TLabelObject;
  using AttributeValueType = double;

  inline AttributeValueType
  operator()(const LabelObjectType * labelObject) const
  {
    return labelObject->GetPerimeter();
  }
};

template <typename TLabelObject>
class RoundnessLabelObjectAccessor
{
public:
  using LabelObjectType = TLabelObject;
  using AttributeValueType = double;

  inline AttributeValueType
  operator()(const LabelObjectType * labelObject) const
  {
    return labelObject->GetRoundness();
  }
};

template <typename TLabelObject>
class EquivalentSphericalRadiusLabelObjectAccessor
{
public:
  using LabelObjectType = TLabelObject;
  using AttributeValueType = double;

  inline AttributeValueType
  operator()(const LabelObjectType * labelObject) const
  {
    return labelObject->GetEquivalentSphericalRadius();
  }
};

template <typename TLabelObject>
class EquivalentSphericalPerimeterLabelObjectAccessor
{
public:
  using LabelObjectType = TLabelObject;
  using AttributeValueType = double;

  inline AttributeValueType
  operator()(const LabelObjectType * labelObject) const
  {
    return labelObject->GetEquivalentSphericalPerimeter();
  }
};

template <typename TLabelObject>
class EquivalentEllipsoidDiameterLabelObjectAccessor
{
public:
  using LabelObjectType = TLabelObject;
  using AttributeValueType = typename LabelObjectType::VectorType;

  inline AttributeValueType
  operator()(const LabelObjectType * labelObject) const
  {
    return labelObject->GetEquivalentEllipsoidDiameter();
  }
};

template <typename TLabelObject>
class FlatnessLabelObjectAccessor
{
public:
  using LabelObjectType = TLabelObject;
  using AttributeValueType = double;

  inline AttributeValueType
  operator()(const LabelObjectType * labelObject) const
  {
    return labelObject->GetFlatness();
  }
};

template <typename TLabelObject>
class PerimeterOnBorderRatioLabelObjectAccessor
{
public:
  using LabelObjectType = TLabelObject;
  using AttributeValueType = double;

  inline AttributeValueType
  operator()(const LabelObjectType * labelObject) const
  {
    return labelObject->GetPerimeterOnBorderRatio();
  }
};

} // namespace Functor
} // end namespace itk

#endif
