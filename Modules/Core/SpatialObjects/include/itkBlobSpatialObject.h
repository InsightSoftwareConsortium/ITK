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
#ifndef itkBlobSpatialObject_h
#define itkBlobSpatialObject_h

#include <list>

#include "itkPointBasedSpatialObject.h"

namespace itk
{

/**
 * \class BlobSpatialObject
 *
 * \brief Spatial object representing a potentially amorphous object.
 *
 * The BlobSpatialObject is a discretized representation of a "blob",
 * which can be taken to be an arbitrary, possibly amorphous shape.
 * The representation is a list of the points (voxel centers) contained
 * in the object.  This can be thought of as an alternate way to
 * represent a binary image.
 *
 * \sa SpatialObjectPoint
 *
 * \ingroup ITKSpatialObjects
 *
 * \sphinx
 * \sphinxexample{Core/SpatialObjects/Blob,Blob}
 * \endsphinx
 */
template <unsigned int TDimension = 3>
class ITK_TEMPLATE_EXPORT BlobSpatialObject : public PointBasedSpatialObject<TDimension, SpatialObjectPoint<TDimension>>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(BlobSpatialObject);

  using Self = BlobSpatialObject;
  using Superclass = PointBasedSpatialObject<TDimension, SpatialObjectPoint<TDimension>>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  using ScalarType = double;

  using BlobPointType = SpatialObjectPoint<TDimension>;
  using BlobPointListType = std::vector<BlobPointType>;

  using PointType = typename Superclass::PointType;
  using SpatialObjectPointType = typename Superclass::SpatialObjectPointType;
  using TransformType = typename Superclass::TransformType;
  using BoundingBoxType = typename Superclass::BoundingBoxType;
  using PointContainerType = VectorContainer<IdentifierType, PointType>;
  using PointContainerPointer = SmartPointer<PointContainerType>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Method for creation through the object factory. */
  itkTypeMacro(BlobSpatialObject, SpatialObject);

protected:
  BlobSpatialObject();
  ~BlobSpatialObject() override = default;

  /** Method to print the object. */
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  typename LightObject::Pointer
  InternalClone() const override;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkBlobSpatialObject.hxx"
#endif

#endif // itkBlobSpatialObject_h
