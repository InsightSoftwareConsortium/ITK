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
#ifndef itkImageMaskSpatialObject_h
#define itkImageMaskSpatialObject_h

#include "itkImageSpatialObject.h"
#include "itkImageSliceConstIteratorWithIndex.h"

namespace itk
{
/** \class ImageMaskSpatialObject
 * \brief Implementation of an image mask as spatial object.
 *
 * This class derives from the ImageSpatialObject and overloads the IsInsideInWorldSpace()
 * method.  One of the common uses of this class is to serve as Mask for the
 * Image Registration Metrics.
 *
 * \sa ImageSpatialObject SpatialObject CompositeSpatialObject
 * \ingroup ITKSpatialObjects
 */

template< unsigned int TDimension = 3 >
class ITK_TEMPLATE_EXPORT ImageMaskSpatialObject:
  public ImageSpatialObject< TDimension, unsigned char >
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(ImageMaskSpatialObject);

  using Self = ImageMaskSpatialObject< TDimension >;
  using Superclass = ImageSpatialObject< TDimension >;
  using Pointer = SmartPointer< Self >;
  using ConstPointer = SmartPointer< const Self >;

  using ScalarType = typename Superclass::ScalarType;
  using PixelType = typename Superclass::PixelType;
  using ImageType = typename Superclass::ImageType;
  using ImagePointer = typename Superclass::ImagePointer;
  using IndexType = typename Superclass::IndexType;
  using RegionType = typename Superclass::RegionType;
  using SizeType = typename Superclass::SizeType;
  using TransformType = typename Superclass::TransformType;
  using PointType = typename Superclass::PointType;
  using BoundingBoxType = typename Superclass::BoundingBoxType;
  using InterpolatorType = typename Superclass::InterpolatorType;

  using SliceIteratorType = itk::ImageSliceConstIteratorWithIndex<ImageType>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageMaskSpatialObject, ImageSpatialObject);

  /** Returns true if the point is inside, false otherwise. */
  bool IsInsideInWorldSpace(const PointType & point, unsigned int depth=0,
    const std::string & name="") const override;

  /** Get the boundaries of a specific object.  This function needs to
   *  be called every time one of the object's components is
   *  changed. */
  bool ComputeMyBoundingBoxInWorldSpace() const override;

protected:
  ImageMaskSpatialObject();
  ~ImageMaskSpatialObject() override = default;

  void PrintSelf(std::ostream & os, Indent indent) const override;
};
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageMaskSpatialObject.hxx"
#endif

#endif //itkImageMaskSpatialObject_h
