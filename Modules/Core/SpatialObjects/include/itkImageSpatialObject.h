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
#ifndef itkImageSpatialObject_h
#define itkImageSpatialObject_h

#include "itkImage.h"
#include "itkContinuousIndex.h"
#include "itkSpatialObject.h"
#include "itkNearestNeighborInterpolateImageFunction.h"

namespace itk
{
/** \class ImageSpatialObject
 * \brief Implementation of an image as spatial object.
 *
 * This class combines functionnalities from a spatial object,
 * and an image.
 *
 * \sa SpatialObject CompositeSpatialObject
 * \ingroup ITKSpatialObjects
 */

template< unsigned int TDimension = 3,
          typename TPixelType = unsigned char
          >
class ITK_TEMPLATE_EXPORT ImageSpatialObject:
  public SpatialObject< TDimension >
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(ImageSpatialObject);

  using ScalarType = double;
  using Self = ImageSpatialObject< TDimension, TPixelType >;
  using Superclass = SpatialObject< TDimension >;
  using Pointer = SmartPointer< Self >;
  using ConstPointer = SmartPointer< const Self >;

  using ContinuousIndexType = ContinuousIndex< double, TDimension >;
  using PixelType = TPixelType;
  using ImageType = Image< PixelType, TDimension >;
  using ImagePointer = typename ImageType::ConstPointer;
  using IndexType = typename ImageType::IndexType;
  using RegionType = typename ImageType::RegionType;
  using TransformType = typename Superclass::TransformType;
  using PointType = typename Superclass::PointType;
  using BoundingBoxType = typename Superclass::BoundingBoxType;
  using InterpolatorType = InterpolateImageFunction< ImageType >;

  using NNInterpolatorType = NearestNeighborInterpolateImageFunction<ImageType>;

  using PointContainerType = VectorContainer< IdentifierType, PointType >;
  using PointContainerPointer = typename PointContainerType::Pointer;

  static constexpr unsigned int ObjectDimension = TDimension;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageSpatialObject, SpatialObject);

  /** Set the image. */
  void SetImage(const ImageType *image);

  /** Get a pointer to the image currently attached to the object. */
  const ImageType * GetImage() const;

  /** Returns true if the point is inside, false otherwise. */
  bool IsInsideInObjectSpace(const PointType & point, unsigned int depth=0,
    const std::string & name = "") const override;

  /** Returns the value of the image at the requested point.
   *  Returns true if that value is valid */
  bool ValueAtInObjectSpace(const PointType & point, double & value,
    unsigned int depth = 0, const std::string & name = "") const override;

  /** Returns the latest modified time of the object and its component. */
  ModifiedTimeType GetMTime() const override;

  /** Set the slice position */
  itkSetMacro( SliceNumber, IndexType );
  void SetSliceNumber(unsigned int dimension, int position);

  /** Get the slice position */
  itkGetConstMacro( SliceNumber, IndexType );
  int GetSliceNumber(unsigned int dimension)
  { return m_SliceNumber[dimension]; }

  const char * GetPixelTypeName()
  { return m_PixelType.c_str(); }

  /** Set/Get the interpolator */
  void SetInterpolator(InterpolatorType *interpolator);
  itkGetConstMacro(Interpolator, InterpolatorType *);

protected:
  /** Compute the boundaries of the image spatial object. */
  bool ProtectedComputeMyBoundingBox() const override;

  ImageSpatialObject();
  ~ImageSpatialObject() override;

  void PrintSelf(std::ostream & os, Indent indent) const override;

  typename LightObject::Pointer InternalClone() const override;

private:

  ImagePointer    m_Image;

  IndexType       m_SliceNumber;
  std::string     m_PixelType;

  typename InterpolatorType::Pointer m_Interpolator;

  template <typename T>
  void SetPixelTypeName(const T *)
  { itkWarningMacro("itk::ImageSpatialObject() : PixelType not recognized"); }

  void SetPixelTypeName(const short *)
  { m_PixelType = "short"; }

  void SetPixelTypeName(const unsigned char *)
  { m_PixelType = "unsigned char"; }

  void SetPixelTypeName(const unsigned short *)
  { m_PixelType = "unsigned short"; }

  void SetPixelTypeName(const float *)
  { m_PixelType = "float"; }

  void SetPixelTypeName(const double *)
  { m_PixelType = "double"; }

};
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageSpatialObject.hxx"
#endif

#endif //itkImageSpatialObject_h
