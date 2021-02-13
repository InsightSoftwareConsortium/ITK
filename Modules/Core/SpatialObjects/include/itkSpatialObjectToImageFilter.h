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
#ifndef itkSpatialObjectToImageFilter_h
#define itkSpatialObjectToImageFilter_h

#include "itkImageSource.h"
#include "itkConceptChecking.h"

namespace itk
{
/**
 *\class SpatialObjectToImageFilter
 * \brief Base class for filters that take a SpatialObject
 *        as input and produce an image as output.
 *  By default, if the user does not specify the size of the output image,
 *  the maximum size of the object's bounding box is used.
 *  The spacing of the image is given by the spacing of the input
 *  Spatial object.
 * \ingroup ITKSpatialObjects
 *
 * \sphinx
 * \sphinxexample{Core/SpatialObjects/ConvertSpatialObjectToImage,Convert Spatial Object To Image}
 * \endsphinx
 */
template <typename TInputSpatialObject, typename TOutputImage>
class ITK_TEMPLATE_EXPORT SpatialObjectToImageFilter : public ImageSource<TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(SpatialObjectToImageFilter);

  /** Standard class type aliases. */
  using Self = SpatialObjectToImageFilter;
  using Superclass = ImageSource<TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  using OutputImageType = TOutputImage;
  using SizeType = typename OutputImageType::SizeType;
  using PointType = typename OutputImageType::PointType;
  using OutputImagePointer = typename OutputImageType::Pointer;
  using ValueType = typename OutputImageType::ValueType;
  using SpacingType = typename OutputImageType::SpacingType;
  using DirectionType = typename OutputImageType::DirectionType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(SpatialObjectToImageFilter, ImageSource);

  /** Superclass type alias. */
  using OutputImageRegionType = typename Superclass::OutputImageRegionType;

  /** Some convenient type alias. */
  using InputSpatialObjectType = TInputSpatialObject;
  using InputSpatialObjectPointer = typename InputSpatialObjectType::Pointer;
  using InputSpatialObjectConstPointer = typename InputSpatialObjectType::ConstPointer;
  using ChildrenListType = typename TInputSpatialObject::ChildrenListType;

  /** ImageDimension constants */
  static constexpr unsigned int ObjectDimension = InputSpatialObjectType::ObjectDimension;

  static constexpr unsigned int OutputImageDimension = TOutputImage::ImageDimension;

  /** Set/Get the image input of this process object.  */
  using Superclass::SetInput;
  virtual void
  SetInput(const InputSpatialObjectType * input);

  virtual void
  SetInput(unsigned int, const InputSpatialObjectType * object);

  const InputSpatialObjectType *
  GetInput();

  const InputSpatialObjectType *
  GetInput(unsigned int idx);

  /** Generate an output image that matches the origin,
   * size, direction, and spacing of this image. */
  template <class TReferenceImage>
  void
  SetReferenceImage(TReferenceImage * refImage)
  {
    this->SetOrigin(refImage->GetOrigin());
    this->SetSpacing(refImage->GetSpacing());
    this->SetDirection(refImage->GetDirection());
    this->SetSize(refImage->GetLargestPossibleRegion().GetSize());
  }

  /** Spacing (size of a pixel) of the output image. The
   * spacing is the geometric distance between image samples.
   * It is stored internally as double, but may be set from
   * float. \sa GetSpacing() */
  virtual void
  SetSpacing(const SpacingType & spacing);

  virtual void
  SetSpacing(const double * spacing);

  virtual void
  SetSpacing(const float * spacing);

  virtual const double *
  GetSpacing() const;

  /** Directions of the output image. The
   * direction is for oriented images. */
  virtual void
  SetDirection(const DirectionType & dir);

  virtual const DirectionType &
  GetDirection() const;

  /** Set/Get the value for pixels inside the spatial object.
   * By default, this filter will return an image
   * that contains values from the spatial object specified as input.
   * If this "inside" value is changed to a non-null value,
   * the output produced by this filter will be a mask with inside/outside values
   * specified by the user. */
  itkSetMacro(InsideValue, ValueType);
  itkGetConstMacro(InsideValue, ValueType);

  /** Set/Get the value for pixels outside the spatial object.
   * By default, this filter will return an image
   * that contains values from the spatial object specified as input.
   * If this "outside" value is changed to a non-null value,
   * the output produced by this filter will be a mask with inside/outside values
   * specified by the user. */
  itkSetMacro(OutsideValue, ValueType);
  itkGetConstMacro(OutsideValue, ValueType);

  /** The origin of the output image. The origin is the geometric
   * coordinates of the index (0,0,...,0).  It is stored internally
   * as double but may be set from float.
   * \sa GetOrigin() */
  virtual void
  SetOrigin(const PointType & origin);

  virtual void
  SetOrigin(const double * origin);

  virtual void
  SetOrigin(const float * origin);

  virtual const double *
  GetOrigin() const;

  /** The spatial object being transformed can be part of a hierarchy.
   * How deep in the hierarchy should we descend in generating the
   * image?  A ChildrenDepth of 0 means to only include the object
   * itself. */
  itkSetMacro(ChildrenDepth, unsigned int);
  itkGetConstMacro(ChildrenDepth, unsigned int);

  /** Set/Get Size */
  itkSetMacro(Size, SizeType);
  itkGetConstMacro(Size, SizeType);

  /** If UseObjectValue is set to true, then the filter uses
   *  the ValueAtInWorldSpace() function instead of IsInsideInWorldSpace() */
  itkSetMacro(UseObjectValue, bool);
  itkGetConstMacro(UseObjectValue, bool);
  itkBooleanMacro(UseObjectValue);

protected:
  SpatialObjectToImageFilter();
  ~SpatialObjectToImageFilter() override = default;

  void
  GenerateOutputInformation() override
  {} // do nothing
  void
  GenerateData() override;

  SizeType      m_Size;
  double        m_Spacing[OutputImageDimension];
  double        m_Origin[OutputImageDimension];
  DirectionType m_Direction;

  unsigned int m_ChildrenDepth;

  ValueType m_InsideValue;
  ValueType m_OutsideValue;

  bool m_UseObjectValue;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkSpatialObjectToImageFilter.hxx"
#endif

#endif
