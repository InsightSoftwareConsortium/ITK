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
#ifndef itkPointSetToImageFilter_h
#define itkPointSetToImageFilter_h

#include "itkImageSource.h"
#include "itkConceptChecking.h"

namespace itk
{
/**
 *\class PointSetToImageFilter
 * \brief Base class for filters that take a PointSet
 *        as input and produce an image as output.
 *  By default, if the user does not specify the size of the output image,
 *  the maximum size of the point-set's bounding box is used.
 * \ingroup ITKCommon
 */
template <typename TInputPointSet, typename TOutputImage>
class ITK_TEMPLATE_EXPORT PointSetToImageFilter : public ImageSource<TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(PointSetToImageFilter);

  /** Standard class type aliases. */
  using Self = PointSetToImageFilter;
  using Superclass = ImageSource<TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using SizeType = typename TOutputImage::SizeType;
  using OutputImageType = TOutputImage;
  using OutputImagePointer = typename OutputImageType::Pointer;
  using ValueType = typename OutputImageType::ValueType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(PointSetToImageFilter, ImageSource);

  /** Superclass type alias. */
  using OutputImageRegionType = typename Superclass::OutputImageRegionType;

  /** Some convenient type alias. */
  using InputPointSetType = TInputPointSet;
  using InputPointSetPointer = typename InputPointSetType::Pointer;
  using InputPointSetConstPointer = typename InputPointSetType::ConstPointer;

  /** Dimension constants */
  static constexpr unsigned int InputPointSetDimension = InputPointSetType::PointDimension;
  static constexpr unsigned int OutputImageDimension = TOutputImage::ImageDimension;

  /** Image spacing and origin type alias */
  using SpacingType = typename TOutputImage::SpacingType;
  using DirectionType = typename TOutputImage::DirectionType;
  using PointType = typename TOutputImage::PointType;

  /** Set/Get the input point-set of this process object.  */
  using Superclass::SetInput;
  virtual void
  SetInput(const InputPointSetType * input);

  virtual void
  SetInput(unsigned int, const InputPointSetType * pointset);

  const InputPointSetType *
  GetInput();

  const InputPointSetType *
  GetInput(unsigned int idx);

  /** Set the spacing (size of a pixel) of the image. The
   * spacing is the geometric distance between image samples.
   * It is stored internally as double, but may be set from
   * float. \sa GetSpacing() */
  itkSetMacro(Spacing, SpacingType);
  virtual void
  SetSpacing(const double * v);

  virtual void
  SetSpacing(const float * v);

  /** Get the spacing (size of a pixel) of the image. The
   * spacing is the geometric distance between image samples.
   * The value returned is a pointer to a double array.
   * For ImageBase and Image, the default data spacing is unity. */
  itkGetConstReferenceMacro(Spacing, SpacingType);

  /** Get/Set the direction of the image. The
   * direction is the relationship of the grid to physical
   * coordinates. */
  itkSetMacro(Direction, DirectionType);
  itkGetConstReferenceMacro(Direction, DirectionType);

  /** Set the origin of the image. The origin is the geometric
   * coordinates of the image origin.  It is stored internally
   * as double but may be set from float.
   * \sa GetOrigin() */
  itkSetMacro(Origin, PointType);
  virtual void
  SetOrigin(const double * v);

  virtual void
  SetOrigin(const float * v);

  /** Get the origin of the image. The origin is the geometric
   * coordinates of the index (0,0).  The value returned is a pointer
   * to a double array.  For ImageBase and Image, the default origin is
   * 0. */
  itkGetConstReferenceMacro(Origin, PointType);

  /** Set/Get the value for pixels in the point-set.
   * By default, this filter will return an image
   * that contains values from the point-set specified as input.
   * If this "inside" value is changed to a non-null value,
   * the output produced by this filter will be a mask with inside/outside values
   * specified by the user. */
  itkSetMacro(InsideValue, ValueType);
  itkGetConstMacro(InsideValue, ValueType);

  /** Set/Get the value for pixels outside the point-set.
   * By default, this filter will return an image
   * that contains values from the point specified as input.
   * If this "outside" value is changed to a non-null value,
   * the output produced by this filter will be a mask with inside/outside values
   * specified by the user. */
  itkSetMacro(OutsideValue, ValueType);
  itkGetConstMacro(OutsideValue, ValueType);

  /** Set/Get Size */
  itkSetMacro(Size, SizeType);
  itkGetConstMacro(Size, SizeType);

protected:
  PointSetToImageFilter();
  ~PointSetToImageFilter() override = default;

  void
  GenerateOutputInformation() override
  {} // do nothing
  void
  GenerateData() override;

  SizeType m_Size;

  PointType m_Origin;

  SpacingType m_Spacing;

  DirectionType m_Direction;

  ValueType m_InsideValue;
  ValueType m_OutsideValue;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkPointSetToImageFilter.hxx"
#endif

#endif
