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
#ifndef itkTransformToDisplacementFieldFilter_h
#define itkTransformToDisplacementFieldFilter_h

#include "itkDataObjectDecorator.h"
#include "itkTransform.h"
#include "itkImageSource.h"

namespace itk
{
/** \class TransformToDisplacementFieldFilter
 * \brief Generate a displacement field from a coordinate transform
 *
 * Output information (spacing, size and direction) for the output
 * image should be set. This information has the normal defaults of
 * unit spacing, zero origin and identity direction. Optionally, the
 * output information can be obtained from a reference image. If the
 * reference image is provided and UseReferenceImage is On, then the
 * spacing, origin and direction of the reference image will be used.
 *
 * Since this filter produces an image which is a different size than
 * its input, it needs to override several of the methods defined
 * in ProcessObject in order to properly manage the pipeline execution model.
 * In particular, this filter overrides
 * ProcessObject::GenerateOutputInformation().
 *
 * This filter is implemented as a multithreaded filter.  It provides a
 * ThreadedGenerateData() method for its implementation.
 *
 * \author Marius Staring, Leiden University Medical Center, The Netherlands.
 *
 * This class was taken from the Insight Journal paper:
 * https://www.insight-journal.org/browse/publication/240
 *
 * \ingroup GeometricTransform
 * \ingroup ITKDisplacementField
 */
template <typename TOutputImage, typename TParametersValueType = double>
class ITK_TEMPLATE_EXPORT TransformToDisplacementFieldFilter : public ImageSource<TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(TransformToDisplacementFieldFilter);

  /** Standard class type aliases. */
  using Self = TransformToDisplacementFieldFilter;
  using Superclass = ImageSource<TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  using OutputImageType = TOutputImage;
  using OutputImageRegionType = typename OutputImageType::RegionType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(TransformToDisplacementFieldFilter, ImageSource);

  /** Number of dimensions. */
  static constexpr unsigned int ImageDimension = TOutputImage::ImageDimension;

  /** Typedefs for transform. */
  using TransformType = Transform<TParametersValueType, ImageDimension, ImageDimension>;
  using TransformInputType = DataObjectDecorator<TransformType>;

  /** Typedefs for output image. */
  using PixelType = typename OutputImageType::PixelType;
  using PixelValueType = typename PixelType::ValueType;
  using RegionType = typename OutputImageType::RegionType;
  using SizeType = typename RegionType::SizeType;
  using IndexType = typename OutputImageType::IndexType;
  using PointType = typename OutputImageType::PointType;
  using SpacingType = typename OutputImageType::SpacingType;
  using OriginType = typename OutputImageType::PointType;
  using DirectionType = typename OutputImageType::DirectionType;

  /** Typedef the reference image ImageBase. */
  using ReferenceImageBaseType = ImageBase<ImageDimension>;

  /** Get/Set the coordinate transformation.
   * Set the coordinate transform to use for resampling.  Note that this must
   * be in physical coordinates and it is the output-to-input transform, NOT
   * the input-to-output transform that you might naively expect. */
  using Superclass::SetInput;
  virtual void
  SetInput(const TransformInputType * input);
  const TransformInputType *
  GetInput() const;
  itkSetGetDecoratedObjectInputMacro(Transform, TransformType);

  /** Set/Get the start index of the output largest possible region.
   * The default is an index of all zeros. */
  itkSetMacro(OutputStartIndex, IndexType);
  itkGetConstReferenceMacro(OutputStartIndex, IndexType);

  /** Set/Get the size of the output image. */
  itkSetMacro(Size, SizeType);
  itkGetConstReferenceMacro(Size, SizeType);

  /** Set the output image spacing. */
  itkSetMacro(OutputSpacing, SpacingType);
  virtual void
  SetOutputSpacing(const SpacePrecisionType * spacing);

  /** Get the output image spacing. */
  itkGetConstReferenceMacro(OutputSpacing, SpacingType);

  /** Set the output image origin. */
  itkSetMacro(OutputOrigin, OriginType);
  virtual void
  SetOutputOrigin(const SpacePrecisionType * origin);

  /** Get the output image origin. */
  itkGetConstReferenceMacro(OutputOrigin, OriginType);

  /** Set the output direction cosine matrix. */
  itkSetMacro(OutputDirection, DirectionType);
  itkGetConstReferenceMacro(OutputDirection, DirectionType);

  /** Set a reference image to use to define the output information.
   *  By default, output information is specified through the
   *  SetOutputSpacing, Origin, and Direction methods.  Alternatively,
   *  this method can be used to specify an image from which to
   *  copy the information. UseReferenceImageOn must be set to utilize the
   *  reference image. */
  itkSetInputMacro(ReferenceImage, ReferenceImageBaseType);

  /** Get the reference image that is defining the output information. */
  itkGetInputMacro(ReferenceImage, ReferenceImageBaseType);

  /** Turn on/off whether a specified reference image should be used to define
   *  the output information. */
  itkSetMacro(UseReferenceImage, bool);
  itkBooleanMacro(UseReferenceImage);
  itkGetConstMacro(UseReferenceImage, bool);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  static constexpr unsigned int PixelDimension = PixelType::Dimension;
  itkConceptMacro(SameDimensionCheck, (Concept::SameDimension<ImageDimension, PixelDimension>));
  // End concept checking
#endif

protected:
  TransformToDisplacementFieldFilter();
  ~TransformToDisplacementFieldFilter() override = default;

  /** Produces a Vector Image. */
  void
  GenerateOutputInformation() override;

  /** TransformToDisplacementFieldFilter is implemented as a multithreaded filter. */
  void
  DynamicThreadedGenerateData(const OutputImageRegionType & outputRegionForThread) override;


  /** Default implementation for resampling that works for any
   * transformation type.
   */
  void
  NonlinearThreadedGenerateData(const OutputImageRegionType & outputRegionForThread);

  /** Faster implementation for resampling that works for with linear
   *  transformation types.
   */
  void
  LinearThreadedGenerateData(const OutputImageRegionType & outputRegionForThread);

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  /** Member variables. */
  SizeType      m_Size;             // size of the output region
  IndexType     m_OutputStartIndex; // start index of the output region
  SpacingType   m_OutputSpacing;    // output image spacing
  OriginType    m_OutputOrigin;     // output image origin
  DirectionType m_OutputDirection;  // output image direction cosines
  bool          m_UseReferenceImage{ false };
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkTransformToDisplacementFieldFilter.hxx"
#endif

#endif
