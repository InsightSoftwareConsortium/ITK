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
#ifndef itkWarpImageFilter_h
#define itkWarpImageFilter_h
#include "itkImageBase.h"
#include "itkImageToImageFilter.h"
#include "itkLinearInterpolateImageFunction.h"

namespace itk
{
/**
 *\class WarpImageFilter
 * \brief Warps an image using an input displacement field.
 *
 * WarpImageFilter warps an existing image with respect to
 * a given displacement field.
 *
 * A displacement field is represented as a image whose pixel type is some
 * vector type with at least N elements, where N is the dimension of
 * the input image. The vector type must support element access via operator
 * [].
 *
 * The output image is produced by inverse mapping: the output pixels
 * are mapped back onto the input image. This scheme avoids the creation of
 * any holes and overlaps in the output image.
 *
 * Each vector in the displacement field represent the distance between
 * a geometric point in the input space and a point in the output space such
 * that:
 *
 * \f[ p_{in} = p_{out} + d \f]
 *
 * Typically the mapped position does not correspond to an integer pixel
 * position in the input image. Interpolation via an image function
 * is used to compute values at non-integer positions. The default
 * interpolation typed used is the LinearInterpolateImageFunction.
 * The user can specify a particular interpolation function via
 * SetInterpolator(). Note that the input interpolator must derive
 * from base class InterpolateImageFunction.
 *
 * Position mapped to outside of the input image buffer are assigned
 * a edge padding value.
 *
 * The LargetPossibleRegion for the output is inherited
 * from the input displacement field. The output image
 * spacing, origin and orientation may be set via
 * SetOutputSpacing, SetOutputOrigin and
 * SetOutputDirection. The default are respectively a
 * vector of 1's, a vector of 0's and an identity matrix.
 *
 * This class is templated over the type of the input image, the
 * type of the output image and the type of the displacement field.
 *
 * The input image is set via SetInput. The input displacement field
 * is set via SetDisplacementField.
 *
 * This filter is implemented as a multithreaded filter.
 *
 * \warning This filter assumes that the input type, output type
 * and displacement field type all have the same number of dimensions.
 *
 * \ingroup GeometricTransform MultiThreaded Streamed
 * \ingroup ITKImageGrid
 *
 * \sphinx
 * \sphinxexample{Filtering/ImageGrid/WarpAnImageUsingADeformationField,Warp An Image Using A Deformation Field}
 * \endsphinx
 */
template <typename TInputImage, typename TOutputImage, typename TDisplacementField>
class ITK_TEMPLATE_EXPORT WarpImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(WarpImageFilter);

  /** Standard class type aliases. */
  using Self = WarpImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods) */
  itkTypeMacro(WarpImageFilter, ImageToImageFilter);

  /** Typedef to describe the output image region type. */
  using OutputImageRegionType = typename TOutputImage::RegionType;

  /** Inherit some types from the superclass. */
  using InputImageType = typename Superclass::InputImageType;
  using InputImagePointer = typename Superclass::InputImagePointer;
  using OutputImageType = typename Superclass::OutputImageType;
  using OutputImagePointer = typename Superclass::OutputImagePointer;
  using InputImageConstPointer = typename Superclass::InputImageConstPointer;
  using IndexType = typename OutputImageType::IndexType;
  using IndexValueType = typename OutputImageType::IndexValueType;
  using SizeType = typename OutputImageType::SizeType;
  using PixelType = typename OutputImageType::PixelType;
  using PixelComponentType = typename OutputImageType::InternalPixelType;
  using SpacingType = typename OutputImageType::SpacingType;

  /** Determine the image dimension. */
  static constexpr unsigned int ImageDimension = TOutputImage::ImageDimension;
  static constexpr unsigned int InputImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int DisplacementFieldDimension = TDisplacementField::ImageDimension;
  /** type alias for base image type at the current ImageDimension */
  using ImageBaseType = ImageBase<Self::ImageDimension>;

  /** Displacement field type alias support */
  using DisplacementFieldType = TDisplacementField;
  using DisplacementFieldPointer = typename DisplacementFieldType::Pointer;
  using DisplacementType = typename DisplacementFieldType::PixelType;

  /** Interpolator type alias support */
  using CoordRepType = double;
  using InterpolatorType = InterpolateImageFunction<InputImageType, CoordRepType>;
  using InterpolatorPointer = typename InterpolatorType::Pointer;
  using DefaultInterpolatorType = LinearInterpolateImageFunction<InputImageType, CoordRepType>;

  /** Point type */
  using PointType = Point<CoordRepType, Self::ImageDimension>;

  /** Type for representing the direction of the output image */
  using DirectionType = typename TOutputImage::DirectionType;


  /** Set the displacement field. */
  itkSetInputMacro(DisplacementField, DisplacementFieldType);

  /** Get a pointer the displacement field. */
  itkGetInputMacro(DisplacementField, DisplacementFieldType);

  /** Get/Set the interpolator function. */
  itkSetObjectMacro(Interpolator, InterpolatorType);
  itkGetModifiableObjectMacro(Interpolator, InterpolatorType);

  /** Set the output image spacing. */
  itkSetMacro(OutputSpacing, SpacingType);
  virtual void
  SetOutputSpacing(const double * values);

  /** Get the output image spacing. */
  itkGetConstReferenceMacro(OutputSpacing, SpacingType);

  /** Set the output image origin. */
  itkSetMacro(OutputOrigin, PointType);
  virtual void
  SetOutputOrigin(const double * values);

  /** Get the output image origin. */
  itkGetConstReferenceMacro(OutputOrigin, PointType);

  /** Set/Get the direction (orientation) of the output image */
  itkSetMacro(OutputDirection, DirectionType);
  itkGetConstReferenceMacro(OutputDirection, DirectionType);

  /** Helper method to set the output parameters based on this image */
  void
  SetOutputParametersFromImage(const ImageBaseType * image);

  /** Set the start index of the output largest possible region.
   * The default is an index of all zeros. */
  itkSetMacro(OutputStartIndex, IndexType);

  /** Get the start index of the output largest possible region. */
  itkGetConstReferenceMacro(OutputStartIndex, IndexType);

  /** Set the size of the output image. */
  itkSetMacro(OutputSize, SizeType);

  /** Get the size of the output image. */
  itkGetConstReferenceMacro(OutputSize, SizeType);

  /** Set the edge padding value */
  itkSetMacro(EdgePaddingValue, PixelType);

  /** Get the edge padding value */
  itkGetConstMacro(EdgePaddingValue, PixelType);

  /** WarpImageFilter produces an image which is a different
   * size than its input image. As such, it needs to provide an
   * implementation for GenerateOutputInformation() which set
   * the output information according the OutputSpacing, OutputOrigin
   * and the displacement field's LargestPossibleRegion. */
  void
  GenerateOutputInformation() override;

  /** It is difficult to compute in advance the input image region
   * required to compute the requested output region. Thus the safest
   * thing to do is to request for the whole input image.
   *
   * For the displacement field, the input requested region
   * set to be the same as that of the output requested region. */
  void
  GenerateInputRequestedRegion() override;

  /** This method is used to set the state of the filter before
   * multi-threading. */
  void
  BeforeThreadedGenerateData() override;

  /** This method is used to set the state of the filter after
   * multi-threading. */
  void
  AfterThreadedGenerateData() override;

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(SameDimensionCheck1, (Concept::SameDimension<ImageDimension, InputImageDimension>));
  itkConceptMacro(SameDimensionCheck2, (Concept::SameDimension<ImageDimension, DisplacementFieldDimension>));
  itkConceptMacro(InputHasNumericTraitsCheck, (Concept::HasNumericTraits<typename TInputImage::InternalPixelType>));
  itkConceptMacro(DisplacementFieldHasNumericTraitsCheck,
                  (Concept::HasNumericTraits<typename TDisplacementField::PixelType::ValueType>));
  // End concept checking
#endif

protected:
  WarpImageFilter();
  // ~WarpImageFilter() {} default implementation is ok

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** WarpImageFilter is implemented as a multi-threaded filter.
   * As such, it needs to provide and implementation for
   * DynamicThreadedGenerateData(). */
  void
  DynamicThreadedGenerateData(const OutputImageRegionType & outputRegionForThread) override;


  /** Override VerifyInputInformation() since this filter's inputs do
   * not need to occupy the same physical space. But check the that
   * deformation field has the same number of components as dimensions
   *
   * \sa ProcessObject::VerifyInputInformation
   */
  void
  VerifyInputInformation() ITKv5_CONST override;

  /** This function should be in an interpolator but none of the ITK
   * interpolators at this point handle edge conditions properly
   *
   * If this method is called in a loop, the
   * EvaluateDisplacementAtPhysicalPoint(const PointType &, const DisplacementFieldType *, DisplacementType &)
   * overload will offer better performance. The displacement field
   * can be obtained using the GetDisplacementField() method
   */
  void
  EvaluateDisplacementAtPhysicalPoint(const PointType & p, DisplacementType & output);

  /** This function should be in an interpolator but none of the ITK
   * interpolators at this point handle edge conditions properly
   */
  void
  EvaluateDisplacementAtPhysicalPoint(const PointType &             p,
                                      const DisplacementFieldType * fieldPtr,
                                      DisplacementType &            output);

  bool m_DefFieldSameInformation;
  // variables for deffield interpolator
  IndexType m_StartIndex, m_EndIndex;

private:
  PixelType     m_EdgePaddingValue;
  SpacingType   m_OutputSpacing;
  PointType     m_OutputOrigin;
  DirectionType m_OutputDirection;

  InterpolatorPointer m_Interpolator;
  SizeType            m_OutputSize;       // Size of the output image
  IndexType           m_OutputStartIndex; // output image start index
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkWarpImageFilter.hxx"
#endif

#endif
