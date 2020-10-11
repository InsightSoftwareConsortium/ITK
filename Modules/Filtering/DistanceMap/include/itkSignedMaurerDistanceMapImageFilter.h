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
#ifndef itkSignedMaurerDistanceMapImageFilter_h
#define itkSignedMaurerDistanceMapImageFilter_h

#include "itkImageToImageFilter.h"

namespace itk
{
/**
 *\class SignedMaurerDistanceMapImageFilter
 *
 *  \brief This filter calculates the Euclidean distance transform
 *  of a binary image in linear time for arbitrary dimensions.
 *
 *  \par Inputs and Outputs
 *  This is an image-to-image filter. The dimensionality is arbitrary. The
 *  only dimensionality constraint is that the input and output images be of
 *  the same dimensions and size. To maintain integer arithmetic within the
 *  filter, the default output is the signed squared distance. This implies
 *  that the input image should be of type "unsigned int" or "int" whereas the
 *  output image is of type "int".  Obviously, if the user wishes to utilize
 *  the image spacing or to have a filter with the Euclidean distance (as
 *  opposed to the squared distance), output image types of float or double
 *  should be used.
 *
 *  The inside is considered as having negative distances. Outside is
 *  treated as having positive distances. To change the convention, use the
 *  InsideIsPositive(bool) function.
 *
 *  \par Parameters
 *  Set/GetBackgroundValue specifies the background of the value of the
 *  input binary image. Normally this is zero and, as such, zero is the
 *  default value.  Other than that, the usage is completely analogous to
 *  the itk::DanielssonDistanceImageFilter class except it does not return
 *  the Voronoi map.
 *
 *  Reference:
 *  C. R. Maurer, Jr., R. Qi, and V. Raghavan, "A Linear Time Algorithm
 *  for Computing Exact Euclidean Distance Transforms of Binary Images in
 *  Arbitrary Dimensions", IEEE - Transactions on Pattern Analysis and
 *  Machine Intelligence, 25(2): 265-270, 2003.
 *
 * \ingroup ImageFeatureExtraction
 * \ingroup ITKDistanceMap
 *
 * \sphinx
 * \sphinxexample{Filtering/DistanceMap/MaurerDistanceMapOfBinary,Maurer Distance Map Of Binary Image}
 * \endsphinx
 */
template <typename TInputImage, typename TOutputImage>
class ITK_TEMPLATE_EXPORT SignedMaurerDistanceMapImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(SignedMaurerDistanceMapImageFilter);

  /** Extract dimension from input and output image. */
  static constexpr unsigned int InputImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int OutputImageDimension = TOutputImage::ImageDimension;
  static constexpr unsigned int ImageDimension = TOutputImage::ImageDimension;

  /** Convenient type alias for simplifying declarations. */
  using InputImageType = TInputImage;
  using InputImageConstPointer = typename InputImageType::ConstPointer;

  using OutputImageType = TOutputImage;
  using OutputImagePointer = typename OutputImageType::Pointer;

  /** Standard class type aliases. */
  using Self = SignedMaurerDistanceMapImageFilter;
  using Superclass = ImageToImageFilter<InputImageType, OutputImageType>;

  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(SignedMaurerDistanceMapImageFilter, ImageToImageFilter);

  using InputRegionType = typename InputImageType::RegionType;
  using OutputRegionType = typename OutputImageType::RegionType;

  /** Image type alias support */
  using InputPixelType = typename InputImageType::PixelType;
  using OutputPixelType = typename OutputImageType::PixelType;

  using InputSizeType = typename InputImageType::SizeType;
  using InputSizeValueType = typename InputImageType::SizeValueType;
  using OutputSizeType = typename OutputImageType::SizeType;
  using OutputSizeValueType = typename OutputImageType::SizeValueType;

  using InputIndexType = typename InputImageType::IndexType;
  using InputIndexValueType = typename InputImageType::IndexValueType;
  using OutputIndexType = typename OutputImageType::IndexType;
  using OutputIndexValueType = typename OutputImageType::IndexValueType;

  using InputSpacingType = typename InputImageType::SpacingType;
  using OutputSpacingType = typename OutputImageType::SpacingType;
  using OutputImageRegionType = typename OutputImageType::RegionType;

  /** Set if the distance should be squared. */
  itkSetMacro(SquaredDistance, bool);

  /** Get the distance squared. */
  itkGetConstReferenceMacro(SquaredDistance, bool);

  /** Set On/Off if the distance is squared. */
  itkBooleanMacro(SquaredDistance);

  /** Set if the inside represents positive values in the signed distance
   *  map. By convention ON pixels are treated as inside pixels.*/
  itkSetMacro(InsideIsPositive, bool);

  /** Get if the inside represents positive values in the signed distance
   * map. \see GetInsideIsPositive()  */
  itkGetConstReferenceMacro(InsideIsPositive, bool);

  /** Set if the inside represents positive values in the signed distance
   * map. By convention ON pixels are treated as inside pixels. Default is
   * true.                             */
  itkBooleanMacro(InsideIsPositive);

  /** Set if image spacing should be used in computing distances. */
  itkSetMacro(UseImageSpacing, bool);

  /** Get whether spacing is used. */
  itkGetConstReferenceMacro(UseImageSpacing, bool);

  /** Set On/Off whether spacing is used. */
  itkBooleanMacro(UseImageSpacing);

  /**
   * Set the background value which defines the object.  Usually this
   * value is = 0.
   */
  itkSetMacro(BackgroundValue, InputPixelType);
  itkGetConstReferenceMacro(BackgroundValue, InputPixelType);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(IntConvertibleToInputCheck, (Concept::Convertible<int, InputPixelType>));
  itkConceptMacro(InputHasNumericTraitsCheck, (Concept::HasNumericTraits<InputPixelType>));
  itkConceptMacro(OutputImagePixelTypeIsFloatingPointCheck, (Concept::IsFloatingPoint<OutputPixelType>));
  // End concept checking
#endif

protected:
  SignedMaurerDistanceMapImageFilter();
  ~SignedMaurerDistanceMapImageFilter() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  GenerateData() override;

  unsigned int
  SplitRequestedRegion(unsigned int i, unsigned int num, OutputImageRegionType & splitRegion) override;

  void
  ThreadedGenerateData(const OutputImageRegionType &, ThreadIdType) override;

  void
  DynamicThreadedGenerateData(const OutputImageRegionType &) override
  {
    itkExceptionMacro("This class requires threadId so it must use classic multi-threading model");
  }

private:
  void
       Voronoi(unsigned int, OutputIndexType idx, OutputImageType * output);
  bool Remove(OutputPixelType, OutputPixelType, OutputPixelType, OutputPixelType, OutputPixelType, OutputPixelType);

  InputPixelType   m_BackgroundValue;
  InputSpacingType m_Spacing;

  unsigned int m_CurrentDimension{ 0 };

  bool m_InsideIsPositive{ false };
  bool m_UseImageSpacing{ true };
  bool m_SquaredDistance{ false };

  const InputImageType * m_InputCache;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkSignedMaurerDistanceMapImageFilter.hxx"
#endif

#endif
