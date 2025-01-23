/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkOrientImageFilter_h
#define itkOrientImageFilter_h

#include "itkPermuteAxesImageFilter.h"
#include "itkFlipImageFilter.h"
#include "itkAnatomicalOrientation.h"
#include <map>
#include <string>

namespace itk
{
/** \class OrientImageFilter
 * \brief Permute axes and then flip images as needed to obtain
 *  agreement in coordinateOrientation codes.
 *
 * This class satisfies a common requirement in medical imaging, which
 * is to properly orient a 3 dimensional image with respect to anatomical
 * features. Due to the wide variety of hardware used to generate 3D images
 * of human anatomy, and the even wider variety of image processing software,
 * it is often necessary to re-orient image volume data.
 *
 * OrientImageFilter depends on representations of the orientation defined in the
 * AnatomicalOrientation class. The orientation is represented by the following anatomical terms with respect to the
 * subject or patient:
 *   - Left and Right (Subject's left and right)
 *   - Anterior and Posterior (Subject's front and back)
 *   - Inferior and Superior (Subject's bottom and top, i.e. feet and head)
 *
 * An AnatomicalOrientation object can be constructed unambiguously with the following syntax:
 *
   \code
       AnatomicalOrientation(AnatomicalOrientation::CoordinateEnum::RightToLeft,
                             AnatomicalOrientation::CoordinateEnum::AnteriorToPosterior,
                             AnatomicalOrientation::CoordinateEnum::InferiorToSuperior);
   \endcode
 *
 *
 * The orientations were previously defined in the itk::SpatialOrientation class. However,
 * the 3 letter code is ambiguous with label and the direction the axis is increasing.
 *
 * In order to use this filter, you need to supply an input
 * image, the current orientation of the input image (set with
 * SetGivenCoordinateOrientation) and the desired orientation.
 * (set with SetDesiredCoordinateOrientation).
 * You may explicitly set the DesiredOrientation with
 * SetDesiredCoordinateOrientation (if UseImageDirection is "off") or
 * you can use the image's direction cosines to set the
 * DesiredOrientation (if UseImageDirection is "on").
 * When reading image files that define the coordinate orientation
 * of the image, the current orientation is stored in the MetadataDictionary
 * for the itk::Image object and the Image.Direction direction cosine
 * matrix created from the file.
 *
 * \ingroup ITKImageGrid
 */
template <typename TInputImage, typename TOutputImage>
class ITK_TEMPLATE_EXPORT OrientImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(OrientImageFilter);

  /** Standard class type aliases. */
  using Self = OrientImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Some convenient type alias. */
  using InputImageType = TInputImage;
  using InputImagePointer = typename InputImageType::Pointer;
  using InputImageConstPointer = typename InputImageType::ConstPointer;
  using InputImageRegionType = typename InputImageType::RegionType;
  using InputImagePixelType = typename InputImageType::PixelType;
  using OutputImageType = TOutputImage;
  using OutputImagePointer = typename OutputImageType::Pointer;
  using OutputImageConstPointer = typename OutputImageType::ConstPointer;
  using OutputImageRegionType = typename OutputImageType::RegionType;
  using OutputImagePixelType = typename OutputImageType::PixelType;
  using CoordinateOrientationCode = AnatomicalOrientation;

  /** Axes permuter type. */
  using PermuterType = PermuteAxesImageFilter<TInputImage>;
  using PermuteOrderArrayType = typename PermuterType::PermuteOrderArrayType;

  /** Axes flipper type. */
  using FlipperType = FlipImageFilter<TInputImage>;
  using FlipAxesArrayType = typename FlipperType::FlipAxesArrayType;

  /** ImageDimension constants */
  static constexpr unsigned int InputImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int OutputImageDimension = TOutputImage::ImageDimension;

  /** Standard New method. */
  itkNewMacro(Self);

  /** \see LightObject::GetNameOfClass() */
  itkOverrideGetNameOfClassMacro(OrientImageFilter);

  /** Set/Get the orientation codes to define the coordinate transform. */
  itkGetEnumMacro(GivenCoordinateOrientation, CoordinateOrientationCode);
  void
  SetGivenCoordinateOrientation(CoordinateOrientationCode newCode);

  inline void
  SetGivenCoordinateDirection(const typename TInputImage::DirectionType & GivenDirection)
  {
    SetGivenCoordinateOrientation(AnatomicalOrientation(GivenDirection));
  }

  itkGetEnumMacro(DesiredCoordinateOrientation, CoordinateOrientationCode);
  void
  SetDesiredCoordinateOrientation(CoordinateOrientationCode newCode);

  inline void
  SetDesiredCoordinateDirection(const typename TOutputImage::DirectionType & DesiredDirection)
  {
    SetDesiredCoordinateOrientation(AnatomicalOrientation(DesiredDirection));
  }

  /** Controls how the GivenCoordinateOrientation is determined.
   * If set to On, the direction cosines determine the coordinate
   * orientation. If set to Off, the user must use the
   * SetGivenCoordinateOrientation method to establish the
   * orientation.
   *
   * For compatibility with the original API, the default value
   * is Off. */
  itkBooleanMacro(UseImageDirection);
  itkGetConstMacro(UseImageDirection, bool);
  itkSetMacro(UseImageDirection, bool);

  /** Get axes permute order. */
  itkGetConstReferenceMacro(PermuteOrder, PermuteOrderArrayType);

  /** Get flip axes. */
  itkGetConstReferenceMacro(FlipAxes, FlipAxesArrayType);

  /** Convenience methods to set desired slice orientation
   *  These methods allow a limited selection of slice orientations
   *  without having to specify the SpatialOrientation.
   *
   *  SetDesiredCoordinateOrientationToAxial is equivalent to AnatomicalOrientation::PositiveEnum::LPS.
   *
   *  SetDesiredCoordinateOrientationToCoronal is equivalent to AnatomicalOrientation::PositiveEnum::LIP.
   *
   *  SetDesiredCoordinateOrientationToSagittal is equivalent to AnatomicalOrientation::PositiveEnum::PIR.
   */
  void
  SetDesiredCoordinateOrientationToAxial()
  {
    this->SetDesiredCoordinateOrientation({ AnatomicalOrientation::CoordinateEnum::RightToLeft,
                                            AnatomicalOrientation::CoordinateEnum::AnteriorToPosterior,
                                            AnatomicalOrientation::CoordinateEnum::InferiorToSuperior });
  }

  void
  SetDesiredCoordinateOrientationToCoronal()
  {
    this->SetDesiredCoordinateOrientation({ AnatomicalOrientation::CoordinateEnum::RightToLeft,
                                            AnatomicalOrientation::CoordinateEnum::SuperiorToInferior,
                                            AnatomicalOrientation::CoordinateEnum::AnteriorToPosterior });
  }

  void
  SetDesiredCoordinateOrientationToSagittal()
  {
    this->SetDesiredCoordinateOrientation({ AnatomicalOrientation::CoordinateEnum::AnteriorToPosterior,
                                            AnatomicalOrientation::CoordinateEnum::SuperiorToInferior,
                                            AnatomicalOrientation::CoordinateEnum::LeftToRight });
  }

  /** OrientImageFilter produces an image which is a different
   * dimensionality than its input image, in general. As such,
   * OrientImageFilter needs to provide an implementation for
   * GenerateOutputInformation() in order to inform the pipeline
   * execution model. The original documentation of this method is
   * below.
   * \sa ProcessObject::GenerateOutputInformation() */
  void
  GenerateOutputInformation() override;

  itkConceptMacro(InputConvertibleToOutput, (Concept::Convertible<InputImagePixelType, OutputImagePixelType>));
  itkConceptMacro(SameDimension, (Concept::SameDimension<Self::InputImageDimension, Self::OutputImageDimension>));
  itkConceptMacro(DimensionShouldBe3, (Concept::SameDimension<Self::InputImageDimension, 3>));

protected:
  OrientImageFilter();
  ~OrientImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** OrientImageFilter needs the entire input be
   * available. Thus, it needs to provide an implementation of
   * GenerateInputRequestedRegion(). */
  void
  GenerateInputRequestedRegion() override;

  /** OrientImageFilter will produce the entire output. */
  void
  EnlargeOutputRequestedRegion(DataObject * itkNotUsed(output)) override;

  void
  VerifyPreconditions() const override;

  /*** Member functions used by GenerateData: */
  void
  DeterminePermutationsAndFlips(const CoordinateOrientationCode fixed_orient,
                                const CoordinateOrientationCode moving_orient);

  /** Returns true if a permute is required. Returns false otherwise. */
  bool
  NeedToPermute();

  /** Returns true if flipping is required. Returns false otherwise. */
  bool
  NeedToFlip();

  /** Single-threaded version of GenerateData. This filter delegates
   * to PermuteAxesImageFilter and FlipImageFilter. */
  void
  GenerateData() override;

private:
  CoordinateOrientationCode m_GivenCoordinateOrientation{ AnatomicalOrientation::CoordinateEnum::RightToLeft,
                                                          AnatomicalOrientation::CoordinateEnum::InferiorToSuperior,
                                                          AnatomicalOrientation::CoordinateEnum::PosteriorToAnterior };
  CoordinateOrientationCode m_DesiredCoordinateOrientation{
    AnatomicalOrientation::CoordinateEnum::RightToLeft,
    AnatomicalOrientation::CoordinateEnum::InferiorToSuperior,
    AnatomicalOrientation::CoordinateEnum::PosteriorToAnterior
  };
  bool m_UseImageDirection{ false };

  PermuteOrderArrayType m_PermuteOrder{};
  FlipAxesArrayType     m_FlipAxes{ false };

}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkOrientImageFilter.hxx"
#endif

#endif
