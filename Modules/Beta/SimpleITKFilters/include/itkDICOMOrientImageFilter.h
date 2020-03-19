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
#ifndef itkDICOMOrientImageFilter_h
#define itkDICOMOrientImageFilter_h

#include "SimpleITKFiltersExport.h"
#include "itkDICOMOrientation.h"
#include "itkPermuteAxesImageFilter.h"
#include "itkFlipImageFilter.h"
#include <map>
#include <string>

namespace itk
{

/** \class DICOMOrientImageFilter
 * \brief Permute axes and flip images as needed to obtain an approximation to the desired orientation.
 *
 * The physical location of all pixels in the image remains the same, but the meta-data and the ordering of the stored
 * pixels may change.
 *
 *
 * DICOMOrientImageFilter depends on a set of constants that describe all possible labels. Directions are labeled in
 * terms of following pairs:
 *   - Left and Right (Subject's left and right)
 *   - Anterior and Posterior (Subject's front and back)
 *   - Inferior and Superior (Subject's bottom and top, i.e. feet and head)
 *
 * The initials of these directions are used in a 3 letter code in the enumerated type OrientationEnum. The initials are
 * given fastest moving index first, second fastest second, third fastest third, where the label's direction indicates
 * increasing values.
 *
 * An ITK image with an identity direction cosine matrix is in LPS (Left, Posterior, Superior) orientation as defined by
 * the DICOM standard.
 *
 * \f[
 * LPS = \begin{Bmatrix}
 * from\ right\ to\ \textbf{L}eft \\
 * from\ anterior\ towards\ \textbf{P}osterior  \\
 * from\ inferior\ towards\ \textbf{S}uperior
 * \end{Bmatrix}
 * \f]
 *
 * The output orientation is specified with SetDesiredCoordinateOrientation. The input coordinate orientation is
 * computed from the input image's direction cosine matrix.
 *
 * \ingroup SimpleITKFilters
 */
template <typename TInputImage>
class ITK_TEMPLATE_EXPORT DICOMOrientImageFilter
  : public ImageToImageFilter<TInputImage, TInputImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(DICOMOrientImageFilter);

  /** Standard class type aliases. */
  using Self = DICOMOrientImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TInputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Some convenient type alias. */
  using ImageType = TInputImage;
  using ImagePointer = typename ImageType::Pointer;
  using ImageConstPointer = typename ImageType::ConstPointer;
  using ImageRegionType = typename ImageType::RegionType;
  using ImagePixelType = typename ImageType::PixelType;
  using DirectionType = typename ImageType::DirectionType;

  /** Axes permuter type. */
  using PermuterType = PermuteAxesImageFilter<TInputImage>;
  using PermuteOrderArrayType = typename PermuterType::PermuteOrderArrayType;

  /** Axes flipper type. */
  using FlipperType = FlipImageFilter<TInputImage>;
  using FlipAxesArrayType = typename FlipperType::FlipAxesArrayType;

  /** ImageDimension constants */
  static constexpr unsigned int ImageDimension = ImageType::ImageDimension;

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(DICOMOrientImageFilter, ImageToImageFilter);

  using OrientationEnum = DICOMOrientation::OrientationEnum;

  /** Get the orientation codes that defines the input coordinate transform.
   *
   *  This value changes during the execution of the Update in the pipeline. */
  itkGetEnumMacro(InputCoordinateOrientation, OrientationEnum);

  /** Set/Get the desired coordinate orientation for the output image */
  itkGetEnumMacro(DesiredCoordinateOrientation, OrientationEnum);
  void
  SetDesiredCoordinateOrientation(OrientationEnum newCode);
  void
  SetDesiredCoordinateOrientation(const std::string & desired);

  /** Set Get the desired coordinate orientation from a direction matrix. */
  inline void
  SetDesiredCoordinateDirection(const typename ImageType::DirectionType & DesiredDirection)
  {
    SetDesiredCoordinateOrientation(Self::DirectionCosinesToOrientation(DesiredDirection));
  }


  /** Get axes permute order.
   *
   * This value is computed during Update.
   * */
  itkGetConstReferenceMacro(PermuteOrder, PermuteOrderArrayType);

  /** Get flip axes.
   *
   * This value is computed during Update.
   * */
  itkGetConstReferenceMacro(FlipAxes, FlipAxesArrayType);


  /** DICOMOrientImageFilter produces an image which is a different
   *  meta-data than its input image.
   * \sa ProcessObject::GenerateOutputInformation() */
  void
  GenerateOutputInformation() override;

  static_assert(ImageDimension == 3, "Only 3 dimensional images are support!" );

protected:
  DICOMOrientImageFilter();
  ~DICOMOrientImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** OrientImageFilter will produce the entire output. */
  void
  GenerateInputRequestedRegion() override;

  /*** Member functions used by GenerateData: */
  void
  DeterminePermutationsAndFlips(DICOMOrientation desired,  DICOMOrientation given);

  /** Returns true if a permute is required. Returns false otherwise. */
  bool
  NeedToPermute();

  /** Returns true if flipping is required. Returns false otherwise. */
  bool
  NeedToFlip();


  void
  SetInputCoordinateOrientation(OrientationEnum newCode);

  void
  VerifyPreconditions() ITKv5_CONST override;

  /** Single-threaded version of GenerateData. This filter delegates
   * to PermuteAxesImageFilter and FlipImageFilter. */
  void
  GenerateData() override;

private:
  DICOMOrientation m_InputCoordinateOrientation{ OrientationEnum::INVALID };
  DICOMOrientation m_DesiredCoordinateOrientation{ OrientationEnum::LPS };

  PermuteOrderArrayType m_PermuteOrder{ { 0, 1, 2 } };
  FlipAxesArrayType     m_FlipAxes{ { false, false, false } };

}; // end of class

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkDICOMOrientImageFilter.hxx"
#endif

#endif
