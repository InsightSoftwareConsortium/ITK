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
 * DICOMOrientImageFilter depends on a set of constants that describe all possible
 * labeled according to the following scheme:
 * Directions are labeled in terms of following pairs:
 *   - Left and Right (Subject's left and right)
 *   - Anterior and Posterior (Subject's front and back)
 *   - Inferior and Superior (Subject's bottom and top, i.e. feet and head)
 *
 * The initials of these directions are used in a 3 letter code in the
 * enumerated type OrientationEnum.
 * The initials are given fastest moving index first, second fastest second,
 * third fastest third.
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
 * The output orientation is specified with SetDesiredCoordinateOrientation. The
 * given coordinate orientation is computed from the input image's direction
 * cosine matrix.
 *
 *
 *
 * \ingroup SimpleFilters
 */
template <typename TInputImage>
class ITK_TEMPLATE_EXPORT DICOMOrientImageFilter : public ImageToImageFilter<TInputImage, TInputImage>
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

  enum class CoordinateEnum : uint8_t
  {
    UNKNOWN = 0,
    Right = 2, // 0b0010
    Left = 3,
    Posterior = 4, // back - 0b0100
    Anterior = 5,  // front
    Inferior = 8,  // below - 0b1000
    Superior = 9   // above
  };

  enum class CoordinateMajornessTermsEnum : uint8_t
  {
    PrimaryMinor = 0,
    SecondaryMinor = 8,
    TertiaryMinor = 16
  };

  static constexpr uint32_t
  toOrientation(CoordinateEnum primary, CoordinateEnum secondary, CoordinateEnum tertiary)
  {
    return (static_cast<uint32_t>(primary) << static_cast<uint8_t>(CoordinateMajornessTermsEnum::PrimaryMinor)) +
           (static_cast<uint32_t>(secondary) << static_cast<uint8_t>(CoordinateMajornessTermsEnum::SecondaryMinor)) +
           (static_cast<uint32_t>(tertiary) << static_cast<uint8_t>(CoordinateMajornessTermsEnum::TertiaryMinor));
  }

  enum class OrientationEnum : uint32_t
  {
    INVALID = 0,
    RIP = toOrientation(CoordinateEnum::Right, CoordinateEnum::Inferior, CoordinateEnum::Posterior),
    LIP = toOrientation(CoordinateEnum::Left, CoordinateEnum::Inferior, CoordinateEnum::Posterior),
    RSP = toOrientation(CoordinateEnum::Right, CoordinateEnum::Superior, CoordinateEnum::Posterior),
    LSP = toOrientation(CoordinateEnum::Left, CoordinateEnum::Superior, CoordinateEnum::Posterior),
    RIA = toOrientation(CoordinateEnum::Right, CoordinateEnum::Inferior, CoordinateEnum::Anterior),
    LIA = toOrientation(CoordinateEnum::Left, CoordinateEnum::Inferior, CoordinateEnum::Anterior),
    RSA = toOrientation(CoordinateEnum::Right, CoordinateEnum::Superior, CoordinateEnum::Anterior),
    LSA = toOrientation(CoordinateEnum::Left, CoordinateEnum::Superior, CoordinateEnum::Anterior),

    IRP = toOrientation(CoordinateEnum::Inferior, CoordinateEnum::Right, CoordinateEnum::Posterior),
    ILP = toOrientation(CoordinateEnum::Inferior, CoordinateEnum::Left, CoordinateEnum::Posterior),
    SRP = toOrientation(CoordinateEnum::Superior, CoordinateEnum::Right, CoordinateEnum::Posterior),
    SLP = toOrientation(CoordinateEnum::Superior, CoordinateEnum::Left, CoordinateEnum::Posterior),
    IRA = toOrientation(CoordinateEnum::Inferior, CoordinateEnum::Right, CoordinateEnum::Anterior),
    ILA = toOrientation(CoordinateEnum::Inferior, CoordinateEnum::Left, CoordinateEnum::Anterior),
    SRA = toOrientation(CoordinateEnum::Superior, CoordinateEnum::Right, CoordinateEnum::Anterior),
    SLA = toOrientation(CoordinateEnum::Superior, CoordinateEnum::Left, CoordinateEnum::Anterior),

    RPI = toOrientation(CoordinateEnum::Right, CoordinateEnum::Posterior, CoordinateEnum::Inferior),
    LPI = toOrientation(CoordinateEnum::Left, CoordinateEnum::Posterior, CoordinateEnum::Inferior),
    RAI = toOrientation(CoordinateEnum::Right, CoordinateEnum::Anterior, CoordinateEnum::Inferior),
    LAI = toOrientation(CoordinateEnum::Left, CoordinateEnum::Anterior, CoordinateEnum::Inferior),
    RPS = toOrientation(CoordinateEnum::Right, CoordinateEnum::Posterior, CoordinateEnum::Superior),
    LPS = toOrientation(CoordinateEnum::Left, CoordinateEnum::Posterior, CoordinateEnum::Superior),
    RAS = toOrientation(CoordinateEnum::Right, CoordinateEnum::Anterior, CoordinateEnum::Superior),
    LAS = toOrientation(CoordinateEnum::Left, CoordinateEnum::Anterior, CoordinateEnum::Superior),

    PRI = toOrientation(CoordinateEnum::Posterior, CoordinateEnum::Right, CoordinateEnum::Inferior),
    PLI = toOrientation(CoordinateEnum::Posterior, CoordinateEnum::Left, CoordinateEnum::Inferior),
    ARI = toOrientation(CoordinateEnum::Anterior, CoordinateEnum::Right, CoordinateEnum::Inferior),
    ALI = toOrientation(CoordinateEnum::Anterior, CoordinateEnum::Left, CoordinateEnum::Inferior),
    PRS = toOrientation(CoordinateEnum::Posterior, CoordinateEnum::Right, CoordinateEnum::Superior),
    PLS = toOrientation(CoordinateEnum::Posterior, CoordinateEnum::Left, CoordinateEnum::Superior),
    ARS = toOrientation(CoordinateEnum::Anterior, CoordinateEnum::Right, CoordinateEnum::Superior),
    ALS = toOrientation(CoordinateEnum::Anterior, CoordinateEnum::Left, CoordinateEnum::Superior),

    IPR = toOrientation(CoordinateEnum::Inferior, CoordinateEnum::Posterior, CoordinateEnum::Right),
    SPR = toOrientation(CoordinateEnum::Superior, CoordinateEnum::Posterior, CoordinateEnum::Right),
    IAR = toOrientation(CoordinateEnum::Inferior, CoordinateEnum::Anterior, CoordinateEnum::Right),
    SAR = toOrientation(CoordinateEnum::Superior, CoordinateEnum::Anterior, CoordinateEnum::Right),
    IPL = toOrientation(CoordinateEnum::Inferior, CoordinateEnum::Posterior, CoordinateEnum::Left),
    SPL = toOrientation(CoordinateEnum::Superior, CoordinateEnum::Posterior, CoordinateEnum::Left),
    IAL = toOrientation(CoordinateEnum::Inferior, CoordinateEnum::Anterior, CoordinateEnum::Left),
    SAL = toOrientation(CoordinateEnum::Superior, CoordinateEnum::Anterior, CoordinateEnum::Left),

    PIR = toOrientation(CoordinateEnum::Posterior, CoordinateEnum::Inferior, CoordinateEnum::Right),
    PSR = toOrientation(CoordinateEnum::Posterior, CoordinateEnum::Superior, CoordinateEnum::Right),
    AIR = toOrientation(CoordinateEnum::Anterior, CoordinateEnum::Inferior, CoordinateEnum::Right),
    ASR = toOrientation(CoordinateEnum::Anterior, CoordinateEnum::Superior, CoordinateEnum::Right),
    PIL = toOrientation(CoordinateEnum::Posterior, CoordinateEnum::Inferior, CoordinateEnum::Left),
    PSL = toOrientation(CoordinateEnum::Posterior, CoordinateEnum::Superior, CoordinateEnum::Left),
    AIL = toOrientation(CoordinateEnum::Anterior, CoordinateEnum::Inferior, CoordinateEnum::Left),
    ASL = toOrientation(CoordinateEnum::Anterior, CoordinateEnum::Superior, CoordinateEnum::Left)
  };

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(DICOMOrientImageFilter, ImageToImageFilter);

  /** Get the orientation codes that defines the input coordinate transform.
   *
   *  This value changed during the execution of the Update in the pipeline. */
  itkGetEnumMacro(InputCoordinateOrientation, OrientationEnum);

  itkGetEnumMacro(DesiredCoordinateOrientation, OrientationEnum);
  void
  SetDesiredCoordinateOrientation(OrientationEnum newCode);

  inline void
  SetDesiredCoordinateDirection(const typename ImageType::DirectionType & DesiredDirection)
  {
    SetDesiredCoordinateOrientation(Self::DirectionCosinesToOrientation(DesiredDirection));
  }

  inline void
  SetDesiredCoordinateDirection(const std::string & desired)
  {
    auto            iter = m_StringToCode.find(desired);
    OrientationEnum desiredEnum = OrientationEnum::INVALID;
    if (iter != m_StringToCode.end())
    {
      desiredEnum = iter->second;
    }
    else
    {
      itkWarningMacro("Invalid desired coordinate direction string: \"" << desired << "\"!");
    }
    this->SetDesiredCoordinateOrientation(desiredEnum);
  }


  static OrientationEnum
  DirectionCosinesToOrientation(const DirectionType & dir);

  /** Get axes permute order. */
  itkGetConstReferenceMacro(PermuteOrder, PermuteOrderArrayType);

  /** Get flip axes. */
  itkGetConstReferenceMacro(FlipAxes, FlipAxesArrayType);


  /** DICOMOrientImageFilter produces an image which is a different
   *  meta-data than its input image.
   * \sa ProcessObject::GenerateOutputInformaton() */
  void
  GenerateOutputInformation() override;

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
  DeterminePermutationsAndFlips(const OrientationEnum desired, const OrientationEnum given);

  /** Returns true if a permute is required. Returns false otherwise. */
  bool
  NeedToPermute();

  /** Returns true if flipping is required. Returns false otherwise. */
  bool
  NeedToFlip();


  void
  SetInputCoordinateOrientation(OrientationEnum newCode);


  /** Single-threaded version of GenerateData. This filter delegates
   * to PermuteAxesImageFilter and FlipImageFilter. */
  void
  GenerateData() override;

private:
  std::string
  GetMajorAxisFromPatientRelativeDirectionCosine(double x, double y, double z);

  inline void
  helperAddCode(const std::string & str, OrientationEnum code)
  {
    assert(m_CodeToString.find(code) == m_CodeToString.end());
    assert(m_StringToCode.find(str) == m_StringToCode.end());

    m_CodeToString[code] = str;
    m_StringToCode[str] = code;
  }

  OrientationEnum m_InputCoordinateOrientation{ OrientationEnum::LPS };
  OrientationEnum m_DesiredCoordinateOrientation{ OrientationEnum::LPS };
  bool            m_UseImageDirection{ true };

  PermuteOrderArrayType m_PermuteOrder;
  FlipAxesArrayType     m_FlipAxes;

  std::map<std::string, OrientationEnum> m_StringToCode;
  std::map<OrientationEnum, std::string> m_CodeToString;
}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkDICOMOrientImageFilter.hxx"
#endif

#endif
