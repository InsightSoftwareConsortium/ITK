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
#ifndef itkAnatomicalOrientation_h
#define itkAnatomicalOrientation_h

#include "ITKCommonExport.h"
#include "itkImageBase.h"
#ifndef ITK_FUTURE_LEGACY_REMOVE
#  include "itkSpatialOrientation.h"
#endif
#include <map>
#include <string>

namespace itk
{

/** \class AnatomicalOrientation
 * \brief Representations of anatomical orientations and methods to convert between conventions.
 *
 *  Defines patient specific anatomical names to the XYZ axes of a 3D image.
 *
 * A class instances holds the patient orientation stored as an enumerated type. Conversion to different representations
 * such as strings and direction cosine matrices are supported.
 *
 * The use of unambiguous anatomical orientation names such as "RightToLeft" is preferred, where "Right" is the
 * "from direction" and the negative direction of the coordinates, while "Left" is the "to direction" and the positive
 * direction. The following is an unambiguous construction of an AnatomicalOrientation object:
 *
 * \code
 *  AnatomicalOrientation(AnatomicalOrientation::CoordinateEnum::RightToLeft,
 *                        AnatomicalOrientation::CoordinateEnum::AnteriorToPosterior,
 *                        AnatomicalOrientation::CoordinateEnum::InferiorToSuperior);
 * \endcode
 *
 *
 * \ingroup ITKCommon
 */
class ITKCommon_EXPORT AnatomicalOrientation
{
public:
  static constexpr unsigned int Dimension = 3;
  using DirectionType = typename ImageBase<Dimension>::DirectionType;
  static constexpr unsigned int ImageDimension = Dimension;

#ifndef ITK_FUTURE_LEGACY_REMOVE
  using LegacyOrientationType = SpatialOrientationEnums::ValidCoordinateOrientations;
#endif

  // Anatomical names for an axis.
  //
  enum class CoordinateEnum : uint8_t
  {
    UNKNOWN = 0,
    RightToLeft = 2, ///< 0b0010
    LeftToRight = 3,
    PosteriorToAnterior = 4, ///< to front - 0b0100
    AnteriorToPosterior = 5, ///< to back
    InferiorToSuperior = 8,  ///< to head - 0b1000
    SuperiorToInferior = 9,  ///< to foot
  };

protected:
  enum class CoordinateMajornessTermsEnum : uint8_t
  {
    PrimaryMinor = 0,
    SecondaryMinor = 8,
    TertiaryMinor = 16
  };

  template <CoordinateEnum VPrimary, CoordinateEnum VSecondary, CoordinateEnum VTertiary>
  static constexpr uint32_t m_OrientationValue =
    (static_cast<uint32_t>(VPrimary) << static_cast<uint8_t>(CoordinateMajornessTermsEnum::PrimaryMinor)) +
    (static_cast<uint32_t>(VSecondary) << static_cast<uint8_t>(CoordinateMajornessTermsEnum::SecondaryMinor)) +
    (static_cast<uint32_t>(VTertiary) << static_cast<uint8_t>(CoordinateMajornessTermsEnum::TertiaryMinor));

  CoordinateEnum
  GetCoordinateTerm(CoordinateMajornessTermsEnum cmt) const;

public:
  // Enumerated acronyms based on the positive or "To" direction of the anatomical coordinates.
  enum class PositiveEnum : uint32_t

  {
    INVALID = 0,

    RIP = m_OrientationValue<CoordinateEnum::LeftToRight,
                             CoordinateEnum::SuperiorToInferior,
                             CoordinateEnum::AnteriorToPosterior>,
    LIP = m_OrientationValue<CoordinateEnum::RightToLeft,
                             CoordinateEnum::SuperiorToInferior,
                             CoordinateEnum::AnteriorToPosterior>,
    RSP = m_OrientationValue<CoordinateEnum::LeftToRight,
                             CoordinateEnum::InferiorToSuperior,
                             CoordinateEnum::AnteriorToPosterior>,
    LSP = m_OrientationValue<CoordinateEnum::RightToLeft,
                             CoordinateEnum::InferiorToSuperior,
                             CoordinateEnum::AnteriorToPosterior>,
    RIA = m_OrientationValue<CoordinateEnum::LeftToRight,
                             CoordinateEnum::SuperiorToInferior,
                             CoordinateEnum::PosteriorToAnterior>,
    LIA = m_OrientationValue<CoordinateEnum::RightToLeft,
                             CoordinateEnum::SuperiorToInferior,
                             CoordinateEnum::PosteriorToAnterior>,
    RSA = m_OrientationValue<CoordinateEnum::LeftToRight,
                             CoordinateEnum::InferiorToSuperior,
                             CoordinateEnum::PosteriorToAnterior>,
    LSA = m_OrientationValue<CoordinateEnum::RightToLeft,
                             CoordinateEnum::InferiorToSuperior,
                             CoordinateEnum::PosteriorToAnterior>,

    IRP = m_OrientationValue<CoordinateEnum::SuperiorToInferior,
                             CoordinateEnum::LeftToRight,
                             CoordinateEnum::AnteriorToPosterior>,
    ILP = m_OrientationValue<CoordinateEnum::SuperiorToInferior,
                             CoordinateEnum::RightToLeft,
                             CoordinateEnum::AnteriorToPosterior>,
    SRP = m_OrientationValue<CoordinateEnum::InferiorToSuperior,
                             CoordinateEnum::LeftToRight,
                             CoordinateEnum::AnteriorToPosterior>,
    SLP = m_OrientationValue<CoordinateEnum::InferiorToSuperior,
                             CoordinateEnum::RightToLeft,
                             CoordinateEnum::AnteriorToPosterior>,
    IRA = m_OrientationValue<CoordinateEnum::SuperiorToInferior,
                             CoordinateEnum::LeftToRight,
                             CoordinateEnum::PosteriorToAnterior>,
    ILA = m_OrientationValue<CoordinateEnum::SuperiorToInferior,
                             CoordinateEnum::RightToLeft,
                             CoordinateEnum::PosteriorToAnterior>,
    SRA = m_OrientationValue<CoordinateEnum::InferiorToSuperior,
                             CoordinateEnum::LeftToRight,
                             CoordinateEnum::PosteriorToAnterior>,
    SLA = m_OrientationValue<CoordinateEnum::InferiorToSuperior,
                             CoordinateEnum::RightToLeft,
                             CoordinateEnum::PosteriorToAnterior>,

    RPI = m_OrientationValue<CoordinateEnum::LeftToRight,
                             CoordinateEnum::AnteriorToPosterior,
                             CoordinateEnum::SuperiorToInferior>,
    LPI = m_OrientationValue<CoordinateEnum::RightToLeft,
                             CoordinateEnum::AnteriorToPosterior,
                             CoordinateEnum::SuperiorToInferior>,
    RAI = m_OrientationValue<CoordinateEnum::LeftToRight,
                             CoordinateEnum::PosteriorToAnterior,
                             CoordinateEnum::SuperiorToInferior>,
    LAI = m_OrientationValue<CoordinateEnum::RightToLeft,
                             CoordinateEnum::PosteriorToAnterior,
                             CoordinateEnum::SuperiorToInferior>,
    RPS = m_OrientationValue<CoordinateEnum::LeftToRight,
                             CoordinateEnum::AnteriorToPosterior,
                             CoordinateEnum::InferiorToSuperior>,
    LPS = m_OrientationValue<CoordinateEnum::RightToLeft,
                             CoordinateEnum::AnteriorToPosterior,
                             CoordinateEnum::InferiorToSuperior>,
    RAS = m_OrientationValue<CoordinateEnum::LeftToRight,
                             CoordinateEnum::PosteriorToAnterior,
                             CoordinateEnum::InferiorToSuperior>,
    LAS = m_OrientationValue<CoordinateEnum::RightToLeft,
                             CoordinateEnum::PosteriorToAnterior,
                             CoordinateEnum::InferiorToSuperior>,

    PRI = m_OrientationValue<CoordinateEnum::AnteriorToPosterior,
                             CoordinateEnum::LeftToRight,
                             CoordinateEnum::SuperiorToInferior>,
    PLI = m_OrientationValue<CoordinateEnum::AnteriorToPosterior,
                             CoordinateEnum::RightToLeft,
                             CoordinateEnum::SuperiorToInferior>,
    ARI = m_OrientationValue<CoordinateEnum::PosteriorToAnterior,
                             CoordinateEnum::LeftToRight,
                             CoordinateEnum::SuperiorToInferior>,
    ALI = m_OrientationValue<CoordinateEnum::PosteriorToAnterior,
                             CoordinateEnum::RightToLeft,
                             CoordinateEnum::SuperiorToInferior>,
    PRS = m_OrientationValue<CoordinateEnum::AnteriorToPosterior,
                             CoordinateEnum::LeftToRight,
                             CoordinateEnum::InferiorToSuperior>,
    PLS = m_OrientationValue<CoordinateEnum::AnteriorToPosterior,
                             CoordinateEnum::RightToLeft,
                             CoordinateEnum::InferiorToSuperior>,
    ARS = m_OrientationValue<CoordinateEnum::PosteriorToAnterior,
                             CoordinateEnum::LeftToRight,
                             CoordinateEnum::InferiorToSuperior>,
    ALS = m_OrientationValue<CoordinateEnum::PosteriorToAnterior,
                             CoordinateEnum::RightToLeft,
                             CoordinateEnum::InferiorToSuperior>,

    IPR = m_OrientationValue<CoordinateEnum::SuperiorToInferior,
                             CoordinateEnum::AnteriorToPosterior,
                             CoordinateEnum::LeftToRight>,
    SPR = m_OrientationValue<CoordinateEnum::InferiorToSuperior,
                             CoordinateEnum::AnteriorToPosterior,
                             CoordinateEnum::LeftToRight>,
    IAR = m_OrientationValue<CoordinateEnum::SuperiorToInferior,
                             CoordinateEnum::PosteriorToAnterior,
                             CoordinateEnum::LeftToRight>,
    SAR = m_OrientationValue<CoordinateEnum::InferiorToSuperior,
                             CoordinateEnum::PosteriorToAnterior,
                             CoordinateEnum::LeftToRight>,
    IPL = m_OrientationValue<CoordinateEnum::SuperiorToInferior,
                             CoordinateEnum::AnteriorToPosterior,
                             CoordinateEnum::RightToLeft>,
    SPL = m_OrientationValue<CoordinateEnum::InferiorToSuperior,
                             CoordinateEnum::AnteriorToPosterior,
                             CoordinateEnum::RightToLeft>,
    IAL = m_OrientationValue<CoordinateEnum::SuperiorToInferior,
                             CoordinateEnum::PosteriorToAnterior,
                             CoordinateEnum::RightToLeft>,
    SAL = m_OrientationValue<CoordinateEnum::InferiorToSuperior,
                             CoordinateEnum::PosteriorToAnterior,
                             CoordinateEnum::RightToLeft>,

    PIR = m_OrientationValue<CoordinateEnum::AnteriorToPosterior,
                             CoordinateEnum::SuperiorToInferior,
                             CoordinateEnum::LeftToRight>,
    PSR = m_OrientationValue<CoordinateEnum::AnteriorToPosterior,
                             CoordinateEnum::InferiorToSuperior,
                             CoordinateEnum::LeftToRight>,
    AIR = m_OrientationValue<CoordinateEnum::PosteriorToAnterior,
                             CoordinateEnum::SuperiorToInferior,
                             CoordinateEnum::LeftToRight>,
    ASR = m_OrientationValue<CoordinateEnum::PosteriorToAnterior,
                             CoordinateEnum::InferiorToSuperior,
                             CoordinateEnum::LeftToRight>,
    PIL = m_OrientationValue<CoordinateEnum::AnteriorToPosterior,
                             CoordinateEnum::SuperiorToInferior,
                             CoordinateEnum::RightToLeft>,
    PSL = m_OrientationValue<CoordinateEnum::AnteriorToPosterior,
                             CoordinateEnum::InferiorToSuperior,
                             CoordinateEnum::RightToLeft>,
    AIL = m_OrientationValue<CoordinateEnum::PosteriorToAnterior,
                             CoordinateEnum::SuperiorToInferior,
                             CoordinateEnum::RightToLeft>,
    ASL = m_OrientationValue<CoordinateEnum::PosteriorToAnterior,
                             CoordinateEnum::InferiorToSuperior,
                             CoordinateEnum::RightToLeft>
  };

  // Enumerated acronyms based on the negative or "From" direction of the anatomical coordinates.
  enum class NegativeEnum : uint32_t

  {
    INVALID = 0,

    RIP = m_OrientationValue<CoordinateEnum::RightToLeft,
                             CoordinateEnum::InferiorToSuperior,
                             CoordinateEnum::PosteriorToAnterior>,
    LIP = m_OrientationValue<CoordinateEnum::LeftToRight,
                             CoordinateEnum::InferiorToSuperior,
                             CoordinateEnum::PosteriorToAnterior>,
    RSP = m_OrientationValue<CoordinateEnum::RightToLeft,
                             CoordinateEnum::SuperiorToInferior,
                             CoordinateEnum::PosteriorToAnterior>,
    LSP = m_OrientationValue<CoordinateEnum::LeftToRight,
                             CoordinateEnum::SuperiorToInferior,
                             CoordinateEnum::PosteriorToAnterior>,
    RIA = m_OrientationValue<CoordinateEnum::RightToLeft,
                             CoordinateEnum::InferiorToSuperior,
                             CoordinateEnum::AnteriorToPosterior>,
    LIA = m_OrientationValue<CoordinateEnum::LeftToRight,
                             CoordinateEnum::InferiorToSuperior,
                             CoordinateEnum::AnteriorToPosterior>,
    RSA = m_OrientationValue<CoordinateEnum::RightToLeft,
                             CoordinateEnum::SuperiorToInferior,
                             CoordinateEnum::AnteriorToPosterior>,
    LSA = m_OrientationValue<CoordinateEnum::LeftToRight,
                             CoordinateEnum::SuperiorToInferior,
                             CoordinateEnum::AnteriorToPosterior>,

    IRP = m_OrientationValue<CoordinateEnum::InferiorToSuperior,
                             CoordinateEnum::RightToLeft,
                             CoordinateEnum::PosteriorToAnterior>,
    ILP = m_OrientationValue<CoordinateEnum::InferiorToSuperior,
                             CoordinateEnum::LeftToRight,
                             CoordinateEnum::PosteriorToAnterior>,
    SRP = m_OrientationValue<CoordinateEnum::SuperiorToInferior,
                             CoordinateEnum::RightToLeft,
                             CoordinateEnum::PosteriorToAnterior>,
    SLP = m_OrientationValue<CoordinateEnum::SuperiorToInferior,
                             CoordinateEnum::LeftToRight,
                             CoordinateEnum::PosteriorToAnterior>,
    IRA = m_OrientationValue<CoordinateEnum::InferiorToSuperior,
                             CoordinateEnum::RightToLeft,
                             CoordinateEnum::AnteriorToPosterior>,
    ILA = m_OrientationValue<CoordinateEnum::InferiorToSuperior,
                             CoordinateEnum::LeftToRight,
                             CoordinateEnum::AnteriorToPosterior>,
    SRA = m_OrientationValue<CoordinateEnum::SuperiorToInferior,
                             CoordinateEnum::RightToLeft,
                             CoordinateEnum::AnteriorToPosterior>,
    SLA = m_OrientationValue<CoordinateEnum::SuperiorToInferior,
                             CoordinateEnum::LeftToRight,
                             CoordinateEnum::AnteriorToPosterior>,

    RPI = m_OrientationValue<CoordinateEnum::RightToLeft,
                             CoordinateEnum::PosteriorToAnterior,
                             CoordinateEnum::InferiorToSuperior>,
    LPI = m_OrientationValue<CoordinateEnum::LeftToRight,
                             CoordinateEnum::PosteriorToAnterior,
                             CoordinateEnum::InferiorToSuperior>,
    RAI = m_OrientationValue<CoordinateEnum::RightToLeft,
                             CoordinateEnum::AnteriorToPosterior,
                             CoordinateEnum::InferiorToSuperior>,
    LAI = m_OrientationValue<CoordinateEnum::LeftToRight,
                             CoordinateEnum::AnteriorToPosterior,
                             CoordinateEnum::InferiorToSuperior>,
    RPS = m_OrientationValue<CoordinateEnum::RightToLeft,
                             CoordinateEnum::PosteriorToAnterior,
                             CoordinateEnum::SuperiorToInferior>,
    LPS = m_OrientationValue<CoordinateEnum::LeftToRight,
                             CoordinateEnum::PosteriorToAnterior,
                             CoordinateEnum::SuperiorToInferior>,
    RAS = m_OrientationValue<CoordinateEnum::RightToLeft,
                             CoordinateEnum::AnteriorToPosterior,
                             CoordinateEnum::SuperiorToInferior>,
    LAS = m_OrientationValue<CoordinateEnum::LeftToRight,
                             CoordinateEnum::AnteriorToPosterior,
                             CoordinateEnum::SuperiorToInferior>,

    PRI = m_OrientationValue<CoordinateEnum::PosteriorToAnterior,
                             CoordinateEnum::RightToLeft,
                             CoordinateEnum::InferiorToSuperior>,
    PLI = m_OrientationValue<CoordinateEnum::PosteriorToAnterior,
                             CoordinateEnum::LeftToRight,
                             CoordinateEnum::InferiorToSuperior>,
    ARI = m_OrientationValue<CoordinateEnum::AnteriorToPosterior,
                             CoordinateEnum::RightToLeft,
                             CoordinateEnum::InferiorToSuperior>,
    ALI = m_OrientationValue<CoordinateEnum::AnteriorToPosterior,
                             CoordinateEnum::LeftToRight,
                             CoordinateEnum::InferiorToSuperior>,
    PRS = m_OrientationValue<CoordinateEnum::PosteriorToAnterior,
                             CoordinateEnum::RightToLeft,
                             CoordinateEnum::SuperiorToInferior>,
    PLS = m_OrientationValue<CoordinateEnum::PosteriorToAnterior,
                             CoordinateEnum::LeftToRight,
                             CoordinateEnum::SuperiorToInferior>,
    ARS = m_OrientationValue<CoordinateEnum::AnteriorToPosterior,
                             CoordinateEnum::RightToLeft,
                             CoordinateEnum::SuperiorToInferior>,
    ALS = m_OrientationValue<CoordinateEnum::AnteriorToPosterior,
                             CoordinateEnum::LeftToRight,
                             CoordinateEnum::SuperiorToInferior>,

    IPR = m_OrientationValue<CoordinateEnum::InferiorToSuperior,
                             CoordinateEnum::PosteriorToAnterior,
                             CoordinateEnum::RightToLeft>,
    SPR = m_OrientationValue<CoordinateEnum::SuperiorToInferior,
                             CoordinateEnum::PosteriorToAnterior,
                             CoordinateEnum::RightToLeft>,
    IAR = m_OrientationValue<CoordinateEnum::InferiorToSuperior,
                             CoordinateEnum::AnteriorToPosterior,
                             CoordinateEnum::RightToLeft>,
    SAR = m_OrientationValue<CoordinateEnum::SuperiorToInferior,
                             CoordinateEnum::AnteriorToPosterior,
                             CoordinateEnum::RightToLeft>,
    IPL = m_OrientationValue<CoordinateEnum::InferiorToSuperior,
                             CoordinateEnum::PosteriorToAnterior,
                             CoordinateEnum::LeftToRight>,
    SPL = m_OrientationValue<CoordinateEnum::SuperiorToInferior,
                             CoordinateEnum::PosteriorToAnterior,
                             CoordinateEnum::LeftToRight>,
    IAL = m_OrientationValue<CoordinateEnum::InferiorToSuperior,
                             CoordinateEnum::AnteriorToPosterior,
                             CoordinateEnum::LeftToRight>,
    SAL = m_OrientationValue<CoordinateEnum::SuperiorToInferior,
                             CoordinateEnum::AnteriorToPosterior,
                             CoordinateEnum::LeftToRight>,

    PIR = m_OrientationValue<CoordinateEnum::PosteriorToAnterior,
                             CoordinateEnum::InferiorToSuperior,
                             CoordinateEnum::RightToLeft>,
    PSR = m_OrientationValue<CoordinateEnum::PosteriorToAnterior,
                             CoordinateEnum::SuperiorToInferior,
                             CoordinateEnum::RightToLeft>,
    AIR = m_OrientationValue<CoordinateEnum::AnteriorToPosterior,
                             CoordinateEnum::InferiorToSuperior,
                             CoordinateEnum::RightToLeft>,
    ASR = m_OrientationValue<CoordinateEnum::AnteriorToPosterior,
                             CoordinateEnum::SuperiorToInferior,
                             CoordinateEnum::RightToLeft>,
    PIL = m_OrientationValue<CoordinateEnum::PosteriorToAnterior,
                             CoordinateEnum::InferiorToSuperior,
                             CoordinateEnum::LeftToRight>,
    PSL = m_OrientationValue<CoordinateEnum::PosteriorToAnterior,
                             CoordinateEnum::SuperiorToInferior,
                             CoordinateEnum::LeftToRight>,
    AIL = m_OrientationValue<CoordinateEnum::AnteriorToPosterior,
                             CoordinateEnum::InferiorToSuperior,
                             CoordinateEnum::LeftToRight>,
    ASL = m_OrientationValue<CoordinateEnum::AnteriorToPosterior,
                             CoordinateEnum::SuperiorToInferior,
                             CoordinateEnum::LeftToRight>

  };


  /** \brief Initialize with CoordinateEnum's from separate axes.
   *
   * If multiple CoordinateEnums are from the same axes then the Orientation value is INVALID.
   */
  constexpr AnatomicalOrientation(CoordinateEnum primary, CoordinateEnum secondary, CoordinateEnum tertiary)
    : m_Value(
        SameOrientationAxes(primary, secondary) || SameOrientationAxes(primary, tertiary) ||
            SameOrientationAxes(secondary, tertiary)
          ? PositiveEnum::INVALID
          : static_cast<PositiveEnum>(
              (static_cast<uint32_t>(primary) << static_cast<uint8_t>(CoordinateMajornessTermsEnum::PrimaryMinor)) +
              (static_cast<uint32_t>(secondary) << static_cast<uint8_t>(CoordinateMajornessTermsEnum::SecondaryMinor)) +
              (static_cast<uint32_t>(tertiary) << static_cast<uint8_t>(CoordinateMajornessTermsEnum::TertiaryMinor))))
  {}


  constexpr AnatomicalOrientation(PositiveEnum toOrientation)
    : m_Value(toOrientation)
  {}


  constexpr AnatomicalOrientation(NegativeEnum fromOrientation)
    : m_Value(PositiveEnum(static_cast<uint32_t>(fromOrientation)))
  {}

#ifndef ITK_FUTURE_LEGACY_REMOVE
  /** \brief Conversion from Legacy SpatialOrientation
   *
   * @param legacyOrientation
   */
#  if defined(ITK_LEGACY_REMOVE) && !defined(ITK_LEGACY_SILENT) && !defined(ITK_WRAPPING)
  [[deprecated("Use the AnatomicalOrientation::FromEnum type instead.")]]
#  endif
  AnatomicalOrientation(LegacyOrientationType legacyOrientation);
#endif

  /** Conversion for a Direction Cosine Matrix to the closest anatomical orientation. Any partial axis rotations are
   *  rounded to the nearest axis, and lost in this conversion.
   */
  explicit AnatomicalOrientation(const DirectionType & d)
    : m_Value(ConvertDirectionToPositiveEnum(d))
  {}

  /** Creates a AnatomicalOrientation from a string with the PositiveEnum encoding. The string is case-insensitive. If
   * the string is not a valid encoding then the orientation is set to INVALID.
   */
  static AnatomicalOrientation
  CreateFromPositiveStringEncoding(std::string str);

  // Same as CreateFromPositiveStringEncoding but for the NegativeEnum encoding.
  static AnatomicalOrientation
  CreateFromNegativeStringEncoding(std::string str);

  operator PositiveEnum() const { return m_Value; }

  /** Returns the PositiveEnum encoding as a string. The string is always upper case. */
  std::string
  GetAsPositiveStringEncoding() const;

  /** Returns the NegativeEnum encoding as a string. The string is always upper case. */
  std::string
  GetAsNegativeStringEncoding() const;

  /** An involution to convert between "positive" and "negative" single character encoding strings.
   *
   * For example the string "RAS" is converted to "LPI" and vice versa.
   *
   * The input maybe upper or lower case, while the output is always upper case.
   * There is no check that the input is a valid encoding.
   *
   * */
  static std::string
  ConvertStringEncoding(std::string str);

  /** \brief Return the direction cosine matrix for the orientation. */
  DirectionType
  GetAsDirection() const
  {
    return ConvertPositiveEnumToDirection(m_Value);
  }

  PositiveEnum
  GetAsPositiveOrientation() const
  {
    return m_Value;
  }

  NegativeEnum
  GetAsNegativeOrientation() const
  {
    return NegativeEnum(uint32_t(this->m_Value));
  }

  CoordinateEnum
  GetPrimaryTerm() const
  {
    return GetCoordinateTerm(CoordinateMajornessTermsEnum::PrimaryMinor);
  }

  CoordinateEnum
  GetSecondaryTerm() const
  {
    return GetCoordinateTerm(CoordinateMajornessTermsEnum::SecondaryMinor);
  }

  CoordinateEnum
  GetTertiaryTerm() const
  {
    return GetCoordinateTerm(CoordinateMajornessTermsEnum::TertiaryMinor);
  }

  std::array<CoordinateEnum, 3>
  GetTerms() const
  {
    return { GetPrimaryTerm(), GetSecondaryTerm(), GetTertiaryTerm() };
  }

  static constexpr bool
  SameOrientationAxes(CoordinateEnum a, CoordinateEnum b)
  {
    const uint8_t AxisField = ~1; // mask the lowest bit
    return (static_cast<uint8_t>(a) & AxisField) == (static_cast<uint8_t>(b) & AxisField);
  }


  friend ITKCommon_EXPORT std::ostream &
                          operator<<(std::ostream & out, PositiveEnum value);

protected:
  /** \brief Return the direction cosine matrix for a orientation. */
  static DirectionType ConvertPositiveEnumToDirection(PositiveEnum);


  /** \brief Return the closest orientation for a direction cosine matrix. */
  static PositiveEnum
  ConvertDirectionToPositiveEnum(const DirectionType & dir);


  /** \brief Return the global instance of the map from orientation enum to strings.
   *
   * The implementation uses a function static local variable so the global is created only if needed, only once.
   */
  static const std::map<PositiveEnum, std::string> &
  GetCodeToString();

  /** \brief Return the global instance of the map from string to orientation enum.
   */
  static const std::map<std::string, PositiveEnum> &
  GetStringToCode();

  PositiveEnum m_Value;
};

/** Outputs unambiguous anatomical orientation names such as "right-to-left". */
ITKCommon_EXPORT std::ostream &
                 operator<<(std::ostream & out, typename AnatomicalOrientation::CoordinateEnum value);

/** Outputs the PositiveEnum encoding as a string such as "LPS". */
ITKCommon_EXPORT std::ostream &
                 operator<<(std::ostream & out, typename AnatomicalOrientation::PositiveEnum value);


/** Outputs the NegativeEnum encoding as a string such as "RAI" */
ITKCommon_EXPORT std::ostream &
                 operator<<(std::ostream & out, typename AnatomicalOrientation::NegativeEnum value);

ITKCommon_EXPORT std::ostream &
                 operator<<(std::ostream & out, const AnatomicalOrientation & orientation);


} // end namespace itk


#endif
