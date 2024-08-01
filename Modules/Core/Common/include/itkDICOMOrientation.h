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
#ifndef itkDICOMOrientation_h
#define itkDICOMOrientation_h

#include "ITKCommonExport.h"
#include "itkImageBase.h"
#ifndef ITK_FUTURE_LEGACY_REMOVE
#include "itkSpatialOrientation.h"
#endif
#include <map>
#include <string>

namespace itk
{

/** \class DICOMOrientation
 * \brief A supporting class for DICOMOrientImageFilter.
 *
 *  Defines enums to for patient orientation in a compatible way with DICOM.
 *
 *  Instances hold the patient orientation enum and allow conversion to and from enums, string and direction cosine
 * matrices. Conversions from a direction cosine matrix is approximated with the orientation of the closes axes.
 *
 * \ingroup ITKCommon
 */
class ITKCommon_EXPORT DICOMOrientation
{
public:
  constexpr static unsigned int Dimension = 3;
  using DirectionType = typename ImageBase<Dimension>::DirectionType;
  constexpr static unsigned int ImageDimension = Dimension;

#ifndef ITK_FUTURE_LEGACY_REMOVE
  using LegacyOrientationType = SpatialOrientationEnums::ValidCoordinateOrientations;
#endif

  enum class CoordinateEnum : uint8_t
  {
    UNKNOWN = 0,
    Left = 2,  ///< 0b0010
    RightToLeft = 2,
    Right = 3,
    LeftToRight = 3,
    Anterior = 4,  ///< front - 0b0100
    PosteriorToAnterior = 4,
    Posterior = 5, ///< back
    AnteriorToPosterior = 5,
    Superior = 8,   ///< above - 0b1000
    InferiorToSuperior = 8,
    Inferior = 9,  ///< bottom
    SuperiorToInferior = 9,
  };

private:
  enum class CoordinateMajornessTermsEnum : uint8_t
  {
    PrimaryMinor = 0,
    SecondaryMinor = 8,
    TertiaryMinor = 16
  };

  constexpr static uint32_t
  toOrientation(const CoordinateEnum primary, const CoordinateEnum secondary, const CoordinateEnum tertiary)
  {
    return (static_cast<uint32_t>(primary) << static_cast<uint8_t>(CoordinateMajornessTermsEnum::PrimaryMinor)) +
           (static_cast<uint32_t>(secondary) << static_cast<uint8_t>(CoordinateMajornessTermsEnum::SecondaryMinor)) +
           (static_cast<uint32_t>(tertiary) << static_cast<uint8_t>(CoordinateMajornessTermsEnum::TertiaryMinor));
  }

  CoordinateEnum
  GetCoordinateTerm(CoordinateMajornessTermsEnum cmt) const;

public:
#if 0
// When this is enabled the following error occurs:
//ITK/Modules/Core/Common/include/itkDICOMOrientation.h:93:11: error: enumerator value is not a constant expression
//    RIP = ITK_ORIENTATIONENUM(CoordinateEnum::Right, CoordinateEnum::Inferior, CoordinateEnum::Posterior),

#  define ITK_ORIENTATIONENUM DICOMOrientation::toOrientation
#else

#  define ITK_ORIENTATIONENUM(P, S, T)                                                                                 \
    (static_cast<uint32_t>(P) << static_cast<uint8_t>(CoordinateMajornessTermsEnum::PrimaryMinor)) +                   \
      (static_cast<uint32_t>(S) << static_cast<uint8_t>(CoordinateMajornessTermsEnum::SecondaryMinor)) +               \
      (static_cast<uint32_t>(T) << static_cast<uint8_t>(CoordinateMajornessTermsEnum::TertiaryMinor))

#endif

  enum class OrientationEnum : uint32_t

  {
    INVALID = 0,
    RIP = ITK_ORIENTATIONENUM(CoordinateEnum::LeftToRight, CoordinateEnum::SuperiorToInferior, CoordinateEnum::AnteriorToPosterior),
    LIP = ITK_ORIENTATIONENUM(CoordinateEnum::RightToLeft, CoordinateEnum::SuperiorToInferior, CoordinateEnum::AnteriorToPosterior),
    RSP = ITK_ORIENTATIONENUM(CoordinateEnum::LeftToRight, CoordinateEnum::InferiorToSuperior, CoordinateEnum::AnteriorToPosterior),
    LSP = ITK_ORIENTATIONENUM(CoordinateEnum::RightToLeft, CoordinateEnum::InferiorToSuperior, CoordinateEnum::AnteriorToPosterior),
    RIA = ITK_ORIENTATIONENUM(CoordinateEnum::LeftToRight, CoordinateEnum::SuperiorToInferior, CoordinateEnum::PosteriorToAnterior),
    LIA = ITK_ORIENTATIONENUM(CoordinateEnum::RightToLeft, CoordinateEnum::SuperiorToInferior, CoordinateEnum::PosteriorToAnterior),
    RSA = ITK_ORIENTATIONENUM(CoordinateEnum::LeftToRight, CoordinateEnum::InferiorToSuperior, CoordinateEnum::PosteriorToAnterior),
    LSA = ITK_ORIENTATIONENUM(CoordinateEnum::RightToLeft, CoordinateEnum::InferiorToSuperior, CoordinateEnum::PosteriorToAnterior),

    IRP = ITK_ORIENTATIONENUM(CoordinateEnum::SuperiorToInferior, CoordinateEnum::LeftToRight, CoordinateEnum::AnteriorToPosterior),
    ILP = ITK_ORIENTATIONENUM(CoordinateEnum::SuperiorToInferior, CoordinateEnum::RightToLeft, CoordinateEnum::AnteriorToPosterior),
    SRP = ITK_ORIENTATIONENUM(CoordinateEnum::InferiorToSuperior, CoordinateEnum::LeftToRight, CoordinateEnum::AnteriorToPosterior),
    SLP = ITK_ORIENTATIONENUM(CoordinateEnum::InferiorToSuperior, CoordinateEnum::RightToLeft, CoordinateEnum::AnteriorToPosterior),
    IRA = ITK_ORIENTATIONENUM(CoordinateEnum::SuperiorToInferior, CoordinateEnum::LeftToRight, CoordinateEnum::PosteriorToAnterior),
    ILA = ITK_ORIENTATIONENUM(CoordinateEnum::SuperiorToInferior, CoordinateEnum::RightToLeft, CoordinateEnum::PosteriorToAnterior),
    SRA = ITK_ORIENTATIONENUM(CoordinateEnum::InferiorToSuperior, CoordinateEnum::LeftToRight, CoordinateEnum::PosteriorToAnterior),
    SLA = ITK_ORIENTATIONENUM(CoordinateEnum::InferiorToSuperior, CoordinateEnum::RightToLeft, CoordinateEnum::PosteriorToAnterior),

    RPI = ITK_ORIENTATIONENUM(CoordinateEnum::LeftToRight, CoordinateEnum::AnteriorToPosterior, CoordinateEnum::SuperiorToInferior),
    LPI = ITK_ORIENTATIONENUM(CoordinateEnum::RightToLeft, CoordinateEnum::AnteriorToPosterior, CoordinateEnum::SuperiorToInferior),
    RAI = ITK_ORIENTATIONENUM(CoordinateEnum::LeftToRight, CoordinateEnum::PosteriorToAnterior, CoordinateEnum::SuperiorToInferior),
    LAI = ITK_ORIENTATIONENUM(CoordinateEnum::RightToLeft, CoordinateEnum::PosteriorToAnterior, CoordinateEnum::SuperiorToInferior),
    RPS = ITK_ORIENTATIONENUM(CoordinateEnum::LeftToRight, CoordinateEnum::AnteriorToPosterior, CoordinateEnum::InferiorToSuperior),
    LPS = ITK_ORIENTATIONENUM(CoordinateEnum::RightToLeft, CoordinateEnum::AnteriorToPosterior, CoordinateEnum::InferiorToSuperior),
    RAS = ITK_ORIENTATIONENUM(CoordinateEnum::LeftToRight, CoordinateEnum::PosteriorToAnterior, CoordinateEnum::InferiorToSuperior),
    LAS = ITK_ORIENTATIONENUM(CoordinateEnum::RightToLeft, CoordinateEnum::PosteriorToAnterior, CoordinateEnum::InferiorToSuperior),

    PRI = ITK_ORIENTATIONENUM(CoordinateEnum::AnteriorToPosterior, CoordinateEnum::LeftToRight, CoordinateEnum::SuperiorToInferior),
    PLI = ITK_ORIENTATIONENUM(CoordinateEnum::AnteriorToPosterior, CoordinateEnum::RightToLeft, CoordinateEnum::SuperiorToInferior),
    ARI = ITK_ORIENTATIONENUM(CoordinateEnum::PosteriorToAnterior, CoordinateEnum::LeftToRight, CoordinateEnum::SuperiorToInferior),
    ALI = ITK_ORIENTATIONENUM(CoordinateEnum::PosteriorToAnterior, CoordinateEnum::RightToLeft, CoordinateEnum::SuperiorToInferior),
    PRS = ITK_ORIENTATIONENUM(CoordinateEnum::AnteriorToPosterior, CoordinateEnum::LeftToRight, CoordinateEnum::InferiorToSuperior),
    PLS = ITK_ORIENTATIONENUM(CoordinateEnum::AnteriorToPosterior, CoordinateEnum::RightToLeft, CoordinateEnum::InferiorToSuperior),
    ARS = ITK_ORIENTATIONENUM(CoordinateEnum::PosteriorToAnterior, CoordinateEnum::LeftToRight, CoordinateEnum::InferiorToSuperior),
    ALS = ITK_ORIENTATIONENUM(CoordinateEnum::PosteriorToAnterior, CoordinateEnum::RightToLeft, CoordinateEnum::InferiorToSuperior),

    IPR = ITK_ORIENTATIONENUM(CoordinateEnum::SuperiorToInferior, CoordinateEnum::AnteriorToPosterior, CoordinateEnum::LeftToRight),
    SPR = ITK_ORIENTATIONENUM(CoordinateEnum::InferiorToSuperior, CoordinateEnum::AnteriorToPosterior, CoordinateEnum::LeftToRight),
    IAR = ITK_ORIENTATIONENUM(CoordinateEnum::SuperiorToInferior, CoordinateEnum::PosteriorToAnterior, CoordinateEnum::LeftToRight),
    SAR = ITK_ORIENTATIONENUM(CoordinateEnum::InferiorToSuperior, CoordinateEnum::PosteriorToAnterior, CoordinateEnum::LeftToRight),
    IPL = ITK_ORIENTATIONENUM(CoordinateEnum::SuperiorToInferior, CoordinateEnum::AnteriorToPosterior, CoordinateEnum::RightToLeft),
    SPL = ITK_ORIENTATIONENUM(CoordinateEnum::InferiorToSuperior, CoordinateEnum::AnteriorToPosterior, CoordinateEnum::RightToLeft),
    IAL = ITK_ORIENTATIONENUM(CoordinateEnum::SuperiorToInferior, CoordinateEnum::PosteriorToAnterior, CoordinateEnum::RightToLeft),
    SAL = ITK_ORIENTATIONENUM(CoordinateEnum::InferiorToSuperior, CoordinateEnum::PosteriorToAnterior, CoordinateEnum::RightToLeft),

    PIR = ITK_ORIENTATIONENUM(CoordinateEnum::AnteriorToPosterior, CoordinateEnum::SuperiorToInferior, CoordinateEnum::LeftToRight),
    PSR = ITK_ORIENTATIONENUM(CoordinateEnum::AnteriorToPosterior, CoordinateEnum::InferiorToSuperior, CoordinateEnum::LeftToRight),
    AIR = ITK_ORIENTATIONENUM(CoordinateEnum::PosteriorToAnterior, CoordinateEnum::SuperiorToInferior, CoordinateEnum::LeftToRight),
    ASR = ITK_ORIENTATIONENUM(CoordinateEnum::PosteriorToAnterior, CoordinateEnum::InferiorToSuperior, CoordinateEnum::LeftToRight),
    PIL = ITK_ORIENTATIONENUM(CoordinateEnum::AnteriorToPosterior, CoordinateEnum::SuperiorToInferior, CoordinateEnum::RightToLeft),
    PSL = ITK_ORIENTATIONENUM(CoordinateEnum::AnteriorToPosterior, CoordinateEnum::InferiorToSuperior, CoordinateEnum::RightToLeft),
    AIL = ITK_ORIENTATIONENUM(CoordinateEnum::PosteriorToAnterior, CoordinateEnum::SuperiorToInferior, CoordinateEnum::RightToLeft),
    ASL = ITK_ORIENTATIONENUM(CoordinateEnum::PosteriorToAnterior, CoordinateEnum::InferiorToSuperior, CoordinateEnum::RightToLeft)
  };

#undef ITK_ORIENTATIONENUM


  /** \brief Initialize with CoordinateEnum's from separate axes.
   *
   * If multiple CoordinateEnums are from the same axes then the Orientation value is INVALID.
   */
  DICOMOrientation(CoordinateEnum primary, CoordinateEnum secondary, CoordinateEnum tertiary);

  DICOMOrientation(OrientationEnum orientation)
    : m_Value(orientation)
  {}


#ifndef ITK_FUTURE_LEGACY_REMOVE
  /** \brief Conversion from Legacy SpatialOrientation
   *
   * @param legacyOrientation
   */
  DICOMOrientation(LegacyOrientationType legacyOrientation);
#endif

  explicit DICOMOrientation(const DirectionType & d)
    : m_Value(DirectionCosinesToOrientation(d))
  {}

  explicit DICOMOrientation(std::string str);

  operator OrientationEnum() const { return m_Value; }

  const std::string &
  GetAsString() const;

  DirectionType
  GetAsDirection() const
  {
    return OrientationToDirectionCosines(m_Value);
  }

  OrientationEnum
  GetAsOrientation() const
  {
    return m_Value;
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


  static bool
  SameOrientationAxes(CoordinateEnum a, CoordinateEnum b)
  {
    const unsigned int AxisField = 0xE; // b1110, mask lowest bit
    return (static_cast<uint8_t>(a) & AxisField) == (static_cast<uint8_t>(b) & AxisField);
  }

  /** \brief Return the closest orientation for a direction cosine matrix. */
  static OrientationEnum
  DirectionCosinesToOrientation(const DirectionType & dir);

  /** \brief Return the direction cosine matrix for a orientation. */
  static DirectionType OrientationToDirectionCosines(OrientationEnum);


  friend ITKCommon_EXPORT std::ostream &
                                 operator<<(std::ostream & out, OrientationEnum value);


private:
  // Private methods to create the maps, these will only be called once.
  static std::map<OrientationEnum, std::string>
  CreateCodeToString();
  static std::map<std::string, OrientationEnum>
  CreateStringToCode();

  /** \brief Return the global instance of the map from orientation enum to strings.
   *
   * The implementation uses a function static local variable so the global is created only if needed, only once.
   */
  static const std::map<OrientationEnum, std::string> &
  GetCodeToString();

  /** \brief Return the global instance of the map from string to orientation enum.
   */
  static const std::map<std::string, OrientationEnum> &
  GetStringToCode();

  OrientationEnum m_Value;
};


ITKCommon_EXPORT std::ostream &
                        operator<<(std::ostream & out, typename DICOMOrientation::OrientationEnum value);

ITKCommon_EXPORT std::ostream &
                        operator<<(std::ostream & out, const DICOMOrientation & orientation);


} // end namespace itk


#endif
