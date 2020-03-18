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
#ifndef itkDICOMOrientation_h
#define itkDICOMOrientation_h

#include "SimpleITKFiltersExport.h"
#include "itkImageBase.h"
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
 * \ingroup SimpleITKFilters
 */
class SimpleITKFilters_EXPORT DICOMOrientation
{
public:
  constexpr static unsigned int Dimension = 3;
  using DirectionType = typename ImageBase<Dimension>::DirectionType;
  constexpr static unsigned int ImageDimension = Dimension;


  enum class CoordinateEnum : uint8_t
  {
    UNKNOWN = 0,
    Right = 2, // 0b0010
    Left = 3,
    Anterior = 4,  ///< front - 0b0100
    Posterior = 5, ///< back
    Inferior = 8,  ///< bottom - 0b1000
    Superior = 9   ///< above
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

public:
#if 0
#  define ITK_ORIENTATIONENUM toOrientation
#else

#  define ITK_ORIENTATIONENUM(P, S, T)                                                                                 \
    (static_cast<uint32_t>(P) << static_cast<uint8_t>(CoordinateMajornessTermsEnum::PrimaryMinor)) +                   \
      (static_cast<uint32_t>(S) << static_cast<uint8_t>(CoordinateMajornessTermsEnum::SecondaryMinor)) +               \
      (static_cast<uint32_t>(T) << static_cast<uint8_t>(CoordinateMajornessTermsEnum::TertiaryMinor))

#endif

  enum class OrientationEnum : uint32_t

  {
    INVALID = 0,
    RIP = ITK_ORIENTATIONENUM(CoordinateEnum::Right, CoordinateEnum::Inferior, CoordinateEnum::Posterior),
    LIP = ITK_ORIENTATIONENUM(CoordinateEnum::Left, CoordinateEnum::Inferior, CoordinateEnum::Posterior),
    RSP = ITK_ORIENTATIONENUM(CoordinateEnum::Right, CoordinateEnum::Superior, CoordinateEnum::Posterior),
    LSP = ITK_ORIENTATIONENUM(CoordinateEnum::Left, CoordinateEnum::Superior, CoordinateEnum::Posterior),
    RIA = ITK_ORIENTATIONENUM(CoordinateEnum::Right, CoordinateEnum::Inferior, CoordinateEnum::Anterior),
    LIA = ITK_ORIENTATIONENUM(CoordinateEnum::Left, CoordinateEnum::Inferior, CoordinateEnum::Anterior),
    RSA = ITK_ORIENTATIONENUM(CoordinateEnum::Right, CoordinateEnum::Superior, CoordinateEnum::Anterior),
    LSA = ITK_ORIENTATIONENUM(CoordinateEnum::Left, CoordinateEnum::Superior, CoordinateEnum::Anterior),

    IRP = ITK_ORIENTATIONENUM(CoordinateEnum::Inferior, CoordinateEnum::Right, CoordinateEnum::Posterior),
    ILP = ITK_ORIENTATIONENUM(CoordinateEnum::Inferior, CoordinateEnum::Left, CoordinateEnum::Posterior),
    SRP = ITK_ORIENTATIONENUM(CoordinateEnum::Superior, CoordinateEnum::Right, CoordinateEnum::Posterior),
    SLP = ITK_ORIENTATIONENUM(CoordinateEnum::Superior, CoordinateEnum::Left, CoordinateEnum::Posterior),
    IRA = ITK_ORIENTATIONENUM(CoordinateEnum::Inferior, CoordinateEnum::Right, CoordinateEnum::Anterior),
    ILA = ITK_ORIENTATIONENUM(CoordinateEnum::Inferior, CoordinateEnum::Left, CoordinateEnum::Anterior),
    SRA = ITK_ORIENTATIONENUM(CoordinateEnum::Superior, CoordinateEnum::Right, CoordinateEnum::Anterior),
    SLA = ITK_ORIENTATIONENUM(CoordinateEnum::Superior, CoordinateEnum::Left, CoordinateEnum::Anterior),

    RPI = ITK_ORIENTATIONENUM(CoordinateEnum::Right, CoordinateEnum::Posterior, CoordinateEnum::Inferior),
    LPI = ITK_ORIENTATIONENUM(CoordinateEnum::Left, CoordinateEnum::Posterior, CoordinateEnum::Inferior),
    RAI = ITK_ORIENTATIONENUM(CoordinateEnum::Right, CoordinateEnum::Anterior, CoordinateEnum::Inferior),
    LAI = ITK_ORIENTATIONENUM(CoordinateEnum::Left, CoordinateEnum::Anterior, CoordinateEnum::Inferior),
    RPS = ITK_ORIENTATIONENUM(CoordinateEnum::Right, CoordinateEnum::Posterior, CoordinateEnum::Superior),
    LPS = ITK_ORIENTATIONENUM(CoordinateEnum::Left, CoordinateEnum::Posterior, CoordinateEnum::Superior),
    RAS = ITK_ORIENTATIONENUM(CoordinateEnum::Right, CoordinateEnum::Anterior, CoordinateEnum::Superior),
    LAS = ITK_ORIENTATIONENUM(CoordinateEnum::Left, CoordinateEnum::Anterior, CoordinateEnum::Superior),

    PRI = ITK_ORIENTATIONENUM(CoordinateEnum::Posterior, CoordinateEnum::Right, CoordinateEnum::Inferior),
    PLI = ITK_ORIENTATIONENUM(CoordinateEnum::Posterior, CoordinateEnum::Left, CoordinateEnum::Inferior),
    ARI = ITK_ORIENTATIONENUM(CoordinateEnum::Anterior, CoordinateEnum::Right, CoordinateEnum::Inferior),
    ALI = ITK_ORIENTATIONENUM(CoordinateEnum::Anterior, CoordinateEnum::Left, CoordinateEnum::Inferior),
    PRS = ITK_ORIENTATIONENUM(CoordinateEnum::Posterior, CoordinateEnum::Right, CoordinateEnum::Superior),
    PLS = ITK_ORIENTATIONENUM(CoordinateEnum::Posterior, CoordinateEnum::Left, CoordinateEnum::Superior),
    ARS = ITK_ORIENTATIONENUM(CoordinateEnum::Anterior, CoordinateEnum::Right, CoordinateEnum::Superior),
    ALS = ITK_ORIENTATIONENUM(CoordinateEnum::Anterior, CoordinateEnum::Left, CoordinateEnum::Superior),

    IPR = ITK_ORIENTATIONENUM(CoordinateEnum::Inferior, CoordinateEnum::Posterior, CoordinateEnum::Right),
    SPR = ITK_ORIENTATIONENUM(CoordinateEnum::Superior, CoordinateEnum::Posterior, CoordinateEnum::Right),
    IAR = ITK_ORIENTATIONENUM(CoordinateEnum::Inferior, CoordinateEnum::Anterior, CoordinateEnum::Right),
    SAR = ITK_ORIENTATIONENUM(CoordinateEnum::Superior, CoordinateEnum::Anterior, CoordinateEnum::Right),
    IPL = ITK_ORIENTATIONENUM(CoordinateEnum::Inferior, CoordinateEnum::Posterior, CoordinateEnum::Left),
    SPL = ITK_ORIENTATIONENUM(CoordinateEnum::Superior, CoordinateEnum::Posterior, CoordinateEnum::Left),
    IAL = ITK_ORIENTATIONENUM(CoordinateEnum::Inferior, CoordinateEnum::Anterior, CoordinateEnum::Left),
    SAL = ITK_ORIENTATIONENUM(CoordinateEnum::Superior, CoordinateEnum::Anterior, CoordinateEnum::Left),

    PIR = ITK_ORIENTATIONENUM(CoordinateEnum::Posterior, CoordinateEnum::Inferior, CoordinateEnum::Right),
    PSR = ITK_ORIENTATIONENUM(CoordinateEnum::Posterior, CoordinateEnum::Superior, CoordinateEnum::Right),
    AIR = ITK_ORIENTATIONENUM(CoordinateEnum::Anterior, CoordinateEnum::Inferior, CoordinateEnum::Right),
    ASR = ITK_ORIENTATIONENUM(CoordinateEnum::Anterior, CoordinateEnum::Superior, CoordinateEnum::Right),
    PIL = ITK_ORIENTATIONENUM(CoordinateEnum::Posterior, CoordinateEnum::Inferior, CoordinateEnum::Left),
    PSL = ITK_ORIENTATIONENUM(CoordinateEnum::Posterior, CoordinateEnum::Superior, CoordinateEnum::Left),
    AIL = ITK_ORIENTATIONENUM(CoordinateEnum::Anterior, CoordinateEnum::Inferior, CoordinateEnum::Left),
    ASL = ITK_ORIENTATIONENUM(CoordinateEnum::Anterior, CoordinateEnum::Superior, CoordinateEnum::Left)
  };

#undef ITK_ORIENTATIONENUM


  DICOMOrientation(CoordinateEnum primary, CoordinateEnum secondary, CoordinateEnum tertiary);

  DICOMOrientation(OrientationEnum orientation)
    : m_Value(orientation)
  {}

  explicit DICOMOrientation(const DirectionType & d)
    : m_Value(DirectionCosinesToOrientation(d))
  {}

  explicit DICOMOrientation(const std::string & str);

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
  GetCoordinateTerm(CoordinateMajornessTermsEnum cmt) const;

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
    const unsigned int AxisField = 0xE; // b1110, make lowest bit
    return (static_cast<uint8_t>(a) & AxisField) == (static_cast<uint8_t>(b) & AxisField);
  }

  /** \brief Return the closest orientation for a direction cosine matrix. */
  static OrientationEnum
  DirectionCosinesToOrientation(const DirectionType & dir);

  /** \brief Return the direction cosine matrix for a orientation. */
  static DirectionType OrientationToDirectionCosines(OrientationEnum);


  friend SimpleITKFilters_EXPORT std::ostream &
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


SimpleITKFilters_EXPORT std::ostream &
                        operator<<(std::ostream & out, typename DICOMOrientation::OrientationEnum value);

SimpleITKFilters_EXPORT std::ostream &
                        operator<<(std::ostream & out, const DICOMOrientation & orientation);


} // end namespace itk


#endif
