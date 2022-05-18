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
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#ifndef itkSpatialOrientation_h
#define itkSpatialOrientation_h

#include <cstdint>
#include <ostream>

#include "ITKCommonExport.h"

namespace itk
{
/** \class SpatialOrientationEnums
 *
 * \brief Enums for spatial orientation of images.
 *
 * Enums used in itk::OrientImageFilter and related classes.
 *
 * \ingroup ITKCommon
 */
class SpatialOrientationEnums
{
public:
  /** \class CoordinateTerms
   *
   * Coordinate orientation codes have a place-value organization such that
   * an ImageDimension-al sequence of subcodes says both which varies fastest
   * through which varies slowest, but also which end of the frame of
   * reference is considered zero for each of the coordinates.
   * For example, 'RIP' means Right to Left varies fastest,
   * then Inferior to Superior, and Posterior to Anterior varies the slowest.
   *
   * \ingroup ITKCommon
   */
  enum class CoordinateTerms : uint8_t
  {
    ITK_COORDINATE_UNKNOWN = 0,
    ITK_COORDINATE_Right = 2,
    ITK_COORDINATE_Left = 3,
    ITK_COORDINATE_Posterior = 4, // back
    ITK_COORDINATE_Anterior = 5,  // front
    ITK_COORDINATE_Inferior = 8,  // below
    ITK_COORDINATE_Superior = 9   // above
                                  // ITK_COORDINATE_Historical=16,
                                  // ITK_COORDINATE_Future=17
  };

  /** \class CoordinateMajornessTerms
   *
   * These code place values have to be far enough apart to
   * separate the CoordinateTerms above.
   * However, if we added History/Future direction in time,
   * we would need at least 5 bits of separation.
   *
   * Majorness is in terms of rank-minor because a
   * fourth dimension, time, would be even more
   * major than the
   * PrimaryMajor==TertiaryMinor.
   * ITK_COORDINATE_QuaternaryMinor=24
   *
   * \ingroup ITKCommon
   */
  enum class CoordinateMajornessTerms : uint8_t
  {
    ITK_COORDINATE_PrimaryMinor = 0,
    ITK_COORDINATE_SecondaryMinor = 8,
    ITK_COORDINATE_TertiaryMinor = 16
  };

  /** \class CoordinateMajornessTerms
   *
   * Adding time IN GENERAL would make these 8 x 6 = 48 triples into 16
   * x 24 = 384 4-tuples.
   * A general fourth dimension would need a unique pair of letters to
   * add to the code;
   * Maybe use H and F, from History to Future? Maybe use 48 x 2 with
   * time ALWAYS highest
   * order, or 48 x 2 x 2, with 3-space always highest or lowest order
   * Multispectra might be
   * co-registered and pieced together.... PD-T2 interleaving of
   * slices is handled with choosing
   * which spectrum to load via the prototypical file name.
   *
   * \ingroup ITKCommon
   */
  enum class ValidCoordinateOrientations : uint32_t
  {
    ITK_COORDINATE_ORIENTATION_INVALID = static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_UNKNOWN),
    ITK_COORDINATE_ORIENTATION_RIP =
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Right)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_PrimaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Inferior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_SecondaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Posterior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_TertiaryMinor)),
    ITK_COORDINATE_ORIENTATION_LIP =
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Left)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_PrimaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Inferior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_SecondaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Posterior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_TertiaryMinor)),
    ITK_COORDINATE_ORIENTATION_RSP =
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Right)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_PrimaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Superior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_SecondaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Posterior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_TertiaryMinor)),
    ITK_COORDINATE_ORIENTATION_LSP =
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Left)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_PrimaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Superior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_SecondaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Posterior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_TertiaryMinor)),
    ITK_COORDINATE_ORIENTATION_RIA =
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Right)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_PrimaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Inferior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_SecondaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Anterior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_TertiaryMinor)),
    ITK_COORDINATE_ORIENTATION_LIA =
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Left)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_PrimaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Inferior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_SecondaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Anterior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_TertiaryMinor)),
    ITK_COORDINATE_ORIENTATION_RSA =
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Right)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_PrimaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Superior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_SecondaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Anterior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_TertiaryMinor)),
    ITK_COORDINATE_ORIENTATION_LSA =
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Left)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_PrimaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Superior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_SecondaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Anterior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_TertiaryMinor)),

    ITK_COORDINATE_ORIENTATION_IRP =
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Inferior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_PrimaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Right)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_SecondaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Posterior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_TertiaryMinor)),
    ITK_COORDINATE_ORIENTATION_ILP =
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Inferior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_PrimaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Left)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_SecondaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Posterior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_TertiaryMinor)),
    ITK_COORDINATE_ORIENTATION_SRP =
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Superior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_PrimaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Right)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_SecondaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Posterior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_TertiaryMinor)),
    ITK_COORDINATE_ORIENTATION_SLP =
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Superior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_PrimaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Left)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_SecondaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Posterior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_TertiaryMinor)),
    ITK_COORDINATE_ORIENTATION_IRA =
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Inferior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_PrimaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Right)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_SecondaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Anterior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_TertiaryMinor)),
    ITK_COORDINATE_ORIENTATION_ILA =
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Inferior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_PrimaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Left)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_SecondaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Anterior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_TertiaryMinor)),
    ITK_COORDINATE_ORIENTATION_SRA =
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Superior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_PrimaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Right)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_SecondaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Anterior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_TertiaryMinor)),
    ITK_COORDINATE_ORIENTATION_SLA =
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Superior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_PrimaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Left)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_SecondaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Anterior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_TertiaryMinor)),

    ITK_COORDINATE_ORIENTATION_RPI =
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Right)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_PrimaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Posterior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_SecondaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Inferior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_TertiaryMinor)),
    ITK_COORDINATE_ORIENTATION_LPI =
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Left)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_PrimaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Posterior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_SecondaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Inferior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_TertiaryMinor)),
    ITK_COORDINATE_ORIENTATION_RAI =
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Right)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_PrimaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Anterior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_SecondaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Inferior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_TertiaryMinor)),
    ITK_COORDINATE_ORIENTATION_LAI =
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Left)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_PrimaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Anterior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_SecondaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Inferior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_TertiaryMinor)),
    ITK_COORDINATE_ORIENTATION_RPS =
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Right)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_PrimaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Posterior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_SecondaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Superior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_TertiaryMinor)),
    ITK_COORDINATE_ORIENTATION_LPS =
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Left)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_PrimaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Posterior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_SecondaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Superior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_TertiaryMinor)),
    ITK_COORDINATE_ORIENTATION_RAS =
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Right)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_PrimaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Anterior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_SecondaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Superior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_TertiaryMinor)),
    ITK_COORDINATE_ORIENTATION_LAS =
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Left)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_PrimaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Anterior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_SecondaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Superior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_TertiaryMinor)),

    ITK_COORDINATE_ORIENTATION_PRI =
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Posterior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_PrimaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Right)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_SecondaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Inferior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_TertiaryMinor)),
    ITK_COORDINATE_ORIENTATION_PLI =
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Posterior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_PrimaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Left)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_SecondaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Inferior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_TertiaryMinor)),
    ITK_COORDINATE_ORIENTATION_ARI =
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Anterior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_PrimaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Right)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_SecondaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Inferior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_TertiaryMinor)),
    ITK_COORDINATE_ORIENTATION_ALI =
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Anterior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_PrimaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Left)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_SecondaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Inferior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_TertiaryMinor)),
    ITK_COORDINATE_ORIENTATION_PRS =
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Posterior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_PrimaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Right)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_SecondaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Superior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_TertiaryMinor)),
    ITK_COORDINATE_ORIENTATION_PLS =
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Posterior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_PrimaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Left)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_SecondaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Superior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_TertiaryMinor)),
    ITK_COORDINATE_ORIENTATION_ARS =
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Anterior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_PrimaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Right)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_SecondaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Superior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_TertiaryMinor)),
    ITK_COORDINATE_ORIENTATION_ALS =
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Anterior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_PrimaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Left)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_SecondaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Superior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_TertiaryMinor)),

    ITK_COORDINATE_ORIENTATION_IPR =
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Inferior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_PrimaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Posterior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_SecondaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Right)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_TertiaryMinor)),
    ITK_COORDINATE_ORIENTATION_SPR =
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Superior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_PrimaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Posterior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_SecondaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Right)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_TertiaryMinor)),
    ITK_COORDINATE_ORIENTATION_IAR =
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Inferior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_PrimaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Anterior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_SecondaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Right)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_TertiaryMinor)),
    ITK_COORDINATE_ORIENTATION_SAR =
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Superior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_PrimaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Anterior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_SecondaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Right)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_TertiaryMinor)),
    ITK_COORDINATE_ORIENTATION_IPL =
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Inferior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_PrimaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Posterior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_SecondaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Left)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_TertiaryMinor)),
    ITK_COORDINATE_ORIENTATION_SPL =
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Superior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_PrimaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Posterior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_SecondaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Left)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_TertiaryMinor)),
    ITK_COORDINATE_ORIENTATION_IAL =
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Inferior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_PrimaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Anterior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_SecondaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Left)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_TertiaryMinor)),
    ITK_COORDINATE_ORIENTATION_SAL =
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Superior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_PrimaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Anterior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_SecondaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Left)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_TertiaryMinor)),

    ITK_COORDINATE_ORIENTATION_PIR =
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Posterior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_PrimaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Inferior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_SecondaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Right)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_TertiaryMinor)),
    ITK_COORDINATE_ORIENTATION_PSR =
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Posterior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_PrimaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Superior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_SecondaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Right)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_TertiaryMinor)),
    ITK_COORDINATE_ORIENTATION_AIR =
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Anterior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_PrimaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Inferior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_SecondaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Right)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_TertiaryMinor)),
    ITK_COORDINATE_ORIENTATION_ASR =
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Anterior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_PrimaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Superior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_SecondaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Right)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_TertiaryMinor)),
    ITK_COORDINATE_ORIENTATION_PIL =
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Posterior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_PrimaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Inferior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_SecondaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Left)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_TertiaryMinor)),
    ITK_COORDINATE_ORIENTATION_PSL =
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Posterior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_PrimaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Superior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_SecondaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Left)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_TertiaryMinor)),
    ITK_COORDINATE_ORIENTATION_AIL =
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Anterior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_PrimaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Inferior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_SecondaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Left)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_TertiaryMinor)),
    ITK_COORDINATE_ORIENTATION_ASL =
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Anterior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_PrimaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Superior)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_SecondaryMinor)) +
      (static_cast<uint32_t>(CoordinateTerms::ITK_COORDINATE_Left)
       << static_cast<uint32_t>(CoordinateMajornessTerms::ITK_COORDINATE_TertiaryMinor))
  };
};

// Define how to print enumeration
extern ITKCommon_EXPORT std::ostream &
                        operator<<(std::ostream & out, const SpatialOrientationEnums::CoordinateTerms value);
extern ITKCommon_EXPORT std::ostream &
                        operator<<(std::ostream & out, const SpatialOrientationEnums::CoordinateMajornessTerms value);
extern ITKCommon_EXPORT std::ostream &
                        operator<<(std::ostream & out, const SpatialOrientationEnums::ValidCoordinateOrientations value);

#ifndef ITK_LEGACY_REMOVE
namespace SpatialOrientation
{

using CoordinateTerms = SpatialOrientationEnums::CoordinateTerms;

static constexpr CoordinateTerms ITK_COORDINATE_UNKNOWN = CoordinateTerms::ITK_COORDINATE_UNKNOWN;
static constexpr CoordinateTerms ITK_COORDINATE_Right = CoordinateTerms::ITK_COORDINATE_Right;
static constexpr CoordinateTerms ITK_COORDINATE_Left = CoordinateTerms::ITK_COORDINATE_Left;
static constexpr CoordinateTerms ITK_COORDINATE_Posterior = CoordinateTerms::ITK_COORDINATE_Posterior;
static constexpr CoordinateTerms ITK_COORDINATE_Anterior = CoordinateTerms::ITK_COORDINATE_Anterior;
static constexpr CoordinateTerms ITK_COORDINATE_Inferior = CoordinateTerms::ITK_COORDINATE_Inferior;
static constexpr CoordinateTerms ITK_COORDINATE_Superior = CoordinateTerms::ITK_COORDINATE_Superior;

using CoordinateMajornessTerms = SpatialOrientationEnums::CoordinateMajornessTerms;

static constexpr CoordinateMajornessTerms ITK_COORDINATE_PrimaryMinor =
  CoordinateMajornessTerms::ITK_COORDINATE_PrimaryMinor;
static constexpr CoordinateMajornessTerms ITK_COORDINATE_SecondaryMinor =
  CoordinateMajornessTerms::ITK_COORDINATE_SecondaryMinor;
static constexpr CoordinateMajornessTerms ITK_COORDINATE_TertiaryMinor =
  CoordinateMajornessTerms::ITK_COORDINATE_TertiaryMinor;

using ValidCoordinateOrientationFlags = SpatialOrientationEnums::ValidCoordinateOrientations;

static constexpr ValidCoordinateOrientationFlags ITK_COORDINATE_ORIENTATION_RIP =
  ValidCoordinateOrientationFlags::ITK_COORDINATE_ORIENTATION_RIP;
static constexpr ValidCoordinateOrientationFlags ITK_COORDINATE_ORIENTATION_LIP =
  ValidCoordinateOrientationFlags::ITK_COORDINATE_ORIENTATION_LIP;
static constexpr ValidCoordinateOrientationFlags ITK_COORDINATE_ORIENTATION_RSP =
  ValidCoordinateOrientationFlags::ITK_COORDINATE_ORIENTATION_RSP;
static constexpr ValidCoordinateOrientationFlags ITK_COORDINATE_ORIENTATION_LSP =
  ValidCoordinateOrientationFlags::ITK_COORDINATE_ORIENTATION_LSP;
static constexpr ValidCoordinateOrientationFlags ITK_COORDINATE_ORIENTATION_RIA =
  ValidCoordinateOrientationFlags::ITK_COORDINATE_ORIENTATION_RIA;
static constexpr ValidCoordinateOrientationFlags ITK_COORDINATE_ORIENTATION_LIA =
  ValidCoordinateOrientationFlags::ITK_COORDINATE_ORIENTATION_LIA;
static constexpr ValidCoordinateOrientationFlags ITK_COORDINATE_ORIENTATION_RSA =
  ValidCoordinateOrientationFlags::ITK_COORDINATE_ORIENTATION_RSA;
static constexpr ValidCoordinateOrientationFlags ITK_COORDINATE_ORIENTATION_LSA =
  ValidCoordinateOrientationFlags::ITK_COORDINATE_ORIENTATION_LSA;

static constexpr ValidCoordinateOrientationFlags ITK_COORDINATE_ORIENTATION_IRP =
  ValidCoordinateOrientationFlags::ITK_COORDINATE_ORIENTATION_IRP;
static constexpr ValidCoordinateOrientationFlags ITK_COORDINATE_ORIENTATION_ILP =
  ValidCoordinateOrientationFlags::ITK_COORDINATE_ORIENTATION_ILP;
static constexpr ValidCoordinateOrientationFlags ITK_COORDINATE_ORIENTATION_SRP =
  ValidCoordinateOrientationFlags::ITK_COORDINATE_ORIENTATION_SRP;
static constexpr ValidCoordinateOrientationFlags ITK_COORDINATE_ORIENTATION_SLP =
  ValidCoordinateOrientationFlags::ITK_COORDINATE_ORIENTATION_SLP;
static constexpr ValidCoordinateOrientationFlags ITK_COORDINATE_ORIENTATION_IRA =
  ValidCoordinateOrientationFlags::ITK_COORDINATE_ORIENTATION_IRA;
static constexpr ValidCoordinateOrientationFlags ITK_COORDINATE_ORIENTATION_ILA =
  ValidCoordinateOrientationFlags::ITK_COORDINATE_ORIENTATION_ILA;
static constexpr ValidCoordinateOrientationFlags ITK_COORDINATE_ORIENTATION_SRA =
  ValidCoordinateOrientationFlags::ITK_COORDINATE_ORIENTATION_SRA;
static constexpr ValidCoordinateOrientationFlags ITK_COORDINATE_ORIENTATION_SLA =
  ValidCoordinateOrientationFlags::ITK_COORDINATE_ORIENTATION_SLA;

static constexpr ValidCoordinateOrientationFlags ITK_COORDINATE_ORIENTATION_RPI =
  ValidCoordinateOrientationFlags::ITK_COORDINATE_ORIENTATION_RPI;
static constexpr ValidCoordinateOrientationFlags ITK_COORDINATE_ORIENTATION_LPI =
  ValidCoordinateOrientationFlags::ITK_COORDINATE_ORIENTATION_LPI;
static constexpr ValidCoordinateOrientationFlags ITK_COORDINATE_ORIENTATION_RAI =
  ValidCoordinateOrientationFlags::ITK_COORDINATE_ORIENTATION_RAI;
static constexpr ValidCoordinateOrientationFlags ITK_COORDINATE_ORIENTATION_LAI =
  ValidCoordinateOrientationFlags::ITK_COORDINATE_ORIENTATION_LAI;
static constexpr ValidCoordinateOrientationFlags ITK_COORDINATE_ORIENTATION_RPS =
  ValidCoordinateOrientationFlags::ITK_COORDINATE_ORIENTATION_RPS;
static constexpr ValidCoordinateOrientationFlags ITK_COORDINATE_ORIENTATION_LPS =
  ValidCoordinateOrientationFlags::ITK_COORDINATE_ORIENTATION_LPS;
static constexpr ValidCoordinateOrientationFlags ITK_COORDINATE_ORIENTATION_RAS =
  ValidCoordinateOrientationFlags::ITK_COORDINATE_ORIENTATION_RAS;
static constexpr ValidCoordinateOrientationFlags ITK_COORDINATE_ORIENTATION_LAS =
  ValidCoordinateOrientationFlags::ITK_COORDINATE_ORIENTATION_LAS;

static constexpr ValidCoordinateOrientationFlags ITK_COORDINATE_ORIENTATION_PRI =
  ValidCoordinateOrientationFlags::ITK_COORDINATE_ORIENTATION_PRI;
static constexpr ValidCoordinateOrientationFlags ITK_COORDINATE_ORIENTATION_PLI =
  ValidCoordinateOrientationFlags::ITK_COORDINATE_ORIENTATION_PLI;
static constexpr ValidCoordinateOrientationFlags ITK_COORDINATE_ORIENTATION_ARI =
  ValidCoordinateOrientationFlags::ITK_COORDINATE_ORIENTATION_ARI;
static constexpr ValidCoordinateOrientationFlags ITK_COORDINATE_ORIENTATION_ALI =
  ValidCoordinateOrientationFlags::ITK_COORDINATE_ORIENTATION_ALI;
static constexpr ValidCoordinateOrientationFlags ITK_COORDINATE_ORIENTATION_PRS =
  ValidCoordinateOrientationFlags::ITK_COORDINATE_ORIENTATION_PRS;
static constexpr ValidCoordinateOrientationFlags ITK_COORDINATE_ORIENTATION_PLS =
  ValidCoordinateOrientationFlags::ITK_COORDINATE_ORIENTATION_PLS;
static constexpr ValidCoordinateOrientationFlags ITK_COORDINATE_ORIENTATION_ARS =
  ValidCoordinateOrientationFlags::ITK_COORDINATE_ORIENTATION_ARS;
static constexpr ValidCoordinateOrientationFlags ITK_COORDINATE_ORIENTATION_ALS =
  ValidCoordinateOrientationFlags::ITK_COORDINATE_ORIENTATION_ALS;

static constexpr ValidCoordinateOrientationFlags ITK_COORDINATE_ORIENTATION_IPR =
  ValidCoordinateOrientationFlags::ITK_COORDINATE_ORIENTATION_IPR;
static constexpr ValidCoordinateOrientationFlags ITK_COORDINATE_ORIENTATION_SPR =
  ValidCoordinateOrientationFlags::ITK_COORDINATE_ORIENTATION_SPR;
static constexpr ValidCoordinateOrientationFlags ITK_COORDINATE_ORIENTATION_IAR =
  ValidCoordinateOrientationFlags::ITK_COORDINATE_ORIENTATION_IAR;
static constexpr ValidCoordinateOrientationFlags ITK_COORDINATE_ORIENTATION_SAR =
  ValidCoordinateOrientationFlags::ITK_COORDINATE_ORIENTATION_SAR;
static constexpr ValidCoordinateOrientationFlags ITK_COORDINATE_ORIENTATION_IPL =
  ValidCoordinateOrientationFlags::ITK_COORDINATE_ORIENTATION_IPL;
static constexpr ValidCoordinateOrientationFlags ITK_COORDINATE_ORIENTATION_SPL =
  ValidCoordinateOrientationFlags::ITK_COORDINATE_ORIENTATION_SPL;
static constexpr ValidCoordinateOrientationFlags ITK_COORDINATE_ORIENTATION_IAL =
  ValidCoordinateOrientationFlags::ITK_COORDINATE_ORIENTATION_IAL;
static constexpr ValidCoordinateOrientationFlags ITK_COORDINATE_ORIENTATION_SAL =
  ValidCoordinateOrientationFlags::ITK_COORDINATE_ORIENTATION_SAL;

static constexpr ValidCoordinateOrientationFlags ITK_COORDINATE_ORIENTATION_PIR =
  ValidCoordinateOrientationFlags::ITK_COORDINATE_ORIENTATION_PIR;
static constexpr ValidCoordinateOrientationFlags ITK_COORDINATE_ORIENTATION_PSR =
  ValidCoordinateOrientationFlags::ITK_COORDINATE_ORIENTATION_PSR;
static constexpr ValidCoordinateOrientationFlags ITK_COORDINATE_ORIENTATION_AIR =
  ValidCoordinateOrientationFlags::ITK_COORDINATE_ORIENTATION_AIR;
static constexpr ValidCoordinateOrientationFlags ITK_COORDINATE_ORIENTATION_ASR =
  ValidCoordinateOrientationFlags::ITK_COORDINATE_ORIENTATION_ASR;
static constexpr ValidCoordinateOrientationFlags ITK_COORDINATE_ORIENTATION_PIL =
  ValidCoordinateOrientationFlags::ITK_COORDINATE_ORIENTATION_PIL;
static constexpr ValidCoordinateOrientationFlags ITK_COORDINATE_ORIENTATION_PSL =
  ValidCoordinateOrientationFlags::ITK_COORDINATE_ORIENTATION_PSL;
static constexpr ValidCoordinateOrientationFlags ITK_COORDINATE_ORIENTATION_AIL =
  ValidCoordinateOrientationFlags::ITK_COORDINATE_ORIENTATION_AIL;
static constexpr ValidCoordinateOrientationFlags ITK_COORDINATE_ORIENTATION_ASL =
  ValidCoordinateOrientationFlags::ITK_COORDINATE_ORIENTATION_ASL;

} // end of namespace SpatialOrientation
#endif // !ITK_LEGACY_REMOVE
} // end namespace itk

#endif
