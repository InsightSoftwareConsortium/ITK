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

#include "itkIntTypes.h"
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
} // end namespace itk

#endif
