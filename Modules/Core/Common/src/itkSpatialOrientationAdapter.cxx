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
#include "itkSpatialOrientationAdapter.h"

namespace itk
{
SpatialOrientationAdapter::OrientationType
SpatialOrientationAdapter::FromDirectionCosines(const DirectionType & Dir)
{
  using SpatialOrientationTerms = SpatialOrientationEnums::CoordinateTerms;

  SpatialOrientationTerms terms[3] = { SpatialOrientationTerms::ITK_COORDINATE_UNKNOWN,
                                       SpatialOrientationTerms::ITK_COORDINATE_UNKNOWN,
                                       SpatialOrientationTerms::ITK_COORDINATE_UNKNOWN };


  std::multimap<double, std::pair<unsigned int, unsigned int>> value_to_idx;
  for (unsigned int c = 0; c < 3; ++c)
  {
    for (unsigned int r = 0; r < 3; ++r)
    {
      value_to_idx.emplace(itk::Math::abs(Dir[c][r]), std::make_pair(c, r));
    }
  }

  for (unsigned int i = 0; i < 3; ++i)
  {

    auto               max_idx = value_to_idx.rbegin()->second;
    const unsigned int max_c = max_idx.first;
    const unsigned int max_r = max_idx.second;

    const int max_sgn = Math::sgn(Dir[max_c][max_r]);

    for (auto it = value_to_idx.begin(); it != value_to_idx.end();)
    {
      if (it->second.first == max_c || it->second.second == max_r)
      {
        value_to_idx.erase(it++);
      }
      else
      {
        ++it;
      }
    }

    switch (max_c)
    {
      case 0:
      {
        // When the dominant axis sign is positive, assign the coordinate for the direction we are increasing away from.
        terms[max_r] =
          (max_sgn == 1) ? SpatialOrientationTerms::ITK_COORDINATE_Right : SpatialOrientationTerms::ITK_COORDINATE_Left;
        break;
      }
      case 1:
      {
        terms[max_r] = (max_sgn == 1) ? SpatialOrientationTerms::ITK_COORDINATE_Anterior
                                      : SpatialOrientationTerms::ITK_COORDINATE_Posterior;
        break;
      }
      case 2:
      {
        terms[max_r] = (max_sgn == 1) ? SpatialOrientationTerms::ITK_COORDINATE_Inferior
                                      : SpatialOrientationTerms::ITK_COORDINATE_Superior;
        break;
      }
      default:
        itkGenericExceptionMacro("Unexpected Axis");
    }
  }


  //
  // all terms must be defined, otherwise just punt
  if (terms[0] == SpatialOrientationTerms::ITK_COORDINATE_UNKNOWN ||
      terms[1] == SpatialOrientationTerms::ITK_COORDINATE_UNKNOWN ||
      terms[2] == SpatialOrientationTerms::ITK_COORDINATE_UNKNOWN)
  {
    return SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_RIP;
  }

  return static_cast<SpatialOrientationEnums::ValidCoordinateOrientations>(
    (static_cast<uint32_t>(terms[0]) << static_cast<uint32_t>(
       SpatialOrientationEnums::CoordinateMajornessTerms::ITK_COORDINATE_PrimaryMinor)) +
    (static_cast<uint32_t>(terms[1]) << static_cast<uint32_t>(
       SpatialOrientationEnums::CoordinateMajornessTerms::ITK_COORDINATE_SecondaryMinor)) +
    (static_cast<uint32_t>(terms[2]) << static_cast<uint32_t>(
       SpatialOrientationEnums::CoordinateMajornessTerms::ITK_COORDINATE_TertiaryMinor)));
}

SpatialOrientationAdapter::DirectionType
SpatialOrientationAdapter::ToDirectionCosines(const OrientationType & Or)
{
  using CoordinateTerms = SpatialOrientationEnums::CoordinateTerms;

  CoordinateTerms terms[3];
  terms[0] = static_cast<CoordinateTerms>(
    (static_cast<uint32_t>(Or) >>
     static_cast<uint32_t>(SpatialOrientationEnums::CoordinateMajornessTerms::ITK_COORDINATE_PrimaryMinor)) &
    0xff);
  terms[1] = static_cast<CoordinateTerms>(
    (static_cast<uint32_t>(Or) >>
     static_cast<uint32_t>(SpatialOrientationEnums::CoordinateMajornessTerms::ITK_COORDINATE_SecondaryMinor)) &
    0xff);
  terms[2] = static_cast<CoordinateTerms>(
    (static_cast<uint32_t>(Or) >>
     static_cast<uint32_t>(SpatialOrientationEnums::CoordinateMajornessTerms::ITK_COORDINATE_TertiaryMinor)) &
    0xff);
  DirectionType direction;
  direction.Fill(0.0);
  for (unsigned int i = 0; i < DirectionType::ColumnDimensions; ++i)
  {
    switch (terms[i])
    {
      case CoordinateTerms::ITK_COORDINATE_Right:
        direction[0][i] = 1;
        break;
      case CoordinateTerms::ITK_COORDINATE_Left:
        direction[0][i] = -1;
        break;
      case CoordinateTerms::ITK_COORDINATE_Anterior:
        if (DirectionType::RowDimensions > 1)
        {
          direction[1][i] = 1;
        }
        break;
      case CoordinateTerms::ITK_COORDINATE_Posterior:
        if (DirectionType::RowDimensions > 1)
        {
          direction[1][i] = -1;
        }
        break;
      case CoordinateTerms::ITK_COORDINATE_Inferior:
        if (DirectionType::RowDimensions > 2)
        {
          direction[2][i] = 1;
        }
        break;
      case CoordinateTerms::ITK_COORDINATE_Superior:
        if (DirectionType::RowDimensions > 2)
        {
          direction[2][i] = -1;
        }
        break;
      case CoordinateTerms::ITK_COORDINATE_UNKNOWN:
      default:
        // TODO:  Should there be a default?  Throw an exception?
        break;
    }
  }
  // TODO:  Should check that directions are orthogonal.
  return direction;
}
} // end namespace itk
