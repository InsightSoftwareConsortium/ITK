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
#ifndef itkSpatialOrientationAdapter_h
#define itkSpatialOrientationAdapter_h
#include "itkOrientationAdapterBase.h"
#include "itkSpatialOrientation.h"
#include "itkConceptChecking.h"

namespace itk
{
//
// Helper functions, better than Macros
//
namespace Function
{
inline unsigned
Max3(double x, double y, double z)
{
  constexpr double obliquityThresholdCosineValue = 0.001;

  double absX = itk::Math::abs(x);
  double absY = itk::Math::abs(y);
  double absZ = itk::Math::abs(z);

  if ((absX > obliquityThresholdCosineValue) && (absX > absY) && (absX > absZ))
  {
    return 0;
  }
  else if ((absY > obliquityThresholdCosineValue) && (absY > absX) && (absY > absZ))
  {
    return 1;
  }
  else if ((absZ > obliquityThresholdCosineValue) && (absZ > absX) && (absZ > absY))
  {
    return 2;
  }
  // they must all be equal, so just say x
  return 0;
}

inline int
Sign(double x)
{
  if (x < 0)
  {
    return -1;
  }
  return 1;
}
} // namespace Function

/** \class SpatialOrientationAdapter
 *  \brief Converts SpatialOrientation flags to/from direction cosines.
 * \ingroup ITKCommon
 */
class ITKCommon_EXPORT SpatialOrientationAdapter
  : public OrientationAdapterBase<SpatialOrientation::ValidCoordinateOrientationFlags, 3>
{
public:
  /** type alias for superclass */
  using Self = SpatialOrientationAdapter;

  using Superclass = OrientationAdapterBase<SpatialOrientation::ValidCoordinateOrientationFlags, 3>;

  using OrientationType = SpatialOrientation::ValidCoordinateOrientationFlags;

  /** type alias for direction cosines */
  using DirectionType = Superclass::DirectionType;

  /** Constructor */
  SpatialOrientationAdapter() = default;

  /** convert from direction cosines. */
  OrientationType
  FromDirectionCosines(const DirectionType & Dir) override;

  /** convert to direction cosines. */
  DirectionType
  ToDirectionCosines(const OrientationType & Or) override;
};
} // namespace itk

#endif // itkSpatialOrientationAdapter_h
