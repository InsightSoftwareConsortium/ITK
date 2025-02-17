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
#ifndef itkEllipsoidInteriorExteriorSpatialFunction_hxx
#define itkEllipsoidInteriorExteriorSpatialFunction_hxx

#include "itkMath.h"
#include <cmath>

namespace itk
{
template <unsigned int VDimension, typename TInput>
auto
EllipsoidInteriorExteriorSpatialFunction<VDimension, TInput>::Evaluate(const InputType & position) const -> OutputType
{
  // Project the position onto each of the axes, normalize by axis length,
  // and determine whether position is inside ellipsoid. The length of axis0,
  // m_Axis[0] is orientated in the direction of m_Orientations[0].
  Vector<double, VDimension> pointVector;
  for (unsigned int i = 0; i < VDimension; ++i)
  {
    pointVector[i] = position[i] - m_Center[i];
  }

  double distanceSquared = 0;
  for (unsigned int i = 0; i < VDimension; ++i)
  {
    Vector<double, VDimension> orientationVector;
    for (unsigned int j = 0; j < VDimension; ++j)
    {
      orientationVector[j] = m_Orientations[i][j];
    }
    distanceSquared += Math::sqr(static_cast<double>((orientationVector * pointVector) / (.5 * m_Axes[i])));
  }

  if (distanceSquared <= 1)
  {
    return 1; // Inside the ellipsoid.
  }
  // Default return value assumes outside the ellipsoid
  return 0; // Outside the ellipsoid.
}

template <unsigned int VDimension, typename TInput>
void
EllipsoidInteriorExteriorSpatialFunction<VDimension, TInput>::SetOrientations(const OrientationType & orientations)
{
  // Set orientation vectors (must be orthogonal).
  m_Orientations = orientations;
}

template <unsigned int VDimension, typename TInput>
void
EllipsoidInteriorExteriorSpatialFunction<VDimension, TInput>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Lengths of Ellipsoid Axes: " << m_Axes << std::endl;
  os << indent << "Origin of Ellipsoid: " << m_Center << std::endl;
  os << indent << "Orientations: " << std::endl;
  for (unsigned int i = 0; i < VDimension; ++i)
  {
    for (unsigned int j = 0; j < VDimension; ++j)
    {
      os << indent << indent << m_Orientations[i][j] << ' ';
    }
    os << std::endl;
  }
}
} // end namespace itk

#endif
