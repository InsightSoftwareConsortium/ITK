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

#include "itkIsotropicWaveletsUtilities.h"

namespace itk
{
long
IsotropicWaveletsUtilities::Factorial(const long n)
{
  return itk::utils::Factorial(n);
}

unsigned int
IsotropicWaveletsUtilities::ComputeNumberOfComponents(const unsigned int & order, const unsigned int & dimension)
{
  return itk::utils::ComputeNumberOfComponents(order, dimension);
}

IsotropicWaveletsUtilities::IndexPairType
IsotropicWaveletsUtilities::IndexToLevelBandSteerablePyramid(unsigned int linearIndex,
                                                             unsigned int levels,
                                                             unsigned int bands)
{
  return itk::utils::IndexToLevelBandSteerablePyramid(linearIndex, levels, bands);
}


unsigned int
IsotropicWaveletsUtilities::ComputeMaxNumberOfLevels(const Size<2> & inputSize, const unsigned int & scaleFactor)
{
  return itk::utils::ComputeMaxNumberOfLevels<2>(inputSize, scaleFactor);
}

unsigned int
IsotropicWaveletsUtilities::ComputeMaxNumberOfLevels(const Size<3> & inputSize, const unsigned int & scaleFactor)
{
  return itk::utils::ComputeMaxNumberOfLevels<3>(inputSize, scaleFactor);
}

} // end namespace itk
