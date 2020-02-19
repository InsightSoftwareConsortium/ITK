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

#include "itkWaveletUtilities.h"

namespace itk
{
namespace utils
{

IndexPairType
IndexToLevelBandSteerablePyramid(unsigned int linearIndex, unsigned int levels, unsigned int bands)
{
  unsigned int totalOutputs = 1 + levels * bands;
  if (linearIndex > totalOutputs - 1)
  {
    itkGenericExceptionMacro(<< "Failed converting linearIndex " << linearIndex << " with levels: " << levels
                             << " bands: " << bands << " to Level,Band pair : out of bounds");
  }

  // Low pass (band = 0).
  if (linearIndex == totalOutputs - 1)
  {
    return std::make_pair(levels, 0);
  }

  unsigned int band = (linearIndex) % bands + 1;
  // note integer division ahead.
  unsigned int level = (linearIndex) / bands;
  itkAssertInDebugAndIgnoreInReleaseMacro(level < levels);
  return std::make_pair(level, band);
}

// Instantiation
template <>
unsigned int
ComputeMaxNumberOfLevels<3>(const Size<3> & inputSize, const unsigned int & scaleFactor);

template <>
unsigned int
ComputeMaxNumberOfLevels<2>(const Size<2> & inputSize, const unsigned int & scaleFactor);
} // end namespace utils
} // end namespace itk
