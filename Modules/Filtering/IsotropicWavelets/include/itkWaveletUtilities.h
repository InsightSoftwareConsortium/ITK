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
#ifndef itkWaveletUtilities_h
#define itkWaveletUtilities_h

#include <algorithm>
#include <cmath>
#include <vector>
#include <itkFixedArray.h>
#include <itkMath.h>
#include <itkSize.h>

#include "IsotropicWaveletsExport.h"

namespace itk
{
namespace utils
{
using IndexPairType = std::pair<unsigned int, unsigned int>;

/** Get the (Level,Band) from a linear index output.
 * The index corresponding to the low-pass image is the last one, corresponding to the
 * IndexPairType(this->GetLevels(), 0).
 *
 * In a steerable pyramid: TotalOutputs = 1 + Levels * Bands
 *
 * The outputs are ordered, if n is the \c linearIndex:
 *
 * n:0 ---> level:0 band:1,
 * n:1 ---> l:0, b:2, etc. until b == bands.
 * n:bands-1 ---> l:0, b=bands
 *
 * If there is more than one level:
 *
 * n:bands ---> l:1, b=1
 *
 * if only one level:
 *
 * n:bands ---> l:0, b=0
 *
 * Independently of the numbers of levels or bands, the last index is always the low pass:
 *
 * nLowPass ---> l:Levels, b=0
 *
 * Note that bands and levels are always >= 1. The level/bands returned here corresponds to an index.
 */
IsotropicWavelets_EXPORT IndexPairType
IndexToLevelBandSteerablePyramid(unsigned int linearIndex, unsigned int levels, unsigned int bands);

/** Compute max number of levels depending on the size of the image.
 * Return J: $ J = \text{min_element}\{J_0,\ldots, J_d\} $;
 * where each $J_i$ is the  number of integer divisions that can be done with the $i$ size and the scale factor.
 * returns 1 if any size is not divisible by the scale factor.
 */
template <unsigned int VImageDimension>
ITK_TEMPLATE_EXPORT unsigned int
ComputeMaxNumberOfLevels(const Size<VImageDimension> & inputSize, const unsigned int & scaleFactor)
{
  FixedArray<unsigned int, VImageDimension> exponentPerAxis;
  // The minimum level is 1.
  exponentPerAxis.Fill(1);
  for (unsigned int axis = 0; axis < VImageDimension; ++axis)
  {
    size_t sizeAxis = inputSize[axis];
    double exponent = std::log(static_cast<double>(sizeAxis)) / std::log(static_cast<double>(scaleFactor));
    // check that exponent is integer: the fractional part is 0
    double exponentIntPart;
    double exponentFractionPart = std::modf(exponent, &exponentIntPart);
    // DEV (phcerdan): Increased epsilon due to numeric errors with Alpine-musl (Travis CI).
    // Failing test when exponent == log(27) / log(3).
    if (itk::Math::FloatAlmostEqual(exponentFractionPart, 0.0, 4, 10 * itk::NumericTraits<double>::epsilon()))
    {
      exponentPerAxis[axis] = static_cast<unsigned int>(exponent);
    }
    else
    {
      // increase valid levels until the division size/scale_factor gives a non-integer.
      auto sizeAtLevel = static_cast<double>(sizeAxis);
      for (;;)
      {
        double division = sizeAtLevel / static_cast<double>(scaleFactor);
        double intPartDivision;
        double fractionPartDivision = std::modf(division, &intPartDivision);
        if (itk::Math::FloatAlmostEqual(fractionPartDivision, 0.0))
        {
          exponentPerAxis[axis]++;
          sizeAtLevel = intPartDivision;
        }
        else
        {
          break;
        }
      }
    }
  }
  // return the min_element of array:
  //  - 1 is any size is not divisible by scale factor
  return *std::min_element(exponentPerAxis.Begin(), exponentPerAxis.End());
}

} // end namespace utils
} // end namespace itk

#endif
