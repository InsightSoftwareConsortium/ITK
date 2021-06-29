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

#ifndef itkIsotropicWaveletsUtilities_h
#define itkIsotropicWaveletsUtilities_h

#include "itkRieszUtilities.h"
#include "itkWaveletUtilities.h"

namespace itk
{
/**
 * Utility class for wrappings purposes.
 *
 * The static functions call the free function versions from
 * itkRieszUtilities.h and itkWaveletUtilities.h
 *
 * From python, it can be used as:
 * itk.IsotropicWaveletsUtilities.Factorial(2)
 */
struct IsotropicWaveletsUtilities
{
  using IndexPairType = itk::utils::IndexPairType;

  /// Factorial
  static long
  Factorial(const long n);

  /**
   * Compute number of components p(N, d), where N = Order, d = Dimension.
   * p(N,d) = (N + d - 1)!/( (d-1)! N! )
   *
   * @param order N of the Riesz transform
   * @param dimension d of the image
   *
   * @return NumberOfComponents given the order for the ImageDimension.
   */
  static unsigned int
  ComputeNumberOfComponents(const unsigned int & order, const unsigned int & dimension);

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
  static IndexPairType
  IndexToLevelBandSteerablePyramid(unsigned int linearIndex, unsigned int levels, unsigned int bands);

  /** Compute max number of levels depending on the size of the image.
   * Return J: $ J = \text{min_element}\{J_0,\ldots, J_d\} $;
   * where each $J_i$ is the  number of integer divisions that can be done with the $i$ size and the scale factor.
   * returns 1 if any size is not divisible by the scale factor.
   * Size<2> version.
   */
  static unsigned int
  ComputeMaxNumberOfLevels(const Size<2> & inputSize, const unsigned int & scaleFactor);
  /** Compute max number of levels depending on the size of the image.
   * Return J: $ J = \text{min_element}\{J_0,\ldots, J_d\} $;
   * where each $J_i$ is the  number of integer divisions that can be done with the $i$ size and the scale factor.
   * returns 1 if any size is not divisible by the scale factor.
   * Size<3> version.
   */
  static unsigned int
  ComputeMaxNumberOfLevels(const Size<3> & inputSize, const unsigned int & scaleFactor);
};
} // end namespace itk
#endif
