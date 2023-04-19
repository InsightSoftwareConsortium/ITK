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
#ifndef itkImageHelper_h
#define itkImageHelper_h

#include "itkMacro.h" // For ITK_TEMPLATE_EXPORT.

namespace itk
{
/** \class ImageHelper
 *  \brief Fast Index/Offset computation
 *
 * These helper methods use recursive templates to unroll the loops
 * of simple calculations. The resulting speed improvement varies from
 * compiler to compiler. Some gcc compilers with debug turned on
 * exhibit slight speed increases, but most compilers see
 * improvement. The ComputeOffset performance improvement is
 * impressive. For example, the Windows VS7.0 compiler shows almost a
 * factor of 2 speed improvement with the recursive templates. Usually
 * recursive templates use partial specialization to terminate
 * loops. Here we use a technique used by Brad King in the itk Concept
 * Checking code.
 *
 * \note This work is part of the National Alliance for Medical Image
 * Computing (NAMIC), funded by the National Institutes of Health
 * through the NIH Roadmap for Medical Research, Grant U54 EB005149.
 * Information on the National Centers for Biomedical Computing
 * can be obtained from http://commonfund.nih.gov/bioinformatics.
 *
 * \ingroup ITKCommon
 */

// Forward reference because of circular dependencies
template <unsigned int VImageDimension>
class ITK_TEMPLATE_EXPORT ImageBase;

template <unsigned int VImageDimension, unsigned int VLoop>
class ImageHelper
{
public:
  using ImageType = ImageBase<VImageDimension>;
  using IndexType = typename ImageType::IndexType;
  using OffsetType = typename ImageType::OffsetType;
  using OffsetValueType = typename ImageType::OffsetValueType;
  using IndexValueType = typename ImageType::IndexValueType;

  /** ComputeIndex with recursive templates */
  inline static void
  ComputeIndex(const IndexType &                      bufferedRegionIndex,
               OffsetValueType                        offset,
               [[maybe_unused]] const OffsetValueType offsetTable[],
               IndexType &                            index)
  {
    static_assert(VLoop <= VImageDimension);

    if constexpr (VLoop > 1)
    {
      constexpr unsigned int loopIndex{ VLoop - 1 };

      index[loopIndex] = static_cast<IndexValueType>(offset / offsetTable[loopIndex]);
      offset -= (index[loopIndex] * offsetTable[loopIndex]);
      index[loopIndex] += bufferedRegionIndex[loopIndex];
      ImageHelper<VImageDimension, loopIndex>::ComputeIndex(bufferedRegionIndex, offset, offsetTable, index);
    }
    else
    {
      static_assert(VLoop == 1);

      // Do last
      index[0] = bufferedRegionIndex[0] + static_cast<IndexValueType>(offset);
    }
  }

  // ComputeOffset
  //
  inline static void
  ComputeOffset(const IndexType &                      bufferedRegionIndex,
                const IndexType &                      index,
                [[maybe_unused]] const OffsetValueType offsetTable[],
                OffsetValueType &                      offset)
  {
    static_assert(VLoop <= VImageDimension);

    if constexpr (VLoop > 1)
    {
      constexpr unsigned int loopIndex{ VLoop - 1 };

      offset += (index[loopIndex] - bufferedRegionIndex[loopIndex]) * offsetTable[loopIndex];
      ImageHelper<VImageDimension, loopIndex>::ComputeOffset(bufferedRegionIndex, index, offsetTable, offset);
    }
    else
    {
      static_assert(VLoop == 1);

      // Do last
      offset += index[0] - bufferedRegionIndex[0];
    }
  }
};
} // end namespace itk

#endif
