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
#ifndef itkBresenhamLine_h
#define itkBresenhamLine_h

#include "itkVector.h"
#include "itkIndex.h"
#include <vector>

namespace itk
{

/** \class BresenhamLine
 * \brief Compute indices along a line in n dimensions.
 *
 * Returns an array of indices that are offsets along the line.
 * The line can be described by a direction vector and a length.
 * The line can be described by starting and ending indices.
 *
 * \ingroup ImageAccess
 * \ingroup ITKCommon
 *
 * \sphinx
 * \sphinxexample{Core/Common/BresenhamLine,Bresenham Line}
 * \endsphinx
 */
template <unsigned int VDimension>
class ITK_TEMPLATE_EXPORT BresenhamLine
{
public:
  using Self = BresenhamLine;
  // This defines the line direction
  using LType = Vector<float, VDimension>;
  using OffsetType = Offset<VDimension>;
  using IndexType = Index<VDimension>;
  using OffsetArray = std::vector<OffsetType>;
  using IndexArray = std::vector<IndexType>;

  // constructors
  BresenhamLine() = default;
  ~BresenhamLine() = default;

  /** Build a line in a specified Direction. */
  OffsetArray
  BuildLine(LType Direction, IdentifierType length);

  /** Build a line between two pixels. */
  IndexArray
  BuildLine(IndexType p0, IndexType p1);
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkBresenhamLine.hxx"
#endif

#endif
