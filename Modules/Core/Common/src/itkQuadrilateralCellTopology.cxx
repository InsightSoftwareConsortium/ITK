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
#include "itkQuadrilateralCellTopology.h"

namespace itk
{
/**
 * The quadrilateral's topology data: Edges.
 */
const int QuadrilateralCellTopology ::m_Edges[4][2] = { { 0, 1 }, { 1, 2 }, { 2, 3 }, { 3, 0 } };

QuadrilateralCellTopology::QuadrilateralCellTopology() = default;

QuadrilateralCellTopology::~QuadrilateralCellTopology() = default;
} // end namespace itk
