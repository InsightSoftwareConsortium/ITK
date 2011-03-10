/*=========================================================================
 *
 *  Copyright Insight Software Consortium
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
#include "itkHexahedronCellTopology.h"

namespace itk
{
/**
 * The hexahedron's topology data: Edges.
 */
const int
HexahedronCellTopology
:: m_Edges[12][2] =
            { { 0, 1 }, { 1, 2 }, { 3, 2 }, { 0, 3 },
              { 4, 5 }, { 5, 6 }, { 7, 6 }, { 4, 7 },
              { 0, 4 }, { 1, 5 }, { 3, 7 }, { 2, 6 } };

/**
 * The hexahedron's topology data: Faces.
 */
const int
HexahedronCellTopology
:: m_Faces[6][4] =
            { { 0, 4, 7, 3 }, { 1, 2, 6, 5 },
              { 0, 1, 5, 4 }, { 3, 7, 6, 2 },
              { 0, 3, 2, 1 }, { 4, 5, 6, 7 } };

HexahedronCellTopology
::HexahedronCellTopology()
{}

HexahedronCellTopology
::~HexahedronCellTopology()
{}
} // end namespace itk
