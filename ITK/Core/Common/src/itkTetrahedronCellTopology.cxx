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
#include "itkTetrahedronCellTopology.h"

namespace itk
{
/**
 * The tetrahedron's topology data: Faces
 */
const int
TetrahedronCellTopology
:: m_Faces[4][3] = { { 0, 1, 3 }, { 1, 2, 3 }, { 2, 0, 3 }, { 0, 2, 1 } };

/**
 * The tetrahedron's topology data: Faces
 */
const int
TetrahedronCellTopology
:: m_Edges[6][2] = { { 0, 1 }, { 1, 2 }, { 2, 0 }, { 0, 3 }, { 1, 3 }, { 2, 3 } };

TetrahedronCellTopology
::TetrahedronCellTopology()
{}

TetrahedronCellTopology
::~TetrahedronCellTopology()
{}
} // end namespace itk
