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
#ifndef itkQuadraticTriangleCellTopology_h
#define itkQuadraticTriangleCellTopology_h

#include "itkMacro.h"

namespace itk
{
/** \class QuadraticTriangleCellTopology
 *  \brief QuadraticTriangleCellTopology holds data defining the topological
 *         connections of the vertices and edges of a TriangleCell.
 *
 * This class is used to localize static variables out of .hxx
 * files. This prevents multiple definition of static variables.
 *
 * \ingroup MeshObjects
 * \ingroup ITKCommon
 */

class ITKCommon_EXPORT QuadraticTriangleCellTopology
{
public:
  QuadraticTriangleCellTopology();
  virtual ~QuadraticTriangleCellTopology();

protected:
  /** Triangle topology data. */
  static const int m_Edges[3][3];
};
} // end namespace itk

#endif
