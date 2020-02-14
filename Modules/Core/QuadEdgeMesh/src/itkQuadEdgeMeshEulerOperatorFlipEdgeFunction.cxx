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
#include "../include/itkQuadEdgeMeshEulerOperatorFlipEdgeFunction.h"

namespace itk
{
/** Print enum values */
std::ostream &
operator<<(std::ostream & out, const QuadEdgeMeshEulerOperatorFlipEdgeFunctionEnums::EdgeStatusType value)
{
  return out << [value] {
    switch (value)
    {
      case QuadEdgeMeshEulerOperatorFlipEdgeFunctionEnums::EdgeStatusType::STANDARD_CONFIG:
        return "itk::QuadEdgeMeshEulerOperatorFlipEdgeFunctionEnums::EdgeStatusType::STANDARD_CONFIG";
      case QuadEdgeMeshEulerOperatorFlipEdgeFunctionEnums::EdgeStatusType::EDGE_NULL:
        return "itk::QuadEdgeMeshEulerOperatorFlipEdgeFunctionEnums::EdgeStatusType::EDGE_NULL";
      case QuadEdgeMeshEulerOperatorFlipEdgeFunctionEnums::EdgeStatusType::MESH_NULL:
        return "itk::QuadEdgeMeshEulerOperatorFlipEdgeFunctionEnums::EdgeStatusType::MESH_NULL";
      case QuadEdgeMeshEulerOperatorFlipEdgeFunctionEnums::EdgeStatusType::NON_INTERNAL_EDGE:
        return "itk::QuadEdgeMeshEulerOperatorFlipEdgeFunctionEnums::EdgeStatusType::NON_INTERNAL_EDGE";
      case QuadEdgeMeshEulerOperatorFlipEdgeFunctionEnums::EdgeStatusType::NON_TRIANGULAR_RIGHT_FACE:
        return "itk::QuadEdgeMeshEulerOperatorFlipEdgeFunctionEnums::EdgeStatusType::NON_TRIANGULAR_RIGHT_FACE";
      case QuadEdgeMeshEulerOperatorFlipEdgeFunctionEnums::EdgeStatusType::NON_TRIANGULAR_LEFT_FACE:
        return "itk::QuadEdgeMeshEulerOperatorFlipEdgeFunctionEnums::EdgeStatusType::NON_TRIANGULAR_LEFT_FACE";
      case QuadEdgeMeshEulerOperatorFlipEdgeFunctionEnums::EdgeStatusType::EXISTING_OPPOSITE_EDGE:
        return "itk::QuadEdgeMeshEulerOperatorFlipEdgeFunctionEnums::EdgeStatusType::EXISTING_OPPOSITE_EDGE";
      default:
        return "INVALID VALUE FOR itk::QuadEdgeMeshEulerOperatorFlipEdgeFunctionEnums::EdgeStatusType";
    }
  }();
}
} // end namespace itk
