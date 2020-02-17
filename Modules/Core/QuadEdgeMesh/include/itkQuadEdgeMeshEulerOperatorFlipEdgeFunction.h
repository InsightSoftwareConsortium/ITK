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
#ifndef itkQuadEdgeMeshEulerOperatorFlipEdgeFunction_h
#define itkQuadEdgeMeshEulerOperatorFlipEdgeFunction_h

#include "itkQuadEdgeMeshFunctionBase.h"
#include "ITKQuadEdgeMeshExport.h"

namespace itk
{
/** \class QuadEdgeMeshEulerOperatorFlipEdgeFunctionEnums
 *
 * \brief Contains enumerations for QuadEdgeMeshEulerOperatorFlipEdgeFunction class.
 *
 * \ingroup ITKQuadEdgeMesh
 * */
class QuadEdgeMeshEulerOperatorFlipEdgeFunctionEnums
{
public:
  /***\class EdgeStatusType
   * \ingroup ITKQuadEdgeMesh
   * Status of edge
   */
  enum class EdgeStatusType : uint8_t
  {
    STANDARD_CONFIG = 0,
    EDGE_NULL,
    MESH_NULL,
    NON_INTERNAL_EDGE,
    NON_TRIANGULAR_RIGHT_FACE,
    NON_TRIANGULAR_LEFT_FACE,
    EXISTING_OPPOSITE_EDGE
  };
};
// Define how to print enumerations
extern ITKQuadEdgeMesh_EXPORT std::ostream &
                              operator<<(std::ostream & out, QuadEdgeMeshEulerOperatorFlipEdgeFunctionEnums::EdgeStatusType value);

/**
 * \class QuadEdgeMeshEulerOperatorFlipEdgeFunction
 * \brief Flip an edge.
 *
 * The original FlipEdge operator required both faces of the input edge
 * to be triangles (and to be set). This version does not have such requirement.
 * Either or both faces can be polygonal, the org and dest of the edge
 * is then "rotated" around the big polygon that would exist if the two faces
 * of the edge e were joined.
 *
 * \ingroup QEMeshModifierFunctions
 * \ingroup ITKQuadEdgeMesh
 */
template <typename TMesh, typename TQEType>
class ITK_TEMPLATE_EXPORT QuadEdgeMeshEulerOperatorFlipEdgeFunction : public QuadEdgeMeshFunctionBase<TMesh, TQEType *>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(QuadEdgeMeshEulerOperatorFlipEdgeFunction);

  /** Standard class type aliases. */
  using Self = QuadEdgeMeshEulerOperatorFlipEdgeFunction;
  using Superclass = QuadEdgeMeshFunctionBase<TMesh, TQEType *>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  itkNewMacro(Self);
  /** Run-time type information (and related methods). */
  itkTypeMacro(QuadEdgeMeshEulerOperatorFlipEdgeFunction, QuadEdgeMeshFunctionBase);

  /** Type of QuadEdge with which to apply slicing. */
  using QEType = TQEType;

  using MeshType = typename Superclass::MeshType;
  using OutputType = typename Superclass::OutputType;

  using EdgeStatusEnum = QuadEdgeMeshEulerOperatorFlipEdgeFunctionEnums::EdgeStatusType;
#if !defined(ITK_LEGACY_REMOVE)
  // We need to expose the enum values at the class level
  // for backwards compatibility
  static constexpr EdgeStatusEnum STANDARD_CONFIG = EdgeStatusEnum::STANDARD_CONFIG;
  static constexpr EdgeStatusEnum EDGE_NULL = EdgeStatusEnum::EDGE_NULL;
  static constexpr EdgeStatusEnum MESH_NULL = EdgeStatusEnum::MESH_NULL;
  static constexpr EdgeStatusEnum NON_INTERNAL_EDGE = EdgeStatusEnum::NON_INTERNAL_EDGE;
  static constexpr EdgeStatusEnum NON_TRIANGULAR_RIGHT_FACE = EdgeStatusEnum::NON_TRIANGULAR_RIGHT_FACE;
  static constexpr EdgeStatusEnum NON_TRIANGULAR_LEFT_FACE = EdgeStatusEnum::NON_TRIANGULAR_LEFT_FACE;
  static constexpr EdgeStatusEnum EXISTING_OPPOSITE_EDGE = EdgeStatusEnum::EXISTING_OPPOSITE_EDGE;
#endif
  /** Evaluate at the specified input position */
  virtual OutputType
  Evaluate(QEType * h);

  // itkGetConstMacro( EdgeStatus, EdgeStatusType );

protected:
  QuadEdgeMeshEulerOperatorFlipEdgeFunction();
  ~QuadEdgeMeshEulerOperatorFlipEdgeFunction() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  EdgeStatusEnum m_EdgeStatus{ EdgeStatusEnum::STANDARD_CONFIG };

  void
  CheckStatus(QEType * h);

  OutputType
  Process(QEType * h);
};
} // end namespace itk

#include "itkQuadEdgeMeshEulerOperatorFlipEdgeFunction.hxx"

#endif
