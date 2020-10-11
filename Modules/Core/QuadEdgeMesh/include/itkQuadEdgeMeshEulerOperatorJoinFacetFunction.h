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
#ifndef itkQuadEdgeMeshEulerOperatorJoinFacetFunction_h
#define itkQuadEdgeMeshEulerOperatorJoinFacetFunction_h

#include "itkQuadEdgeMeshFunctionBase.h"

namespace itk
{
/**
 * \class QuadEdgeMeshEulerOperatorJoinFacetFunction
 * \brief Join the two facets which are on both sides of a given internal edge.
 *
 * \ingroup QEMeshModifierFunctions
 * \ingroup ITKQuadEdgeMesh
 */
template <typename TMesh, typename TQEType>
class ITK_TEMPLATE_EXPORT QuadEdgeMeshEulerOperatorJoinFacetFunction : public QuadEdgeMeshFunctionBase<TMesh, TQEType *>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(QuadEdgeMeshEulerOperatorJoinFacetFunction);

  /** Standard class type aliases. */
  using Self = QuadEdgeMeshEulerOperatorJoinFacetFunction;
  using Superclass = QuadEdgeMeshFunctionBase<TMesh, TQEType *>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  itkNewMacro(Self);
  /** Run-time type information (and related methods). */
  itkTypeMacro(QuadEdgeMeshEulerOperatorJoinFacetFunction, QuadEdgeMeshFunctionBase);

  /** Type of QuadEdge with which to apply slicing. */
  using QEType = TQEType;

  using MeshType = typename Superclass::MeshType;
  using OutputType = typename Superclass::OutputType;

  /** Evaluate at the specified input position */
  virtual OutputType
  Evaluate(QEType * e);

protected:
  QuadEdgeMeshEulerOperatorJoinFacetFunction() = default;
  ~QuadEdgeMeshEulerOperatorJoinFacetFunction() override = default;
};
} // end namespace itk

#include "itkQuadEdgeMeshEulerOperatorJoinFacetFunction.hxx"

#endif
