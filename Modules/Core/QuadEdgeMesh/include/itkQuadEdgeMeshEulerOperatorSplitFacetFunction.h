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
#ifndef itkQuadEdgeMeshEulerOperatorSplitFacetFunction_h
#define itkQuadEdgeMeshEulerOperatorSplitFacetFunction_h

#include "itkQuadEdgeMeshFunctionBase.h"

namespace itk
{
/**
 * \class QuadEdgeMeshEulerOperatorSplitFacetFunction
 * \brief Given two edges h and g sharing the same Left() face, create a
 * new edge joining h->Destination() to g->Destination(), thus splitting
 * the original Left().
 *
 * \ingroup QuadEdgeMeshModifierFunctions
 * \ingroup ITKQuadEdgeMesh
 */
template <typename TMesh, typename TQEType>
class ITK_TEMPLATE_EXPORT QuadEdgeMeshEulerOperatorSplitFacetFunction
  : public QuadEdgeMeshFunctionBase<TMesh, TQEType *>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(QuadEdgeMeshEulerOperatorSplitFacetFunction);

  /** Standard class type aliases. */
  using Self = QuadEdgeMeshEulerOperatorSplitFacetFunction;
  using Superclass = QuadEdgeMeshFunctionBase<TMesh, TQEType *>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  itkNewMacro(Self);
  /** Run-time type information (and related methods). */
  itkTypeMacro(QuadEdgeMeshEulerOperatorSplitFacetFunction, QuadEdgeMeshFunctionBase);

  /** Type of QuadEdge with which to apply slicing. */
  using QEType = TQEType;

  using MeshType = typename Superclass::MeshType;
  using OutputType = typename Superclass::OutputType;
  using EdgeCellType = typename Superclass::EdgeCellType;

  /** Evaluate at the specified input position */
  virtual OutputType
  Evaluate(QEType * h, QEType * g);

protected:
  QuadEdgeMeshEulerOperatorSplitFacetFunction() = default;
  ~QuadEdgeMeshEulerOperatorSplitFacetFunction() override = default;
};
} // end namespace itk

#include "itkQuadEdgeMeshEulerOperatorSplitFacetFunction.hxx"

#endif
