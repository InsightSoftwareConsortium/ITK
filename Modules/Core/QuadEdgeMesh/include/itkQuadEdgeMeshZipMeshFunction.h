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
#ifndef itkQuadEdgeMeshZipMeshFunction_h
#define itkQuadEdgeMeshZipMeshFunction_h

#include "itkQuadEdgeMeshFunctionBase.h"

namespace itk
{
/**
 * \class QuadEdgeMeshZipMeshFunction
 * \brief Fuse the incoming edge and it's Onext() follower (like a zipper does).
 *
 * \ingroup QEMeshModifierFunctions
 * \ingroup ITKQuadEdgeMesh
 */
template <typename TMesh, typename TQEType>
class ITK_TEMPLATE_EXPORT QuadEdgeMeshZipMeshFunction
  : public QuadEdgeMeshFunctionBase<TMesh, typename TQEType::OriginRefType>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(QuadEdgeMeshZipMeshFunction);

  /** Standard class type aliases. */
  using Self = QuadEdgeMeshZipMeshFunction;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  using Superclass = QuadEdgeMeshFunctionBase<TMesh, typename TQEType::OriginRefType>;

  itkNewMacro(Self);
  /** Run-time type information (and related methods). */
  itkTypeMacro(QuadEdgeMeshZipMeshFunction, QuadEdgeMeshFunctionBase);

  /** Type of QuadEdge with which to apply slicing. */
  using QEType = TQEType;

  using MeshType = typename Superclass::MeshType;
  using OutputType = typename Superclass::OutputType;

  /**
   * \return The OriginRefType of the point that will be removed during the
   * zipping process.
   */
  virtual OutputType
  Evaluate(QEType * e);

protected:
  QuadEdgeMeshZipMeshFunction() = default;
  ~QuadEdgeMeshZipMeshFunction() override = default;
};
} // end namespace itk

#include "itkQuadEdgeMeshZipMeshFunction.hxx"

#endif
