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
template< typename TMesh, typename TQEType >
class ITK_TEMPLATE_EXPORT QuadEdgeMeshEulerOperatorSplitFacetFunction:
  public QuadEdgeMeshFunctionBase< TMesh, TQEType * >
{
public:
  /** Standard class typedefs. */
  typedef QuadEdgeMeshEulerOperatorSplitFacetFunction  Self;
  typedef QuadEdgeMeshFunctionBase< TMesh, TQEType * > Superclass;
  typedef SmartPointer< Self >                         Pointer;
  typedef SmartPointer< const Self >                   ConstPointer;

  itkNewMacro(Self);
  /** Run-time type information (and related methods). */
  itkTypeMacro(QuadEdgeMeshEulerOperatorSplitFacetFunction, QuadEdgeMeshFunctionBase);

  /** Type of QuadEdge with which to apply slicing. */
  typedef TQEType QEType;

  typedef typename Superclass::MeshType     MeshType;
  typedef typename Superclass::OutputType   OutputType;
  typedef typename Superclass::EdgeCellType EdgeCellType;

  /** Evaluate at the specified input position */
  virtual OutputType Evaluate(QEType *h, QEType *g);

protected:
  QuadEdgeMeshEulerOperatorSplitFacetFunction(){}
  ~QuadEdgeMeshEulerOperatorSplitFacetFunction() ITK_OVERRIDE {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(QuadEdgeMeshEulerOperatorSplitFacetFunction);
};
} // namespace itk

#include "itkQuadEdgeMeshEulerOperatorSplitFacetFunction.hxx"

#endif

// eof - itkQuadEdgeMeshEulerOperatorSplitFacetFunction.h
