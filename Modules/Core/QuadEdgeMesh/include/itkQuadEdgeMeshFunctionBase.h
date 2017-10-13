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
#ifndef itkQuadEdgeMeshFunctionBase_h
#define itkQuadEdgeMeshFunctionBase_h

#include "itkObject.h"
#include "itkObjectFactory.h"

namespace itk
{
/**
 * \class MeshFunctionBase
 * \ingroup QEMeshModifierFunctions
 * \brief Base class for mesh function object modifiers.
 *
 * MeshFunctionBase is the base class for itkQE function objects specialised
 * in Mesh "small" (reduced in range) modification.
 * Subclasses of itk::FunctionBase cannot modify their InputType since
 * the signature of their Evaluate( const InputType& ) method guarantees it.
 * Consider a method that modifies (the geometry, the connectivity or both)
 * a mesh.
 * For large modifications of this mesh we follow the classical itk Filter
 * schema, which implies duplicating the mesh which can be space consuming.
 * But for small modifications (think of the Euler operators) that an
 * algorithm needs to apply many times, this systematic duplication can
 * be daunting.
 * MeshFunctionBase thus offers a leightweight alternative to itk Filter.
 * Subclasses of MeshFunctionBase, which should override Evaluate(), are
 * function objects that apply reduced and localised modifications
 * (geometry, or connectivity) on the InputType mesh.
 *
 * This class is template over the mesh type (to be modified) and
 * the output (usually a created/deleted vertex or face) type.
 *
 * \ingroup Functions
 *
 * \ingroup ITKQuadEdgeMesh
 */
template< typename TMesh, typename TOutput >
class QuadEdgeMeshFunctionBase:public Object
{
public:
  /** Standard class typedefs. */
  typedef QuadEdgeMeshFunctionBase   Self;
  typedef Object                     Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(QuadEdgeMeshFunctionBase, Object);

  /** Mesh type that must be modified */
  typedef TMesh                           MeshType;
  typedef typename MeshType::EdgeCellType EdgeCellType;

  /** Output type */
  typedef TOutput OutputType;

  /** Set the mesh to be modified */
  virtual void SetInput(MeshType *input)
  {
    this->m_Mesh = input;
  }

  /** Evaluate at the specified input position */
  //virtual OutputType Evaluate( )
  //  {
  //  return( (OutputType) 0 );
  //  }

protected:
  QuadEdgeMeshFunctionBase()
  {
    m_Mesh = (MeshType *)ITK_NULLPTR;
  }

  ~QuadEdgeMeshFunctionBase() ITK_OVERRIDE {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(QuadEdgeMeshFunctionBase);

protected:
  /** Mesh on which to apply the modification */
  MeshType *m_Mesh;
};
} // namespace itk

#endif

// eof - itkQuadEdgeMeshFunctionBase.h
