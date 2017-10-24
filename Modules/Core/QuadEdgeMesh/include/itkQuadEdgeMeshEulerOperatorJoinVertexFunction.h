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
#ifndef itkQuadEdgeMeshEulerOperatorJoinVertexFunction_h
#define itkQuadEdgeMeshEulerOperatorJoinVertexFunction_h

#include "itkQuadEdgeMeshFunctionBase.h"

#include <stack>

namespace itk
{
/**
 * \class QuadEdgeMeshEulerOperatorJoinVertexFunction
 *
 * \brief Collapse a given edge by joining its dest and its org.
 *
 * Collapse the argument edge e of Evaluate
 * by joining the two vertices incident to e (i.e. its endpoints).
 * The destination vertex of e is set aside (i.e. disconneted from its
 * edge entry and no edge has this vertex as endpoint). Note that the
 * vertex itself is not removed from the container (and hence there is no
 * loss of geometrical information). On success JoinVertex returns the Id
 * of the disconected vertex (i.e. the destination of e) and it is up to
 * the caller to "take care" of it.
 *
 * Precondition: the edge should be adjacent at least to an other edge
 * (i.e. not be isolated at both endpoints).
 *
 * \warning JoinVertex.Evaluate( h) and JoinVertex.Evaluate( h->GetSym() )
 * are topologically identical. But their differ in their geometrical
 * result, since JoinVertex removes the destination vertex of the
 * incoming argument. A simple way to visualize this difference is to
 * imagine a mesh constituted of squares laid out on a regular grid
 * and for each such square an added diagonal. No imagine what happens
 * when one operates JoinVertex on a diagonal edge of a corner square
 * (and how it differs with feeding JoinVertex with the opposite edge).
 *
 * \ingroup QEMeshModifierFunctions
 * \ingroup ITKQuadEdgeMesh
 */
template< typename TMesh, typename TQEType >
class ITK_TEMPLATE_EXPORT QuadEdgeMeshEulerOperatorJoinVertexFunction:
  public QuadEdgeMeshFunctionBase< TMesh, TQEType * >
{
public:
  /** Standard class typedefs. */
  typedef QuadEdgeMeshEulerOperatorJoinVertexFunction  Self;
  typedef SmartPointer< Self >                         Pointer;
  typedef SmartPointer< const Self      >              ConstPointer;
  typedef QuadEdgeMeshFunctionBase< TMesh, TQEType * > Superclass;

  itkNewMacro(Self);
  /** Run-time type information (and related methods). */
  itkTypeMacro(QuadEdgeMeshEulerOperatorJoinVertexFunction, QuadEdgeMeshFunctionBase);

  /** Type of QuadEdge with which to apply slicing. */
  typedef TQEType QEType;

  typedef typename Superclass::MeshType   MeshType;
  typedef typename Superclass::OutputType OutputType;

  typedef typename MeshType::PointIdentifier PointIdentifier;
  typedef typename MeshType::CellIdentifier  CellIdentifier;
  typedef typename MeshType::FaceRefType     FaceRefType;

  /** Evaluate at the specified input position */
  virtual OutputType Evaluate(QEType *h);

  enum EdgeStatusType {
    STANDARD_CONFIG = 0,
    EDGE_NULL,                     //1
    MESH_NULL,                     //2
    EDGE_ISOLATED,                 //3
    TOO_MANY_COMMON_VERTICES,      //4
    TETRAHEDRON_CONFIG,            //5
    QUADEDGE_ISOLATED,             //6
    FACE_ISOLATED,                 //7
    SAMOSA_CONFIG,                 //8
    EYE_CONFIG,                    //9
    EDGE_JOINING_DIFFERENT_BORDERS //10
    };

  itkGetConstMacro(OldPointID, PointIdentifier);
  itkGetConstMacro(EdgeStatus, EdgeStatusType);

protected:
  QuadEdgeMeshEulerOperatorJoinVertexFunction();
  ~QuadEdgeMeshEulerOperatorJoinVertexFunction() ITK_OVERRIDE {}

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  PointIdentifier m_OldPointID;

  EdgeStatusType m_EdgeStatus;

  /**
   * \brief
   * \param[in] e
   * \return The number of common vertices in the 0-ring of e->GetOrigin() and
   * e->GetDestination()
   */
  PointIdentifier CommonVertexNeighboor(QEType *e);

  /**
   * \brief
   * \param[in] e
   * \return true if it is a tetrahedron
   * \return false else
   */
  bool IsTetrahedron(QEType *e);

  /**
   * \brief
   * \param[in] e
   * \param[in] iWasLeftFace
   * \param[out] oToBeDeleted
   * \return true if the face is isolated
   * \return false else
   */
  bool IsFaceIsolated(QEType *e, const bool & iWasLeftFace,
                      std::stack< TQEType * > & oToBeDeleted);

  bool IsSamosa(QEType *e);

  bool IsEye(QEType *e);

  bool IsEdgeLinkingTwoDifferentBorders(QEType *e);

  EdgeStatusType CheckStatus(QEType *e, std::stack< TQEType * > & oToBeDeleted);

  QEType * Process(QEType *e);

  QEType * ProcessIsolatedQuadEdge(QEType *e);

  QEType * ProcessIsolatedFace(QEType *e, std::stack< QEType * > & EdgesToBeDeleted
                               );

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(QuadEdgeMeshEulerOperatorJoinVertexFunction);
};
} // namespace itk

#include "itkQuadEdgeMeshEulerOperatorJoinVertexFunction.hxx"

#endif

// eof - itkQuadEdgeMeshEulerOperatorJoinVertexFunction.h
