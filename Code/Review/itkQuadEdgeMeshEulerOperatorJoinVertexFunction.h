/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkQuadEdgeMeshEulerOperatorJoinVertexFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkQuadEdgeMeshEulerOperatorJoinVertexFunction_h
#define __itkQuadEdgeMeshEulerOperatorJoinVertexFunction_h

#include "itkQuadEdgeMeshFunctionBase.h"

#include <list>
#include <algorithm>

namespace itk
{

//FIXME http://www.cgal.org/Manual/doc_html/cgal_manual/Polyhedron_ref/Class_Polyhedron_3.html
/**
 * \class EulerOperatorJoinVertexFunction
 * \ingroup QEMeshModifierFunctions 
 * 
 * \brief Collapse a given edge by joining its dest and its org.
 *
 * Collapse the argument edge e of \ref Evaluate by joining the two vertices
 * incident to e (i.e. its endpoints). The destination vertex of e is set
 * aside (i.e. disconneted from its edge entry and no edge has this vertex
 * as endpoint). Note that the vertex itself is not removed from the container
 * (and hence there is no loss of geometrical information). On success
 * JoinVertex returns the Id of the disconected vertex (i.e. the destination
 * of e) and it is up to the caller to "take care" of it.
 * Precondition: the edge should be adjacent at least to an other edge
 * (i.e. not be isolated at both endpoints).
 * \warning JoinVertex.Evaluate( h) and JoinVertex.Evaluate( h->GetSym() )
 *    are topologically identical. But their differ in their geometrical
 *    result, since JoinVertex removes the destination vertex of the
 *    incoming argument. A simple way to visualize this difference is to
 *    imagine a mesh constituted of squares layed out on a regular grid
 *    and for each such square an added diagonal. No imagine what happens
 *    when one operates JoinVertex on a diagonal edge of a corner square
 *    (and how it differs with feeding JoinVertex with the opposite edge).
 * \sa The operator \ref itkQE::EulerOperatorJoinVertexFunction can be 
 *    seen as the inverse operator.
 */
template < class TMesh, class TQEType >
class ITK_EXPORT QuadEdgeMeshEulerOperatorJoinVertexFunction :
   public QuadEdgeMeshFunctionBase< TMesh, TQEType* >
{
public:
  /** Standard class typedefs. */
  typedef QuadEdgeMeshEulerOperatorJoinVertexFunction     Self;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self      >                 ConstPointer;
  typedef QuadEdgeMeshFunctionBase< TMesh, TQEType* >     Superclass;
  
  itkNewMacro( Self );
  /** Run-time type information (and related methods). */
  itkTypeMacro( QuadEdgeMeshEulerOperatorJoinVertexFunction, QuadEdgeMeshFunctionBase );

  /** Type of QuadEdge with which to apply slicing. */
  typedef TQEType QEType;

  typedef typename Superclass::MeshType   MeshType;
  typedef typename Superclass::OutputType OutputType;

  typedef typename MeshType::PointIdentifier PointIdentifier;
  typedef typename MeshType::FaceRefType     FaceRefType;

  /** Evaluate at the specified input position */
  virtual OutputType Evaluate( QEType* h );

  PointIdentifier GetOldPointID( )
    {
    return( this->m_OldPointID );
    }
  
protected:
  QuadEdgeMeshEulerOperatorJoinVertexFunction()
    {
    m_OldPointID = (PointIdentifier)0;
    }
  ~QuadEdgeMeshEulerOperatorJoinVertexFunction(){};

private:
  QuadEdgeMeshEulerOperatorJoinVertexFunction(const Self& ); //purposely not implemented
  void operator=(const Self& );        //purposely not implemented

  PointIdentifier m_OldPointID;

  bool CommonVertexNeighboor( QEType* e );

};

} // namespace itk

#include "itkQuadEdgeMeshEulerOperatorJoinVertexFunction.txx"

#endif

// eof - itkQuadEdgeMeshEulerOperatorJoinVertexFunction.h
