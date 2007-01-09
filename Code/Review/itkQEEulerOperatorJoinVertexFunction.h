// -------------------------------------------------------------------------
// itkQEEulerOperatorJoinVertexFunction.h
// $Revision: 1.1 $
// $Author: sylvain $
// $Name:  $
// $Date: 2007-01-09 00:58:17 $
// -------------------------------------------------------------------------
// This code is an implementation of the well known quad edge (QE) data
// structure in the ITK library. Although the original QE can handle non
// orientable 2-manifolds and its dual and its mirror, this implementation
// is specifically dedicated to handle orientable 2-manifolds along with
// their dual.
//
// Any comment, criticism and/or donation is welcome.
//
// Please contact any member of the team:
//
// - The frog master (Eric Boix)       eboix@ens-lyon.fr
// - The duck master (Alex Gouaillard) alexandre.gouaillard@sun.com
// - The cow  master (Leonardo Florez) florez@creatis.insa-lyon.fr
// -------------------------------------------------------------------------
#ifndef __ITKQUADEDGEMESH__ITKQEEULEROPERATORJOINVERTEXFUNCTION__H__
#define __ITKQUADEDGEMESH__ITKQEEULEROPERATORJOINVERTEXFUNCTION__H__

#include "itkQEMeshFunctionBase.h"

namespace itkQE
{

//FIXME http://www.cgal.org/Manual/doc_html/cgal_manual/Polyhedron_ref/Class_Polyhedron_3.html
/**
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
class ITK_EXPORT EulerOperatorJoinVertexFunction :
   public MeshFunctionBase< TMesh, TQEType* >
{
public:
  /** Standard class typedefs. */
  typedef EulerOperatorJoinVertexFunction     Self;
  typedef MeshFunctionBase< TMesh, TQEType* > Superclass;
  typedef itk::SmartPointer< Self >           Pointer;
  typedef itk::SmartPointer< const Self >     ConstPointer;
  
  itkNewMacro( Self );
  /** Run-time type information (and related methods). */
  itkTypeMacro( EulerOperatorJoinVertexFunction, MeshFunctionBase );

  /** Type of QuadEdge with which to apply slicing. */
  typedef TQEType QEType;

  typedef typename Superclass::MeshType   MeshType;
  typedef typename Superclass::OutputType OutputType;

  typedef typename MeshType::PointIdentifier PointIdentifier;
  typedef typename MeshType::FaceRefType FaceRefType;

  /** Evaluate at the specified input position */
  virtual OutputType Evaluate( QEType* h );
  
protected:
  EulerOperatorJoinVertexFunction(){};
  ~EulerOperatorJoinVertexFunction(){};

private:
  EulerOperatorJoinVertexFunction(const Self& ); //purposely not implemented
  void operator=(const Self& );        //purposely not implemented
};

} // namespace itkQE

#include "itkQEEulerOperatorJoinVertexFunction.txx"

#endif // __ITKQUADEDGEMESH__ITKQEEULEROPERATORJOINVERTEXFUNCTION__H__

// eof - itkQEEulerOperatorJoinVertexFunction.h

