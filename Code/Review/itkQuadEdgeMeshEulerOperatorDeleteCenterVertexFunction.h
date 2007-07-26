// -------------------------------------------------------------------------
// itkQuadEdgeMeshEulerOperatorDeleteCenterVertexFunction.h
// $Revision: 1.1 $
// $Author: hanfei $
// $Name:  $
// $Date: 2007-07-26 06:30:26 $
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
#ifndef __itkQuadEdgeMeshEulerOperatorDeleteCenterVertexFunction_h
#define __itkQuadEdgeMeshEulerOperatorDeleteCenterVertexFunction_h

#include "itkQuadEdgeMeshFunctionBase.h"

namespace itk
{
/**
 * \class EulerOperatorDeleteCenterVertexFunction
 * \ingroup QEMeshModifierFunctions
 *
 * \brief Delete the vertex, connected edges and faces and create a new face
 *        In place of the previous vertex' one-ring.
 */
template < class TMesh, class TQEType >
class ITK_EXPORT QuadEdgeMeshEulerOperatorDeleteCenterVertexFunction :
   public QuadEdgeMeshFunctionBase< TMesh, TQEType* >
{
public:
  /** Standard class typedefs. */
  typedef QuadEdgeMeshEulerOperatorDeleteCenterVertexFunction      Self;
  typedef QuadEdgeMeshFunctionBase< TMesh, TQEType* >              Superclass;
  typedef itk::SmartPointer< Self >                                Pointer;
  typedef itk::SmartPointer< const Self >                          ConstPointer;
  
  itkNewMacro( Self );
  /** Run-time type information (and related methods). */
  itkTypeMacro( QuadEdgeMeshEulerOperatorDeleteCenterVertexFunction, QuadEdgeMeshFunctionBase );

  /** Type of QuadEdge with which to apply slicing. */
  typedef TQEType QEType;

  typedef typename Superclass::MeshType   MeshType;
  typedef typename Superclass::OutputType OutputType;

  typedef typename MeshType::PointIdentifier PointIdentifier;
  typedef typename MeshType::FaceRefType     FaceRefType;

  /** Evaluate at the specified input position */
  virtual OutputType Evaluate( QEType* e );
  PointIdentifier GetOldPointID( )
    {
    return( this->m_OldPointID );
    }
protected:
  QuadEdgeMeshEulerOperatorDeleteCenterVertexFunction(){};
  ~QuadEdgeMeshEulerOperatorDeleteCenterVertexFunction(){};

private:
  QuadEdgeMeshEulerOperatorDeleteCenterVertexFunction(const Self& );
  //purposely not implemented
  void operator=(const Self& );
  //purposely not implemented
  PointIdentifier m_OldPointID;

};

} // namespace itk

#include "itkQuadEdgeMeshEulerOperatorDeleteCenterVertexFunction.txx"

#endif

// eof - itkQuadEdgeMeshEulerOperatorDeleteCenterVertexFunction.h
