// -------------------------------------------------------------------------
// itkQuadEdgeMeshEulerOperatorCreateCenterVertexFunction.h
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
#ifndef __itkQuadEdgeMeshEulerOperatorCreateCenterVertexFunction_h
#define __itkQuadEdgeMeshEulerOperatorCreateCenterVertexFunction_h

#include "itkQuadEdgeMeshFunctionBase.h"

namespace itk
{
/**
 * \class EulerOperatorCreateCenterVertexFunction
 * \ingroup QEMeshModifierFunctions
 *
 * \brief Create a vertex at the barycenter of the given face.
 *
 */
template < class TMesh, class TQEType >
class ITK_EXPORT QuadEdgeMeshEulerOperatorCreateCenterVertexFunction :
   public QuadEdgeMeshFunctionBase< TMesh, TQEType* >
{
public:
  /** Standard class typedefs. */
  typedef QuadEdgeMeshEulerOperatorCreateCenterVertexFunction      Self;
  typedef QuadEdgeMeshFunctionBase< TMesh, TQEType* >              Superclass;
  typedef itk::SmartPointer< Self >                                Pointer;
  typedef itk::SmartPointer< const Self >                          ConstPointer;

  itkNewMacro( Self );
  /** Run-time type information (and related methods). */
  itkTypeMacro( QuadEdgeMeshEulerOperatorCreateCenterVertexFunction, QuadEdgeMeshFunctionBase );

  /** Type of QuadEdge with which to apply slicing. */
  typedef TQEType QEType;

  typedef typename Superclass::MeshType                MeshType;
  typedef typename Superclass::OutputType              OutputType;

  typedef typename MeshType::PointIdentifier           PointIdentifier;
  typedef typename MeshType::PointType                 PointType;
  typedef typename MeshType::CoordRepType              CoordRepType;
  typedef typename MeshType::VectorType                VectorType;

  /** Evaluate at the specified input position */
  virtual OutputType Evaluate( QEType* e );
  PointIdentifier GetNewPointID( )
    {
    return( this->m_NewPointID );
    };

protected:
  QuadEdgeMeshEulerOperatorCreateCenterVertexFunction( )
    {
    this->m_NewPointID = (PointIdentifier)0;
    }

  ~QuadEdgeMeshEulerOperatorCreateCenterVertexFunction( ) { };

private:
  QuadEdgeMeshEulerOperatorCreateCenterVertexFunction( const Self& );
  //purposely not implemented
  void operator=( const Self& );
  //purposely not implemented
  PointIdentifier m_NewPointID;

};

} // namespace itk

#include "itkQuadEdgeMeshEulerOperatorCreateCenterVertexFunction.txx"

#endif // __ITKQUADEDGEMESH__ITKQEEULEROPERATORCREATECENTERVERTEXFUNCTION__H__

// eof - itkQuadEdgeMeshEulerOperatorCreateCenterVertexFunction.h
