// -------------------------------------------------------------------------
// itkQuadEdgeMeshTopologyChecker.h
// $Revision: 1.1 $
// $Author: ibanez $
// $Name:  $
// $Date: 2007-01-13 12:42:15 $
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
#ifndef __itkQuadEdgeMeshTopologyChecker_h
#define __itkQuadEdgeMeshTopologyChecker_h


namespace itk
{

/** \brief Make some basic checks in order to assert that the considered
 *         mesh is not degenerated and correctly represents a surface
 *         with a potential boundary.
 *
 * We check that they are no isolated vertices, no isolated edges and
 * that the Euler formula is possible.
 */
template< class TMesh >
class ITK_EXPORT QuadEdgeMeshTopologyChecker : public Object
{
public:
  // Standard types
  typedef QuadEdgeMeshTopologyChecker          Self;
  typedef Object                               Superclass;
  typedef SmartPointer< Self >                 Pointer;
  typedef SmartPointer< const Self >           ConstPointer;

  typedef TMesh                                MeshType;

public:
  itkNewMacro( Self );
  itkTypeMacro( QuadEdgeMeshTopologyChecker, Object );

  itkSetConstObjectMacro( Mesh, MeshType ); 

  // FIXME this probably should be taken from the traits of the Mesh
  typedef unsigned long IdentifierType; 

  itkSetMacro( ExpectedNumberOfPoints, IdentifierType );
  itkSetMacro( ExpectedNumberOfEdges, IdentifierType );
  itkSetMacro( ExpectedNumberOfFaces, IdentifierType );
  itkSetMacro( ExpectedNumberOfBoundaries, IdentifierType );
  itkSetMacro( ExpectedGenus, IdentifierType );

  bool ValidateEulerCharacteristic() const;

protected:
  QuadEdgeMeshTopologyChecker();

private:
  QuadEdgeMeshTopologyChecker( const Self& ); //purposely not implemented 
  void operator=( const Self& );     //purposely not implemented    

  typedef typename MeshType::ConstPointer   MeshPointer;

  MeshPointer   m_Mesh;
 
  IdentifierType  m_ExpectedNumberOfPoints;
  IdentifierType  m_ExpectedNumberOfEdges;
  IdentifierType  m_ExpectedNumberOfFaces;
  IdentifierType  m_ExpectedNumberOfBoundaries;
  IdentifierType  m_ExpectedGenus;
};

} 

#if ITK_TEMPLATE_TXX
#include "itkQuadEdgeMeshTopologyChecker.txx"
#endif 

#endif 

