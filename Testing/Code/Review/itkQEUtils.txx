#ifndef __ITKQUADEDGEMESH__LIGHTWEIGHTVTKUTILS__H__
#define __ITKQUADEDGEMESH__LIGHTWEIGHTVTKUTILS__H__

#include "itkQESanityCheckMeshFunction.h"
#include "itkQEBoundaryRepresentativeEdgesMeshFunction.h"

namespace itkQE
{
/**
 * A small utility that checks the topological invariants
 * (number of vertices, edges, faces and boundaries)
 * @param mesh          The mesh to be checked
 * @param refNumPoints  The reference number of vertices
 * @param refNumEdges   The reference number of edges
 * @param refNumFaces   The reference number of faces
 * @param refNumBounds  The reference number of boundaries (default: -1)
 * @param refTwiceGenus Twice the reference genus (default: -1)
 * \warning Assertion is only applied for the reference arguments that are
 *          positive or null.
 * \return True when invariants are correct, False otherwise
 */
template< class TMesh>
   bool
   AssertTopologicalInvariants(
      typename TMesh::Pointer mesh,
      long refNumPoints,
      long refNumEdges,
      long refNumFaces,
      long refNumBounds  = -1,
      long refTwiceGenus = -1 )
{
  typedef itkQE::BoundaryRepresentativeEdgesMeshFunction< TMesh >
    BoundaryRepresentativeEdges;
  typename BoundaryRepresentativeEdges::Pointer boundaryRepresentativeEdges
    = BoundaryRepresentativeEdges::New();

  unsigned long numPoints = mesh->ComputeNumberOfPoints();
  unsigned long numEdges  = mesh->ComputeNumberOfEdges();
  unsigned long numFaces  = mesh->ComputeNumberOfFaces();

  int numBounds = boundaryRepresentativeEdges->Evaluate( *mesh )->size();

  if( refNumPoints >= 0 )
    {
    if( numPoints != (unsigned long)refNumPoints )
      {
      std::cout << "ERROR: numPoints= " << numPoints << " refNumPoints="
      << refNumPoints << std::endl;
      return false;
      }
     }

  if( refNumEdges >= 0 )
    {
    if( numEdges != refNumEdges )
      {
      std::cout << "ERROR: numEdges= " << numEdges << " refNumEdges= "
      << refNumEdges << std::endl;
      return false;
      }
    }

  if( refNumFaces >= 0 )
    {
    if( numFaces != refNumFaces )
      {
      std::cout << "ERROR: numFaces= " << numFaces << " refNumFaces= "
      << refNumFaces << std::endl;
      return false;
      }
    }

  if( refNumBounds >= 0 )
    {
    if( numBounds != refNumBounds )
      {
      std::cout << "ERROR: numBounds= " << numBounds << " refNumBounds= "
      << refNumBounds << std::endl;
      return false;
      }
    }

  if( refTwiceGenus >= 0 )
    {
    int twiceGenus = 2 - numBounds - numFaces + numEdges - numPoints;

    if( twiceGenus != refTwiceGenus )
      {
      std::cout << "ERROR: twiceGenus= " << twiceGenus << " refTwiceGenus= "
      << refTwiceGenus << std::endl;
      return false;
      }
    }

  typedef itkQE::SanityCheckMeshFunction< TMesh > SanityCheck;

  if ( ! SanityCheck::New()->Evaluate( *mesh ) )
    {
    std::cout << "ERROR: SanityCheck returned false" << std::endl;
    }

  return true;
}

} // enamespace

#endif // __ITKQUADEDGEMESH__LIGHTWEIGHTVTKUTILS__H__
