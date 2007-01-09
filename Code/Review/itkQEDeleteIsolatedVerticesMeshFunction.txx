// -------------------------------------------------------------------------
// itkQEDeleteIsolatedVerticesMeshFunction.txx
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
// - The duck master (Alex Gouaillard) gouaillard@creatis.insa-lyon.fr
// - The cow  master (Leonardo Florez) florez@creatis.insa-lyon.fr
// -------------------------------------------------------------------------
#ifndef __ITKQUADEDGEMESH__ITKQEDELETEISOLATEDVERTICESMESHFUNCTION__TXX__
#define __ITKQUADEDGEMESH__ITKQEDELETEISOLATEDVERTICESMESHFUNCTION__TXX__

#include "itkQEMesh.h"  // Just to mark the dependance towards this class.

namespace itkQE
{

template < class TMesh >
  typename DeleteIsolatedVerticesMeshFunction< TMesh >::OutputType
  DeleteIsolatedVerticesMeshFunction< TMesh >::
  Evaluate( )
{
   typedef typename MeshType::PointsContainer         PointsContainer;
   typedef typename MeshType::PointsContainerIterator PointsContainerIterator;
   typedef typename MeshType::PointIdentifier         PointIdentifier;

   PointsContainer* points = this->m_Mesh->GetPoints( );
   PointsContainerIterator pointIterator = points->Begin();
   PointsContainerIterator pointEnd      = points->End();
   while( pointIterator != pointEnd )
   {
       if(  !pointIterator.Value( ).GetEdge() )
       {
           // We first need to advance the iterator before deleting
           // (or we end up in trouble with dangling iterator):
           PointIdentifier toDeletePid = pointIterator.Index( );
           ++pointIterator;
           this->m_Mesh->DeletePoint( toDeletePid );
       }
       else
           ++pointIterator;
   } // elihw
   this->Modified( );
}

} // namespace itkQE

#endif // __ITKQUADEDGEMESH__ITKQEDELETEISOLATEDVERTICESMESHFUNCTION__TXX__

// eof - itkQEDeleteIsolatedVerticesMeshFunction.txx

