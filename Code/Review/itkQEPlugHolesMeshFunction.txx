// -------------------------------------------------------------------------
// itkQEPlugHolesMeshFunction.txx
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
#ifndef __ITKQUADEDGEMESH__ITKQEPLUGHOLESMESHFUNCTION__TXX__
#define __ITKQUADEDGEMESH__ITKQEPLUGHOLESMESHFUNCTION__TXX__

#include "itkQEMesh.h"  // Just to mark the dependance towards this class.
#include "itkQEBoundaryRepresentativeEdgesMeshFunction.h"

namespace itkQE
{

template < class TMesh >
  typename PlugHolesMeshFunction< TMesh >::OutputType
  PlugHolesMeshFunction< TMesh >::
  Evaluate( )
{
   if( !this->m_Mesh )
   {
       itkWarningMacro( "No mesh present." );
       return;
   } // fi

   typedef itkQE::BoundaryRepresentativeEdgesMeshFunction< MeshType >
                                           BoundaryRepresentativeEdges;
   typename BoundaryRepresentativeEdges::Pointer boundaryRepresentativeEdges =
                                           BoundaryRepresentativeEdges::New( );

   typename MeshType::EdgeListType* boundaries =
                        boundaryRepresentativeEdges->Evaluate( *this->m_Mesh );

   if( boundaries->empty( ) )
   {
      return;
   }

   typedef typename MeshType::QEPrimal QEPrimal;
   while( ! boundaries->empty( ) )
   {
      QEPrimal* bdryEdge = boundaries->front( );
      boundaries->pop_front( );
      // Follow, with Lnext(), the boundary:
      typename MeshType::PointIdList pList;
      typename QEPrimal::IteratorGeom it = bdryEdge->BeginGeomLnext( );
      for( ; it != bdryEdge->EndGeomLnext( ); it++ )
      {
          pList.push_back( it.Value( )->GetOrg( ) );
      }
      this->m_Mesh->AddFace( pList );
   } // elihw
}

} // namespace itkQE

#endif // __ITKQUADEDGEMESH__ITKQEPLUGHOLESMESHFUNCTION__TXX__

// eof - itkQEPlugHolesMeshFunction.txx

