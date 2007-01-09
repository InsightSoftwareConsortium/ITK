// -------------------------------------------------------------------------
// itkQECleanUpToManifoldMeshFunction.txx
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
#ifndef __ITKQUADEDGEMESH__ITKQECLEANUPTOMANIFOLDMESHFUNCTION__TXX__
#define __ITKQUADEDGEMESH__ITKQECLEANUPTOMANIFOLDMESHFUNCTION__TXX__

#include "itkQEMesh.h"  // Just to mark the dependance towards this class.
#include "itkQECleanUpToManifoldMeshFunction.h"
#include "itkQEDeleteIsolatedVerticesMeshFunction.h"

namespace itkQE
{

template < class TMesh, class TOutput >
  typename CleanUpToManifoldMeshFunction< TMesh, TOutput >::OutputType
  CleanUpToManifoldMeshFunction< TMesh, TOutput >::Evaluate( )
{
   // First remove all edges not adjacent to a face
   typedef typename MeshType::CellsContainerIterator CellsContainerIterator;
   CellsContainerIterator cellIterator = this->m_Mesh->GetCells()->Begin();
   CellsContainerIterator cellEnd      = this->m_Mesh->GetCells()->End();
   while( cellIterator != cellEnd )
   {
      if ( QEPrimal* edge =
                        dynamic_cast< QEPrimal* >( cellIterator.Value()) )
      {
         // We first need to advance the iterator (in case and) before
         // deletion (or we end up in trouble with dangling iterator):
         ++cellIterator;
         if( edge->IsWire( ) )
            this->m_Mesh->DeleteEdge( edge );
      }
      else
         ++cellIterator;
   }

   // Then remove all the isolated vertices (including the potential
   // ones left by the first stage):
   typedef DeleteIsolatedVerticesMeshFunction< MeshType >
                                                 DeleteIsolatedVertices;
   typename DeleteIsolatedVertices::Pointer deleteIsolatedVertices
                           = DeleteIsolatedVertices::New( );
   deleteIsolatedVertices->SetInput( this->m_Mesh );
   deleteIsolatedVertices->Evaluate( );
   this->m_Mesh->Modified( );
}

} // namespace itkQE

#endif // __ITKQUADEDGEMESH__ITKQECLEANUPTOMANIFOLDMESHFUNCTION__TXX__

// eof - itkQECleanUpToManifoldMeshFunction.txx

