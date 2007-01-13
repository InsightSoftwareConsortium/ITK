// -------------------------------------------------------------------------
// itkQESanityCheckMeshFunction.txx
// $Revision: 1.2 $
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
// - The duck master (Alex Gouaillard) gouaillard@creatis.insa-lyon.fr
// - The cow  master (Leonardo Florez) florez@creatis.insa-lyon.fr
// -------------------------------------------------------------------------
#ifndef __ITKQUADEDGEMESH__ITKQESANITYCHECKMESHFUNCTION__TXX__
#define __ITKQUADEDGEMESH__ITKQESANITYCHECKMESHFUNCTION__TXX__

#include "itkQESanityCheckMeshFunction.h" 
#include "itkQuadEdgeMeshBoundaryEdgesMeshFunction.h"

namespace itk
{

template< typename TMesh >
  typename SanityCheckMeshFunction< TMesh >::OutputType
  SanityCheckMeshFunction< TMesh >::Evaluate( const InputType& mesh )
  const
{
   typedef typename MeshType::QEPrimal        QEPrimal;  
   typedef typename MeshType::CellsContainerConstIterator
                                              CellsContainerConstIterator; 
   typedef QuadEdgeMeshBoundaryEdgesMeshFunction< MeshType > BoundaryEdges;
   typename BoundaryEdges::Pointer boundaryEdges = BoundaryEdges::New( );

   unsigned long numPoints = mesh.ComputeNumberOfPoints( );
   unsigned long numEdges  = mesh.ComputeNumberOfEdges( );
   unsigned long numFaces  = mesh.ComputeNumberOfFaces( );
   unsigned long numBounds = boundaryEdges->Evaluate( mesh )->size( );

   if( mesh.GetNumberOfPoints() != numPoints )
     {
     // They are isolated vertices:
     return( false );
     }

   // The euler formula states:
   //     numFaces - numEdges + numPoints == 2 - 2 * genus - numBounds
   // hence ( 2 - numBounds - numFaces + numEdges - numPoints ) must
   // be an odd number. Let's check it out:
   unsigned long twiceGenus = 2 - numBounds - numFaces + numEdges - numPoints;

   if ( twiceGenus % 2 )
     {
     return( false );
     }

   // Look is they are isolated edges
   CellsContainerConstIterator cellIterator = mesh.GetCells()->Begin();
   CellsContainerConstIterator cellEnd      = mesh.GetCells()->End();
   while( cellIterator != cellEnd )
   {
      if ( QEPrimal* edge = dynamic_cast< QEPrimal* >( cellIterator.Value()) )
      {
         if( edge->IsWire( ) ) 
         {
            return( false );
         }
      }
      ++cellIterator;
   }

   return( true );
};

} // enamespace

#endif // __ITKQUADEDGEMESH__ITKQESANITYCHECKMESHFUNCTION__TXX__

// eof - itkQESanityCheckMeshFunction.txx
