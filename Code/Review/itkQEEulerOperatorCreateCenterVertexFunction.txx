// -------------------------------------------------------------------------
// itkQEEulerOperatorCreateCenterVertexFunction.txx
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
#ifndef __ITKQUADEDGEMESH__ITKQEEULEROPERATORCREATECENTERVERTEXFUNCTION__TXX__
#define __ITKQUADEDGEMESH__ITKQEEULEROPERATORCREATECENTERVERTEXFUNCTION__TXX__

#include "itkQEMesh.h"  // Just to mark the dependance towards this class.

namespace itkQE
{
template < class TMesh, class TQEType >
  typename EulerOperatorCreateCenterVertexFunction< TMesh, TQEType >::OutputType
  EulerOperatorCreateCenterVertexFunction< TMesh, TQEType >::
  Evaluate( QEType* e )
{
   // Is there any input ?
   if( !this->m_Mesh )
   {
      itkDebugMacro( "No mesh present." );
      return( (OutputType) 0 );
   }

   // Is left face set ?
   if ( !e->IsLeftSet( ) )
   {
      itkDebugMacro( "Argument edge has no left face." );
      return( (OutputType) 0 );
   }
   
   // FIXME: why is this not defined from itkQEMesh ??
   int PointDimension = 3;
      
   // remove left face
   this->m_Mesh->DeleteFace( e->GetLeft( ) );
   
   /**************************/
   /*  Create center vertex  */
   /**************************/
   
   // loop on edges around the face, stock geometry and count occurence
   PointType oMeanPt;
   PointType PtTmp;
   QEType* edgeRef;
   CoordRepType degree = 1.;
   PtTmp = this->m_Mesh->GetPoint( e->GetOrg( ) );
   for( unsigned int k = 0; k < PointDimension; k++ )
      oMeanPt[k] = PtTmp[k];

   for( edgeRef = e->GetLnext( );
        edgeRef != e;
        edgeRef = edgeRef->GetLnext( ) )
   {
      PtTmp = this->m_Mesh->GetPoint( edgeRef->GetOrg( ) );
      degree += 1.;

      for( unsigned int k = 0; k < PointDimension; k++ )
         oMeanPt[k] += PtTmp[k];

   } // end of for
   
   // average geometry
   for( unsigned int k=0; k < PointDimension; k++ )
         oMeanPt[k] /= degree;

   // add new point to mesh
   PointIdentifier PointID = this->m_Mesh->AddPoint( oMeanPt );
   PointIdentifier tempPoint;
   
   // create edges and faces
   tempPoint = e->GetDest( );
   this->m_Mesh->AddFaceTriangle( PointID, 
                                  e->GetOrg( ), 
                                  tempPoint 
                                  );                                     
   edgeRef = this->m_Mesh->FindEdge( PointID, tempPoint );
   while ( !edgeRef->IsLeftSet( ) )
   {
      tempPoint = edgeRef->GetLnext( )->GetDest( );                               
      this->m_Mesh->AddFaceTriangle( PointID, 
                                     edgeRef->GetLnext( )->GetOrg( ), 
                                     tempPoint 
                                     );
      edgeRef = this->m_Mesh->FindEdge( PointID, tempPoint );
   }                               
   
   return( e );

}

} // namespace itkQE

#endif // __ITKQUADEDGEMESH__ITKQEEULEROPERATORCREATECENTERVERTEXFUNCTION__TXX__

// eof - itkQEEulerOperatorCreateCenterVertexFunction.txx

