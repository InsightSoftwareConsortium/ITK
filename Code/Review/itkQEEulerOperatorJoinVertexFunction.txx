// -------------------------------------------------------------------------
// itkQEEulerOperatorJoinVertexFunction.txx
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
#ifndef __ITKQUADEDGEMESH__ITKQEEULEROPERATORJOINVERTEXFUNCTION__TXX__
#define __ITKQUADEDGEMESH__ITKQEEULEROPERATORJOINVERTEXFUNCTION__TXX__

#include "itkQEMesh.h"  // Just to mark the dependance towards this class.
#include "itkQEZipMeshFunction.h"

namespace itkQE
{

//--------------------------------------------------------------------------
template < class TMesh, class TQEType >
typename EulerOperatorJoinVertexFunction< TMesh, TQEType >::OutputType
EulerOperatorJoinVertexFunction< TMesh, TQEType >::
Evaluate( QEType* e )
{
   if( !this->m_Mesh )
   {
      itkDebugMacro( "No mesh present." );
      return( (OutputType) 0 );
   }

   if( e->IsIsolated( ) && e->GetSym( )->IsIsolated( ) )
   {
      itkDebugMacro( "Argument edge isolated." );
      return( (OutputType) 0 );
   }
   
   // test to return removed Point ID
   PointIdentifier removedPointId = e->GetDest( );

   if( e->IsIsolated( ) || e->GetSym( )->IsIsolated( ) )
   {
      // One the endpoints (and only one) of the incoming edge is isolated.
      // Instead of "shrinking" the edge it suffice to delete it. Note that
      // this also avoids trouble since the definition of leftZip or riteZip
      // would be improper in this case (in fact leftZip or riteZip would
      // in fact be e or e->GetSym( )...
      //
      // When e is adjacent to a face, we must retrieve the edge [ a, b ]
      // in order to rebuild the face after edge deletion. If the left face
      // of e is set then the right face is also set... since it is the same
      // face ! Here is the situation:
      //
      //        b----a----X
      //        |    |    |
      //        |    e    |
      //        |    |    |
      //        |         |
      //        X----X----X
      //
      // We are not yet sure of the orientation of e and which endpoint 
      // of e is attached in a. Find it out:
      QEType* rebuildEdge = (QEType*)0;
      if( e->IsLeftSet( ) )
      {
         if( e->IsIsolated( ) )
         {
            rebuildEdge = e->GetSym( )->GetOprev( );
         }
         else
         {
            rebuildEdge = e->GetOprev( );
         }
      }
      this->m_Mesh->LightWeightDeleteEdge( e );
      if( rebuildEdge )
      {
         this->m_Mesh->AddFace( rebuildEdge );
      }
      return( (OutputType) 0 );
   }

   bool wasLeftFace     = e->IsLeftSet( );
   bool wasRiteFace     = e->IsRightSet( );
   bool wasLeftTriangle = e->IsLnextOfTriangle( );
   bool wasRiteTriangle = e->GetSym( )->IsLnextOfTriangle( );
   QEType* leftZip = e->GetLnext( );
   QEType* riteZip = e->GetOprev( );

   /*
    *                   \   |   /
    *                    \  |  / 
    *                     \ | /
    *    ------------------ b ------------- Y
    *                  ___/ |               |  
    *     _<_leftZip__/     |               |
    *    /                  ^               |
    *   X      left         e     rite      |
    *    \____________      |               |
    *                 \___  |               |
    *                     \ |               |
    *    ------------------ a --riteZip->-- Y
    *                     / | \
    *                    /  |  \
    *                   /   |   \
    */
   // typename QEType::OrgRefType a = e->GetOrg( );
   // typename QEType::OrgRefType b = e->GetDest( );
   // (void)a; (void)b;
   this->m_Mesh->LightWeightDeleteEdge( e );
   removedPointId = this->m_Mesh->Splice( leftZip, riteZip );

   /*
    *                           |      /       __Y 
    *                           |     /     __/  |  
    *      __<_leftZip___       |    /   __/     |
    *     /              \      |   / __/        |
    *    /                \__   |  / /           |
    *   X      NOFACE      __ [a = b]    NOFACE  |
    *    \                /  / / | \ \__         |
    *     \______________/ _/ /  |  \   \__      |
    *                   __/  /   |   \  riteZip  |
    *                __/    /    |    \       \__|
    *               /      /     |     \         Y
    *
    * When the Lnext and/or the Rnext ring of the argument edge was originaly
    * the one[s] of a triangle, the above edge deletion created the odd
    * situation of having two different edges adjacent to the same two
    * vertices (which is quite a bad thing). This is was is depicted on 
    * the above ascii-graph, the original left face was a triangle and
    * the resulting situation has two different edges adjacent to the
    * two vertices X and a. In order to clean up things, we can call the
    * Zip(MeshFunction) algorithm which handles this case.
    * Once things are back to normal, we recreate faces when they were
    * originaly present.
    */

   typedef itkQE::ZipMeshFunction< MeshType, QEType > Zip;
   if( wasLeftTriangle )
   {
      typename Zip::Pointer zip = Zip::New( );
      zip->SetInput( this->m_Mesh );
      if( QEType::NOPOINT != zip->Evaluate( leftZip ) )
      {
         itkDebugMacro( "Zip must return NoPoint (left)." );
         return( (OutputType) 0 );
      }
   } 
   else if( wasLeftFace )
   {
      this->m_Mesh->AddFace( leftZip );
   }
   
   if( wasRiteTriangle )
   {
      typename Zip::Pointer zip = Zip::New( );
      zip->SetInput( this->m_Mesh );
      if( QEType::NOPOINT != zip->Evaluate( riteZip ) )
      {
         itkDebugMacro( "Zip must return NoPoint (right)." );
         return( (OutputType) 0 );
      }
   }
   else if( wasRiteFace )
   {
      this->m_Mesh->AddFace( riteZip );
   }

   // this->m_Mesh->DeletePoint( removedPointId );
   // FIXME: get the Id of the removed vertex i.e. removedPointId 
   //        and return it !
   //        This requires changing the signature of the method of course

   //should return removed point ID
   return( (OutputType) 0 );

}

} // namespace itkQE

#endif // __ITKQUADEDGEMESH__ITKQEEULEROPERATORJoinVertexFUNCTION__TXX__

// eof - itkQEEulerOperatorJoinVertexFunction.txx

