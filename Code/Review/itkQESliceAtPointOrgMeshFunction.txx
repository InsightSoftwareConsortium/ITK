// -------------------------------------------------------------------------
// itkQESliceAtPointOrgMeshFunction.txx
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
#ifndef __ITKQUADEDGEMESH__ITKQESLICEATPOINTORGMESHFUNCTION__TXX__
#define __ITKQUADEDGEMESH__ITKQESLICEATPOINTORGMESHFUNCTION__TXX__

#include "itkQEMesh.h"  // Just to mark the dependance towards this class.

namespace itkQE
{

template < class TMesh, class TQEType >
  typename SliceAtPointOrgMeshFunction< TMesh, TQEType >::OutputType
  SliceAtPointOrgMeshFunction< TMesh, TQEType >::
  Evaluate( QEType* entryEdge )
{
   if( !entryEdge )
   {
       itkWarningMacro( "Called with no argument." );
       return( QEType::NOPOINT );
   } // fi

   if( entryEdge->IsOrgInternal( ) )
   {
       itkWarningMacro( "Internal point." );
       return( QEType::NOPOINT );
   } // fi

   /* Assume the point Org (the origin of the incoming edge i.e.
    * entryEdge->Orgv()) is (at least) TWICE on the border (i.e. it
    * is at least twice adjacent to noface). We can consider the situation
    * of the following diagram, which depicts some Onext() ring around
    * point Org:
    *
    *              \         /
    *               \   *   /
    *               e3     e2              counter-clockwise
    *         *       \   /   NO FACE      Onext() order.
    *                  \ /
    *        ----e4----Org---e1------
    *                  /|\
    *      NO FACE    / | \
    *                /  ^  \    *  <------ a * indicates the
    *               /   e   \              the presence of a face
    *              /    n    \
    *            e5     t     e7
    *            /   *  r  *   \
    *           /       y       \
    *
    * The result of this method is then:
    *
    *         \         /
    *          \   *   /
    *          e3     e2
    *    *       \   /
    *             \ /    NO FACE
    * -Oborder---newOrg
    *
    *                  Org----border----
    *      NO FACE     /|\
    *                 / | \
    *                /  |  \    *
    *               /   |   \
    *              /    |    \
    *            e5    e6     e7
    *            /   *  |  *   \
    *           /       |       \
    */

   QEType* borderEdge = entryEdge->GetNextBorderEdgeWithUnsetLeft( );
   if( ! borderEdge )
   {
       itkWarningMacro( "No boundary edge found." );
       return( QEType::NOPOINT );
   } // fi

   QEType* otherBorderEdge =
      entryEdge->GetNextBorderEdgeWithUnsetLeft( borderEdge->GetOnext( ) );
   if( borderEdge == otherBorderEdge )
   {
       itkDebugMacro( "NOT twice adjacent to boundary point" );
       return( QEType::NOPOINT );
   } // fi

   // We now only need to do two things:
   //  1/ make the proper SPlice at Org
   //  2/ duplicate the geometry of ORG to newORG in order to have
   //     an itk compliant Mesh.

   // Handle connectivity at QE level:
   OutputType result = this->m_Mesh->Splice( borderEdge, otherBorderEdge );

   this->m_Mesh->Modified( );
   return result;
}

} // namespace itkQE

#endif // __ITKQUADEDGEMESH__ITKQESLICEATPOINTORGMESHFUNCTION__TXX__

// eof - itkQESliceAtPointOrgMeshFunction.txx

