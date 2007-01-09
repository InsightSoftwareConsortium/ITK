// -------------------------------------------------------------------------
// itkQEMeshWithDual.txx
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

#ifndef __ITKQUADEDGEMESHWITHDUAL__MESH__TXX__
#define __ITKQUADEDGEMESHWITHDUAL__MESH__TXX__

namespace itkQE
{
   template< typename TPixel, unsigned int VDimension, typename TTraits >
      MeshWithDual< TPixel, VDimension, TTraits >:: 
      MeshWithDual()
      : Superclass( )
   { 
      this->m_DualPointsSet = PointSetType::New();
      this->m_DualPointsSet->SetPoints(PointsContainer::New());
   }

   template< typename TPixel, unsigned int VDimension, typename TTraits >
      typename MeshWithDual< TPixel, VDimension, TTraits >::QEPrimal*
      MeshWithDual< TPixel, VDimension, TTraits >:: 
      AddFace( PointIdList& points )
   {
      // first call the superclass to add the face
      QEPrimal* EdgeHandle = Superclass::AddFace(points);

      // if it worked properly, it returns an handle on one of the QE of the face
      if ( EdgeHandle )
         if ( !EdgeHandle->IsLeftSet( ) )
         {

            unsigned int sum = 0;
            VectorType vec;
            vec.Fill( 0 );

            // we can then use an iterator over the QE of the face
            QEIterator lit = EdgeHandle->BeginGeomLnext( );

            // along with the ID of the dual point we are going to create
            PointIdentifier pointId = m_DualPointsSet->GetNumberOfPoints();

            // sum up the coordinates of the primal points in vec
            // count the number of faces in sum to later normalyze vec
            // link the dual QE with the to-be-created dual point
            for( ; lit != EdgeHandle->EndGeomLnext( ); lit++ ) 
            {
               QEPrimal* g = lit.Value( );
               vec += this->GetVector( g->GetOrg( ) );
               sum++;
               g->SetLeft( pointId );

            } //ENDFOR

            // Normalyze
            vec /= CoordRepType( sum );
            PointType p = vec;

            // set the dual point in the container
            m_DualPointsSet->SetPoint( pointId, p );

         } //ENDIF

      return ( EdgeHandle );

   }

}

#endif
