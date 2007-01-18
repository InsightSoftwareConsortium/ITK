// -------------------------------------------------------------------------
// itkQuadEdgeMeshToQuadEdgeMeshFilter.txx
// $Revision: 1.1 $
// $Author: sylvain $
// $Name:  $
// $Date: 2007-01-18 19:22:19 $
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

#ifndef __ITKQUADEDGEMESH__MESHCOPY__TXX__
#define __ITKQUADEDGEMESH__MESHCOPY__TXX__

namespace itkQE
{
    // ---------------------------------------------------------------------
    template< class TInputMesh, class TOutputMesh >
        MeshCopy< TInputMesh, TOutputMesh >::
        MeshCopy( )
            : Superclass( )
    {
        this->Superclass::SetNumberOfRequiredInputs( 1 );
        this->Superclass::SetNumberOfRequiredOutputs( 1 );

        typename TInputMesh::Pointer out = TInputMesh::New( );
        this->Superclass::SetNthOutput( 0, out.GetPointer( ) );
    }

    // ---------------------------------------------------------------------
    template< class TInputMesh, class TOutputMesh >
        void MeshCopy< TInputMesh, TOutputMesh >::
        GenerateData( )
    {
        InputMeshConstPointer in = this->GetInput( );
        OutputMeshPointer out = this->GetOutput( );

        // Copy points
        InputPointsContainerConstIterator inIt = in->GetPoints( )->Begin( );
        for( ; inIt != in->GetPoints( )->End( ); inIt++ )
        {
            OutputPointType pOut;
            pOut.CastFrom( inIt.Value( ) );
            out->SetPoint( inIt.Index( ), pOut );
        } // rof

        // Copy cells
        InputCellsContainerConstIterator cIt = in->GetCells( )->Begin( );
        for( ; cIt != in->GetCells( )->End( ); cIt++ )
        {
            InputEdgeCellType* qe = (InputEdgeCellType*)0;
            InputPolygonCellType* pe = (InputPolygonCellType*)0;
            if( ( qe = dynamic_cast< InputEdgeCellType* >( cIt.Value( ) ) ) )
            {
                out->AddEdge( qe->GetOrg( ), qe->GetDest( ) );
            }
            else
            {
                if(( pe = dynamic_cast< InputPolygonCellType* >( cIt.Value( ))))
                {
                    InputPointIdList points;
                    typename InputPolygonCellType::PointIdIterator pit =
                        pe->PointIdsBegin( );
                    for( ; pit != pe->PointIdsEnd( ); pit++ )
                        points.push_back( ( *pit ) );
                    out->AddFace( points );
                } // fi
            } //fi
        } // rof
    }

} // enamespace


#endif // __ITKQUADEDGEMESH__MESHCOPY__TXX__

// eof - itkQuadEdgeMeshToQuadEdgeMeshFilter.txx
