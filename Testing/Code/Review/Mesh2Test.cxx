#include "itkQEMesh.h"
#include "itkQELineCell.h"

int Mesh2Test( int , char* [] )
{
    std::cout << "Testing points and simple edges... " << std::ends;

    typedef double                      PixelType;
    typedef itkQE::Mesh< PixelType, 3 > MeshType;
    typedef MeshType::CellType          CellType;
    typedef itkQE::LineCell< CellType > LineType;
    typedef CellType::CellAutoPointer   CellAutoPointer;

    MeshType::Pointer  mesh = MeshType::New( );

    MeshType::PointType p0;
    MeshType::PointType p1;
    MeshType::PointType p2;

    p0[ 0 ] = -1.0; p0[ 1 ] = 0.0; p0[ 2 ] = 0.0;
    p1[ 0 ] =  1.0; p1[ 1 ] = 0.0; p1[ 2 ] = 0.0;
    p2[ 0 ] =  1.0; p2[ 1 ] = 1.0; p2[ 2 ] = 0.0;

    mesh->SetPoint( 0, p0 );
    mesh->SetPoint( 1, p1 );
    mesh->SetPoint( 2, p2 );

    CellAutoPointer line0;
    line0.TakeOwnership( new LineType );
    line0->SetPointId( 0, 0 );
    line0->SetPointId( 1, 1 );
    mesh->SetCell( 0, line0 );

    CellAutoPointer line1;
    line1.TakeOwnership( new LineType );
    line1->SetPointId( 0, 1 );
    line1->SetPointId( 1, 2 );
    mesh->SetCell( 1, line1 );

    CellAutoPointer line2;
    line2.TakeOwnership( new LineType );
    line2->SetPointId( 0, 2 );
    line2->SetPointId( 1, 0 );
    mesh->SetCell( 2, line2 );

    if( mesh->GetNumberOfPoints( ) != 3 ) {

        std::cout << "Not all points added." << std::endl;
        return( 1 );

    } // fi

    if( mesh->GetNumberOfCells( ) != 3 ) {

        std::cout << "Not all cells added." << std::endl;
        return( 1 );

    } // fi

    typedef MeshType::CellsContainer::Iterator CellIterator;
    CellIterator cellIterator = mesh->GetCells( )->Begin( );
    unsigned int ids[ ] = { 0, 1, 2, 1, 2, 0, 2, 0, 1 };
    int itIds = 0;

    for( ; cellIterator != mesh->GetCells( )->End( ); cellIterator++ ) {

        MeshType::CellType* cellptr = cellIterator.Value( );
        LineType* line = dynamic_cast< LineType* >( cellptr );
        LineType::IteratorGeom git = line->BeginGeomLnext( );
        for( ; git != line->EndGeomLnext( ); git++ ) {

            if( ids[ itIds ] != *git ) {

                std::cout << "Problem with splicing edges." << std::endl;
                return( 1 );

            } // fi
            itIds++;

        } // rof

    } // rof

    std::cout << "done!" << std::endl;
    return( 0 );
}

// eof - Mesh2Test.cxx
