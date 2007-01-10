#include "itkQEMesh.h"
#include "itkQELineCell.h"
#include "itkQEPolygonCell.h"

int Mesh3Test( int , char* [] )
{
    typedef double                         PixelType;
    typedef itkQE::Mesh< PixelType, 3 >    MeshType;
    typedef MeshType::CellType             CellType;
    typedef MeshType::QEPrimal             QEPrimal;
    typedef itkQE::LineCell< CellType >    LineType;
    typedef itkQE::PolygonCell< CellType > QEPolygonCellType;

    MeshType::Pointer mesh = MeshType::New( );

    MeshType::PointType point0;
    MeshType::PointType point1;
    MeshType::PointType point2;
    MeshType::PointType point3;

    point0[ 0 ] = -1; point0[ 1 ] = -1; point0[ 2 ] = -1; 
    point1[ 0 ] =  1; point1[ 1 ] =  1; point1[ 2 ] = -1; 
    point2[ 0 ] =  1; point2[ 1 ] = -1; point2[ 2 ] =  1; 
    point3[ 0 ] = -1; point3[ 1 ] =  1; point3[ 2 ] =  1; 

    mesh->SetPoint( 0, point0 );
    mesh->SetPoint( 1, point1 );
    mesh->SetPoint( 2, point2 );
    mesh->SetPoint( 3, point3 );

    CellType::CellAutoPointer cellpointer;
    QEPolygonCellType* poly;
    LineType* edge;

    poly = new QEPolygonCellType( 3 );
    cellpointer.TakeOwnership( poly );
    cellpointer->SetPointId( 0, 0 );
    cellpointer->SetPointId( 1, 1 );
    cellpointer->SetPointId( 2, 2 );
    mesh->SetCell( 0, cellpointer );

    poly = new QEPolygonCellType( 3 );
    cellpointer.TakeOwnership( poly );
    cellpointer->SetPointId( 0, 0 );
    cellpointer->SetPointId( 1, 2 );
    cellpointer->SetPointId( 2, 3 );
    mesh->SetCell( 1, cellpointer );

    poly = new QEPolygonCellType( 3 );
    cellpointer.TakeOwnership( poly );
    cellpointer->SetPointId( 0, 0 );
    cellpointer->SetPointId( 1, 3 );
    cellpointer->SetPointId( 2, 1 );
    mesh->SetCell( 2, cellpointer );

    poly = new QEPolygonCellType( 3 );
    cellpointer.TakeOwnership( poly );
    cellpointer->SetPointId( 0, 3 );
    cellpointer->SetPointId( 1, 2 );
    cellpointer->SetPointId( 2, 1 );
    mesh->SetCell( 3, cellpointer );

    edge = new LineType;
    cellpointer.TakeOwnership( edge );
    cellpointer->SetPointId( 0, 0 );
    cellpointer->SetPointId( 1, 1 );
    mesh->SetCell( 4, cellpointer );

    edge = new LineType;
    cellpointer.TakeOwnership( edge );
    cellpointer->SetPointId( 0, 1 );
    cellpointer->SetPointId( 1, 2 );
    mesh->SetCell( 5, cellpointer );

    edge = new LineType;
    cellpointer.TakeOwnership( edge );
    cellpointer->SetPointId( 0, 2 );
    cellpointer->SetPointId( 1, 0 );
    mesh->SetCell( 6, cellpointer );

    edge = new LineType;
    cellpointer.TakeOwnership( edge );
    cellpointer->SetPointId( 0, 1 );
    cellpointer->SetPointId( 1, 3 );
    mesh->SetCell( 7, cellpointer );

    edge = new LineType;
    cellpointer.TakeOwnership( edge );
    cellpointer->SetPointId( 0, 3 );
    cellpointer->SetPointId( 1, 2 );
    mesh->SetCell( 8, cellpointer );

    edge = new LineType;
    cellpointer.TakeOwnership( edge );
    cellpointer->SetPointId( 0, 3 );
    cellpointer->SetPointId( 1, 0 );
    mesh->SetCell( 9, cellpointer );

    std::cout << "# Points = " << mesh->GetNumberOfPoints( ) << std::endl;
    std::cout << "# Cell   = " << mesh->GetNumberOfCells( ) << std::endl;

    typedef MeshType::PointsContainer::ConstIterator  PointIterator;
    PointIterator pointIterator = mesh->GetPoints( )->Begin( );
    PointIterator pointEnd      = mesh->GetPoints( )->End( );
  
    while( pointIterator != pointEnd ) {

        std::cout << pointIterator.Value( ) << std::endl;
        pointIterator++;

    } // elihw

    typedef MeshType::CellsContainer::ConstIterator CellIterator;

    CellIterator cellIterator = mesh->GetCells( )->Begin( );
    CellIterator cellEnd      = mesh->GetCells( )->End( );
  
    while( cellIterator != cellEnd ) {

        CellType* cell = cellIterator.Value( );
        std::cout << cell->GetNumberOfPoints( ) << std::endl;
        ++cellIterator;

    } // elihw
    cellIterator = mesh->GetCells( )->Begin( );
    cellEnd      = mesh->GetCells( )->End( );
  
    while( cellIterator != cellEnd ) {

        CellType* cell = cellIterator.Value( );

        std::cout << "cell with " << cell->GetNumberOfPoints( );
        std::cout << " points   : ";

        typedef CellType::PointIdIterator PointIdIterator;

        PointIdIterator pointIditer = cell->PointIdsBegin( );
        PointIdIterator pointIdend  = cell->PointIdsEnd( );

        while( pointIditer != pointIdend ) {

            std::cout << *pointIditer << " -> ";
            ++pointIditer;

        } // elihw
        std::cout << std::endl;

        ++cellIterator;

    } // elihw

    return( 0 );
}

// eof - Mesh3Test.cxx
