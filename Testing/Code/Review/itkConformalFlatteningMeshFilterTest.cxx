/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkConformalFlatteningMeshFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "itkMesh.h"
#include "itkLineCell.h"
#include "itkTriangleCell.h"
#include "itkConformalFlatteningMeshFilter.h"

#include <fstream>

#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

int itkConformalFlatteningMeshFilterTest(int argc, char *argv[])
{
  if( argc != 6 )
    {
    std::cerr << "Usage: itkConformalFlatteningMeshFilterTest "
     << "vtkInputFilename vtkOutputFilename " 
     << "polarCellId scale mapToSphere[0:1]" << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int pointDimension   = 3;
  const unsigned int maxCellDimension = 2;

  typedef double FloatingPointType;

  typedef itk::DefaultStaticMeshTraits<
    FloatingPointType,
    pointDimension,
    maxCellDimension,
    FloatingPointType,
    FloatingPointType  >       MeshTraits;

  typedef itk::Mesh<
    FloatingPointType,
    pointDimension,
    MeshTraits              >     MeshType;


  typedef MeshType::PointsContainer::ConstIterator PointIterator;

  typedef MeshType::CellType CellType;
  typedef MeshType::CellsContainer::ConstIterator CellIterator;

  typedef CellType::PointIdIterator PointIdIterator;

  typedef itk::TriangleCell< CellType > TriangleCellType;

  //
  // Read mesh file
  //
  std::ifstream inputFile(argv[1]);

  if( !inputFile.is_open() )
    {
    std::cout << "ERROR: Unable to open file" << std::endl;
    std::cout << "       inputFilename= " << argv[1] << std::endl;
    return EXIT_FAILURE;
    }

  // Create a new mesh
  MeshType::Pointer mesh = MeshType::New();

  std::string line;

  while( !inputFile.eof() )
    {
    getline(inputFile,line);
    std::cout << line << std::endl;

    if( line.find("POINTS") != std::string::npos )
      {
      break;
      }
    }

  std::cout << "POINTS line" << std::endl;
  std::cout << line << std::endl;

  std::string subline( line, strlen("POINTS "), line.length() );
  //std::cout << "subline " << subline << std::endl;

  int numberOfPoints = -1;

  if( sscanf(subline.c_str(),"%d",&numberOfPoints) != 1 )
    {
    std::cout << "ERROR: Failed to read numberOfPoints from subline"
      << std::endl;
    std::cout << "       subline= " << subline << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "numberOfPoints= " << numberOfPoints << std::endl;

  if( numberOfPoints < 1 )
    {
    std::cout << "ERROR: numberOfPoints < 1" << std::endl;
    std::cout << "       numberOfPoints= " << numberOfPoints << std::endl;
    return EXIT_FAILURE;
    }

  mesh->GetPoints()->Reserve( numberOfPoints );

  //
  // Load the point coordinates into the itk::Mesh
  //

  double doublePoint[3];
  MeshType::PointType point;
  long pointId = 0;

  for( unsigned int pn=0; pn < numberOfPoints; pn++ )
    {
    inputFile >> point;
    std::cout << point << std::endl;
    mesh->SetPoint( pointId++, point );
    }

/*
  while( !inputFile.eof() )
    {
    getline( inputFile, line );

    if( line.find("POLYGONS") != std::string::npos )
      {
      break;
      }

    if( sscanf( line.c_str(), "%lf %lf %lf", &doublePoint[0],
          &doublePoint[1], &doublePoint[2] ) != 3 )
      {
      std::cout << "ERROR: Cannot read 3 point coordinates from line"
        << std::endl;
      std::cout << "       line= " << line << std::endl;
      return EXIT_FAILURE;
      }

    //std::cout << "doublePoint"
    //<< " " << doublePoint[0]
    //<< " " << doublePoint[1]
    //<< " " << doublePoint[2] << std::endl;

    point[0] = doublePoint[0];
    point[1] = doublePoint[1];
    point[2] = doublePoint[2];

    std::cout << point[0] << " " << point[1] << " " << point[2]
      << std::endl;

    mesh->SetPoint( pointId++, point );
    }
*/
  std::cout << "pointId= " << pointId << std::endl;
  std::cout << "numberOfPoints= " << numberOfPoints << std::endl;

  if( pointId != numberOfPoints )
    {
    std::cout << "ERROR: pointId != numberOfPoints" << std::endl;
    std::cout << "       pointId= " << pointId << std::endl;
    std::cout << "       numberOfPoints= " << numberOfPoints << std::endl;
    return EXIT_FAILURE;
    }

  // Continue searching for the POLYGONS line
  while( !inputFile.eof() && ( line.find("POLYGONS") == std::string::npos ) )
    {
    getline( inputFile, line );
    }

  std::cout << "POLYGONS line" << std::endl;
  std::cout << line << std::endl;

  std::string subline2(line,strlen("POLYGONS "),line.length());
  std::cout << "subline2 " << subline2 << std::endl;

  int numberOfPolygons = -1;

  //
  // Read the number of polygons
  //

  if( sscanf( subline2.c_str(), "%d", &numberOfPolygons ) != 1 )
    {
    std::cout << "ERROR: Failed to read numberOfPolygons from subline2"
      << std::endl;
    std::cout << "       subline2= " << subline2 << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "numberOfPolygons " << numberOfPolygons << std::endl;

  if( numberOfPolygons < 1 )
    {
    std::cout << "ERROR: numberOfPolygons < 1" << std::endl;
    std::cout << "       numberOfPolygons= " << numberOfPolygons
      << std::endl;
    return EXIT_FAILURE;
    }

  mesh->GetCells()->Reserve( numberOfPolygons );

  //
  // Load the polygons into the itk::Mesh
  //

  unsigned long numberOfCellPoints;
  unsigned long ids[3];

  int cellId = 0;
  int numberOfIndices = 0;

  while( !inputFile.eof() )
    {
    getline( inputFile, line );

    if( line.find("DATA") != std::string::npos )
      {
      break;
      }

    if( sscanf( line.c_str(), "%ld %ld %ld %ld", &numberOfCellPoints,
          &ids[0], &ids[1], &ids[2] ) != 4 )
      {
      break;
      }

    numberOfIndices += numberOfCellPoints + 1;

    if( numberOfCellPoints != 3 )
      {
      std::cout << "ERROR: numberOfCellPoints != 3" << std::endl;
      std::cout << "       numberOfCellPoints= " << numberOfCellPoints
        << std::endl;
      return EXIT_FAILURE;
      }

    if( ids[0] < 0 || ids[1] < 0 || ids[2] < 0 )
      {
      std::cout << "ERROR: Incorrect point ids" << std::endl;
      std::cout << "       ids=" << ids[0] << " " << ids[1] << " " << ids[2]
       << std::endl;
      return EXIT_FAILURE;
      }

    if( ids[0] >= numberOfPoints || ids[1] >= numberOfPoints ||
        ids[2] >= numberOfPoints )
      {
      std::cout << "ERROR: Incorrect point ids" << std::endl;
      std::cout << "       ids=" << ids[0] << " " << ids[1] << " " << ids[2]
       << std::endl;
      return EXIT_FAILURE;
      }

    unsigned long pointIds[3];
    pointIds[0] = ids[0];
    pointIds[1] = ids[1];
    pointIds[2] = ids[2];

    MeshType::CellAutoPointer cell;
    TriangleCellType * triangleCell = new TriangleCellType;
    triangleCell->SetPointIds( (unsigned long*)pointIds );

    cell.TakeOwnership( triangleCell );
    mesh->SetCell( cellId, cell );

    cellId++;
    }

  std::cout << "cellId= " << cellId << std::endl;
  std::cout << "numberOfPolygons= " << numberOfPolygons << std::endl;

  inputFile.close();

  if( cellId != numberOfPolygons )
    {
    std::cout << "ERROR: cellId != numberOfPolygons" << std::endl;
    std::cout << "       cellId= " << cellId << std::endl;
    std::cout << "       numberOfPolygons= " << numberOfPolygons
      << std::endl;
    return EXIT_FAILURE;
    }

  //
  // Test itkConformalFlatteningMeshFilter
  //

  typedef itk::ConformalFlatteningMeshFilter< MeshType, MeshType>  FilterType;
  FilterType::Pointer filter = FilterType::New();

  // Connect the inputs
  filter->SetInput( mesh );

  typedef MeshType::CellIdentifier  CellIdentifier;

  CellIdentifier  polarCellId = atoi( argv[3] );
  filter->SetPolarCellIdentifier( polarCellId );

  int mapToSphere = atoi( argv[5] );
  if( mapToSphere == 1 )
    {
    filter->MapToSphere();
    }
  else
    {
    filter->MapToPlane();
    }

  double scale = atof( argv[4] );

  filter->SetScale( scale );

  // Execute the filter
  try
    {
    filter->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }
    

  // Get the Smart Pointer to the Filter Output
  MeshType::Pointer newMesh = filter->GetOutput();

  //
  // Write to file
  //

  std::ofstream outputFile( argv[2] );

  outputFile << "# vtk DataFile Version 2.0" << std::endl;
  outputFile << "Output of the itkConformalFlatteningMeshFilter Test" << std::endl;
  outputFile << "ASCII" << std::endl;
  outputFile << "DATASET POLYDATA" << std::endl;
  outputFile << "POINTS " << numberOfPoints << " float" << std::endl;

  // Write the point coordinates to the file

  PointIterator pointIterator = mesh->GetPoints()->Begin();
  PointIterator pointItEnd = mesh->GetPoints()->End();

  int numberOfPointsWritten = 0;

  while( pointIterator != pointItEnd )
    {
    point = pointIterator.Value();

    outputFile << point[0] << " " << point[1] << " " << point[2]
      << std::endl;

    pointIterator++;
    numberOfPointsWritten++;
    }

  if( numberOfPointsWritten != numberOfPoints )
    {
    std::cout << "ERROR: numberOfPointsWritten != numberOfPoints"
      << std::endl;
    std::cout << "       numberOfPointsWritten= " << numberOfPointsWritten
      << std::endl;
    std::cout << "       numberOfPoints= " << numberOfPoints << std::endl;
    return EXIT_FAILURE;
    }

  outputFile << "POLYGONS " << numberOfPolygons << " "
    << numberOfIndices << std::endl;

  CellIterator cellIt = mesh->GetCells()->Begin();
  CellIterator cellItEnd = mesh->GetCells()->End();

  int numberOfPolygonsWritten = 0;

  for(unsigned int i=0; cellIt != cellItEnd; ++i, ++cellIt)
    {
    CellType * cellptr = cellIt.Value();

    PointIdIterator pntIdIter = cellptr->PointIdsBegin();
    PointIdIterator pntIdEnd = cellptr->PointIdsEnd();

    outputFile << "3";

    while( pntIdIter != pntIdEnd )
      {
      outputFile << " " << *pntIdIter;
      pntIdIter++;
      }

    outputFile << std::endl;
    numberOfPolygonsWritten++;
    }

  if( numberOfPolygonsWritten != numberOfPolygons )
    {
    std::cout << "ERROR: numberOfPolygonsWritten != numberOfPolygons"
      << std::endl;
    std::cout << "       numberOfPolygonsWritten= "
      << numberOfPolygonsWritten << std::endl;
    std::cout << "       numberOfPolygons= " << numberOfPolygons
      << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}

