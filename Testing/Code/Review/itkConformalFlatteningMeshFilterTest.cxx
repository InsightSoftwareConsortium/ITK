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

  const unsigned int inputPointDimension   = 3;
  const unsigned int outputPointDimension  = 3;

  const unsigned int maxCellDimension = 2;

  typedef double FloatingPointType;

  typedef itk::DefaultStaticMeshTraits<
    FloatingPointType,
    inputPointDimension,
    maxCellDimension,
    FloatingPointType,
    FloatingPointType  >       InputMeshTraits;

  typedef itk::Mesh<
    FloatingPointType,
    inputPointDimension,
    InputMeshTraits              >     InputMeshType;


  typedef itk::DefaultStaticMeshTraits<
    FloatingPointType,
    outputPointDimension,
    maxCellDimension,
    FloatingPointType,
    FloatingPointType  >       OutputMeshTraits;

  typedef itk::Mesh<
    FloatingPointType,
    inputPointDimension,
    OutputMeshTraits              >     OutputMeshType;


  typedef OutputMeshType::CellType OutputCellType;
  typedef OutputMeshType::CellsContainer::ConstIterator CellIterator;
  typedef OutputCellType::PointIdIterator OutputPointIdIterator;

  typedef InputMeshType::CellType InputCellType;

  typedef itk::TriangleCell< InputCellType > InputTriangleCellType;

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
  InputMeshType::Pointer mesh = InputMeshType::New();

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

  InputMeshType::PointType inputPoint;
  long pointId = 0;

  for( unsigned int pn=0; pn < numberOfPoints; pn++ )
    {
    inputFile >> inputPoint;
    std::cout << inputPoint << std::endl;
    mesh->SetPoint( pointId++, inputPoint );
    }

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

    InputMeshType::CellAutoPointer cell;
    InputTriangleCellType * triangleCell = new InputTriangleCellType;
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

  typedef itk::ConformalFlatteningMeshFilter< 
    InputMeshType, OutputMeshType>  FilterType;

  FilterType::Pointer filter = FilterType::New();

  // Connect the inputs
  filter->SetInput( mesh );

  typedef InputMeshType::CellIdentifier  CellIdentifier;

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
  OutputMeshType::Pointer newMesh = filter->GetOutput();

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

  typedef OutputMeshType::PointsContainer::ConstIterator OutputPointIterator;
  OutputPointIterator pointIterator = newMesh->GetPoints()->Begin();
  OutputPointIterator pointItEnd = newMesh->GetPoints()->End();

  int numberOfPointsWritten = 0;

  OutputMeshType::PointType outputPoint;

  while( pointIterator != pointItEnd )
    {
    outputPoint = pointIterator.Value();

// FIXME    outputFile << outputPoint[0] << " " << outputPoint[1] << " " << outputPoint[2]
    outputFile << outputPoint[0] << " " << outputPoint[1] << " 0.0 "
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

  CellIterator cellIt = newMesh->GetCells()->Begin();
  CellIterator cellItEnd = newMesh->GetCells()->End();

  int numberOfPolygonsWritten = 0;

  for(unsigned int i=0; cellIt != cellItEnd; ++i, ++cellIt)
    {
    OutputCellType * cellptr = cellIt.Value();

    OutputPointIdIterator pntIdIter = cellptr->PointIdsBegin();
    OutputPointIdIterator pntIdEnd = cellptr->PointIdsEnd();

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

