/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    $RCSfile: itkVTKTetrahedralMeshReader.hxx,v $
  Language:  C++
  Date:      $Date: 2011-10-05 18:01:00 $
  Version:   $Revision: 1.19 $

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or https://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef itkVTKTetrahedralMeshReader_hxx
#define itkVTKTetrahedralMeshReader_hxx

#include "itkVTKTetrahedralMeshReader.h"
#include <fstream>
#include <cstdio>
#include <cstring>

namespace itk
{

//
// Constructor
//
template<typename TOutputMesh>
VTKTetrahedralMeshReader<TOutputMesh>
::VTKTetrahedralMeshReader()
{
  //
  // Create the output
  //
  typename TOutputMesh::Pointer output = TOutputMesh::New();
  this->ProcessObject::SetNumberOfRequiredOutputs(1);
  this->ProcessObject::SetNthOutput(0, output.GetPointer());
}

//
// Destructor
//
template<typename TOutputMesh>
VTKTetrahedralMeshReader<TOutputMesh>
::~VTKTetrahedralMeshReader()
{
}

template<typename TOutputMesh>
void
VTKTetrahedralMeshReader<TOutputMesh>
::GenerateData()
{
    OutputMeshType * outputMesh = this->GetOutput();

    outputMesh->SetCellsAllocationMethod(
      OutputMeshType::CellsAllocatedDynamicallyCellByCell );

    if( m_FileName == "" )
      {
      itkExceptionMacro(<< "No input FileName");
      }

    //
    // Read input file
    //
    std::ifstream inputFile( m_FileName.c_str() );

    if( !inputFile.is_open() )
      {
      itkExceptionMacro(<< "Unable to open file\n"
        << "inputFilename= " << m_FileName );
      }

    inputFile.imbue(std::locale::classic());
    std::string line;

    // The first line must be "# vtk DataFile Version x.x" where x.x can
    // vary
    std::getline( inputFile, m_Version, '\n' );
    if (inputFile.fail())
      {
      itkExceptionMacro(<< "Error reading file: " << m_FileName
        << "\nUnexpected end-of-file trying to read first line.");
      }

    if ( m_Version.find("# vtk DataFile Version ") == std::string::npos )
      {
      itkExceptionMacro(<< "Error reading file: " << m_FileName
        << "\nOnly vtk legacy format files can be read."
        << "\nThis file does not start with the line: # vtk DataFile Version x.x where x.x is the version.");
      }

    // Next is a one line description
    std::getline( inputFile, m_Header, '\n' );
    if (inputFile.eof())
      {
      itkExceptionMacro(<< "Error reading file: " << m_FileName
        << "\nUnexpected end-of-file trying to read header.");
      }

    // Next is the file format
    std::getline( inputFile, line, '\n' );
    if (inputFile.eof())
      {
      itkExceptionMacro(<< "Error reading file: " << m_FileName
        << "\nUnexpected end-of-file trying to file format.");
      }

    if (line.find("ASCII") == std::string::npos)
      {
      itkExceptionMacro(<< "Error reading file: " << m_FileName
        << "\nFile format is " << line
        << " but only ASCII files can be read.");
      }

    bool foundPoints = false;
    while( !inputFile.eof() )
      {
      std::getline( inputFile, line, '\n' );

      if( line.find("POINTS") != std::string::npos )
        {
        foundPoints = true;
        break;
        }
      }

    if (!foundPoints)
      {
      itkExceptionMacro(<< "Error reading file: " << m_FileName
        << "\nUnexpected end-of-file before finding POINTS.");
      }
    itkDebugMacro("POINTS line" << line );

    std::string pointLine( line, strlen("POINTS "), line.length() );
    itkDebugMacro("pointLine " << pointLine );

    unsigned long numberOfPoints = NumericTraits< unsigned long >::ZeroValue();

    if( sscanf(pointLine.c_str(),"%lu",&numberOfPoints) != 1 )
      {
      itkExceptionMacro(<< "Error reading file: " << m_FileName
        << "\nFailed to read numberOfPoints.\n"
        << "       pointLine= " << pointLine );
      }

    itkDebugMacro("numberOfPoints= " << numberOfPoints );

    if( numberOfPoints < 1 )
      {
      itkExceptionMacro(<< "Error reading file: " << m_FileName
        << "numberOfPoints < 1"
        << "       numberOfPoints line= " << numberOfPoints );
      }

    PointsContainer * points = outputMesh->GetPoints();
    points->Reserve( numberOfPoints );

    //
    // Load the point coordinates into the itk::Mesh
    //

    PointType point;

    for( PointIdentifier pointId = 0; pointId < numberOfPoints; pointId++ )
      {
      inputFile >> point;
      if (inputFile.eof())
        {
        itkExceptionMacro(<< "Error while reading file: " << m_FileName
          << "\nUnexpected end-of-file trying to read points.");
        }

      if (inputFile.fail())
        {
        itkExceptionMacro(<< "Error reading file: " << m_FileName
          << "\nInput could not be interpreted as a point.");
        }

      outputMesh->SetPoint( pointId, point );
      }

    // Continue searching for the CELLS line
    bool foundCells = false;
    while( !inputFile.eof() )
      {
      std::getline( inputFile, line, '\n' );
      if (line.find("CELLS") != std::string::npos )
        {
        foundCells = true;
        break;
        }
      }

    if ( !foundCells )
      {
      itkExceptionMacro(<< "Error reading file: " << m_FileName
        << "\nUnexpected end-of-file before finding CELLS.");
      }

    itkDebugMacro( "CELLS line" << line );

    std::string cellsLine( line, strlen("CELLS "), line.length() );
    itkDebugMacro( "cellsLine " << cellsLine);

    //
    // Read the number of cells
    //

    unsigned long numberOfCells   = NumericTraits< unsigned long >::ZeroValue();
    unsigned long numberOfIndices = NumericTraits< unsigned long >::ZeroValue();

    if( sscanf( cellsLine.c_str(), "%lu %lu", &numberOfCells,
        &numberOfIndices ) != 2 )
      {
      itkExceptionMacro(<< "Error reading file: " << m_FileName
        << "\nFailed to read numberOfCells from subline2"
        << "\ncellsLine = " << cellsLine );
      }

    itkDebugMacro("numberOfCells " << numberOfCells );
    itkDebugMacro("numberOfIndices " << numberOfIndices );

    if( numberOfCells < 1 )
      {
      itkExceptionMacro(<< "Error reading file: " << m_FileName
        << "\nnumberOfCells < 1\nnumberOfCells= "
        << numberOfCells );
      }

    if( numberOfIndices < numberOfCells )
      {
      itkExceptionMacro(<< "Error reading file: " << m_FileName
        << "\nnumberOfIndices < numberOfCells\n"
        << "numberOfIndices= " << numberOfIndices << "\n"
        << "numberOfCells= " << numberOfCells );
      }

    //
    // Load the cells into the itk::Mesh
    //

    unsigned long numberOfCellPoints;
    long ids[4];

    for( CellIdentifier cellId = 0; cellId < numberOfCells; cellId++ )
      {
      std::getline( inputFile, line, '\n' );
      if( inputFile.eof() )
        {
        itkExceptionMacro(<< "Error reading file: " << m_FileName
          << "\nFailed to read " << numberOfCells
          << " cells before the end of file."
          << " Only read " << cellId+1);
        }

      if( line.find("DATA") != std::string::npos )
        {
        itkExceptionMacro(<< "Error reading file: " << m_FileName
          << "\nRead keyword DATA");
        }

      unsigned long numberOfPointsFound;
      if( (numberOfPointsFound = sscanf( line.c_str(), "%lu %ld %ld %ld %ld", &numberOfCellPoints,
           &ids[0], &ids[1], &ids[2], &ids[3] )) != 5 )
        {
        itkExceptionMacro(<< "Error reading file: " << m_FileName
          << "\nError parsing Tetrahedron CELLS . Expected 5 items but got "
          << numberOfPointsFound << std::endl
          << "Line is: " << line);
        }

      if( numberOfCellPoints != 4 )
        {
        itkExceptionMacro(<< "Error reading file: " << m_FileName
          << "\nnumberOfCellPoints != 4\n"
          << "numberOfCellPoints= " << numberOfCellPoints
          << "\ncellId = "<< cellId
          << ". VTKTetrahedralMeshReader can only read tetrahedra");
        }

      if( ids[0] < 0 || ids[1] < 0 || ids[2] < 0 || ids[3] < 0 )
        {
        itkExceptionMacro(<< "Error reading file: " << m_FileName
          << "point ids must be >= 0.\n"
          "ids=" << ids[0] << " " << ids[1] << " " << ids[2] << " " << ids[3]);
        }

      if( static_cast<PointIdentifier>( ids[0] ) >= numberOfPoints ||
          static_cast<PointIdentifier>( ids[1] ) >= numberOfPoints ||
          static_cast<PointIdentifier>( ids[2] ) >= numberOfPoints ||
          static_cast<PointIdentifier>( ids[3] ) >= numberOfPoints )
        {
        itkExceptionMacro(<< "Error reading file: " << m_FileName
          << "Point ids must be < number of points: "
          << numberOfPoints
          << "\nids= " << ids[0] << " " << ids[1] << " " << ids[2] << " " << ids[3]);
        }

      CellAutoPointer cell;

      TetrahedronCellType * tetrahedronCell = new TetrahedronCellType;
      for( PointIdentifier pointId = 0; pointId < 4; pointId++ )
        {
        tetrahedronCell->SetPointId( pointId, ids[pointId] );
        }

      cell.TakeOwnership( tetrahedronCell );
      outputMesh->SetCell( cellId, cell );
      }


    // Continue searching for the CELL_TYPES line
    bool bfoundCellTypes = false;
    while( !inputFile.eof() )
      {
      std::getline( inputFile, line, '\n' );
      if (line.find("CELL_TYPES") != std::string::npos )
        {
        bfoundCellTypes = true;
        break;
        }
      }

    if ( !bfoundCellTypes )
      {
      itkExceptionMacro(<< "Error reading file: " << m_FileName
        << "\nUnexpected end-of-file before finding CELL_TYPES.");
      }

    itkDebugMacro( "CELL_TYPES line" << line );

    std::string cellsTypesLine( line, strlen("CELL_TYPES "), line.length() );

    itkDebugMacro( "cellsTypesLine " << cellsTypesLine);


    unsigned int numberOfCellTypes = 0;
    if( sscanf( cellsTypesLine.c_str(), "%u", &numberOfCellTypes) != 1 )
      {
      itkExceptionMacro(<< "Error reading file: " << m_FileName
        << "\nFailed to read numberOfCellTypes from subline2"
        << "\ncellsTypesLine = " << cellsTypesLine);
      }


    if( static_cast< CellIdentifier>( numberOfCellTypes ) != numberOfCells )
      {
      itkExceptionMacro(<< "Error reading file: " << m_FileName
        << "\nnumberOfCellTypes < numberOfCells ");
      }


    for(CellIdentifier cellId = 0; cellId < numberOfCellTypes; cellId++)
      {
      std::getline( inputFile, line, '\n' );
      if( inputFile.eof() )
        {
        itkExceptionMacro(<< "Error reading file: " << m_FileName
          << "\nFailed to read " << numberOfCellTypes
          << " cells before the end of file."
          << " Only read " << cellId+1);
        }


        int cellTypeID = 0;
        int cellTypeFound;
        if( (cellTypeFound = sscanf( line.c_str(), "%d ", &cellTypeID )) != 1 )
          {
          itkExceptionMacro(<< "Error reading file: " << m_FileName
            << "\nError parsing Cells types. Expected 1 item but got "
            << cellTypeFound << std::endl
            << "Line is: " << line);
          }


        const int tetrahedralCellTypeID = 10;

        if( cellTypeID != tetrahedralCellTypeID )
          {
          itkExceptionMacro(<< "Error reading file: " << m_FileName
            << "\nCell type != 10\n"
            << "Cell type = " << cellTypeID
            << ". VTKTetrahedralMeshReader can only read tetrahedra");
          }

      }

    bool foundPointData = false;

    while( !inputFile.eof() )
      {
      std::getline( inputFile, line, '\n' );

      if( line.find("POINT_DATA") != std::string::npos )
        {
        foundPointData = true;
        break;
        }
      }

    if( foundPointData )
      {
      typedef typename OutputMeshType::PointDataContainer PointDataContainer;
      typedef typename PointDataContainer::Pointer PointDataContainerPointer;

      PointDataContainerPointer pointDataContainer = PointDataContainer::New();
      pointDataContainer->Reserve( numberOfPoints );

      outputMesh->SetPointData( pointDataContainer );


      itkDebugMacro("POINT_DATA line" << line );

      // Skip two lines
      for(int j = 0; j < 2; j++)
        {
        if (!inputFile.eof())
          {
          std::getline( inputFile, line, '\n' );
          }
        else
          {
          inputFile.close();
          return;
          }
        }


      double pointData;

      for( PointIdentifier pid = 0; pid < numberOfPoints; pid++ )
        {
        if ( inputFile.eof() )
          {
          itkExceptionMacro(<< "Error reading file: " << m_FileName
            << "\nUnexpected end-of-file while trying to read POINT_DATA."
            << "Failed while trying to reading point data for id: " << pid);
          }
        inputFile >> pointData;
        outputMesh->SetPointData( pid, pointData );
        }
      }
    inputFile.close();
}

template<typename TOutputMesh>
void
VTKTetrahedralMeshReader<TOutputMesh>
::PrintSelf( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "FileName: " << m_FileName << std::endl;
  os << indent << "Version: " << m_Version << std::endl;
  os << indent << "Header: " << m_Header << std::endl;
}

} //end of namespace itk


#endif
