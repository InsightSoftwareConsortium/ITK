/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVTKPolyDataReader.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkVTKPolyDataReader_txx
#define __itkVTKPolyDataReader_txx

#include "itkVTKPolyDataReader.h"
#include <fstream>

namespace itk
{

/*
 * Constructor
 */
template<class TOutputMesh>
VTKPolyDataReader<TOutputMesh>
::VTKPolyDataReader()
{
  /*
   * Create the output
   */
  typename TOutputMesh::Pointer output = TOutputMesh::New();
  this->ProcessObject::SetNumberOfRequiredOutputs(1);
  this->ProcessObject::SetNthOutput(0, output.GetPointer());
}

template<class TOutputMesh>
void
VTKPolyDataReader<TOutputMesh>
::GenerateData()
{
  typename OutputMeshType::Pointer outputMesh = this->GetOutput();

  outputMesh->SetCellsAllocationMethod(
      OutputMeshType::CellsAllocatedDynamicallyCellByCell );

  if( m_FileName == "" )
    {
    itkExceptionMacro("No input FileName");
    return;
    }

  //
  // Read input file
  //
  std::ifstream inputFile( m_FileName.c_str() );

  if( !inputFile.is_open() )
    {
    itkExceptionMacro("Unable to open file\n"
        "inputFilename= " << m_FileName );
    return;
    }

  std::string line;

  while( !inputFile.eof() )
    {
    std::getline( inputFile, line );

    if( line.find("POINTS") != std::string::npos )
      {
      break;
      }
    }

  itkDebugMacro("POINTS line" << line );

  std::string pointLine( line, strlen("POINTS "), line.length() );
  itkDebugMacro("pointLine " << pointLine );

  int numberOfPoints = -1;

  if( sscanf(pointLine.c_str(),"%d",&numberOfPoints) != 1 )
    {
    itkExceptionMacro("ERROR: Failed to read numberOfPoints\n"
        "       pointLine= " << pointLine );
    return;
    }

  itkDebugMacro("numberOfPoints= " << numberOfPoints );

  if( numberOfPoints < 1 )
    {
    itkExceptionMacro("numberOfPoints < 1"
        << "       numberOfPoints= " << numberOfPoints );
    return;
    }

  outputMesh->GetPoints()->Reserve( numberOfPoints );

  //
  // Load the point coordinates into the itk::Mesh
  //

  PointType point;

  for( unsigned int i=0; i < numberOfPoints; i++ )
    {
    inputFile >> point;
    outputMesh->SetPoint( i, point );
    }

  // Continue searching for the POLYGONS line
  while( !inputFile.eof() && line.find("POLYGONS") == std::string::npos )
    {
    std::getline( inputFile, line );
    }

  itkDebugMacro( "POLYGONS line" << line );

  std::string polygonLine( line, strlen("POLYGONS "), line.length() );
  itkDebugMacro( "polygonLine " << polygonLine );

  //
  // Read the number of polygons
  //

  int numberOfPolygons = -1;
  int numberOfIndices = -1;

  if( sscanf( polygonLine.c_str(), "%d %d", &numberOfPolygons,
        &numberOfIndices ) != 2 )
    {
    itkExceptionMacro("ERROR: Failed to read numberOfPolygons from subline2"
        "\npolygonLine= " << polygonLine );
    return;
    }

  itkDebugMacro("numberOfPolygons " << numberOfPolygons );
  itkDebugMacro("numberOfIndices " << numberOfIndices );

  if( numberOfPolygons < 1 )
    {
    itkExceptionMacro("ERROR: numberOfPolygons < 1\nnumberOfPolygons= "
        << numberOfPolygons );
    return;
    }

  if( numberOfIndices < numberOfPolygons )
    {
    itkExceptionMacro("ERROR: numberOfIndices < numberOfPolygons\n"
        << "numberOfIndices= " << numberOfIndices << "\n"
        << "numberOfPolygons= " << numberOfPolygons );
    return;
    }

  //
  // Load the polygons into the itk::Mesh
  //

  unsigned long numberOfCellPoints;
  unsigned long ids[3];

  for(unsigned long i=0; i<numberOfPolygons; i++)
    {
    if( inputFile.eof() )
      {
      itkExceptionMacro("Failed to read " << numberOfPolygons
          << " polygons before the end of file");
      return;
      }

    std::getline( inputFile, line );

    if( line.find("DATA") != std::string::npos )
      {
      itkExceptionMacro("Read keyword DATA");
      return;
      }

    if( sscanf( line.c_str(), "%ld %ld %ld %ld", &numberOfCellPoints,
          &ids[0], &ids[1], &ids[2] ) != 4 )
      {
      break;
      }

    if( numberOfCellPoints != 3 )
      {
      itkExceptionMacro("ERROR: numberOfCellPoints != 3\n"
          << "numberOfCellPoints= " << numberOfCellPoints
          << "itkVTKPolyDataReader can only read triangles");
      return;
      }

    if( ids[0] < 0 || ids[1] < 0 || ids[2] < 0 )
      {
      itkExceptionMacro("ERROR: Incorrect point ids\n"
          "ids=" << ids[0] << " " << ids[1] << " " << ids[2]);
      return;
      }

    if( ids[0] >= numberOfPoints ||
        ids[1] >= numberOfPoints ||
        ids[2] >= numberOfPoints )
      {
      itkExceptionMacro("ERROR: Incorrect point ids\n"
          << "ids=" << ids[0] << " " << ids[1] << " " << ids[2]);
      return;
      }

    CellAutoPointer cell;
    TriangleCellType * triangleCell = new TriangleCellType;
    triangleCell->SetPointIds( (unsigned long*)ids );

    cell.TakeOwnership( triangleCell );
    outputMesh->SetCell( i, cell );
    }

  inputFile.close();
}

template<class TOutputMesh>
void
VTKPolyDataReader<TOutputMesh>
::PrintSelf( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "FileName: " << m_FileName << std::endl;
}


} //end of namespace itk


#endif
