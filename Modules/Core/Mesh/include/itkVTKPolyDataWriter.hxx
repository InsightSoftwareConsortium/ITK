/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkVTKPolyDataWriter_hxx
#define itkVTKPolyDataWriter_hxx

#include "itkVTKPolyDataWriter.h"
#include "itkCellInterface.h"

#include <fstream>

namespace itk
{
//
// Constructor
//
template< typename TInputMesh >
VTKPolyDataWriter< TInputMesh >
::VTKPolyDataWriter()
{
  this->m_Input = ITK_NULLPTR;
  this->m_FileName = "";
}

//
// Destructor
//
template< typename TInputMesh >
VTKPolyDataWriter< TInputMesh >
::~VTKPolyDataWriter()
{}

//
// Set the input mesh
//
template< typename TInputMesh >
void
VTKPolyDataWriter< TInputMesh >
::SetInput(const InputMeshType *input)
{
  this->m_Input = input;
}

//
// Write the input mesh to the output file
//
template< typename TInputMesh >
void VTKPolyDataWriter< TInputMesh >
::Update()
{
  this->GenerateData();
}

//
// Write the input mesh to the output file
//
template< typename TInputMesh >
void VTKPolyDataWriter< TInputMesh >
::Write()
{
  this->GenerateData();
}

template< typename TInputMesh >
void
VTKPolyDataWriter< TInputMesh >
::GenerateData()
{
  if ( this->m_FileName == "" )
    {
    itkExceptionMacro("No FileName");
    return;
    }

  //
  // Write to output file
  //
  std::ofstream outputFile( this->m_FileName.c_str() );

  if ( !outputFile.is_open() )
    {
    itkExceptionMacro("Unable to open file\n"
                      "outputFilename= " << this->m_FileName);
    return;
    }

  outputFile.imbue( std::locale::classic() );
  outputFile << "# vtk DataFile Version 2.0" << std::endl;
  outputFile << "File written by itkVTKPolyDataWriter" << std::endl;
  outputFile << "ASCII" << std::endl;
  outputFile << "DATASET POLYDATA" << std::endl;

  // POINTS go first

  unsigned int numberOfPoints = this->m_Input->GetNumberOfPoints();
  outputFile << "POINTS " << numberOfPoints << " float" << std::endl;

  const PointsContainer *points = this->m_Input->GetPoints();

  std::map< PointIdentifier, PointIdentifier > IdMap;
  PointIdentifier                              k = 0;

  if ( points )
    {
    PointIterator pointIterator = points->Begin();
    PointIterator pointEnd = points->End();

    while ( pointIterator != pointEnd )
      {
      PointType point = pointIterator.Value();

      outputFile << point[0] << " " << point[1];

      if ( TInputMesh::PointDimension > 2 )
        {
        outputFile << " " << point[2];
        }
      else
        {
        outputFile << " " << "0.0";
        }

      outputFile << std::endl;

      IdMap[pointIterator.Index()] = k++;
      pointIterator++;
      }
    }

  unsigned int numberOfVertices = 0;
  unsigned int numberOfEdges = 0;
  unsigned int numberOfPolygons = 0;

  const CellsContainer *cells = this->m_Input->GetCells();

  if ( cells )
    {
    CellIterator cellIterator = cells->Begin();
    CellIterator cellEnd = cells->End();

    while ( cellIterator != cellEnd )
      {
      switch ( cellIterator.Value()->GetType() )
        {
        case 0: //VERTEX_CELL:
          numberOfVertices++;
          break;
        case 1: //LINE_CELL:
        case 7: //QUADRATIC_EDGE_CELL:
          numberOfEdges++;
          break;
        case 2: //TRIANGLE_CELL:
        case 3: //QUADRILATERAL_CELL:
        case 4: //POLYGON_CELL:
        case 8: //QUADRATIC_TRIANGLE_CELL:
          numberOfPolygons++;
          break;
        default:
          std::cerr << "Unhandled cell (volumic?)." << std::endl;
        }
      cellIterator++;
      }

    // VERTICES should go here
    if ( numberOfVertices )
        {}

    // LINES
    if ( numberOfEdges )
      {
      outputFile << "LINES " << numberOfEdges << " " << 3 * numberOfEdges;
      outputFile << std::endl;

      cellIterator = cells->Begin();
      while ( cellIterator != cellEnd )
        {
        CellType *cellPointer = cellIterator.Value();
        switch ( cellIterator.Value()->GetType() )
          {
          case 1: //LINE_CELL:
          case 7: //QUADRATIC_EDGE_CELL:
            {
            PointIdIterator pointIdIterator = cellPointer->PointIdsBegin();
            PointIdIterator pointIdEnd = cellPointer->PointIdsEnd();
            outputFile << cellPointer->GetNumberOfPoints();
            while ( pointIdIterator != pointIdEnd )
              {
              outputFile << " " << IdMap[*pointIdIterator];
              pointIdIterator++;
              }
            outputFile << std::endl;
            break;
            }
          default:
            break;
          }
        cellIterator++;
        }
      }

    // POLYGONS
    if ( numberOfPolygons )
      {
      // This could be optimized but at least now any polygonal
      // mesh can be saved.
      cellIterator = cells->Begin();

      PointIdentifier totalNumberOfPointsInPolygons = NumericTraits<PointIdentifier>::ZeroValue();
      while ( cellIterator != cells->End() )
        {
        CellType *cellPointer = cellIterator.Value();
        if ( cellPointer->GetType() != CellType::VERTEX_CELL
             && cellPointer->GetType() != CellType::LINE_CELL )
          {
          totalNumberOfPointsInPolygons += cellPointer->GetNumberOfPoints();
          }
        cellIterator++;
        }
      outputFile << "POLYGONS " << numberOfPolygons << " ";
      outputFile << totalNumberOfPointsInPolygons + numberOfPolygons; // FIXME: Is this right ?
      outputFile << std::endl;

      cellIterator = cells->Begin();
      while ( cellIterator != cellEnd )
        {
        CellType *cellPointer = cellIterator.Value();
        switch ( cellIterator.Value()->GetType() )
          {
          case 2: //TRIANGLE_CELL:
          case 3: //QUADRILATERAL_CELL:
          case 4: //POLYGON_CELL:
          case 8: //QUADRATIC_TRIANGLE_CELL:
            {
            PointIdIterator pointIdIterator = cellPointer->PointIdsBegin();
            PointIdIterator pointIdEnd = cellPointer->PointIdsEnd();
            outputFile << cellPointer->GetNumberOfPoints();
            while ( pointIdIterator != pointIdEnd )
              {
              outputFile << " " << IdMap[*pointIdIterator];
              pointIdIterator++;
              }
            outputFile << std::endl;
            break;
            }
          default:
            break;
          }
        cellIterator++;
        }
      }
    }

  // TRIANGLE_STRIP should go here
  // except that ... there is no such thing in ITK ...

  outputFile.close();
}

template< typename TInputMesh >
void
VTKPolyDataWriter< TInputMesh >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "FileName: " << this->m_FileName << std::endl;
}
} //end of namespace itk

#endif
