/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVTKPolyDataWriter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkVTKPolyDataWriter_txx
#define __itkVTKPolyDataWriter_txx

#include "itkVTKPolyDataWriter.h"
#include "itkCellInterface.h"

#include <fstream>

namespace itk
{

// 
// Constructor
// 
template<class TInputMesh>
VTKPolyDataWriter<TInputMesh>
::VTKPolyDataWriter()
{
  this->m_Input = NULL;
  this->m_FileName = "";
}

// 
// Destructor
// 
template<class TInputMesh>
VTKPolyDataWriter<TInputMesh>
::~VTKPolyDataWriter()
{
}

// 
// Set the input mesh
// 
template<class TInputMesh>
void
VTKPolyDataWriter<TInputMesh>
::SetInput(InputMeshType * input)
{
  this->m_Input = input;
}

// 
// Write the input mesh to the output file
// 
template<class TInputMesh>
void VTKPolyDataWriter<TInputMesh>
::Update()
{
  this->GenerateData();
}

// 
// Write the input mesh to the output file
// 
template<class TInputMesh>
void VTKPolyDataWriter<TInputMesh>
::Write()
{
  this->GenerateData();
}

template<class TInputMesh>
void
VTKPolyDataWriter<TInputMesh>
::GenerateData()
{
  std::cout << __LINE__ << " GenerateData" << std::endl;

  this->m_Input->SetCellsAllocationMethod(
      InputMeshType::CellsAllocatedDynamicallyCellByCell );

  if( this->m_FileName == "" )
    {
    itkExceptionMacro("No FileName");
    return;
    }

  //
  // Write to output file
  //
  std::ofstream outputFile( this->m_FileName.c_str() );

  if( !outputFile.is_open() )
    {
    itkExceptionMacro("Unable to open file\n"
        "outputFilename= " << this->m_FileName );
    return;
    }

  outputFile << "# vtk DataFile Version 2.0" << std::endl;
  outputFile << "File written by itkVTKPolyDataWriter" << std::endl;
  outputFile << "ASCII" << std::endl;
  outputFile << "DATASET POLYDATA" << std::endl;

  // POINTS go first

  unsigned int numberOfPoints = this->m_Input->GetNumberOfPoints();
  outputFile << "POINTS " << numberOfPoints << " float" << std::endl;

  PointIterator pointIterator = this->m_Input->GetPoints()->Begin();
  PointIterator pointEnd = this->m_Input->GetPoints()->End();
  while( pointIterator != pointEnd )
    {
    PointType point = pointIterator.Value();
    outputFile << point[0] << " " << point[1] << " " << point[2] << std::endl;
    pointIterator++;
    }

  unsigned int numberOfVertices = 0;
  unsigned int numberOfEdges = 0;
  unsigned int numberOfPolygons = 0;
  CellIterator cellIterator = this->m_Input->GetCells()->Begin();
  CellIterator cellEnd = this->m_Input->GetCells()->End();
  while( cellIterator != cellEnd )
    {
    switch( cellIterator.Value()->GetType() )
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

  std::cout << numberOfVertices << std::endl;
  std::cout << numberOfEdges << std::endl;
  std::cout << numberOfPolygons << std::endl;


  // VERTICES should go here
  if( numberOfVertices )
    {
    }

  // LINES
  if( numberOfEdges )
    {
    outputFile << "LINES " << numberOfEdges << " " << 3*numberOfEdges;
    outputFile << std::endl;
  
    cellIterator = this->m_Input->GetCells()->Begin();
    while( cellIterator != cellEnd )
      {
      CellType * cellPointer = cellIterator.Value();
      switch( cellIterator.Value()->GetType() )
        {
        case 1: //LINE_CELL:
        case 7: //QUADRATIC_EDGE_CELL:
          PointIdIterator pointIdIterator = cellPointer->PointIdsBegin();
          PointIdIterator pointIdEnd = cellPointer->PointIdsEnd();
          outputFile << cellPointer->GetNumberOfPoints();
          while( pointIdIterator != pointIdEnd )
            {
            outputFile << " " << *pointIdIterator;
            pointIdIterator++;
            }
          outputFile << std::endl;
          break;
        default:
          break;
        }
      cellIterator++;
      }
    }

  // POLYGONS
  if( numberOfPolygons )
    { 
    // the 4 here supposes only triangles
    // to be rigourous, we would have to compute the number
    // of points id used and add the number of cells
    outputFile << "POLYGONS " << numberOfPolygons << " " << 4*numberOfPolygons;
    outputFile << std::endl;
  
    cellIterator = this->m_Input->GetCells()->Begin();
    while( cellIterator != cellEnd )
      {
      CellType * cellPointer = cellIterator.Value();
      switch( cellIterator.Value()->GetType() )
        {
        case 2: //TRIANGLE_CELL:
        case 3: //QUADRILATERAL_CELL:
        case 4: //POLYGON_CELL:
        case 8: //QUADRATIC_TRIANGLE_CELL: 
          PointIdIterator pointIdIterator = cellPointer->PointIdsBegin();
          PointIdIterator pointIdEnd = cellPointer->PointIdsEnd();
          outputFile << cellPointer->GetNumberOfPoints();
          while( pointIdIterator != pointIdEnd )
            {
            outputFile << " " << *pointIdIterator;
            pointIdIterator++;
            }
          outputFile << std::endl;
          break;
        default:
          break;
        }
      cellIterator++;
      }
    }

  // TRIANGLE_STRIP should go here
  // except that ... there is no such thing in ITK ...

  outputFile.close();
}

template<class TInputMesh>
void
VTKPolyDataWriter<TInputMesh>
::PrintSelf( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "FileName: " << this->m_FileName << std::endl;
}

} //end of namespace itk

#endif
