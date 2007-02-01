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
#include <fstream>

namespace itk
{

/*
 * Constructor
 */
template<class TInputMesh>
VTKPolyDataWriter<TInputMesh>
::VTKPolyDataWriter()
{
  this->m_Input = NULL;
  this->m_FileName = "";
}

/*
 * Destructor
 */
template<class TInputMesh>
VTKPolyDataWriter<TInputMesh>
::~VTKPolyDataWriter()
{
}

/*
 * Set the input mesh
 */
template<class TInputMesh>
void
VTKPolyDataWriter<TInputMesh>
::SetInput(InputMeshType * input)
{
  this->m_Input = input;
}

/*
 * Write the input mesh to the output file
 */
template<class TInputMesh>
void VTKPolyDataWriter<TInputMesh>
::Update()
{
  this->GenerateData();
}

/*
 * Write the input mesh to the output file
 */
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

  unsigned int numberOfPoints = this->m_Input->GetNumberOfPoints();
  unsigned int numberOfCells = this->m_Input->GetNumberOfCells();

  outputFile << "# vtk DataFile Version 2.0" << std::endl;
  outputFile << "File written by itkVTKPolyDataWriter" << std::endl;
  outputFile << "ASCII" << std::endl;
  outputFile << "DATASET POLYDATA" << std::endl;

  outputFile << "POINTS " << numberOfPoints << " float" << std::endl;

  PointIterator pointIterator = this->m_Input->GetPoints()->Begin();
  PointIterator pointEnd = this->m_Input->GetPoints()->End();

  while( pointIterator != pointEnd )
    {
    PointType point = pointIterator.Value();
    outputFile << point[0] << " " << point[1] << " " << point[2] << std::endl;
    pointIterator++;
    }

  outputFile << "POLYGONS " << numberOfCells << " " << 4 * numberOfCells << std::endl;

  CellIterator cellIterator = this->m_Input->GetCells()->Begin();
  CellIterator cellEnd = this->m_Input->GetCells()->End();

  while( cellIterator != cellEnd )
    {
    CellType * cellPointer = cellIterator.Value();

    PointIdIterator pointIdIterator = cellPointer->PointIdsBegin();
    PointIdIterator pointIdEnd = cellPointer->PointIdsEnd();

    outputFile << cellPointer->GetNumberOfPoints();

    while( pointIdIterator != pointIdEnd )
      {
      outputFile << " " << *pointIdIterator;
      pointIdIterator++;
      }

    outputFile << std::endl;
    cellIterator++;
    }

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
