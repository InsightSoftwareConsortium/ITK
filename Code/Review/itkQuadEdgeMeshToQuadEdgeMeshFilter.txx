/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkQuadEdgeMeshToQuadEdgeMeshFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkQuadEdgeMeshToQuadEdgeMeshFilter_txx
#define __itkQuadEdgeMeshToQuadEdgeMeshFilter_txx

#include "itkQuadEdgeMeshToQuadEdgeMeshFilter.h"

namespace itk
{
// ---------------------------------------------------------------------
template< class TInputMesh, class TOutputMesh >
QuadEdgeMeshToQuadEdgeMeshFilter< TInputMesh, TOutputMesh >
::QuadEdgeMeshToQuadEdgeMeshFilter()
{
  this->Superclass::SetNumberOfRequiredInputs( 1 );
  this->Superclass::SetNumberOfRequiredOutputs( 1 );

  this->Superclass::SetNthOutput( 0, OutputMeshType::New() );
}

// ---------------------------------------------------------------------
template< class TInputMesh, class TOutputMesh >
void 
QuadEdgeMeshToQuadEdgeMeshFilter< TInputMesh, TOutputMesh >
::CopyInputMeshToOutputMesh()
{
  this->CopyInputMeshToOutputMeshGeometry();
  this->CopyInputMeshToOutputMeshFieldData();
}

// ---------------------------------------------------------------------
template< class TInputMesh, class TOutputMesh >
void 
QuadEdgeMeshToQuadEdgeMeshFilter< TInputMesh, TOutputMesh >
::CopyInputMeshToOutputMeshGeometry()
{
  this->CopyInputMeshToOutputMeshPoints();
  this->CopyInputMeshToOutputMeshEdgeCells();
  this->CopyInputMeshToOutputMeshCells();
}

// ---------------------------------------------------------------------
template< class TInputMesh, class TOutputMesh >
void 
QuadEdgeMeshToQuadEdgeMeshFilter< TInputMesh, TOutputMesh >
::CopyInputMeshToOutputMeshFieldData()
{
  this->CopyInputMeshToOutputMeshPointData();
  this->CopyInputMeshToOutputMeshCellData();
}

// ---------------------------------------------------------------------
template< class TInputMesh, class TOutputMesh >
void 
QuadEdgeMeshToQuadEdgeMeshFilter< TInputMesh, TOutputMesh >
::CopyInputMeshToOutputMeshPoints()
{

  InputMeshConstPointer in = this->GetInput();
  OutputMeshPointer out = this->GetOutput();

  // Copy points
  InputPointsContainerConstIterator inIt = in->GetPoints()->Begin();
  while( inIt != in->GetPoints()->End() )
    {
    OutputPointType pOut;
    pOut.CastFrom( inIt.Value() );
    out->SetPoint( inIt.Index(), pOut );
    inIt++;
    } 
}

// ---------------------------------------------------------------------
template< class TInputMesh, class TOutputMesh >
void 
QuadEdgeMeshToQuadEdgeMeshFilter< TInputMesh, TOutputMesh >
::CopyInputMeshToOutputMeshEdgeCells()
{
  InputMeshConstPointer in = this->GetInput();
  OutputMeshPointer out = this->GetOutput();

  // Copy Edge Cells
  InputCellsContainerConstIterator ecIt = in->GetEdgeCells()->Begin();
  while( ecIt != in->GetEdgeCells()->End() )
    {
    InputEdgeCellType* pe = 
      dynamic_cast< InputEdgeCellType* >( ecIt.Value());
    //if( pe )
      {
      out->AddEdgeWithSecurePointList( pe->GetQEGeom()->GetOrigin(),
                                       pe->GetQEGeom()->GetDestination() );
      }
    ecIt++;
    }
}


// ---------------------------------------------------------------------
template< class TInputMesh, class TOutputMesh >
void 
QuadEdgeMeshToQuadEdgeMeshFilter< TInputMesh, TOutputMesh >
::CopyInputMeshToOutputMeshCells()
{
  InputMeshConstPointer in = this->GetInput();
  OutputMeshPointer out = this->GetOutput();

  // Copy cells
  InputCellsContainerConstIterator cIt = in->GetCells()->Begin();
  while( cIt != in->GetCells()->End() )
    {
    InputPolygonCellType* pe = 
      dynamic_cast< InputPolygonCellType* >( cIt.Value());
    //if( pe )
      {
      InputPointIdList points;
      InputPointsIdInternalIterator pit = pe->InternalPointIdsBegin();
      while( pit != pe->InternalPointIdsEnd( ) )
        {
        points.push_back( ( *pit ) );
        ++pit;
        }
      out->AddFaceWithSecurePointList( points, false );
      }
    cIt++;
    }
}

// ---------------------------------------------------------------------
template< class TInputMesh, class TOutputMesh >
void 
QuadEdgeMeshToQuadEdgeMeshFilter< TInputMesh, TOutputMesh >
::CopyInputMeshToOutputMeshPointData()
{

  InputMeshConstPointer in = this->GetInput();
  OutputMeshPointer out = this->GetOutput();

  typedef typename InputPointDataContainer::ConstPointer  InputPointDataContainerConstPointer;
  typedef typename OutputPointDataContainer::Pointer      OutputPointDataContainerPointer;

std::cout << "INPUT MESH = " << in.GetPointer() << std::endl;
  InputPointDataContainerConstPointer inputPointData = in->GetPointData();

  if( inputPointData.IsNull() )
    {
    // There is nothing to copy
    itkWarningMacro("Input mesh point data is NULL");
    return;
    }

  OutputPointDataContainerPointer outputPointData = OutputPointDataContainer::New();
  outputPointData->Reserve( inputPointData->Size() );

  // Copy point data
  typedef typename InputPointDataContainer::ConstIterator  InputPointDataContainerConstIterator;
  InputPointDataContainerConstIterator inIt = inputPointData->Begin();
  while( inIt != inputPointData->End() )
    {
    outputPointData->SetElement( inIt.Index(), inIt.Value() );
    inIt++;
    } 

  out->SetPointData( outputPointData );
}


// ---------------------------------------------------------------------
template< class TInputMesh, class TOutputMesh >
void 
QuadEdgeMeshToQuadEdgeMeshFilter< TInputMesh, TOutputMesh >
::CopyInputMeshToOutputMeshCellData()
{

  InputMeshConstPointer in = this->GetInput();
  OutputMeshPointer out = this->GetOutput();

  typedef typename InputCellDataContainer::ConstPointer  InputCellDataContainerConstPointer;
  typedef typename OutputCellDataContainer::Pointer      OutputCellDataContainerPointer;

  InputCellDataContainerConstPointer inputCellData = in->GetCellData();

  if( inputCellData.IsNull() );
    {
    // There is nothing to copy
    return;
    }

  OutputCellDataContainerPointer outputCellData = OutputCellDataContainer::New();
  outputCellData->Reserve( inputCellData->Size() );

  // Copy point data
  typedef typename InputCellDataContainer::ConstIterator  InputCellDataContainerConstIterator;
  InputCellDataContainerConstIterator inIt = inputCellData->Begin();
  while( inIt != inputCellData->End() )
    {
    outputCellData->SetElement( inIt.Index(), inIt.Value() );
    inIt++;
    } 

  out->SetCellData( outputCellData );
}
} // end namespace itk

#endif 
