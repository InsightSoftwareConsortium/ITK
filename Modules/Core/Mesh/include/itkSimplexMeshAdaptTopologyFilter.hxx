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
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#ifndef itkSimplexMeshAdaptTopologyFilter_hxx
#define itkSimplexMeshAdaptTopologyFilter_hxx

#include "itkSimplexMeshAdaptTopologyFilter.h"

namespace itk
{
template< typename TInputMesh, typename TOutputMesh >
SimplexMeshAdaptTopologyFilter< TInputMesh, TOutputMesh >::SimplexMeshAdaptTopologyFilter() :
  m_IdOffset(0),
  m_Threshold(0.5),
  m_SelectionMethod(0),
  m_ModifiedCount(0),
  m_Output(TOutputMesh::New())
{
  this->ProcessObject::SetNumberOfRequiredOutputs(1);
  this->ProcessObject::SetNthOutput( 0, m_Output.GetPointer() );
}

template< typename TInputMesh, typename TOutputMesh >
SimplexMeshAdaptTopologyFilter< TInputMesh, TOutputMesh >
::~SimplexMeshAdaptTopologyFilter()
{}

template< typename TInputMesh, typename TOutputMesh >
void SimplexMeshAdaptTopologyFilter< TInputMesh, TOutputMesh >
::GenerateData()
{
  this->Initialize();
  this->ComputeCellParameters();
}

//
template< typename TInputMesh, typename TOutputMesh >
void SimplexMeshAdaptTopologyFilter< TInputMesh, TOutputMesh >
::Initialize()
{
  m_ModifiedCount = 0;
  this->CopyInputMeshToOutputMeshPoints();
  this->CopyInputMeshToOutputMeshPointData();
  this->CopyInputMeshToOutputMeshCellData();
  this->CopyInputMeshToOutputMeshCells();
  this->CopyInputMeshToOutputMeshGeometryData();
}


template< typename TInputMesh, typename TOutputMesh >
void SimplexMeshAdaptTopologyFilter< TInputMesh, TOutputMesh >
::CopyInputMeshToOutputMeshGeometryData()
{
  const InputMeshType *inputMesh   =  this->GetInput();
  OutputMeshPointer    outputMesh  =  this->GetOutput();

  const PointIdentifier numberOfPoints = inputMesh->GetNumberOfPoints();

  typedef typename InputMeshType::GeometryMapType           GeometryMapType;
  typedef typename InputMeshType::GeometryMapPointer        GeometryMapPointer;
  typedef typename InputMeshType::GeometryMapConstIterator  GeometryMapConstIterator;

  GeometryMapPointer inputGeometryData = inputMesh->GetGeometryData();

  GeometryMapPointer outputGeometryData = GeometryMapType::New();

  outputGeometryData->Reserve( numberOfPoints );

  GeometryMapConstIterator inputGeometryItr = inputGeometryData->Begin();

  for( PointIdentifier pointId = 0; pointId < numberOfPoints; pointId++ )
    {
    SimplexMeshGeometry * outputGeometryDataItem = new SimplexMeshGeometry;
    SimplexMeshGeometry * inputGeometryDataItem = inputGeometryItr.Value();
    outputGeometryDataItem->CopyFrom( *inputGeometryDataItem );
    outputGeometryData->InsertElement( pointId, outputGeometryDataItem );
    ++inputGeometryItr;
    }

  outputMesh->SetGeometryData( outputGeometryData );
  outputMesh->SetLastCellId( inputMesh->GetLastCellId() );
}


template< typename TInputMesh, typename TOutputMesh >
void SimplexMeshAdaptTopologyFilter< TInputMesh, TOutputMesh >
::ComputeCellParameters()
{
  OutputMeshPointer    outputMesh  =  this->GetOutput();

  // Ensure that cells will be deallocated by the Mesh.
  outputMesh->SetCellsAllocationMethod( TInputMesh::CellsAllocatedDynamicallyCellByCell );

  SimplexVisitorInterfacePointer simplexVisitor = SimplexVisitorInterfaceType::New();
  simplexVisitor->mesh = outputMesh;
  CellMultiVisitorPointer mv = CellMultiVisitorType::New();
  mv->AddVisitor(simplexVisitor);
  outputMesh->Accept(mv);

  typename DoubleValueMapType::Pointer areas = simplexVisitor->GetAreaMap();
  DoubleContainerIterator     areaIt = areas->Begin();
  typename DoubleValueMapType::Pointer curvatures = simplexVisitor->GetCurvatureMap();
  DoubleContainerIterator     curvatureIt = curvatures->Begin();

  double averageCurvature = simplexVisitor->GetTotalMeanCurvature();

  double rangeCellSize = simplexVisitor->GetMaximumCellSize() - simplexVisitor->GetMinimumCellSize();
  double rangeCurvature  = simplexVisitor->GetMaximumCurvature() - simplexVisitor->GetMinimumCurvature();

  while ( curvatureIt != curvatures->End() )
    {
    bool doRefinement = false;

    const bool conditionA1 = ( m_SelectionMethod == 0 );
    const bool conditionA2 = ( curvatureIt.Value() > averageCurvature );

    const double limit1 =        0.05 * rangeCellSize + simplexVisitor->GetMinimumCellSize();
    const double limit2 = m_Threshold * rangeCellSize + simplexVisitor->GetMinimumCellSize();

    const bool conditionA3 = ( areaIt.Value() > limit1 );
    const bool conditionA4 = ( areaIt.Value() > limit2 );

    if ( conditionA1 && ( ( conditionA2 && conditionA3 ) || conditionA4 ) )
      {
      doRefinement = true;
      }
    else
      {
      const bool conditionB1 = ( m_SelectionMethod == 1 );
      const bool conditionB2 = curvatureIt.Value() > m_Threshold * rangeCurvature;
      const bool conditionB3 = areaIt.Value() > 0.05 * rangeCellSize;
      const bool conditionB4 = areaIt.Value() > m_Threshold * rangeCellSize;

      if ( conditionB1  && ( ( conditionB2 && conditionB3  ) || conditionB4 ) )
        {
        doRefinement = true;
        }
      }

    if ( doRefinement )
      {
      m_ModifiedCount++;

      InputCellAutoPointer poly;
      outputMesh->GetCell(curvatureIt.Index(), poly);

      InputPointType cellCenter = this->ComputeCellCenter(poly);

      typename InputPolygonType::PointIdIterator pointIds = poly->PointIdsBegin();

      PointIdentifier lineOneFirstIdx = *pointIds;
      pointIds++;
      PointIdentifier lineOneSecondIdx = *pointIds;

      unsigned short cnt = 0;

      while ( cnt < poly->GetNumberOfPoints() / 2 - 1 )
        {
        pointIds++;
        cnt++;
        }
      PointIdentifier lineTwoFirstIdx = *pointIds;
      pointIds++;
      PointIdentifier lineTwoSecondIdx = *pointIds;

      PointIdentifier newPointId = outputMesh->GetNumberOfPoints();
      PointIdentifier firstNewIndex = newPointId;
      PointIdentifier secondNewIndex = newPointId + 1;

      //create first new point
      InputPointType newMidPoint, helperPoint;
      InputPointType p1, p2;
      p1.Fill(0);
      p2.Fill(0);
      outputMesh->GetPoint(lineOneFirstIdx, &p1);
      outputMesh->GetPoint(lineOneSecondIdx, &p2);

      helperPoint.SetToMidPoint(p1, p2);
      newMidPoint.SetToMidPoint(helperPoint, cellCenter);

      outputMesh->SetPoint(firstNewIndex, newMidPoint);
      outputMesh->SetGeometryData( firstNewIndex, new itk::SimplexMeshGeometry() );

      outputMesh->ReplaceNeighbor(lineOneFirstIdx, lineOneSecondIdx, firstNewIndex);
      outputMesh->ReplaceNeighbor(lineOneSecondIdx, lineOneFirstIdx, firstNewIndex);

      //create second new point
      outputMesh->GetPoint(lineTwoFirstIdx, &p1);
      outputMesh->GetPoint(lineTwoSecondIdx, &p2);

      helperPoint.SetToMidPoint(p1, p2);
      newMidPoint.SetToMidPoint(helperPoint, cellCenter);

      outputMesh->SetPoint(secondNewIndex, newMidPoint);
      outputMesh->SetGeometryData( secondNewIndex, new itk::SimplexMeshGeometry() );

      outputMesh->ReplaceNeighbor(lineTwoFirstIdx, lineTwoSecondIdx, secondNewIndex);
      outputMesh->ReplaceNeighbor(lineTwoSecondIdx, lineTwoFirstIdx, secondNewIndex);

      outputMesh->AddNeighbor(firstNewIndex, secondNewIndex);
      outputMesh->AddNeighbor(firstNewIndex, lineOneFirstIdx);
      outputMesh->AddNeighbor(firstNewIndex, lineOneSecondIdx);

      outputMesh->AddNeighbor(secondNewIndex, lineTwoSecondIdx);
      outputMesh->AddNeighbor(secondNewIndex, firstNewIndex);
      outputMesh->AddNeighbor(secondNewIndex, lineTwoFirstIdx);

      CovariantVectorType lineOneFirstNormal = outputMesh->ComputeNormal(lineOneFirstIdx);
      CovariantVectorType firstNewNormal = outputMesh->ComputeNormal(firstNewIndex);

      CovariantVectorType lineTwoFirstNormal = outputMesh->ComputeNormal(lineTwoFirstIdx);
      CovariantVectorType secondNewNormal = outputMesh->ComputeNormal(secondNewIndex);

      double prod;

      prod = dot_product( firstNewNormal.GetVnlVector(), lineOneFirstNormal.GetVnlVector() );

      if ( prod < 0 )
        {
        outputMesh->SwapNeighbors(firstNewIndex, lineOneFirstIdx, lineOneSecondIdx);
        firstNewNormal = outputMesh->ComputeNormal(firstNewIndex);
        }

      prod = dot_product( secondNewNormal.GetVnlVector(), lineTwoFirstNormal.GetVnlVector() );
      if ( prod < 0 )
        {
        outputMesh->SwapNeighbors(secondNewIndex, lineTwoFirstIdx, lineTwoSecondIdx);
        secondNewNormal = outputMesh->ComputeNormal(secondNewIndex);
        }

      outputMesh->AddEdge(firstNewIndex, secondNewIndex);

      // splitting cell
      PointIdentifier    newPointIndex = NumericTraits< PointIdentifier >::ZeroValue();
      OutputPolygonType *polygon = new OutputPolygonType;
      InputCellAutoPointer newPolygonPointer1;
      newPolygonPointer1.TakeOwnership(polygon);

      pointIds = poly->PointIdsBegin();

      PointIdentifier firstPointId = *pointIds++;

      while ( *pointIds != lineTwoSecondIdx )
        {
        newPolygonPointer1->SetPointId(newPointIndex++, *pointIds++);
        }

      newPolygonPointer1->SetPointId(newPointIndex++, secondNewIndex);
      newPolygonPointer1->SetPointId(newPointIndex++, firstNewIndex);

      OutputPolygonType *polygon2 = new OutputPolygonType;
      newPointIndex = 0;

      while ( pointIds != poly->PointIdsEnd() )
        {
        polygon2->SetPointId(newPointIndex++, *pointIds++);
        }

      InputCellAutoPointer newPolygonPointer2;
      newPolygonPointer2.TakeOwnership( polygon2 );

      newPolygonPointer2->SetPointId(newPointIndex++, firstPointId);
      newPolygonPointer2->SetPointId(newPointIndex++, firstNewIndex);
      newPolygonPointer2->SetPointId(newPointIndex++, secondNewIndex);

      outputMesh->ReplaceFace(curvatureIt.Index(), newPolygonPointer1);
      outputMesh->AddFace(newPolygonPointer2);

      outputMesh->BuildCellLinks();

      this->ModifyNeighborCells(lineOneFirstIdx, lineOneSecondIdx, firstNewIndex);
      this->ModifyNeighborCells(lineTwoFirstIdx, lineTwoSecondIdx, secondNewIndex);

      } // end if cell must be modified
    areaIt++;
    curvatureIt++;
    }
}


template< typename TInputMesh, typename TOutputMesh >
void
SimplexMeshAdaptTopologyFilter< TInputMesh, TOutputMesh >
::ModifyNeighborCells(CellIdentifier id1, CellIdentifier id2, PointIdentifier insertPointId)
{
  OutputMeshPointer    outputMesh  =  this->GetOutput();

  std::set< PointIdentifier >           cells1 =   outputMesh->GetCellLinks()->GetElement(id1);
  std::set< PointIdentifier >           cells2 =   outputMesh->GetCellLinks()->GetElement(id2);
  typename std::set< PointIdentifier >::iterator cellIt = cells1.begin();

  std::set< PointIdentifier > result;

  while ( cellIt != cells1.end() )
    {
    typename std::set< PointIdentifier >::iterator found = std::find(cells2.begin(), cells2.end(), *cellIt);
    if ( found != cells2.end() )
      {
      result.insert(*cellIt);
      }
    cellIt++;
    }

  cellIt = result.begin();

  while ( cellIt != result.end() )
    {
    InputCellAutoPointer nextCell;
    outputMesh->GetCell(*cellIt, nextCell);

    if ( nextCell->GetNumberOfPoints() == 2 )
      {
      InputCellPointIdIterator lineIt =  nextCell->PointIdsBegin();
      PointIdentifier          first = *lineIt++;
      PointIdentifier          second = *lineIt;

      outputMesh->AddEdge(first, insertPointId);
      outputMesh->AddEdge(insertPointId, second);

      // Take over the cell and release its memory
      outputMesh->GetCells()->DeleteIndex(*cellIt);
      nextCell.TakeOwnership();
      nextCell.Reset();
      }
    else if ( nextCell->GetNumberOfPoints() > 3 )
      {
      m_NewSimplexCellPointer.TakeOwnership(new OutputPolygonType);
      InputPolygonPointIdIterator             pointIt =  nextCell->PointIdsBegin();
      PointIdentifier cnt = NumericTraits< PointIdentifier >::ZeroValue();
      PointIdentifier first = *pointIt++;
      PointIdentifier startId = first;

      PointIdentifier second = 0;

      while ( pointIt != nextCell->PointIdsEnd() )
        {
        m_NewSimplexCellPointer->SetPointId(cnt++,  first);
        second = *pointIt;

        if ( ( id1 == first && id2 == second ) || ( id2 == first && id1 == second ) )
          {
          m_NewSimplexCellPointer->SetPointId(cnt++,  insertPointId);
          }
        first = second;
        pointIt++;
        }

      m_NewSimplexCellPointer->SetPointId(cnt++,  second);
      if ( ( id1 == second && id2 == startId ) || ( id2 ==  second && id1 == startId ) )
        {
        m_NewSimplexCellPointer->SetPointId(cnt++,  insertPointId);
        }

      outputMesh->ReplaceFace(*cellIt, m_NewSimplexCellPointer);
      }
    cellIt++;
    }

  outputMesh->BuildCellLinks();
}

/* PrintSelf. */
template< typename TInputMesh, typename TOutputMesh >
void
SimplexMeshAdaptTopologyFilter< TInputMesh, TOutputMesh >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Threshold: " << m_Threshold << std::endl;
  os << indent << "SelectionMethod: " << m_SelectionMethod << std::endl;
  os << indent << "ModifiedCount: " << m_ModifiedCount << std::endl;
}

template< typename TInputMesh, typename TOutputMesh >
typename SimplexMeshAdaptTopologyFilter< TInputMesh, TOutputMesh >::InputPointType
SimplexMeshAdaptTopologyFilter< TInputMesh, TOutputMesh >
::ComputeCellCenter(InputCellAutoPointer & simplexCell)
{
  OutputMeshPointer    outputMesh  =  this->GetOutput();
  InputPolygonPointIdIterator pointIt =  simplexCell->PointIdsBegin();

  InputVectorType tmp;
  InputPointType  p1, p2, cellCenter;

  p1.Fill(0);
  cellCenter.Fill(0);

  // compute the cell center first
  while ( pointIt != simplexCell->PointIdsEnd() )
    {
    outputMesh->GetPoint(*pointIt, &p1);
    cellCenter += p1.GetVectorFromOrigin();
    pointIt++;
    }

  tmp.SetVnlVector( cellCenter.GetVnlVector() / simplexCell->GetNumberOfPoints() );
  cellCenter.Fill(0.0);
  cellCenter += tmp;

  return cellCenter;
}
} // end of namspace itk

#endif // itkSimplexMeshAdaptTopologyFilter_hxx
