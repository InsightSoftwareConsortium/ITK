/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSimplexMeshAdaptTopologyFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSimplexMeshAdaptTopologyFilter_txx
#define __itkSimplexMeshAdaptTopologyFilter_txx

#include "itkSimplexMeshAdaptTopologyFilter.h"

namespace itk
{
template< typename TInputMesh, typename TOutputMesh >
SimplexMeshAdaptTopologyFilter< TInputMesh, TOutputMesh >::SimplexMeshAdaptTopologyFilter()
{
  m_Output = TOutputMesh::New();
  this->ProcessObject::SetNumberOfRequiredOutputs(1);
  this->ProcessObject::SetNthOutput( 0, m_Output.GetPointer() );
  m_ModifiedCount = 0;
  m_SelectionMethod = 0;
  m_Threshold = 0.5;
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
  this->InsertNewCells();
}

//
template< typename TInputMesh, typename TOutputMesh >
void SimplexMeshAdaptTopologyFilter< TInputMesh, TOutputMesh >
::Initialize()
{
  m_ModifiedCount = 0;
}

template< typename TInputMesh, typename TOutputMesh >
void SimplexMeshAdaptTopologyFilter< TInputMesh, TOutputMesh >
::ComputeCellParameters()
{
  const InputMeshType *inputMesh = this->GetInput(0);

  // A filter shouldn't modify its input.
  // There is a design flaw here...
  InputMeshType *nonConstInput = const_cast< InputMeshType * >( inputMesh );

  // Ensure that cells will be deallocated by the Mesh.
  nonConstInput->SetCellsAllocationMethod(TInputMesh::CellsAllocatedDynamicallyCellByCell);

  SimplexVisitorInterfacePointer simplexVisitor = SimplexVisitorInterfaceType::New();
  simplexVisitor->mesh = nonConstInput;
  CellMultiVisitorPointer mv = CellMultiVisitorType::New();
  mv->AddVisitor(simplexVisitor);
  this->GetInput(0)->Accept(mv);

  DoubleValueMapType::Pointer areas = simplexVisitor->GetAreaMap();
  DoubleContainerIterator     areaIt = areas->Begin();
  DoubleValueMapType::Pointer curvatures = simplexVisitor->GetCurvatureMap();
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
      inputMesh->GetCell(curvatureIt.Index(), poly);

      InputPointType cellCenter = this->ComputeCellCenter(poly);

      typename InputPolygonType::PointIdIterator pointIds = poly->PointIdsBegin();

      unsigned long lineOneFirstIdx = *pointIds;
      pointIds++;
      unsigned long lineOneSecondIdx = *pointIds;

      unsigned short cnt = 0;

      while ( cnt < poly->GetNumberOfPoints() / 2 - 1 )
        {
        pointIds++;
        cnt++;
        }
      unsigned long lineTwoFirstIdx = *pointIds;
      pointIds++;
      unsigned long lineTwoSecondIdx = *pointIds;

      unsigned long newPointId = inputMesh->GetNumberOfPoints();
      unsigned long firstNewIndex = newPointId;
      unsigned long secondNewIndex = newPointId + 1;

      //create first new point
      InputPointType newMidPoint, helperPoint;
      InputPointType p1, p2;
      p1.Fill(0);
      p2.Fill(0);
      inputMesh->GetPoint(lineOneFirstIdx, &p1);
      inputMesh->GetPoint(lineOneSecondIdx, &p2);

      helperPoint.SetToMidPoint(p1, p2);
      newMidPoint.SetToMidPoint(helperPoint, cellCenter);

      nonConstInput->SetPoint(firstNewIndex, newMidPoint);
      nonConstInput->SetGeometryData( firstNewIndex, new itk::SimplexMeshGeometry() );

      nonConstInput->ReplaceNeighbor(lineOneFirstIdx, lineOneSecondIdx, firstNewIndex);
      nonConstInput->ReplaceNeighbor(lineOneSecondIdx, lineOneFirstIdx, firstNewIndex);

      //create second new point
      inputMesh->GetPoint(lineTwoFirstIdx, &p1);
      inputMesh->GetPoint(lineTwoSecondIdx, &p2);

      helperPoint.SetToMidPoint(p1, p2);
      newMidPoint.SetToMidPoint(helperPoint, cellCenter);

      nonConstInput->SetPoint(secondNewIndex, newMidPoint);
      nonConstInput->SetGeometryData( secondNewIndex, new itk::SimplexMeshGeometry() );

      nonConstInput->ReplaceNeighbor(lineTwoFirstIdx, lineTwoSecondIdx, secondNewIndex);
      nonConstInput->ReplaceNeighbor(lineTwoSecondIdx, lineTwoFirstIdx, secondNewIndex);

      nonConstInput->AddNeighbor(firstNewIndex, secondNewIndex);
      nonConstInput->AddNeighbor(firstNewIndex, lineOneFirstIdx);
      nonConstInput->AddNeighbor(firstNewIndex, lineOneSecondIdx);

      nonConstInput->AddNeighbor(secondNewIndex, lineTwoSecondIdx);
      nonConstInput->AddNeighbor(secondNewIndex, firstNewIndex);
      nonConstInput->AddNeighbor(secondNewIndex, lineTwoFirstIdx);

      CovariantVectorType lineOneFirstNormal = inputMesh->ComputeNormal(lineOneFirstIdx);
      CovariantVectorType firstNewNormal = inputMesh->ComputeNormal(firstNewIndex);

      CovariantVectorType lineTwoFirstNormal = inputMesh->ComputeNormal(lineTwoFirstIdx);
      CovariantVectorType secondNewNormal = inputMesh->ComputeNormal(secondNewIndex);

      double prod;

      prod = dot_product( firstNewNormal.GetVnlVector(), lineOneFirstNormal.GetVnlVector() );

      if ( prod < 0 )
        {
        nonConstInput->SwapNeighbors(firstNewIndex, lineOneFirstIdx, lineOneSecondIdx);
        firstNewNormal = inputMesh->ComputeNormal(firstNewIndex);
        }

      prod = dot_product( secondNewNormal.GetVnlVector(), lineTwoFirstNormal.GetVnlVector() );
      if ( prod < 0 )
        {
        nonConstInput->SwapNeighbors(secondNewIndex, lineTwoFirstIdx, lineTwoSecondIdx);
        secondNewNormal = inputMesh->ComputeNormal(secondNewIndex);
        }

      nonConstInput->AddEdge(firstNewIndex, secondNewIndex);

      // splitting cell
      unsigned long      newPointIndex = 0;
      OutputPolygonType *polygon = new OutputPolygonType;
      m_NewSimplexCellPointer.TakeOwnership(polygon);

      pointIds = poly->PointIdsBegin();
      unsigned long firstPointId = *pointIds++;

      while ( *pointIds != lineTwoSecondIdx )
        {
        m_NewSimplexCellPointer->SetPointId(newPointIndex++, *pointIds++);
        }

      m_NewSimplexCellPointer->SetPointId(newPointIndex++, secondNewIndex);
      m_NewSimplexCellPointer->SetPointId(newPointIndex++, firstNewIndex);
      nonConstInput->ReplaceFace(curvatureIt.Index(), m_NewSimplexCellPointer);

      OutputPolygonType *polygon2 = new OutputPolygonType;
      m_NewSimplexCellPointer.TakeOwnership(polygon2);
      newPointIndex = 0;

      while ( pointIds != poly->PointIdsEnd() )
        {
        m_NewSimplexCellPointer->SetPointId(newPointIndex++, *pointIds++);
        }
      m_NewSimplexCellPointer->SetPointId(newPointIndex++, firstPointId);
      m_NewSimplexCellPointer->SetPointId(newPointIndex++, firstNewIndex);
      m_NewSimplexCellPointer->SetPointId(newPointIndex++, secondNewIndex);
      nonConstInput->AddFace(m_NewSimplexCellPointer);

      nonConstInput->BuildCellLinks();

      this->ModifyNeighborCells(lineOneFirstIdx, lineOneSecondIdx, firstNewIndex);
      this->ModifyNeighborCells(lineTwoFirstIdx, lineTwoSecondIdx, secondNewIndex);

      if ( inputMesh->GetCellsAllocationMethod() == TInputMesh::CellsAllocatedDynamicallyCellByCell )
        {
        delete poly.GetPointer();
        }
      } // end if cell must be modified
    areaIt++;
    curvatureIt++;
    }
}

template< typename TInputMesh, typename TOutputMesh >
void SimplexMeshAdaptTopologyFilter< TInputMesh, TOutputMesh >
::InsertNewCells()
{
  typename TOutputMesh::Pointer output = TOutputMesh::New();
  this->CopyInputMeshToOutputMeshPoints();
  this->CopyInputMeshToOutputMeshPointData();
  this->CopyInputMeshToOutputMeshCellData();
  this->CopyInputMeshToOutputMeshCells();
  output->SetGeometryData( this->GetInput(0)->GetGeometryData() );
  output->SetLastCellId( this->GetInput(0)->GetLastCellId() );
  this->ProcessObject::SetNthOutput( 0, output.GetPointer() );
}

template< typename TInputMesh, typename TOutputMesh >
void
SimplexMeshAdaptTopologyFilter< TInputMesh, TOutputMesh >
::ModifyNeighborCells(unsigned long id1, unsigned long id2, unsigned long insertPointId)
{
  std::set< unsigned long >           cells1 =   this->GetInput(0)->GetCellLinks()->GetElement(id1);
  std::set< unsigned long >           cells2 =   this->GetInput(0)->GetCellLinks()->GetElement(id2);
  std::set< unsigned long >::iterator cellIt = cells1.begin();

  std::set< unsigned long > result;

  const InputMeshType *inputMesh = this->GetInput(0);

  // A filter shouldn't modify its input.
  // There is a design flaw here...
  InputMeshType *nonConstInput = const_cast< InputMeshType * >( inputMesh );

  while ( cellIt != cells1.end() )
    {
    std::set< unsigned long >::iterator found = std::find(cells2.begin(), cells2.end(), *cellIt);
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
    this->GetInput(0)->GetCell(*cellIt, nextCell);

    if ( nextCell->GetNumberOfPoints() == 2 )
      {
      InputCellPointIdIterator lineIt =  nextCell->PointIdsBegin();
      unsigned long            first = *lineIt++;
      unsigned long            second = *lineIt;

      nonConstInput->AddEdge(first, insertPointId);
      nonConstInput->AddEdge(insertPointId, second);
      nonConstInput->GetCells()->DeleteIndex(*cellIt);
      }
    else if ( nextCell->GetNumberOfPoints() > 3 )
      {
      m_NewSimplexCellPointer.TakeOwnership(new OutputPolygonType);
      InputPolygonPointIdIterator pointIt =  nextCell->PointIdsBegin();
      unsigned long               cnt = 0;
      unsigned long               first = *pointIt++;
      unsigned long               startId = first;

      unsigned long second = 0;

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

      nonConstInput->ReplaceFace(*cellIt, m_NewSimplexCellPointer);
      }
    cellIt++;
    }

  this->GetInput(0)->BuildCellLinks();
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
  InputPolygonPointIdIterator pointIt =  simplexCell->PointIdsBegin();

  InputVectorType tmp;
  InputPointType  p1, p2, cellCenter;

  p1.Fill(0);
  cellCenter.Fill(0);

  // compute the cell center first
  while ( pointIt != simplexCell->PointIdsEnd() )
    {
    this->GetInput(0)->GetPoint(*pointIt, &p1);
    cellCenter += p1.GetVectorFromOrigin();
    pointIt++;
    }

  tmp.SetVnlVector( cellCenter.GetVnlVector() / simplexCell->GetNumberOfPoints() );
  cellCenter.Fill(0.0);
  cellCenter += tmp;

  return cellCenter;
}
} // end of namspace itk

#endif // __itkSimplexMeshAdaptTopologyFilter_txx
