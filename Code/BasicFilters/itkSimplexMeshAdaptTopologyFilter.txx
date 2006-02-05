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

  template <typename TInputMesh, typename TOutputMesh>
    SimplexMeshAdaptTopologyFilter<TInputMesh,TOutputMesh>::SimplexMeshAdaptTopologyFilter()
    {
    m_Output = TOutputMesh::New();
    this->ProcessObject::SetNumberOfRequiredOutputs(1);
    this->ProcessObject::SetNthOutput(0, m_Output.GetPointer());
    m_ModifiedCount = 0;
    m_SelectionMethod = 0;
    m_Threshold = 0.5;
    }

  template <typename TInputMesh, typename TOutputMesh>
    SimplexMeshAdaptTopologyFilter<TInputMesh, TOutputMesh>
    ::~SimplexMeshAdaptTopologyFilter()
    {
    }


  template <typename TInputMesh, typename TOutputMesh>
    void SimplexMeshAdaptTopologyFilter<TInputMesh, TOutputMesh>
    ::GenerateData()
    {
    this->Initialize();
    this->ComputeCellParameters();
    InsertNewCells();
    }



  //
  template <typename TInputMesh, typename TOutputMesh>
    void SimplexMeshAdaptTopologyFilter<TInputMesh, TOutputMesh>
    ::Initialize()
    {
    m_ModifiedCount = 0;
    }

  template <typename TInputMesh, typename TOutputMesh>
    void SimplexMeshAdaptTopologyFilter<TInputMesh, TOutputMesh>
    ::ComputeCellParameters()
    {
    InputMeshPointer inputMesh = this->GetInput(0);

    // Ensure that cells will be deallocated by the Mesh.
    inputMesh->SetCellsAllocationMethod( TInputMesh::CellsAllocatedDynamicallyCellByCell );

    SimplexVisitorInterfacePointer simplexVisitor = SimplexVisitorInterfaceType::New();
    simplexVisitor->mesh = inputMesh;
    CellMultiVisitorPointer mv = CellMultiVisitorType::New();
    mv->AddVisitor(simplexVisitor);
    this->GetInput(0)->Accept(mv);


    DoubleValueMapType::Pointer areas = simplexVisitor->GetAreaMap();
    DoubleContainerIterator areaIt = areas->Begin();
    DoubleValueMapType::Pointer curvatures = simplexVisitor->GetCurvatureMap();
    DoubleContainerIterator curvatureIt = curvatures->Begin();

    double averageCurvature = simplexVisitor->GetTotalMeanCurvature();

    double rangeCellSize = simplexVisitor->GetMaximumCellSize() - simplexVisitor->GetMinimumCellSize();
    double rangeCurvature  = simplexVisitor->GetMaximumCurvature() - simplexVisitor->GetMinimumCurvature();

    while ( curvatureIt != curvatures->End())
      {
      bool doRefinement = false;
      if ( (m_SelectionMethod == 0) && 
        (curvatureIt.Value() > averageCurvature 
        && areaIt.Value() > (0.05 * rangeCellSize + simplexVisitor->GetMinimumCellSize()) ) 
        || areaIt.Value() > (m_Threshold * rangeCellSize + simplexVisitor->GetMinimumCellSize())
        )
        {
        doRefinement = true;
        }
      else if ((m_SelectionMethod == 1) &&
        ((curvatureIt.Value() > m_Threshold * rangeCurvature &&  areaIt.Value() > 0.05 * rangeCellSize )
       || areaIt.Value() > m_Threshold * rangeCellSize  ) )
        {
        doRefinement = true;
        }

      if (doRefinement)
        {
        m_ModifiedCount++;

        InputCellAutoPointer poly;
        inputMesh->GetCell(curvatureIt.Index(), poly);

        InputPointType cellCenter = this->ComputeCellCenter( poly );

        typename InputPolygonType::PointIdIterator pointIds = poly->PointIdsBegin();

        unsigned long lineOneFirstIdx = *pointIds;
        pointIds++;
        unsigned long lineOneSecondIdx = *pointIds;

        unsigned short cnt = 0;

        while (cnt < poly->GetNumberOfPoints()/2 - 1)
          {
          pointIds++;
          cnt++;
          }
        unsigned long lineTwoFirstIdx = *pointIds;
        pointIds++;
        unsigned long lineTwoSecondIdx = *pointIds;

        unsigned long newPointId = inputMesh->GetNumberOfPoints();
        unsigned long firstNewIndex = newPointId;
        unsigned long secondNewIndex = newPointId+1;

        //create first new point
        InputPointType newMidPoint, helperPoint;
        InputPointType p1, p2;
        inputMesh->GetPoint(lineOneFirstIdx, &p1);
        inputMesh->GetPoint(lineOneSecondIdx, &p2);

        helperPoint.SetToMidPoint(p1,p2);
        newMidPoint.SetToMidPoint( helperPoint, cellCenter );


        inputMesh->SetPoint( firstNewIndex , newMidPoint);
        inputMesh->SetGeometryData( firstNewIndex , new itk::SimplexMeshGeometry() );

        inputMesh->ReplaceNeighbor( lineOneFirstIdx, lineOneSecondIdx, firstNewIndex);
        inputMesh->ReplaceNeighbor( lineOneSecondIdx, lineOneFirstIdx, firstNewIndex);


        //create second new point
        inputMesh->GetPoint(lineTwoFirstIdx, &p1);
        inputMesh->GetPoint(lineTwoSecondIdx, &p2);

        helperPoint.SetToMidPoint(p1,p2);
        newMidPoint.SetToMidPoint( helperPoint, cellCenter );

        inputMesh->SetPoint( secondNewIndex , newMidPoint );
        inputMesh->SetGeometryData( secondNewIndex  , new itk::SimplexMeshGeometry() );

        inputMesh->ReplaceNeighbor( lineTwoFirstIdx, lineTwoSecondIdx, secondNewIndex);
        inputMesh->ReplaceNeighbor( lineTwoSecondIdx, lineTwoFirstIdx, secondNewIndex);

        inputMesh->AddNeighbor(firstNewIndex, secondNewIndex);
        inputMesh->AddNeighbor(firstNewIndex, lineOneFirstIdx);
        inputMesh->AddNeighbor(firstNewIndex, lineOneSecondIdx);

        inputMesh->AddNeighbor(secondNewIndex, lineTwoSecondIdx);
        inputMesh->AddNeighbor(secondNewIndex, firstNewIndex);
        inputMesh->AddNeighbor(secondNewIndex, lineTwoFirstIdx);

        InputPointType lineOneFirstNormal = inputMesh->ComputeNormal(lineOneFirstIdx);
        InputPointType firstNewNormal = inputMesh->ComputeNormal(firstNewIndex);

        InputPointType lineTwoFirstNormal = inputMesh->ComputeNormal(lineTwoFirstIdx);
        InputPointType secondNewNormal = inputMesh->ComputeNormal(secondNewIndex);

        InputVectorType v1, v2;
        double prod;
        v1 = firstNewNormal.GetVectorFromOrigin();
        v2 = lineOneFirstNormal.GetVectorFromOrigin();

        prod = dot_product( v1.GetVnlVector() , v2.GetVnlVector() );

        if (prod < 0) 
          {
          inputMesh->SwapNeighbors( firstNewIndex, lineOneFirstIdx, lineOneSecondIdx);
          firstNewNormal = inputMesh->ComputeNormal(firstNewIndex);
          }

        prod = dot_product(secondNewNormal.GetVectorFromOrigin().GetVnlVector() ,
                        lineTwoFirstNormal.GetVectorFromOrigin().GetVnlVector() );
        if (prod < 0) 
          {
          inputMesh->SwapNeighbors( secondNewIndex, lineTwoFirstIdx, lineTwoSecondIdx);
          secondNewNormal = inputMesh->ComputeNormal(secondNewIndex);
          }

        this->GetInput(0)->AddEdge( firstNewIndex, secondNewIndex );

        // splitting cell
        unsigned long newPointIndex = 0;
        OutputPolygonType * polygon = new OutputPolygonType;
        NewSimplexCellPointer.TakeOwnership( polygon );

        pointIds = poly->PointIdsBegin();
        unsigned long firstPointId = *pointIds++;

        while (*pointIds != lineTwoSecondIdx )
          {
          NewSimplexCellPointer->SetPointId( newPointIndex++, *pointIds++ );
          }

        NewSimplexCellPointer->SetPointId( newPointIndex++, secondNewIndex );
        NewSimplexCellPointer->SetPointId( newPointIndex++, firstNewIndex );
        this->GetInput(0)->ReplaceFace( curvatureIt.Index(), NewSimplexCellPointer );

        OutputPolygonType * polygon2 = new OutputPolygonType;
        NewSimplexCellPointer.TakeOwnership( polygon2 );
        newPointIndex = 0;

        while ( pointIds != poly->PointIdsEnd() )
          {
          NewSimplexCellPointer->SetPointId( newPointIndex++, *pointIds++ );
          }
        NewSimplexCellPointer->SetPointId( newPointIndex++, firstPointId );
        NewSimplexCellPointer->SetPointId( newPointIndex++, firstNewIndex );
        NewSimplexCellPointer->SetPointId( newPointIndex++, secondNewIndex );
        this->GetInput(0)->AddFace( NewSimplexCellPointer );

        this->GetInput(0)->BuildCellLinks();

        ModifyNeighborCells(lineOneFirstIdx, lineOneSecondIdx, firstNewIndex);
        ModifyNeighborCells(lineTwoFirstIdx, lineTwoSecondIdx, secondNewIndex);     

        if( this->GetInput(0)->GetCellsAllocationMethod() == TInputMesh::CellsAllocatedDynamicallyCellByCell )
          {
          delete poly.GetPointer();
          }
        } // end if cell must be modified
      areaIt++;
      curvatureIt++;
      }


    }

  template <typename TInputMesh, typename TOutputMesh>
    void SimplexMeshAdaptTopologyFilter<TInputMesh, TOutputMesh>
    ::InsertNewCells()
    {
    typename TOutputMesh::Pointer output = TOutputMesh::New();
    output->SetPoints(this->GetInput(0)->GetPoints());
    output->SetPointData(this->GetInput(0)->GetPointData());
    output->SetCells(this->GetInput(0)->GetCells());
    output->SetGeometryData(this->GetInput(0)->GetGeometryData());
    output->SetLastCellId( this->GetInput(0)->GetLastCellId() );
    this->ProcessObject::SetNthOutput(0, output.GetPointer());
    }

  template <typename TInputMesh, typename TOutputMesh>
    void 
    SimplexMeshAdaptTopologyFilter<TInputMesh, TOutputMesh>
    ::ModifyNeighborCells(unsigned long id1, unsigned long id2, unsigned long insertPointId)
    {
    std::set<unsigned long> cells1 =   this->GetInput(0)->GetCellLinks()->GetElement(id1);
    std::set<unsigned long> cells2 =   this->GetInput(0)->GetCellLinks()->GetElement(id2);
    std::set<unsigned long>::iterator cellIt = cells1.begin();

    std::set<unsigned long> result;

    while (cellIt != cells1.end() )
      {
      std::set<unsigned long>::iterator found = std::find(cells2.begin(), cells2.end(), *cellIt);
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
      this->GetInput(0)->GetCell(*cellIt, nextCell );

      if ( nextCell->GetNumberOfPoints() == 2 )
        {
        InputCellPointIdIterator lineIt =  nextCell->PointIdsBegin();
        unsigned long first= *lineIt++;
        unsigned long second= *lineIt;

        this->GetInput(0)->AddEdge( first, insertPointId );
        this->GetInput(0)->AddEdge( insertPointId, second );
        this->GetInput(0)->GetCells()->DeleteIndex( *cellIt );
        }
      else if ( nextCell->GetNumberOfPoints() > 3 )
        {
        NewSimplexCellPointer.TakeOwnership( new OutputPolygonType );
        InputPolygonPointIdIterator pointIt =  nextCell->PointIdsBegin();
        unsigned long cnt = 0;
        unsigned long first = *pointIt++;
        unsigned long startId = first;

        unsigned long second = 0;

        while ( pointIt != nextCell->PointIdsEnd() ) 
          {
          NewSimplexCellPointer->SetPointId( cnt++,  first );
          second = *pointIt;

          if (id1 == first && id2 == second || id2 == first && id1 == second )
            {
            NewSimplexCellPointer->SetPointId( cnt++,  insertPointId );
            }
          first = second;
          pointIt++;
          }

        NewSimplexCellPointer->SetPointId( cnt++,  second );
        if (id1 == second && id2 == startId || id2 ==  second && id1 == startId )
          {
          NewSimplexCellPointer->SetPointId( cnt++,  insertPointId );
          }

        this->GetInput(0)->ReplaceFace( *cellIt, NewSimplexCellPointer );

        }
      cellIt++;  
      }

    this->GetInput(0)->BuildCellLinks();
    }

  /* PrintSelf. */
  template <typename TInputMesh, typename TOutputMesh>
    void
    SimplexMeshAdaptTopologyFilter<TInputMesh, TOutputMesh>
    ::PrintSelf(std::ostream& os, Indent indent) const
    {
    Superclass::PrintSelf(os,indent);
    os << indent << "ToDo: implement PrinSelf!!!";
    }


  template <typename TInputMesh, typename TOutputMesh>
    typename SimplexMeshAdaptTopologyFilter<TInputMesh, TOutputMesh>::InputPointType
    SimplexMeshAdaptTopologyFilter<TInputMesh, TOutputMesh>
    ::ComputeCellCenter(InputCellAutoPointer &simplexCell)
    {
    InputPolygonPointIdIterator pointIt =  simplexCell->PointIdsBegin();

    InputVectorType tmp;
    InputPointType p1, p2, cellCenter;
    cellCenter.Fill(0);

    // compute the cell center first
    while ( pointIt != simplexCell->PointIdsEnd() )
      {
      this->GetInput(0)->GetPoint(*pointIt, &p1);
      cellCenter += p1.GetVectorFromOrigin();
      pointIt++;
      }

    tmp.SetVnlVector( cellCenter.GetVnlVector()/simplexCell->GetNumberOfPoints() );
    cellCenter.Fill(0.0);
    cellCenter += tmp;

    return cellCenter;
    }


  } // end of namspace itk

#endif // __itkSimplexMeshAdaptTopologyFilter_txx
