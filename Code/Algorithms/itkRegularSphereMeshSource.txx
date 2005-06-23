/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegularSphereMeshSource.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __RegularSphereMeshSource_txx
#define __RegularSphereMeshSource_txx

#include "itkRegularSphereMeshSource.h"

namespace itk
{

/*
 *
 */
template<class TOutputMesh>
RegularSphereMeshSource<TOutputMesh>
::RegularSphereMeshSource()
{
  /*
   * Create the output
   */
  typename TOutputMesh::Pointer output = TOutputMesh::New();
  this->ProcessObject::SetNumberOfRequiredOutputs(1);
  this->ProcessObject::SetNthOutput(0, output.GetPointer());
  m_Center.Fill(0);
  m_Scale.Fill(1);
  m_Resolution = 2;
}

/*
 *
 */
template<class TOutputMesh>
void
RegularSphereMeshSource<TOutputMesh>
::GenerateData()
{
  unsigned long tripoints[3] = {0,1,2};
  typename OutputMeshType::Pointer outputMesh = this->GetOutput();  

  outputMesh->SetCellsAllocationMethod( OutputMeshType::CellsAllocatedDynamicallyCellByCell );

  PointsContainerPointer  myPoints = outputMesh->GetPoints();
  
  PointType p1;
  unsigned long idx = 0;


  p1[0] = 1 * m_Scale[0] + m_Center[0];
  p1[1] = 0 * m_Scale[1] + m_Center[1];
  p1[2] = 0 * m_Scale[2] + m_Center[2];
  outputMesh->SetPoint(idx++, p1);

  p1[0] = -1 * m_Scale[0] + m_Center[0];
  p1[1] = 0 * m_Scale[1] + m_Center[1];
  p1[2] = 0 * m_Scale[2] + m_Center[2];
  outputMesh->SetPoint(idx++, p1);

  p1[0] = 0 * m_Scale[0] + m_Center[0];
  p1[1] = 1 * m_Scale[1] + m_Center[1];
  p1[2] = 0 * m_Scale[2] + m_Center[2];
  outputMesh->SetPoint(idx++, p1);

  p1[0] = 0 * m_Scale[0] + m_Center[0];
  p1[1] = -1 * m_Scale[1] + m_Center[1];
  p1[2] = 0* m_Scale[2] + m_Center[2];
  outputMesh->SetPoint(idx++, p1);
  
  p1[0] = 0* m_Scale[0] + m_Center[0];
  p1[1] = 0* m_Scale[1] + m_Center[1];
  p1[2] = 1* m_Scale[2] + m_Center[2];
  outputMesh->SetPoint(idx++, p1);

  p1[0] = 0* m_Scale[0] + m_Center[0];
  p1[1] = 0* m_Scale[1] + m_Center[1];
  p1[2] = -1* m_Scale[2] + m_Center[2];
  outputMesh->SetPoint(idx++, p1);

  /* Six equidistant points lying on the unit sphere */
  const unsigned long XPLUS = 0;
  const unsigned long XMIN  = 1;
  const unsigned long YPLUS = 2;
  const unsigned long YMIN  = 3;
  const unsigned long ZPLUS = 4;
  const unsigned long ZMIN  = 5;


  tripoints[0] = YPLUS; tripoints[1] = ZPLUS; tripoints[2] = XPLUS; 
  this->AddCell( outputMesh, tripoints, 0 );
  
  tripoints[0] = YPLUS; tripoints[1] = XMIN; tripoints[2] = ZPLUS; 
  this->AddCell( outputMesh, tripoints, 1 );
  
  tripoints[0] = XMIN; tripoints[1] = YMIN; tripoints[2] = ZPLUS; 
  this->AddCell( outputMesh, tripoints, 2 );

  tripoints[0] = ZPLUS; tripoints[1] = YMIN; tripoints[2] = XPLUS; 
  this->AddCell( outputMesh, tripoints, 3 );

  tripoints[0] = ZMIN; tripoints[1] = YPLUS; tripoints[2] = XPLUS; 
  this->AddCell( outputMesh, tripoints, 4 );

  tripoints[0] = YPLUS; tripoints[1] =ZMIN ; tripoints[2] = XMIN; 
  this->AddCell( outputMesh, tripoints, 5 );

  tripoints[0] = ZMIN; tripoints[1] = YMIN; tripoints[2] = XMIN; 
  this->AddCell( outputMesh, tripoints, 6 );

  tripoints[0] = ZMIN; tripoints[1] = XPLUS; tripoints[2] = YMIN; 
  this->AddCell( outputMesh, tripoints, 7 );

  for (unsigned int i = 0; i < m_Resolution; i++) 
    {
    typename OutputMeshType::CellsContainerPointer myCells = outputMesh->GetCells();
    typename OutputMeshType::CellsContainer::Iterator cells = myCells->Begin();
    
    typename OutputMeshType::Pointer result = OutputMeshType::New();
    PointType v1, v2, v3;
    PointType* v1_pt;
    PointType* v2_pt;
    PointType* v3_pt;
    v1_pt = &v1;  v2_pt = &v2;  v3_pt = &v3;
    const unsigned long *tp;
    unsigned long pointIdx,cellIdx=0;
    unsigned long pointIdxOffset = outputMesh->GetNumberOfPoints();
    pointIdx = pointIdxOffset;
    unsigned long newIdx[3] = {0,1,2};
  
    PointMapType::Pointer handledEdges = PointMapType::New();

    while( cells != myCells->End() ) 
      {
      tp = cells.Value()->GetPointIds();
      outputMesh->GetPoint(tp[0],v1_pt);
      outputMesh->GetPoint(tp[1],v2_pt);
      outputMesh->GetPoint(tp[2],v3_pt);

      result->SetPoint(tp[0], v1);
      result->SetPoint(tp[1], v2);
      result->SetPoint(tp[2], v3);

   

      if (!handledEdges->IndexExists(std::make_pair(tp[0], tp[1])) &&
              !handledEdges->IndexExists(std::make_pair(tp[1], tp[0])))
        {
        newIdx[0]=pointIdx;
        handledEdges->InsertElement(std::make_pair(tp[0], tp[1]), pointIdx);
        result->SetPoint(pointIdx++, this->Divide(v1,v2) );
        }
      else
        {
        if (handledEdges->IndexExists(std::make_pair(tp[0], tp[1]))) 
          {
          newIdx[0] = handledEdges->GetElement(std::make_pair(tp[0], tp[1]));
          }
        else 
          {
          newIdx[0] = handledEdges->GetElement(std::make_pair(tp[1], tp[0]));
          }
        }


      // point 2
      if (!handledEdges->IndexExists(std::make_pair(tp[1], tp[2])) &&
              !handledEdges->IndexExists(std::make_pair(tp[2], tp[1])))
        {
        newIdx[1] = pointIdx;
        handledEdges->InsertElement(std::make_pair(tp[1], tp[2]), pointIdx);
        result->SetPoint(pointIdx++, this->Divide(v2,v3));
        }
      else
        {
        if (handledEdges->IndexExists(std::make_pair(tp[1], tp[2]))) 
          {
          newIdx[1] = handledEdges->GetElement(std::make_pair(tp[1], tp[2]));
          }
        else 
          {
          newIdx[1] = handledEdges->GetElement(std::make_pair(tp[2], tp[1]));
          }
        }


      // point 3
      if (!handledEdges->IndexExists(std::make_pair(tp[2], tp[0])) &&
              !handledEdges->IndexExists(std::make_pair(tp[0], tp[2])))
        {
        newIdx[2] = pointIdx;
        handledEdges->InsertElement(std::make_pair(tp[2], tp[0]), pointIdx);
        result->SetPoint(pointIdx++, this->Divide(v3,v1));
        }
      else
        {
        if (handledEdges->IndexExists(std::make_pair(tp[2], tp[0]))) 
          {
          newIdx[2] = handledEdges->GetElement(std::make_pair(tp[2], tp[0]));
          }
        else 
          {
          newIdx[2] = handledEdges->GetElement(std::make_pair(tp[0], tp[2]));
          }
        }

      tripoints[0] = tp[0]; 
      tripoints[1] = newIdx[0]; 
      tripoints[2] = newIdx[2]; 
      this->AddCell(result,tripoints,cellIdx);
      cellIdx++;

      tripoints[0] = newIdx[0];
      tripoints[1] = tp[1];
      tripoints[2] = newIdx[1];
      this->AddCell(result,tripoints,cellIdx);
      cellIdx++;

      tripoints[0] = newIdx[1];
      tripoints[1] = tp[2];
      tripoints[2] = newIdx[2]; 
      this->AddCell(result,tripoints,cellIdx);
      cellIdx++;

      tripoints[0] = newIdx[0];
      tripoints[1] = newIdx[1]; 
      tripoints[2] = newIdx[2]; 
      this->AddCell(result,tripoints,cellIdx);
      cellIdx++;
      cells++;
      }
    // This call does not release memory as ref count is not 1
    //outputMesh->Initialize();
    // Release memory
    cells = myCells->Begin();
    while( cells != myCells->End() )
      {
      const CellInterfaceType * cellToBeDeleted = cells->Value();
      delete cellToBeDeleted;
      cells++;
      }
    outputMesh->SetPoints(result->GetPoints());
    outputMesh->SetCells(result->GetCells());
    outputMesh->SetCellData(result->GetCellData());
    }
}


template<class TOutputMesh>
typename RegularSphereMeshSource<TOutputMesh>::PointType
RegularSphereMeshSource<TOutputMesh>
::Divide( const PointType & p1, const PointType & p2) const
  {
    PointType p;
    PointType f;

    VectorType d;
    VectorType c;

    d = p2 - p1;
    p = p1 + (d * 0.5);
    c = p - m_Center;

    f[0] = m_Scale[0] / c.GetNorm();
    f[1] = m_Scale[1] / c.GetNorm();
    f[2] = m_Scale[2] / c.GetNorm();

    c[0] *= f[0];
    c[1] *= f[1];
    c[2] *= f[2];
    
    return (m_Center + c);
}

template<class TOutputMesh>
void
RegularSphereMeshSource<TOutputMesh>
::AddCell( OutputMeshType * mesh, const unsigned long * pointIds, unsigned long idx)
  {
    CellAutoPointer testCell;
    testCell.TakeOwnership( new TriCellType );
    testCell->SetPointIds(pointIds);
    mesh->SetCell(idx, testCell );
  }


template<class TOutputMesh>
void
RegularSphereMeshSource<TOutputMesh>
::PrintSelf( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Center: " << m_Center << std::endl;
  os << indent << "Scale: " << m_Scale << std::endl;
  os << indent << "Resolution: " << m_Resolution << std::endl;
}


} //end of namespace itk


#endif

