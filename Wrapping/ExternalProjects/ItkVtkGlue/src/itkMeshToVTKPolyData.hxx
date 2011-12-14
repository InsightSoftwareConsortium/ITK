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

#ifndef __itkMeshToVTKPolyData_hxx
#define __itkMeshToVTKPolyData_hxx

#include <iostream>
#include "itkMeshToVTKPolyData.h"

#ifndef vtkDoubleType
#define vtkDoubleType double
#endif

#ifndef vtkFloatingPointType
# define vtkFloatingPointType vtkFloatingPointType
typedef float vtkFloatingPointType;
#endif

namespace itk
{

template <class TMesh>
MeshToVTKPolyData <TMesh>
::MeshToVTKPolyData()
{

  m_ItkTriangleMesh = TriangleMeshType::New();
  m_Points = vtkPoints::New();
  m_PolyData = vtkPolyData::New();
  m_Polys = vtkCellArray::New();
}


template <class TMesh>
MeshToVTKPolyData <TMesh>
::~MeshToVTKPolyData()
{

}

template <class TMesh>
void
MeshToVTKPolyData <TMesh>
::SetInput(TriangleMeshType * mesh)
{
  m_ItkTriangleMesh = mesh;
  this->Update();
}

template <class TMesh>
typename MeshToVTKPolyData<TMesh>::TriangleMeshType *
MeshToVTKPolyData <TMesh>
::GetInput()
{
  return m_ItkTriangleMesh.GetPointer();
}

template <class TMesh>
vtkPolyData *
MeshToVTKPolyData<TMesh>
::GetOutput()
{
  return m_PolyData;
}

template <class TMesh>
void
MeshToVTKPolyData <TMesh>
::Update()
{
  int numPoints =  m_ItkTriangleMesh->GetNumberOfPoints();

  InputPointsContainerPointer      myPoints = m_ItkTriangleMesh->GetPoints();
  InputPointsContainerIterator     points = myPoints->Begin();
  PointType point;

  if (numPoints == 0)
    {
      printf( "Aborting: No Points in GRID\n");
      return;
    }

  m_Points->SetNumberOfPoints(numPoints);

  int idx=0;
  double vpoint[3];
  while( points != myPoints->End() )
    {
    point = points.Value();
    vpoint[0]= point[0];
    vpoint[1]= point[1];
    vpoint[2]= point[2];
    m_Points->SetPoint(idx++,vpoint);
    points++;
    }

  m_PolyData->SetPoints(m_Points);

  m_Points->Delete();

  CellsContainerPointer cells = m_ItkTriangleMesh->GetCells();
  CellsContainerIterator cellIt = cells->Begin();
  vtkIdType pts[3];
  while ( cellIt != cells->End() )
    {
  CellType *nextCell = cellIt->Value();
    typename CellType::PointIdIterator pointIt = nextCell->PointIdsBegin();
    PointType  p;
    int i;

    switch (nextCell->GetType())
      {
      case CellType::VERTEX_CELL:
      case CellType::LINE_CELL:
      case CellType::POLYGON_CELL:
        break;
      case CellType::TRIANGLE_CELL:
        i=0;
        while (pointIt != nextCell->PointIdsEnd() )
        {
        pts[i++] = *pointIt++;
        }
        m_Polys->InsertNextCell(3,pts);
        break;
      default:
        printf("something \n");
      }
    cellIt++;

    }

  m_PolyData->SetPolys(m_Polys);
  m_Polys->Delete();

}

}

#endif
