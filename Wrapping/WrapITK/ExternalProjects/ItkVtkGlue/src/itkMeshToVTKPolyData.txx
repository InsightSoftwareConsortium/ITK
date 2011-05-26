#ifndef __itkMeshToVTKPolyData_txx
#define __itkMeshToVTKPolyData_txx

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
