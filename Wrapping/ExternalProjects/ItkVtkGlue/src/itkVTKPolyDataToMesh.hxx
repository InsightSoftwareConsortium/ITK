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

#ifndef __itkVTKPolyDataToMesh_hxx
#define __itkVTKPolyDataToMesh_hxx

#include <iostream>
#include "itkVTKPolyDataToMesh.h"

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
VTKPolyDataToMesh <TMesh>
::VTKPolyDataToMesh()
{

  m_ItkMesh = TriangleMeshType::New();
  m_PolyData = vtkPolyData::New();

}


template <class TMesh>
VTKPolyDataToMesh <TMesh>
::~VTKPolyDataToMesh()
{
  if (m_PolyData)
    {
      m_PolyData->Delete();
    }
}

template <class TMesh>
void
VTKPolyDataToMesh <TMesh>
::SetInput(vtkPolyData * polydata)
{
  m_PolyData = polydata;
  this->Update();
}

template <class TMesh>
vtkPolyData *
VTKPolyDataToMesh <TMesh>
::GetInput()
{
  return m_PolyData;
}

template <class TMesh>
typename VTKPolyDataToMesh<TMesh>::TriangleMeshType *
VTKPolyDataToMesh <TMesh>
::GetOutput()
{
  return m_ItkMesh;
}

template <class TMesh>
void
VTKPolyDataToMesh <TMesh>
::Update()
{
  //
  // Transfer the points from the vtkPolyData into the itk::Mesh
  //
  const unsigned int numberOfPoints = m_PolyData->GetNumberOfPoints();
  vtkPoints * vtkpoints =  m_PolyData->GetPoints();

  m_ItkMesh->GetPoints()->Reserve( numberOfPoints );

  for(unsigned int p =0; p < numberOfPoints; p++)
    {

    vtkFloatingPointType * apoint = vtkpoints->GetPoint( p );
    m_ItkMesh->SetPoint( p, typename TriangleMeshType::PointType( apoint ));

    // Need to convert the point to PoinType
    typename TriangleMeshType::PointType pt;
    for(unsigned int i=0;i<3; i++)
      {
       pt[i] = apoint[i];
       }
     m_ItkMesh->SetPoint( p, pt);

    }
  //
  // Transfer the cells from the vtkPolyData into the itk::Mesh
  //
  vtkCellArray * triangleStrips = m_PolyData->GetStrips();

  vtkIdType  * cellPoints;
  vtkIdType    numberOfCellPoints;

  //
  // First count the total number of triangles from all the triangle strips.
  //
  unsigned int numberOfTriangles = 0;

  triangleStrips->InitTraversal();
  while( triangleStrips->GetNextCell( numberOfCellPoints, cellPoints ) )
    {
    numberOfTriangles += numberOfCellPoints-2;
    }

   vtkCellArray * polygons = m_PolyData->GetPolys();

   polygons->InitTraversal();

   while( polygons->GetNextCell( numberOfCellPoints, cellPoints ) )
     {
     if( numberOfCellPoints == 3 )
       {
        numberOfTriangles ++;
       }
     }

   //
  // Reserve memory in the itk::Mesh for all those triangles
  //
   m_ItkMesh->GetCells()->Reserve( numberOfTriangles );

  //
  // Copy the triangles from vtkPolyData into the itk::Mesh
  //
  //

   typedef typename TriangleMeshType::CellType   CellType;

   typedef TriangleCell< CellType > TriangleCellType;

  // first copy the triangle strips
   int cellId = 0;
   triangleStrips->InitTraversal();
   while( triangleStrips->GetNextCell( numberOfCellPoints, cellPoints ) )
     {
     unsigned int numberOfTrianglesInStrip = numberOfCellPoints - 2;

     unsigned long pointIds[3];
     pointIds[0] = cellPoints[0];
     pointIds[1] = cellPoints[1];
     pointIds[2] = cellPoints[2];

     for( unsigned int t=0; t < numberOfTrianglesInStrip; t++ )
       {
        typename TriangleMeshType::CellAutoPointer c;
        TriangleCellType * tcell = new TriangleCellType;
        tcell->SetPointIds( pointIds );
        c.TakeOwnership( tcell );
        m_ItkMesh->SetCell( cellId, c );
        cellId++;
        pointIds[0] = pointIds[1];
        pointIds[1] = pointIds[2];
        pointIds[2] = cellPoints[t+3];
       }

     }

   // then copy the triangles
   polygons->InitTraversal();
   while( polygons->GetNextCell( numberOfCellPoints, cellPoints ) )
     {
     if( numberOfCellPoints !=3 ) // skip any non-triangle.
       {
       continue;
       }
     typename TriangleMeshType::CellAutoPointer c;
     TriangleCellType * t = new TriangleCellType;
     t->SetPointIds( (unsigned long*)cellPoints );
     c.TakeOwnership( t );
     m_ItkMesh->SetCell( cellId, c );
     cellId++;
     }

}

}

#endif
