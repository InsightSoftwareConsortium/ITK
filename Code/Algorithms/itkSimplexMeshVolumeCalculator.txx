/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkSimplexMeshVolumeCalculator.txx
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) Insight Software Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even 
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __SimplexMeshVolumeCalculator_txx
#define __SimplexMeshVolumeCalculator_txx

#include "itkSimplexMeshVolumeCalculator.h"

namespace itk
{
/*
 * Constructor
 */
template<typename TInputMesh>
SimplexMeshVolumeCalculator<TInputMesh>
::SimplexMeshVolumeCalculator()
{
  m_Volume = m_VolumeX = m_VolumeY = m_VolumeZ = 0.0;
  m_Area = 0.0;
  m_Kx = m_Ky = m_Kz = 0.0;
  m_NumberOfTriangles = 0;
}

template <typename TInputMesh>
SimplexMeshVolumeCalculator<TInputMesh>
::~SimplexMeshVolumeCalculator()
{
}


template <typename TInputMesh>
void SimplexMeshVolumeCalculator<TInputMesh>
::Initialize()
{
  SimplexVisitorInterfacePointer simplexVisitor = SimplexVisitorInterfaceType::New();
  simplexVisitor->SetMesh(m_SimplexMesh );
  CellMultiVisitorPointer mv = CellMultiVisitorType::New();
  mv->AddVisitor(simplexVisitor);
  m_SimplexMesh->Accept(mv);
  m_SimplexMesh->BuildCellLinks();
  m_Centers = simplexVisitor->GetCenterMap();

  m_Volume = m_VolumeX = m_VolumeY = m_VolumeZ = 0.0;
  m_Area = 0.0;
  m_Kx = m_Ky = m_Kz = 0.0;
  m_Muncx = m_Muncy = m_Muncz = 0;
  m_Wxyz = m_Wxy = m_Wxz = m_Wyz = 0;

  m_NumberOfTriangles = 0;
}

template <typename TInputMesh>void SimplexMeshVolumeCalculator<TInputMesh>
::Finalize()
{
  // Compute fraction of elements that primarily point along the x, y
  // and z directions 
  m_Kx = (m_Muncx + (m_Wxyz / 3.0) + ((m_Wxy + m_Wxz) / 2.0)) / m_NumberOfTriangles;
  m_Ky = (m_Muncy + (m_Wxyz / 3.0) + ((m_Wxy + m_Wyz) / 2.0)) / m_NumberOfTriangles;
  m_Kz = (m_Muncz + (m_Wxyz / 3.0) + ((m_Wxz + m_Wyz) / 2.0)) / m_NumberOfTriangles;

  m_Volume =  (m_Kx * m_VolumeX +
               m_Ky * m_VolumeY +
               m_Kz * m_VolumeZ);
  m_Volume =  vcl_fabs(m_Volume);
}

template <typename TInputMesh>
unsigned long SimplexMeshVolumeCalculator<TInputMesh>
::FindCellId(unsigned long id1, unsigned long id2, unsigned long id3)
{
  std::set<unsigned long> cells1 =  m_SimplexMesh->GetCellLinks()->GetElement(id1);
  std::set<unsigned long> cells2 =  m_SimplexMesh->GetCellLinks()->GetElement(id2);
  std::set<unsigned long> cells3 =  m_SimplexMesh->GetCellLinks()->GetElement(id3);
  std::set<unsigned long>::iterator cellIt = cells1.begin();

  while (cellIt != cells1.end() )
    {
    std::set<unsigned long>::iterator found2 = std::find(cells2.begin(), cells2.end(), *cellIt);
    std::set<unsigned long>::iterator found3 = std::find(cells3.begin(), cells3.end(), *cellIt);

    if ( found2 != cells2.end() && found3 != cells3.end() )
      {
      break;
      }
    cellIt++;
    }

  if (cellIt == cells1.end() )
    {
    itkExceptionMacro(<<"Cell was not found, although it should be there");
    }

  return *cellIt;
}


/* calculate volume of triangle */
template <typename TInputMesh>
void
SimplexMeshVolumeCalculator<TInputMesh>
::CalculateTriangleVolume(InputPointType p1, InputPointType p2, InputPointType p3)
{
  double   area;
  double   a,b,c,s;
  double   i[3],j[3],k[3],u[3],absu[3],length;
  double   ii[3],jj[3],kk[3];
  double   xavg,yavg,zavg;

  // Get i j k vectors ... 
  //
  i[0] = ( p2[0] - p1[0]); j[0] = (p2[1] - p1[1]); k[0] = (p2[2] - p1[2]);
  i[1] = ( p3[0] - p1[0]); j[1] = (p3[1] - p1[1]); k[1] = (p3[2] - p1[2]);
  i[2] = ( p3[0] - p2[0]); j[2] = (p3[1] - p2[1]); k[2] = (p3[2] - p2[2]);

  // Cross product between two vectors, to determine normal vector
  //
  u[0] = ( j[0] * k[1] - k[0] * j[1]);
  u[1] = ( k[0] * i[1] - i[0] * k[1]);
  u[2] = ( i[0] * j[1] - j[0] * i[1]);

  // Normalize normal
  //
  length = vcl_sqrt(u[0]*u[0] + u[1]*u[1] + u[2]*u[2]);
  if ( length != 0.0)
    {
    u[0] /= length;
    u[1] /= length;
    u[2] /= length;
    }
  else
    {
    u[0] = u[1] = u[2] = 0.0;
    }
  
  // Determine max unit normal component...
  //
  absu[0] = vcl_fabs(u[0]); absu[1] = vcl_fabs(u[1]); absu[2] = vcl_fabs(u[2]);   
  if (( absu[0] > absu[1]) && ( absu[0] > absu[2]) )
    {
    m_Muncx++;
    }
  else if (( absu[1] > absu[0]) && ( absu[1] > absu[2]) )
    {
    m_Muncy++;
    }
  else if (( absu[2] > absu[0]) && ( absu[2] > absu[1]) )
    {
    m_Muncz++;
    }
  else if (( absu[0] == absu[1])&& ( absu[0] == absu[2]))
    {
    m_Wxyz++;
    }
  else if (( absu[0] == absu[1])&& ( absu[0] > absu[2]) )
    {
    m_Wxy++;
    }
  else if (( absu[0] == absu[2])&& ( absu[0] > absu[1]) )
    {
    m_Wxz++;
    }
  else if (( absu[1] == absu[2])&& ( absu[0] < absu[2]) )
    {
    m_Wyz++;
    }
  else 
    { 
    itkWarningMacro(<<"Unpredicted situation...!");
    std::cout << "absu: " << absu[0] << ", " << absu[1] << ", " << absu[2] << std::endl;
    return; 
    }

  // This is reduced to ...
  //
  ii[0] = i[0] * i[0]; ii[1] = i[1] * i[1]; ii[2] = i[2] * i[2];
  jj[0] = j[0] * j[0]; jj[1] = j[1] * j[1]; jj[2] = j[2] * j[2];
  kk[0] = k[0] * k[0]; kk[1] = k[1] * k[1]; kk[2] = k[2] * k[2];

  // Area of a triangle using Heron's formula...
  //
  a = vcl_sqrt(ii[1] + jj[1] + kk[1]);
  b = vcl_sqrt(ii[0] + jj[0] + kk[0]);
  c = vcl_sqrt(ii[2] + jj[2] + kk[2]);
  s = 0.5 * (a + b + c);
  area = vcl_sqrt(vcl_fabs(s*(s-a)*(s-b)*(s-c)));

  // Volume elements ... 
  //
  zavg = (p1[2] + p2[2] + p3[2]) / 3.0;
  yavg = (p1[1] + p2[1] + p3[1]) / 3.0;
  xavg = (p1[0] + p2[0] + p3[0]) / 3.0;

  m_VolumeX += (area * (double)u[2] * (double)zavg);
  m_VolumeY += (area * (double)u[1] * (double)yavg);
  m_VolumeZ += (area * (double)u[0] * (double)xavg);

  m_Area += area;

  m_NumberOfTriangles++;
}

template <typename TInputMesh>
void SimplexMeshVolumeCalculator<TInputMesh>
::Compute()
{
  this->Initialize();
 
  InputPointType p1,p2,p3;
 
  InputPointsContainerPointer   Points    = m_SimplexMesh->GetPoints();
  InputPointsContainerIterator  pointsIt  = Points->Begin();
  InputPointsContainerIterator  pointsEnd = Points->End();

  while ( pointsIt != pointsEnd )
    {
    typename InputMeshType::IndexArray n = m_SimplexMesh->GetNeighbors( pointsIt.Index() );

    unsigned long newId1 = FindCellId(n[0], pointsIt.Index(), n[1]);
    unsigned long newId2 = FindCellId(n[1], pointsIt.Index(), n[2]);
    unsigned long newId3 = FindCellId(n[2], pointsIt.Index(), n[0]);
    
    bool b1 = m_Centers->GetElementIfIndexExists(newId1, &p1 );
    bool b2 = m_Centers->GetElementIfIndexExists(newId2, &p2 );
    bool b3 = m_Centers->GetElementIfIndexExists(newId3, &p3 );

    CalculateTriangleVolume(p1,p2,p3);

    if( !(b1 && b2 && b3) )
      {
      itkExceptionMacro(<<"Assertion failed for test of GetElementIfIndexExists()");
      }

    pointsIt++;
    }
  this->Finalize();
}

/* PrintSelf. */
template <typename TInputMesh>
void
SimplexMeshVolumeCalculator<TInputMesh>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
//  os << indent << "Mesh   = " << m_SimplexMesh << std::endl;
  os << indent << "Area = " << m_Area << std::endl;
  os << indent << "Volume = " << m_Volume << std::endl;
  os << indent << "VolumeX = " << m_VolumeX << std::endl;
  os << indent << "VolumeY = " << m_VolumeY << std::endl;
  os << indent << "VolumeZ = " << m_VolumeZ << std::endl;
  os << indent << "Kx = " << m_Kx << std::endl;
  os << indent << "Ky = " << m_Ky << std::endl;
  os << indent << "Kz = " << m_Kz << std::endl;
  os << indent << "NumberOfTriangles: " << m_NumberOfTriangles << std::endl;
}

} // end of namspace itk




#endif //__SimplexMeshVolumeCalculator_txx
