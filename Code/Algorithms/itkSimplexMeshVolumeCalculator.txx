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
  m_Volume = 0;
  //m_SimplexMesh = TInpurMesh::New();
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
::CalculateTriangleVolume(double normal_z, InputPointType p1, InputPointType p2, InputPointType p3)
{
  // p1[0]=X1 p1[1]=Y1 p1[2]=Z1 and  p2[0]=X2 p2[1]=Y2 p2[2]=Z2 and p3[0]=X3 p3[1]=Y3 p3[2]=Z3
  // area = 0.5 * ( X1 ( Y3 -Y2) +  X2 ( Y1 -Y3) + X3 ( Y2 -Y1) )
  // area of trianlge in 2D
  double area = 0.5 * ( p1[0] * ( p3[1] - p2[1]) + 
                        p2[0] * ( p1[1] - p3[1]) + 
                        p3[0] * ( p2[1] - p1[1]) );

  // volume underneath triangle

  double volume_underneath = ( p1[2] + p2[2] + p3[2] ) / 3.0 * area;

  if (volume_underneath < 0)
    {
    volume_underneath *= -1;
    }
    

  if (normal_z < 0 )
    {
    m_Volume -= volume_underneath;
    }
  else
    {
    m_Volume += volume_underneath;
    }
    
}

template <typename TInputMesh>
void SimplexMeshVolumeCalculator<TInputMesh>
::Compute()
{
  this->Initialize();
 
  InputPointType p1,p2,p3, normal;
 
  InputPointsContainerPointer   Points    = m_SimplexMesh->GetPoints();
  InputPointsContainerIterator  pointsIt  = Points->Begin();
  InputPointsContainerIterator  pointsEnd = Points->End();

  //InputPointsContainerIterator  pointsIt  = m_SimplexMesh->GetPoints()->Begin();
  //InputPointsContainerIterator  pointsEnd = m_SimplexMesh->GetPoints()->End();

  while ( pointsIt != pointsEnd )
    {
    typename InputMeshType::IndexArray n = m_SimplexMesh->GetNeighbors( pointsIt.Index() );
    //compute normal to point
  
    normal = m_SimplexMesh->ComputeNormal( pointsIt.Index() );

    unsigned long newId1 = FindCellId(n[0], pointsIt.Index(), n[1]);
    unsigned long newId2 = FindCellId(n[1], pointsIt.Index(), n[2]);
    unsigned long newId3 = FindCellId(n[2], pointsIt.Index(), n[0]);
    
    bool b1 = m_Centers->GetElementIfIndexExists(newId1, &p1 );
    bool b2 = m_Centers->GetElementIfIndexExists(newId2, &p2 );
    bool b3 = m_Centers->GetElementIfIndexExists(newId3, &p3 );

      
    CalculateTriangleVolume(normal[2],p1,p2,p3);

    if( !(b1 && b2 && b3) )
      {
      itkExceptionMacro(<<"Assertion failed for test of GetElementIfIndexExists()");
      }

    pointsIt++;
    }

}

/* PrintSelf. */
template <typename TInputMesh>
void
SimplexMeshVolumeCalculator<TInputMesh>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  os << indent << "Volume = " << m_Volume << std::endl;
  os << indent << "Mesh   = " << m_SimplexMesh << std::endl;
}

} // end of namspace itk




#endif //__SimplexMeshVolumeCalculator_txx
