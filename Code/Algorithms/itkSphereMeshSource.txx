/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSphereMeshSource.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkSphereMeshSource_txx
#define _itkSphereMeshSource_txx

#include "itkSphereMeshSource.h"

namespace itk
{

/*
 *
 */
template<class TOutputMesh>
SphereMeshSource<TOutputMesh>
::SphereMeshSource()
{
  /*
   * Create the output
   */
  typename TOutputMesh::Pointer output = TOutputMesh::New();
  this->ProcessObject::SetNumberOfRequiredOutputs(1);
  this->ProcessObject::SetNthOutput(0, output.GetPointer());
  m_Squareness1 = 1.0;
  m_Squareness2 = 1.0;
  m_Center.Fill(0);
  m_Scale.Fill(1);
  m_ResolutionX = 4;
  m_ResolutionY = 4;
}

/*
 *
 */
template<class TOutputMesh>
void
SphereMeshSource<TOutputMesh>
::GenerateData()
{
  unsigned long i, j, jn, p, numpts;
  double ustep, vstep, ubeg, vbeg, u, v; 
  int signu, signv; 

// calculate the number os cells and points
  numpts = m_ResolutionX*m_ResolutionY + 2; 

// calculate the steps using resolution
  ustep = vnl_math::pi / (m_ResolutionX+1); 
  vstep = 2.0*vnl_math::pi / m_ResolutionY; 
  ubeg = (-vnl_math::pi/2.0) + ustep; 
  vbeg = -vnl_math::pi; 

///////////////////////////////////////////////////////////////////////////
// nodes allocation

// the temporary container of nodes' connectness
    unsigned long tripoints[3] = {0,1,2};
  
// memory allocation for nodes
    typename OutputMeshType::Pointer outputMesh = this->GetOutput();  

    outputMesh->GetPoints()->Reserve(numpts);

    outputMesh->SetCellsAllocationMethod( OutputMeshType::CellsAllocatedDynamicallyCellByCell );

    PointsContainerPointer  myPoints = outputMesh->GetPoints();
    typename PointsContainer::Iterator   point = myPoints->Begin();

    OPointType p1;

// calculate all regular nodes
    while( point != myPoints->End() ) 
      { 
      for (u=ubeg, i=0; i < m_ResolutionX; u += ustep, i++) 
        { 
        for (v=vbeg, j=0; j < m_ResolutionY; v += vstep, j++) 
          { 
          if (cos(u) > 0) 
            {
            signu = 1;
            } 
          else 
            {
            signu = -1;
            }
          if (cos(v) > 0) 
            {
            signv = 1;
            } 
          else 
            {
            signv = -1;
            }

          p1[0] = m_Scale[0]*signu*(pow((float)(fabs(cos(u))), (float) m_Squareness1))*signv* 
            (pow((float)(fabs(cos(v))), (float) m_Squareness2)) + m_Center[0]; 

          if (sin(v) > 0) 
            {
            signv = 1;
            } 
          else 
            {
            signv = -1;
            }

          p1[1] = m_Scale[1]*signu*(pow((float)(fabs(cos(u))), (float) m_Squareness1))*signv* 
            (pow((float)(fabs(sin(v))), (float) m_Squareness2)) + m_Center[1]; 

          if (sin(u) > 0) 
            {
            signu = 1;
            } 
          else 
            {
            signu = -1;
            }

          p1[2] = m_Scale[2]*signu*(pow((float)(fabs(sin(u))), (float) m_Squareness1)) + 
            m_Center[2];

          point.Value() = p1;
          ++point;
          } 
        }   

// calculate the south pole node
      p1[0] = (m_Scale[0]*(pow((float)(fabs(cos(-vnl_math::pi/2))),1.0f))* 
               (pow((float)(fabs(cos(0.0))),1.0f)) + m_Center[0]); 
      p1[1] = (m_Scale[1]*(pow((float)(fabs(cos(-vnl_math::pi/2))),1.0f))* 
               (pow((float)(fabs(sin(0.0))),1.0f)) + m_Center[1]); 
      p1[2] = (m_Scale[2]*-1*(pow((float)(fabs(sin(-vnl_math::pi/2))),1.0f)) 
               + m_Center[2]);
      point.Value() = p1;
      ++point;

// calculate the north pole node
      p1[0] = (m_Scale[0]*(pow((float)(fabs(cos(vnl_math::pi/2))),1.0f))* 
               (pow(fabs(cos(0.0)),1.0)) + m_Center[0]); 
      p1[1] = (m_Scale[1]*(pow((float)(fabs(cos(vnl_math::pi/2))),1.0f))* 
               (pow(fabs(sin(0.0)),1.0)) + m_Center[1]); 
      p1[2] = (m_Scale[2]*(pow((float)(fabs(sin(vnl_math::pi/2))),1.0f)) 
               + m_Center[2]);
      point.Value() = p1;
      ++point;
      }

///////////////////////////////////////////////////////////////////////////
// cells allocation
        p = 0;

// store all regular cells
        CellAutoPointer testCell;
        for(unsigned int i=0; i+1 < m_ResolutionX; i++) 
          {
          for (unsigned int j=0; j<m_ResolutionY; j++) 
            {
            jn = (j+1)%m_ResolutionY; 
            tripoints[0] = i*m_ResolutionY+j; 
            tripoints[1] = tripoints[0]-j+jn; 
            tripoints[2] = tripoints[0]+m_ResolutionY; 
            testCell.TakeOwnership( new TriCellType );
            testCell->SetPointIds(tripoints);
            outputMesh->SetCell(p, testCell );
            outputMesh->SetCellData(p, (OPixelType)3.0);
            p++;
            testCell.TakeOwnership( new TriCellType );
            tripoints[0] = tripoints[1]; 
            tripoints[1] = tripoints[0]+m_ResolutionY; 
            testCell->SetPointIds(tripoints);
            outputMesh->SetCell(p, testCell );
            outputMesh->SetCellData(p, (OPixelType)3.0);
            p++;
            }
          }
 
// store cells containing the south pole nodes
        for (unsigned int j=0; j<m_ResolutionY; j++) 
          {
          jn = (j+1)%m_ResolutionY; 
          tripoints[0] = numpts-2; 
          tripoints[1] = jn; 
          tripoints[2] = j; 
          testCell.TakeOwnership( new TriCellType );
          testCell->SetPointIds(tripoints);
          outputMesh->SetCell(p, testCell );
          outputMesh->SetCellData(p, (OPixelType)1.0);
          p++;
          }

// store cells containing the north pole nodes
        for (unsigned int j=0; j<m_ResolutionY; j++) 
          {
          jn = (j+1)%m_ResolutionY; 
          tripoints[2] = (m_ResolutionX-1)*m_ResolutionY+j; 
          tripoints[1] = numpts-1; 
          tripoints[0] = tripoints[2]-j+jn; 
          testCell.TakeOwnership( new TriCellType );
          testCell->SetPointIds(tripoints);
          outputMesh->SetCell(p, testCell );
          outputMesh->SetCellData(p, (OPixelType)2.0);
          p++;
          }


}




template<class TOutputMesh>
void
SphereMeshSource<TOutputMesh>
::PrintSelf( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Center: " << m_Center << std::endl;
  os << indent << "Scale: " << m_Scale << std::endl;
  os << indent << "ResolutionX: " << m_ResolutionX << std::endl;
  os << indent << "ResolutionX: " << m_ResolutionY << std::endl;
  os << indent << "Squareness1: " << m_Squareness1 << std::endl;
  os << indent << "Squareness2: " << m_Squareness2 << std::endl;
}

} // end namespace itk

#endif
