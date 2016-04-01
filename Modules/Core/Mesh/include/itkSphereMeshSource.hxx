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
#ifndef itkSphereMeshSource_hxx
#define itkSphereMeshSource_hxx

#include "itkIntTypes.h"
#include "itkSphereMeshSource.h"

namespace itk
{
/**
 *
 */
template< typename TOutputMesh >
SphereMeshSource< TOutputMesh >
::SphereMeshSource()
{
  /**
   * Create the output
   */
  typename TOutputMesh::Pointer output = TOutputMesh::New();
  this->ProcessObject::SetNumberOfRequiredOutputs(1);
  this->ProcessObject::SetNthOutput( 0, output.GetPointer() );
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
template< typename TOutputMesh >
void
SphereMeshSource< TOutputMesh >
::GenerateData()
{
  IdentifierType i, j, jn, p, numpts;
  double        ustep, vstep, ubeg, vbeg, u, v;
  int           signu, signv;

  // calculate the number os cells and points
  numpts = m_ResolutionX * m_ResolutionY + 2;

  // calculate the steps using resolution
  ustep = itk::Math::pi / ( m_ResolutionX + 1 );
  vstep = 2.0 * itk::Math::pi / m_ResolutionY;
  ubeg = ( -itk::Math::pi / 2.0 ) + ustep;
  vbeg = -itk::Math::pi;

  ///////////////////////////////////////////////////////////////////////////
  // nodes allocation

  // the temporary container of nodes' connectness
  typename OutputMeshType::PointIdentifier tripoints[3] = { 0, 1, 2 };

  // memory allocation for nodes
  typename OutputMeshType::Pointer outputMesh = this->GetOutput();

  outputMesh->GetPoints()->Reserve(numpts);

  outputMesh->SetCellsAllocationMethod(OutputMeshType::CellsAllocatedDynamicallyCellByCell);

  PointsContainerPointer myPoints = outputMesh->GetPoints();
  typename PointsContainer::Iterator point = myPoints->Begin();

  OPointType p1;

  // calculate all regular nodes
  while ( point != myPoints->End() )
    {
    for ( u = ubeg, i = 0; i < m_ResolutionX; u += ustep, i++ )
      {
      for ( v = vbeg, j = 0; j < m_ResolutionY; v += vstep, j++ )
        {
        if ( std::cos(u) > 0 )
          {
          signu = 1;
          }
        else
          {
          signu = -1;
          }
        if ( std::cos(v) > 0 )
          {
          signv = 1;
          }
        else
          {
          signv = -1;
          }

        p1[0] = m_Scale[0] * signu * ( std::pow( (float)( std::fabs( std::cos(u) ) ), (float)m_Squareness1 ) ) * signv
                * ( std::pow( (float)( std::fabs( std::cos(v) ) ), (float)m_Squareness2 ) ) + m_Center[0];

        if ( std::sin(v) > 0 )
          {
          signv = 1;
          }
        else
          {
          signv = -1;
          }

        p1[1] = m_Scale[1] * signu * ( std::pow( (float)( std::fabs( std::cos(u) ) ), (float)m_Squareness1 ) ) * signv
                * ( std::pow( (float)( std::fabs( std::sin(v) ) ), (float)m_Squareness2 ) ) + m_Center[1];

        if ( std::sin(u) > 0 )
          {
          signu = 1;
          }
        else
          {
          signu = -1;
          }

        p1[2] = m_Scale[2] * signu * ( std::pow( (float)( std::fabs( std::sin(u) ) ), (float)m_Squareness1 ) )
                + m_Center[2];

        point.Value() = p1;
        ++point;
        }
      }

    // calculate the south pole node
    p1[0] = ( m_Scale[0] * ( std::pow( (float)( std::fabs( std::cos(-itk::Math::pi / 2) ) ), 1.0f ) )
              * ( std::pow( (float)( std::fabs( std::cos(0.0) ) ), 1.0f ) ) + m_Center[0] );
    p1[1] = ( m_Scale[1] * ( std::pow( (float)( std::fabs( std::cos(-itk::Math::pi / 2) ) ), 1.0f ) )
              * ( std::pow( (float)( std::fabs( std::sin(0.0) ) ), 1.0f ) ) + m_Center[1] );
    p1[2] = ( m_Scale[2] * -1 * ( std::pow( (float)( std::fabs( std::sin(-itk::Math::pi / 2) ) ), 1.0f ) )
              + m_Center[2] );
    point.Value() = p1;
    ++point;

    // calculate the north pole node
    p1[0] = ( m_Scale[0] * ( std::pow( (float)( std::fabs( std::cos(itk::Math::pi / 2) ) ), 1.0f ) )
              * ( std::pow(std::fabs( std::cos(0.0) ), 1.0) ) + m_Center[0] );
    p1[1] = ( m_Scale[1] * ( std::pow( (float)( std::fabs( std::cos(itk::Math::pi / 2) ) ), 1.0f ) )
              * ( std::pow(std::fabs( std::sin(0.0) ), 1.0) ) + m_Center[1] );
    p1[2] = ( m_Scale[2] * ( std::pow( (float)( std::fabs( std::sin(itk::Math::pi / 2) ) ), 1.0f ) )
              + m_Center[2] );
    point.Value() = p1;
    ++point;
    }

  ///////////////////////////////////////////////////////////////////////////
  // cells allocation
  p = 0;

  // store all regular cells
  CellAutoPointer testCell;
  for ( unsigned int ii = 0; ii + 1 < m_ResolutionX; ii++ )
    {
    for ( unsigned int jj = 0; jj < m_ResolutionY; jj++ )
      {
      jn = ( jj + 1 ) % m_ResolutionY;
      tripoints[0] = ii * m_ResolutionY + jj;
      tripoints[1] = tripoints[0] - jj + jn;
      tripoints[2] = tripoints[0] + m_ResolutionY;
      testCell.TakeOwnership(new TriCellType);
      testCell->SetPointIds(tripoints);
      outputMesh->SetCell(p, testCell);
      outputMesh->SetCellData(p, (OPixelType)3.0);
      p++;
      testCell.TakeOwnership(new TriCellType);
      tripoints[0] = tripoints[1];
      tripoints[1] = tripoints[0] + m_ResolutionY;
      testCell->SetPointIds(tripoints);
      outputMesh->SetCell(p, testCell);
      outputMesh->SetCellData(p, (OPixelType)3.0);
      p++;
      }
    }

  // store cells containing the south pole nodes
  for ( unsigned int jj = 0; jj < m_ResolutionY; jj++ )
    {
    jn = ( jj + 1 ) % m_ResolutionY;
    tripoints[0] = numpts - 2;
    tripoints[1] = jn;
    tripoints[2] = jj;
    testCell.TakeOwnership(new TriCellType);
    testCell->SetPointIds(tripoints);
    outputMesh->SetCell(p, testCell);
    outputMesh->SetCellData(p, (OPixelType)1.0);
    p++;
    }

  // store cells containing the north pole nodes
  for ( unsigned int jj = 0; jj < m_ResolutionY; jj++ )
    {
    jn = ( jj + 1 ) % m_ResolutionY;
    tripoints[2] = ( m_ResolutionX - 1 ) * m_ResolutionY + jj;
    tripoints[1] = numpts - 1;
    tripoints[0] = tripoints[2] - jj + jn;
    testCell.TakeOwnership(new TriCellType);
    testCell->SetPointIds(tripoints);
    outputMesh->SetCell(p, testCell);
    outputMesh->SetCellData(p, (OPixelType)2.0);
    p++;
    }
}

template< typename TOutputMesh >
void
SphereMeshSource< TOutputMesh >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Center: " << m_Center << std::endl;
  os << indent << "Scale: " << m_Scale << std::endl;
  os << indent << "ResolutionX: " << m_ResolutionX << std::endl;
  os << indent << "ResolutionX: " << m_ResolutionY << std::endl;
  os << indent << "Squareness1: " << m_Squareness1 << std::endl;
  os << indent << "Squareness2: " << m_Squareness2 << std::endl;
}
} // end namespace itk

#endif
