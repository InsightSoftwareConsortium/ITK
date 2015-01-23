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
#ifndef itkSimplexMeshToTriangleMeshFilter_hxx
#define itkSimplexMeshToTriangleMeshFilter_hxx

#include "itkSimplexMeshToTriangleMeshFilter.h"

namespace itk
{
template< typename TInputMesh, typename TOutputMesh >
SimplexMeshToTriangleMeshFilter< TInputMesh, TOutputMesh >::SimplexMeshToTriangleMeshFilter()
{}

template< typename TInputMesh, typename TOutputMesh >
SimplexMeshToTriangleMeshFilter< TInputMesh, TOutputMesh >
::~SimplexMeshToTriangleMeshFilter()
{}

template< typename TInputMesh, typename TOutputMesh >
void SimplexMeshToTriangleMeshFilter< TInputMesh, TOutputMesh >
::GenerateData()
{
  this->Initialize();
  this->CreateTriangles();
}

template< typename TInputMesh, typename TOutputMesh >
void SimplexMeshToTriangleMeshFilter< TInputMesh, TOutputMesh >
::Initialize()
{
  SimplexVisitorInterfacePointer simplexVisitor = SimplexVisitorInterfaceType::New();

  simplexVisitor->SetMesh( this->GetInput(0) );
  CellMultiVisitorPointer mv = CellMultiVisitorType::New();
  mv->AddVisitor(simplexVisitor);
  this->GetInput(0)->Accept(mv);
  this->GetInput(0)->BuildCellLinks();
  m_Centers = simplexVisitor->GetCenterMap();
}

template< typename TInputMesh, typename TOutputMesh >
void SimplexMeshToTriangleMeshFilter< TInputMesh, TOutputMesh >
::CreateTriangles()
{
  typename AutoMeshSourceType::Pointer meshSource = AutoMeshSourceType::New();
  typename AutoMeshSourceType::PointType p1, p2, p3;

  typename TInputMesh::ConstPointer inputMesh = this->GetInput(0);
  typename InputPointsContainer::ConstPointer points = inputMesh->GetPoints();
  typename TInputMesh::PointsContainerConstIterator pointsIt = points->Begin();

  meshSource->Update();

  while ( pointsIt != points->End() )
    {
    typename InputMeshType::IndexArray n = this->GetInput(0)->GetNeighbors( pointsIt.Index() );

    CellIdentifier newId1 = FindCellId(n[0], pointsIt.Index(), n[1]);
    CellIdentifier newId2 = FindCellId(n[1], pointsIt.Index(), n[2]);
    CellIdentifier newId3 = FindCellId(n[2], pointsIt.Index(), n[0]);

    bool b1 = m_Centers->GetElementIfIndexExists(newId1, &p1);
    bool b2 = m_Centers->GetElementIfIndexExists(newId2, &p2);
    bool b3 = m_Centers->GetElementIfIndexExists(newId3, &p3);

    meshSource->AddTriangle(p1, p2, p3);

    if ( !( b1 && b2 && b3 ) )
      {
      itkExceptionMacro(<< "Assertion failed for test of GetElementIfIndexExists()");
      }

    pointsIt++;
    }

  this->ProcessObject::SetNthOutput( 0,  meshSource->GetOutput() );
}

template< typename TInputMesh, typename TOutputMesh >
typename SimplexMeshToTriangleMeshFilter< TInputMesh, TOutputMesh >::CellIdentifier
SimplexMeshToTriangleMeshFilter< TInputMesh, TOutputMesh >
::FindCellId(CellIdentifier id1, CellIdentifier id2, CellIdentifier id3)
{
  std::set< CellIdentifier >  cells1 =  this->GetInput(0)->GetCellLinks()->GetElement(id1);
  std::set< CellIdentifier >  cells2 =  this->GetInput(0)->GetCellLinks()->GetElement(id2);
  std::set< CellIdentifier >  cells3 =  this->GetInput(0)->GetCellLinks()->GetElement(id3);
  typename std::set< CellIdentifier >::iterator cellIt = cells1.begin();

  while ( cellIt != cells1.end() )
    {
    typename std::set< CellIdentifier >::iterator found2 = std::find(cells2.begin(), cells2.end(), *cellIt);
    typename std::set< CellIdentifier >::iterator found3 = std::find(cells3.begin(), cells3.end(), *cellIt);

    if ( found2 != cells2.end() && found3 != cells3.end() )
      {
      break;
      }
    cellIt++;
    }

  if ( cellIt == cells1.end() )
    {
    itkExceptionMacro(<< "Cell was not found, although it should be there");
    }

  return *cellIt;
}

/* PrintSelf. */
template< typename TInputMesh, typename TOutputMesh >
void
SimplexMeshToTriangleMeshFilter< TInputMesh, TOutputMesh >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "ToDo: implement PrinSelf!!!";
}
} // end of namspace itk

#endif //__SimplexMeshToTriangleMeshFilter_hxx
