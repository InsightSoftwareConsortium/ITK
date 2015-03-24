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
#ifndef itkTransformMeshFilter_hxx
#define itkTransformMeshFilter_hxx

#include "itkTransformMeshFilter.h"
#include "itkMacro.h"

namespace itk
{
/**
 *
 */
template< typename TInputMesh, typename TOutputMesh, typename TTransform >
TransformMeshFilter< TInputMesh, TOutputMesh, TTransform >
::TransformMeshFilter()
{
  m_Transform = ITK_NULLPTR; // has to be provided by the user.
}

/**
 *
 */
template< typename TInputMesh, typename TOutputMesh, typename TTransform >
void
TransformMeshFilter< TInputMesh, TOutputMesh, TTransform >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  if ( m_Transform )
    {
    os << indent << "Transform: " << m_Transform << std::endl;
    }
}

/**
 * This method causes the filter to generate its output.
 */
template< typename TInputMesh, typename TOutputMesh, typename TTransform >
void
TransformMeshFilter< TInputMesh, TOutputMesh, TTransform >
::GenerateData(void)
{
  typedef typename TInputMesh::PointsContainer  InputPointsContainer;
  typedef typename TOutputMesh::PointsContainer OutputPointsContainer;

  typedef typename TInputMesh::PointsContainerConstPointer InputPointsContainerConstPointer;
  typedef typename TOutputMesh::PointsContainerPointer     OutputPointsContainerPointer;

  const InputMeshType *inputMesh   =  this->GetInput();
  OutputMeshPointer    outputMesh   =  this->GetOutput();

  if ( !inputMesh )
    {
    itkExceptionMacro(<< "Missing Input Mesh");
    }

  if ( !outputMesh )
    {
    itkExceptionMacro(<< "Missing Output Mesh");
    }

  if ( !m_Transform )
    {
    itkExceptionMacro(<< "Missing Input Transform");
    }

  outputMesh->SetBufferedRegion( outputMesh->GetRequestedRegion() );

  InputPointsContainerConstPointer inPoints  = inputMesh->GetPoints();
  OutputPointsContainerPointer     outPoints = outputMesh->GetPoints();

  outPoints->Reserve( inputMesh->GetNumberOfPoints() );
  outPoints->Squeeze();  // in case the previous mesh had
                         // allocated a larger memory

  typename InputPointsContainer::ConstIterator inputPoint  = inPoints->Begin();
  typename OutputPointsContainer::Iterator outputPoint = outPoints->Begin();

  while ( inputPoint != inPoints->End() )
    {
    outputPoint.Value() =
      m_Transform->TransformPoint( inputPoint.Value() );

    ++inputPoint;
    ++outputPoint;
    }

  // Create duplicate references to the rest of data on the mesh
  this->CopyInputMeshToOutputMeshPointData();
  this->CopyInputMeshToOutputMeshCellLinks();
  this->CopyInputMeshToOutputMeshCells();
  this->CopyInputMeshToOutputMeshCellData();

  // FIXME: DELETEME outputMesh->SetCellLinks(  inputMesh->GetCellLinks() );
  // FIXME: DELETEME outputMesh->SetCells(  inputMesh->GetCells() );
  // FIXME: DELETEME outputMesh->SetCellData(  inputMesh->GetCellData() );

  unsigned int maxDimension = TInputMesh::MaxTopologicalDimension;

  for ( unsigned int dim = 0; dim < maxDimension; dim++ )
    {
    outputMesh->SetBoundaryAssignments( dim,
                                        inputMesh->GetBoundaryAssignments(dim) );
    }
}
} // end namespace itk

#endif
