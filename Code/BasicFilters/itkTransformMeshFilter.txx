/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTransformMeshFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkTransformMeshFilter_txx
#define __itkTransformMeshFilter_txx

#include "itkTransformMeshFilter.h"
#include "itkExceptionObject.h"

namespace itk
{
/**
 *
 */
template< class TInputMesh, class TOutputMesh, class TTransform >
TransformMeshFilter< TInputMesh, TOutputMesh, TTransform >
::TransformMeshFilter()
{
  m_Transform = TransformType::New();
}

/**
 *
 */
template< class TInputMesh, class TOutputMesh, class TTransform >
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
template< class TInputMesh, class TOutputMesh, class TTransform >
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
