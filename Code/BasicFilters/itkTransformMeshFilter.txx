/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTransformMeshFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkTransformMeshFilter_txx
#define _itkTransformMeshFilter_txx

#include "itkTransformMeshFilter.h"
#include "itkExceptionObject.h"

namespace itk
{
  
/**
 *
 */
template <class TInputMesh, class TOutputMesh, class TTransform>
TransformMeshFilter<TInputMesh,TOutputMesh,TTransform>
::TransformMeshFilter()
{
  m_Transform = TransformType::New();
}


/**
 *
 */
template <class TInputMesh, class TOutputMesh, class TTransform>
void 
TransformMeshFilter<TInputMesh,TOutputMesh,TTransform>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  if (m_Transform)
    {
    os << indent << "Transform: " << m_Transform << std::endl;
    }
}


/**
 * This method causes the filter to generate its output.
 */
template <class TInputMesh, class TOutputMesh, class TTransform>
void 
TransformMeshFilter<TInputMesh,TOutputMesh,TTransform>
::GenerateData(void) 
{
  
  typedef typename TInputMesh::PointsContainer  InputPointsContainer;
  typedef typename TOutputMesh::PointsContainer OutputPointsContainer;

  typedef typename TInputMesh::PointsContainerPointer  InputPointsContainerPointer;
  typedef typename TOutputMesh::PointsContainerPointer OutputPointsContainerPointer;

  InputMeshPointer    inputMesh      =  this->GetInput();
  OutputMeshPointer   outputMesh     =  this->GetOutput();
  
  if( !inputMesh )
    {
    itkExceptionMacro(<<"Missing Input Mesh");
    }

  if( !outputMesh )
    {
    itkExceptionMacro(<<"Missing Output Mesh");
    }

  outputMesh->SetBufferedRegion( outputMesh->GetRequestedRegion() );

  InputPointsContainerPointer  inPoints  = inputMesh->GetPoints();
  OutputPointsContainerPointer outPoints = outputMesh->GetPoints();

  outPoints->Reserve( inputMesh->GetNumberOfPoints() );
  outPoints->Squeeze();  // in case the previous mesh had 
                         // allocated a larger memory

  typename InputPointsContainer::ConstIterator  inputPoint  = inPoints->Begin();
  typename OutputPointsContainer::Iterator      outputPoint = outPoints->Begin();

  while( inputPoint != inPoints->End() ) 
    {
    outputPoint.Value() = 
      m_Transform->TransformPoint( inputPoint.Value() );

    ++inputPoint;
    ++outputPoint;
    }


  // Create duplicate references to the rest of data on the mesh

  outputMesh->SetPointData(  inputMesh->GetPointData() );
  
  outputMesh->SetCellLinks(  inputMesh->GetCellLinks() );
  
  outputMesh->SetCells(  inputMesh->GetCells() );
  outputMesh->SetCellData(  inputMesh->GetCellData() );
  
  
  unsigned int maxDimension = TInputMesh::MaxTopologicalDimension;

  for( unsigned int dim = 0; dim < maxDimension; dim++ )
    {
    outputMesh->SetBoundaries(    dim, inputMesh->GetBoundaries(dim)   );
    outputMesh->SetBoundaryData(  dim, inputMesh->GetBoundaryData(dim) );
    outputMesh->SetBoundaryAssignments(  dim,
                                         inputMesh->GetBoundaryAssignments(dim) );
    }
}

} // end namespace itk

#endif
