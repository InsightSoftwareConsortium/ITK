/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkWarpMeshFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkWarpMeshFilter_txx
#define __itkWarpMeshFilter_txx

#include "itkWarpMeshFilter.h"
#include "itkExceptionObject.h"

namespace itk
{
/**
 *
 */
template< class TInputMesh, class TOutputMesh, class TDeformationField >
WarpMeshFilter< TInputMesh, TOutputMesh, TDeformationField >
::WarpMeshFilter()
{
  // Setup the number of required inputs.
  // This filter requires as input one Mesh and one Vector image.
  this->SetNumberOfRequiredInputs(2);
}

template< class TInputMesh, class TOutputMesh, class TDeformationField >
const typename WarpMeshFilter< TInputMesh, TOutputMesh, TDeformationField >::DeformationFieldType *
WarpMeshFilter< TInputMesh, TOutputMesh, TDeformationField >
::GetDeformationField(void) const
{
  return static_cast< const DeformationFieldType * >
         ( this->ProcessObject::GetInput(1) );
}

template< class TInputMesh, class TOutputMesh, class TDeformationField >
void
WarpMeshFilter< TInputMesh, TOutputMesh, TDeformationField >
::SetDeformationField(const DeformationFieldType *field)
{
  // const cast is needed because the pipeline is not const-correct.
  DeformationFieldType *input =
    const_cast< DeformationFieldType * >( field );

  this->ProcessObject::SetNthInput(1, input);
}

/**
 *
 */
template< class TInputMesh, class TOutputMesh, class TDeformationField >
void
WarpMeshFilter< TInputMesh, TOutputMesh, TDeformationField >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

/**
 * This method causes the filter to generate its output.
 */
template< class TInputMesh, class TOutputMesh, class TDeformationField >
void
WarpMeshFilter< TInputMesh, TOutputMesh, TDeformationField >
::GenerateData(void)
{
  typedef typename TInputMesh::PointsContainer  InputPointsContainer;
  typedef typename TOutputMesh::PointsContainer OutputPointsContainer;

  typedef typename TInputMesh::PointsContainerPointer  InputPointsContainerPointer;
  typedef typename TOutputMesh::PointsContainerPointer OutputPointsContainerPointer;

  const InputMeshType *   inputMesh   =  this->GetInput();
  OutputMeshPointer       outputMesh     =  this->GetOutput();
  DeformationFieldPointer fieldPtr   =  this->GetDeformationField();

  if ( !inputMesh )
    {
    itkExceptionMacro(<< "Missing Input Mesh");
    }

  if ( !outputMesh )
    {
    itkExceptionMacro(<< "Missing Output Mesh");
    }

  outputMesh->SetBufferedRegion( outputMesh->GetRequestedRegion() );

  const InputPointsContainer * inPoints  = inputMesh->GetPoints();
  OutputPointsContainerPointer outPoints = outputMesh->GetPoints();

  outPoints->Reserve( inputMesh->GetNumberOfPoints() );
  outPoints->Squeeze();  // in case the previous mesh had
                         // allocated a larger memory

  typename InputPointsContainer::ConstIterator inputPoint  = inPoints->Begin();
  typename OutputPointsContainer::Iterator outputPoint = outPoints->Begin();

  typedef typename InputMeshType::PointType        InputPointType;
  typedef typename OutputMeshType::PointType       OutputPointType;
  typedef typename DeformationFieldType::IndexType IndexType;
  IndexType index;

  OutputPointType displacedPoint;

  DisplacementType displacement;

  const unsigned int Dimension = fieldPtr->GetImageDimension();

  while ( inputPoint != inPoints->End() )
    {
    const InputPointType & originalPoint = inputPoint.Value();
    fieldPtr->TransformPhysicalPointToIndex(originalPoint, index);
    displacement = fieldPtr->GetPixel(index);

    for ( unsigned int i = 0; i < Dimension; i++ )
      {
      displacedPoint[i] = originalPoint[i] + displacement[i];
      }

    outputPoint.Value() = displacedPoint;

    ++inputPoint;
    ++outputPoint;
    }

  // Create duplicate references to the rest of data on the mesh

  this->CopyInputMeshToOutputMeshPointData();
  this->CopyInputMeshToOutputMeshCells();
  this->CopyInputMeshToOutputMeshCellLinks();
  this->CopyInputMeshToOutputMeshCellData();

  unsigned int maxDimension = TInputMesh::MaxTopologicalDimension;

  for ( unsigned int dim = 0; dim < maxDimension; dim++ )
    {
    outputMesh->SetBoundaryAssignments( dim,
                                        inputMesh->GetBoundaryAssignments(dim) );
    }
}
} // end namespace itk

#endif
