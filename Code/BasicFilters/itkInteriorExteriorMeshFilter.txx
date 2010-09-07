/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkInteriorExteriorMeshFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkInteriorExteriorMeshFilter_txx
#define __itkInteriorExteriorMeshFilter_txx

#include "itkInteriorExteriorMeshFilter.h"
#include "itkExceptionObject.h"
#include "itkNumericTraits.h"
#include "itkProgressReporter.h"

namespace itk
{
/**
 *
 */
template< class TInputMesh, class TOutputMesh, class TSpatialFunction >
InteriorExteriorMeshFilter< TInputMesh, TOutputMesh, TSpatialFunction >
::InteriorExteriorMeshFilter()
{
  m_SpatialFunction = SpatialFunctionType::New();
  SpatialFunctionDataObjectPointer spatialFunctionObject =
    SpatialFunctionDataObjectType::New();
  spatialFunctionObject->Set(m_SpatialFunction);
  this->ProcessObject::SetNthInput(1, spatialFunctionObject);
}

/**
 *
 */
template< class TInputMesh, class TOutputMesh, class TSpatialFunction >
void
InteriorExteriorMeshFilter< TInputMesh, TOutputMesh, TSpatialFunction >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << m_SpatialFunction << std::endl;
}

/**
 * This method causes the filter to generate its output.
 */
template< class TInputMesh, class TOutputMesh, class TSpatialFunction >
void
InteriorExteriorMeshFilter< TInputMesh, TOutputMesh, TSpatialFunction >
::GenerateData(void)
{
  typedef typename TInputMesh::PointsContainer  InputPointsContainer;
  typedef typename TOutputMesh::PointsContainer OutputPointsContainer;

  typedef typename TInputMesh::PointsContainerConstPointer InputPointsContainerConstPointer;
  typedef typename TOutputMesh::PointsContainerPointer     OutputPointsContainerPointer;

  typedef typename TInputMesh::PointDataContainer  InputPointDataContainer;
  typedef typename TOutputMesh::PointDataContainer OutputPointDataContainer;

  typedef typename TInputMesh::PointDataContainerConstPointer InputPointDataContainerConstPointer;
  typedef typename TOutputMesh::PointDataContainerPointer     OutputPointDataContainerPointer;

  const InputMeshType *inputMesh =  this->GetInput();
  OutputMeshPointer    outputMesh =  this->GetOutput();

  if ( !inputMesh )
    {
    ExceptionObject exception(__FILE__, __LINE__);
    exception.SetDescription("Missing Input Mesh");
    exception.SetLocation(ITK_LOCATION);
    throw exception;
    }

  if ( !outputMesh )
    {
    ExceptionObject exception(__FILE__, __LINE__);
    exception.SetDescription("Missing Output Mesh");
    exception.SetLocation(ITK_LOCATION);
    throw exception;
    }

  outputMesh->SetBufferedRegion( outputMesh->GetRequestedRegion() );

  InputPointsContainerConstPointer    inPoints  = inputMesh->GetPoints();
  InputPointDataContainerConstPointer inData = inputMesh->GetPointData();

  typename InputPointsContainer::ConstIterator inputPoint = inPoints->Begin();
  typename InputPointDataContainer::ConstIterator inputData;

  bool inputDataExists = false;
  if ( inData )
    {
    inputDataExists = true;
    }

  if ( inputDataExists )
    {
    inputData = inData->Begin();
    }

  // support progress methods/callbacks
  ProgressReporter progress( this, 0, inPoints->Size() );

  typedef typename TSpatialFunction::OutputType ValueType;

  typedef typename TOutputMesh::PointIdentifier PointIdType;
  PointIdType pointId = NumericTraits< PointIdType >::Zero;

  while ( inputPoint != inPoints->End() )
    {
    ValueType value = m_SpatialFunction->Evaluate( inputPoint.Value() );

    if ( value ) // Assumes return type is "bool"
      {
      outputMesh->SetPoint( pointId, inputPoint.Value() );
      if ( inputDataExists )
        {
        outputMesh->SetPointData( pointId, inputData.Value() );
        }
      pointId++;
      }

    ++inputPoint;
    if ( inputDataExists )
      {
      ++inputData;
      }
    progress.CompletedPixel();
    }

  // Create duplicate references to the rest of data in the mesh
  this->CopyInputMeshToOutputMeshCellLinks();
  this->CopyInputMeshToOutputMeshCells();
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
