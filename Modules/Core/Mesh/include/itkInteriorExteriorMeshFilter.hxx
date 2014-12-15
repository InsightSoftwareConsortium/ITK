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
#ifndef itkInteriorExteriorMeshFilter_hxx
#define itkInteriorExteriorMeshFilter_hxx

#include "itkInteriorExteriorMeshFilter.h"
#include "itkNumericTraits.h"
#include "itkProgressReporter.h"

namespace itk
{
/**
 *
 */
template< typename TInputMesh, typename TOutputMesh, typename TSpatialFunction >
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
template< typename TInputMesh, typename TOutputMesh, typename TSpatialFunction >
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
template< typename TInputMesh, typename TOutputMesh, typename TSpatialFunction >
void
InteriorExteriorMeshFilter< TInputMesh, TOutputMesh, TSpatialFunction >
::GenerateData(void)
{
  typedef typename TInputMesh::PointsContainer  InputPointsContainer;

  typedef typename TInputMesh::PointsContainerConstPointer InputPointsContainerConstPointer;

  typedef typename TInputMesh::PointDataContainer  InputPointDataContainer;

  typedef typename TInputMesh::PointDataContainerConstPointer InputPointDataContainerConstPointer;

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
  PointIdType pointId = NumericTraits< PointIdType >::ZeroValue();

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
