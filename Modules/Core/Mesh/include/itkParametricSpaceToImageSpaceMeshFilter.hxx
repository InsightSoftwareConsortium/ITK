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
#ifndef itkParametricSpaceToImageSpaceMeshFilter_hxx
#define itkParametricSpaceToImageSpaceMeshFilter_hxx

#include "itkParametricSpaceToImageSpaceMeshFilter.h"
#include "itkMacro.h"
#include "itkProgressReporter.h"

namespace itk
{
/**
 *
 */
template< typename TInputMesh, typename TOutputMesh >
ParametricSpaceToImageSpaceMeshFilter< TInputMesh, TOutputMesh >
::ParametricSpaceToImageSpaceMeshFilter()
{
  this->SetNumberOfRequiredInputs(1);
}

/**
 *
 */
template< typename TInputMesh, typename TOutputMesh >
void
ParametricSpaceToImageSpaceMeshFilter< TInputMesh, TOutputMesh >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

/**
 * This method causes the filter to generate its output.
 */
template< typename TInputMesh, typename TOutputMesh >
void
ParametricSpaceToImageSpaceMeshFilter< TInputMesh, TOutputMesh >
::GenerateData(void)
{
  typedef typename TInputMesh::PointsContainer  InputPointsContainer;
  typedef typename TOutputMesh::PointsContainer OutputPointsContainer;

  typedef typename TOutputMesh::PointsContainerPointer OutputPointsContainerPointer;

  typedef typename TInputMesh::PointDataContainer  InputPointDataContainer;
  typedef typename TOutputMesh::PointDataContainer OutputPointDataContainer;

  typedef typename TOutputMesh::PointDataContainerPointer OutputPointDataContainerPointer;

  const InputMeshType *inputMesh    =  this->GetInput();
  OutputMeshPointer    outputMesh     =  this->GetOutput();

  if ( !inputMesh )
    {
    itkExceptionMacro(<< "Missing Input Mesh");
    }

  if ( !outputMesh )
    {
    itkExceptionMacro(<< "Missing Output Mesh");
    }

  outputMesh->SetBufferedRegion( outputMesh->GetRequestedRegion() );

  const InputPointsContainer * inPoints = inputMesh->GetPoints();
  OutputPointsContainerPointer outPoints = OutputPointsContainer::New();

  outPoints->Reserve( inputMesh->GetNumberOfPoints() );

  const InputPointDataContainer * inData = inputMesh->GetPointData();
  OutputPointDataContainerPointer outData = OutputPointDataContainer::New();

  outData->Reserve( inputMesh->GetNumberOfPoints() );

  outputMesh->SetPoints( outPoints.GetPointer() );
  outputMesh->SetPointData( outData.GetPointer() );

  if ( !inData )
    {
    return;
    }

  if ( !inPoints )
    {
    return;
    }

  typename InputPointsContainer::ConstIterator inputPointIt   = inPoints->Begin();
  typename InputPointsContainer::ConstIterator inputPointEnd  = inPoints->End();
  typename InputPointDataContainer::ConstIterator inputDataIt = inData->Begin();

  typename OutputPointsContainer::Iterator outputPointIt    = outPoints->Begin();
  typename OutputPointDataContainer::Iterator outputDataIt  = outData->Begin();

  // support progress methods/callbacks
  ProgressReporter progress( this, 0, inPoints->Size() );

  const unsigned int OutputDimension = TOutputMesh::PointDimension;

  typename TOutputMesh::PointType point;

  while ( inputPointIt != inputPointEnd )
    {
    for ( unsigned int i = 0; i < OutputDimension; i++ )
      {
      // Conver Index coordinates to MeshSpace
      point[i] = inputDataIt.Value()[i];
      }

    outputPointIt.Value() = point;
    outputDataIt.Value()  = inputPointIt.Value();

    ++inputDataIt;
    ++inputPointIt;
    ++outputPointIt;
    ++outputDataIt;
    progress.CompletedPixel();
    }
}

/**
 * copy information from first input to all outputs
 */
template< typename TInputMesh, typename TOutputMesh >
void
ParametricSpaceToImageSpaceMeshFilter< TInputMesh, TOutputMesh >
::GenerateOutputInformation()
{
  // No additional information needs to be copied
}
} // end namespace itk

#endif
