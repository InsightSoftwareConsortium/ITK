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
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#ifndef itkMeshToMeshFilter_hxx
#define itkMeshToMeshFilter_hxx

#include "itkMeshToMeshFilter.h"

namespace itk
{
/**
 *
 */
template< typename TInputMesh, typename TOutputMesh >
MeshToMeshFilter< TInputMesh, TOutputMesh >
::MeshToMeshFilter()
{
  // Modify superclass default values, can be overridden by subclasses
  this->SetNumberOfRequiredInputs(1);
}

/**
 *
 */
template< typename TInputMesh, typename TOutputMesh >
void
MeshToMeshFilter< TInputMesh, TOutputMesh >
::SetInput(const TInputMesh *input)
{
  // Process object is not const-correct so the const_cast is required here
  this->ProcessObject::SetNthInput( 0,
                                    const_cast< TInputMesh * >( input ) );
}

/**
 *
 */
template< typename TInputMesh, typename TOutputMesh >
const typename MeshToMeshFilter< TInputMesh, TOutputMesh >::InputMeshType *
MeshToMeshFilter< TInputMesh, TOutputMesh >
::GetInput() const
{
  return itkDynamicCastInDebugMode< const TInputMesh * >( this->GetPrimaryInput() );
}

/**
 *
 */
template< typename TInputMesh, typename TOutputMesh >
const typename MeshToMeshFilter< TInputMesh, TOutputMesh >::InputMeshType *
MeshToMeshFilter< TInputMesh, TOutputMesh >
::GetInput(unsigned int idx) const
{
  return dynamic_cast< const TInputMesh * >
         ( this->ProcessObject::GetInput(idx) );
}

template< typename TInputMesh, typename TOutputMesh >
void
MeshToMeshFilter< TInputMesh, TOutputMesh >
::CopyInputMeshToOutputMeshPoints(void)
{
  const InputMeshType *inputMesh   =  this->GetInput();
  OutputMeshPointer    outputMesh   =  this->GetOutput();

  typedef typename TOutputMesh::PointsContainer OutputPointsContainer;
  typedef typename TInputMesh::PointsContainer  InputPointsContainer;

  typename OutputPointsContainer::Pointer outputPoints = OutputPointsContainer::New();
  const InputPointsContainer *inputPoints = inputMesh->GetPoints();

  if ( inputPoints )
    {
    outputPoints->Reserve( inputPoints->Size() );

    typename InputPointsContainer::ConstIterator inputItr = inputPoints->Begin();
    typename InputPointsContainer::ConstIterator inputEnd = inputPoints->End();

    typename OutputPointsContainer::Iterator outputItr = outputPoints->Begin();

    while ( inputItr != inputEnd )
      {
      outputItr.Value() = inputItr.Value();
      ++inputItr;
      ++outputItr;
      }

    outputMesh->SetPoints(outputPoints);
    }
}

template< typename TInputMesh, typename TOutputMesh >
void
MeshToMeshFilter< TInputMesh, TOutputMesh >
::CopyInputMeshToOutputMeshPointData(void)
{
  const InputMeshType *inputMesh   =  this->GetInput();
  OutputMeshPointer    outputMesh   =  this->GetOutput();

  typedef typename TOutputMesh::PointDataContainer OutputPointDataContainer;
  typedef typename TInputMesh::PointDataContainer  InputPointDataContainer;

  typename OutputPointDataContainer::Pointer outputPointData = OutputPointDataContainer::New();
  const InputPointDataContainer *inputPointData = inputMesh->GetPointData();

  if ( inputPointData )
    {
    outputPointData->Reserve( inputPointData->Size() );

    typename InputPointDataContainer::ConstIterator inputItr = inputPointData->Begin();
    typename InputPointDataContainer::ConstIterator inputEnd = inputPointData->End();

    typename OutputPointDataContainer::Iterator outputItr = outputPointData->Begin();

    while ( inputItr != inputEnd )
      {
      outputItr.Value() = inputItr.Value();
      ++inputItr;
      ++outputItr;
      }

    outputMesh->SetPointData(outputPointData);
    }
}

template< typename TInputMesh, typename TOutputMesh >
void
MeshToMeshFilter< TInputMesh, TOutputMesh >
::CopyInputMeshToOutputMeshCellLinks(void)
{
  const InputMeshType *inputMesh   =  this->GetInput();
  OutputMeshPointer    outputMesh   =  this->GetOutput();

  typedef typename TOutputMesh::CellLinksContainer OutputCellLinksContainer;
  typedef typename TInputMesh::CellLinksContainer  InputCellLinksContainer;

  typename OutputCellLinksContainer::Pointer outputCellLinks = OutputCellLinksContainer::New();
  const InputCellLinksContainer *inputCellLinks = inputMesh->GetCellLinks();

  if ( inputCellLinks )
    {
    outputCellLinks->Reserve( inputCellLinks->Size() );

    typename InputCellLinksContainer::ConstIterator inputItr = inputCellLinks->Begin();
    typename InputCellLinksContainer::ConstIterator inputEnd = inputCellLinks->End();

    typename OutputCellLinksContainer::Iterator outputItr = outputCellLinks->Begin();

    while ( inputItr != inputEnd )
      {
      outputItr.Value() = inputItr.Value();
      ++inputItr;
      ++outputItr;
      }

    outputMesh->SetCellLinks(outputCellLinks);
    }
}

template< typename TInputMesh, typename TOutputMesh >
void
MeshToMeshFilter< TInputMesh, TOutputMesh >
::CopyInputMeshToOutputMeshCells(void)
{
  const InputMeshType *inputMesh   =  this->GetInput();
  OutputMeshPointer    outputMesh   =  this->GetOutput();

  typedef typename TOutputMesh::CellsContainer  OutputCellsContainer;
  typedef typename TInputMesh::CellsContainer   InputCellsContainer;
  typedef typename TOutputMesh::CellAutoPointer CellAutoPointer;

  outputMesh->SetCellsAllocationMethod(OutputMeshType::CellsAllocatedDynamicallyCellByCell);

  typename OutputCellsContainer::Pointer outputCells = OutputCellsContainer::New();
  const InputCellsContainer *inputCells = inputMesh->GetCells();

  if ( inputCells )
    {
    outputCells->Reserve( inputCells->Size() );

    typename InputCellsContainer::ConstIterator inputItr = inputCells->Begin();
    typename InputCellsContainer::ConstIterator inputEnd = inputCells->End();

    typename OutputCellsContainer::Iterator outputItr = outputCells->Begin();

    CellAutoPointer clone;

    while ( inputItr != inputEnd )
      {
//      outputItr.Value() = inputItr.Value();
      // BUG: FIXME: Here we are copying a pointer, which is a mistake. What we
      // should do is to clone the cell.
      inputItr.Value()->MakeCopy(clone);
      outputItr.Value() = clone.ReleaseOwnership();

      ++inputItr;
      ++outputItr;
      }

    outputMesh->SetCells(outputCells);
    }
}

template< typename TInputMesh, typename TOutputMesh >
void
MeshToMeshFilter< TInputMesh, TOutputMesh >
::CopyInputMeshToOutputMeshCellData(void)
{
  const InputMeshType *inputMesh   =  this->GetInput();
  OutputMeshPointer    outputMesh   =  this->GetOutput();

  typedef typename TOutputMesh::CellDataContainer OutputCellDataContainer;
  typedef typename TInputMesh::CellDataContainer  InputCellDataContainer;

  typename OutputCellDataContainer::Pointer outputCellData = OutputCellDataContainer::New();
  const InputCellDataContainer *inputCellData = inputMesh->GetCellData();

  if ( inputCellData )
    {
    outputCellData->Reserve( inputCellData->Size() );

    typename InputCellDataContainer::ConstIterator inputItr = inputCellData->Begin();
    typename InputCellDataContainer::ConstIterator inputEnd = inputCellData->End();

    typename OutputCellDataContainer::Iterator outputItr = outputCellData->Begin();

    while ( inputItr != inputEnd )
      {
      outputItr.Value() = inputItr.Value();
      ++inputItr;
      ++outputItr;
      }

    outputMesh->SetCellData(outputCellData);
    }
}
} // end namespace itk

#endif
