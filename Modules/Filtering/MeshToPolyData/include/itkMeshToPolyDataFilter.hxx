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
#ifndef itkMeshToPolyDataFilter_hxx
#define itkMeshToPolyDataFilter_hxx

#include "itkMeshToPolyDataFilter.h"

namespace itk
{

template< typename TInputMesh >
MeshToPolyDataFilter< TInputMesh >
::MeshToPolyDataFilter()
{
  // Modify superclass default values, can be overridden by subclasses
  this->SetNumberOfRequiredInputs(1);

  typename PolyDataType::Pointer output =
    static_cast< PolyDataType * >( this->MakeOutput(0).GetPointer() );
  this->ProcessObject::SetNumberOfRequiredOutputs(1);
  this->ProcessObject::SetNthOutput( 0, output.GetPointer() );
}


template< typename TInputMesh >
void
MeshToPolyDataFilter< TInputMesh >
::PrintSelf( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf( os, indent );
}


template< typename TInputMesh >
void
MeshToPolyDataFilter< TInputMesh >
::SetInput(const TInputMesh *input)
{
  // Process object is not const-correct so the const_cast is required here
  this->ProcessObject::SetNthInput( 0, const_cast< TInputMesh * >( input ) );
}


template< typename TInputMesh >
const typename MeshToPolyDataFilter< TInputMesh >::InputMeshType *
MeshToPolyDataFilter< TInputMesh >
::GetInput() const
{
  return itkDynamicCastInDebugMode< const TInputMesh * >( this->GetPrimaryInput() );
}


template< typename TInputMesh >
const typename MeshToPolyDataFilter< TInputMesh >::InputMeshType *
MeshToPolyDataFilter< TInputMesh >
::GetInput(unsigned int idx) const
{
  return dynamic_cast< const TInputMesh * > ( this->ProcessObject::GetInput(idx) );
}


template< typename TInputMesh >
ProcessObject::DataObjectPointer
MeshToPolyDataFilter< TInputMesh >
::MakeOutput(ProcessObject::DataObjectPointerArraySizeType)
{
  return PolyDataType::New().GetPointer();
}


template< typename TInputMesh >
ProcessObject::DataObjectPointer
MeshToPolyDataFilter< TInputMesh >
::MakeOutput(const ProcessObject::DataObjectIdentifierType &)
{
  return PolyDataType::New().GetPointer();
}


template< typename TInputMesh >
typename MeshToPolyDataFilter< TInputMesh >::PolyDataType *
MeshToPolyDataFilter< TInputMesh >
::GetOutput()
{
  // we assume that the first output is of the templated type
  return itkDynamicCastInDebugMode< PolyDataType * >( this->GetPrimaryOutput() );
}


template< typename TInputMesh >
const typename MeshToPolyDataFilter< TInputMesh >::PolyDataType *
MeshToPolyDataFilter< TInputMesh >
::GetOutput() const
{
  // we assume that the first output is of the templated type
  return itkDynamicCastInDebugMode< const PolyDataType * >( this->GetPrimaryOutput() );
}


template< typename TInputMesh >
typename MeshToPolyDataFilter< TInputMesh >::PolyDataType *
MeshToPolyDataFilter< TInputMesh >
::GetOutput(unsigned int idx)
{
  auto * out = dynamic_cast< PolyDataType * > ( this->ProcessObject::GetOutput(idx) );

  if ( out == nullptr && this->ProcessObject::GetOutput(idx) != nullptr )
    {
    itkWarningMacro (<< "Unable to convert output number " << idx << " to type " <<  typeid( PolyDataType ).name () );
    }
  return out;
}


template< typename TInputMesh >
void
MeshToPolyDataFilter< TInputMesh >
::GenerateData()
{
  const InputMeshType * inputMesh = this->GetInput();
  PolyDataType * outputPolyData = this->GetOutput();

  using PointsContainerType = typename PolyDataType::PointsContainer;
  const PointsContainerType * inputPoints = inputMesh->GetPoints();
  typename PointsContainerType::Pointer outputPoints = PointsContainerType::New();
  outputPoints->Reserve( inputPoints->Size() );

  typename PointsContainerType::ConstIterator inputPointItr = inputPoints->Begin();
  typename PointsContainerType::ConstIterator inputPointEnd = inputPoints->End();

  typename PointsContainerType::Iterator outputPointItr = outputPoints->Begin();

  while ( inputPointItr != inputPointEnd )
    {
    outputPointItr.Value() = inputPointItr.Value();
    ++inputPointItr;
    ++outputPointItr;
    }
  outputPolyData->SetPoints( outputPoints );

  using PointDataContainerType = typename PolyDataType::PointDataContainer;
  const PointDataContainerType * inputPointData = inputMesh->GetPointData();
  if( inputPointData )
    {
    typename PointDataContainerType::Pointer outputPointData = PointDataContainerType::New();
    outputPointData->Reserve( inputPointData->Size() );

    typename PointDataContainerType::ConstIterator inputPointDataItr = inputPointData->Begin();
    typename PointDataContainerType::ConstIterator inputPointDataEnd = inputPointData->End();

    typename PointDataContainerType::Iterator outputPointDataItr = outputPointData->Begin();

    while( inputPointDataItr != inputPointDataEnd )
      {
      outputPointDataItr.Value() = inputPointDataItr.Value();
      ++inputPointDataItr;
      ++outputPointDataItr;
      }
    outputPolyData->SetPointData( outputPointData );
    }


  //vtkUnstructuredGrid *input=static_cast<vtkUnstructuredGrid *>(dataSetInput);
  //vtkCellArray *connectivity = input->GetCells();
  //if (connectivity == nullptr)
  //{
    //return;
  //}
  //IdentifierType numberOfPoints = 0;
  //vtkIdType *pts = nullptr;
  //vtkPoints *p = input->GetPoints();
  //vtkIdType numCells=input->GetNumberOfCells();
  //vtkPointData *pd = input->GetPointData();
  //vtkCellData *cellData = input->GetCellData();
  //vtkPointData *outputPD = output->GetPointData();
  //vtkCellData *outputCellData = output->GetCellData();
  //vtkCellArray *verts, *lines, *polys, *strips;
  //vtkIdList *cellIds, *faceIds;
  //int faceId, *faceVerts, numFacePts;
  //double x[3];

  //// Check input
  //if ( connectivity == nullptr )
  //{
    //vtkDebugMacro(<<"Nothing to extract");
    //return;
  //}

  //// Determine nature of what we have to do
  //cellIds = vtkIdList::New();
  //faceIds = vtkIdList::New();

  //// Just pass points through, never merge
  //output->SetPoints(input->GetPoints());
  //outputPD->PassData(pd);

  //outputCellData->CopyGlobalIdsOn();
  //outputCellData->CopyAllocate(cellData,numCells,numCells/2);

  //verts = vtkCellArray::New();
  //verts->Allocate(numCells/4+1,numCells);
  //lines = vtkCellArray::New();
  //lines->Allocate(numCells/4+1,numCells);
  //polys = vtkCellArray::New();
  //polys->Allocate(numCells/4+1,numCells);
  //strips = vtkCellArray::New();
  //strips->Allocate(numCells/4+1,numCells);

  //// Used for nonlinear cells only
  //vtkNew<vtkGenericCell> cell;
  //vtkNew<vtkIdList> ipts;
  //vtkNew<vtkPoints> coords;
  //vtkNew<vtkIdList> icellIds;

  //// These store the cell ids of the input that map to the
  //// new vert/line/poly/strip cells, for copying cell data
  //// in appropriate order.
  //std::vector< vtkIdType > vertCellIds;
  //std::vector< vtkIdType > lineCellIds;
  //std::vector< vtkIdType > polyCellIds;
  //std::vector< vtkIdType > stripCellIds;
  //vertCellIds.reserve( numCells );
  //lineCellIds.reserve( numCells );
  //polyCellIds.reserve( numCells );
  //stripCellIds.reserve( numCells );

  //// Loop over all cells now that visibility is known
  //// (Have to compute visibility first for 3D cell boundaries)
  //int progressInterval = numCells/20 + 1;
  //for (IdentifierType cellId = 0, connectivity->InitTraversal();
       //connectivity->GetNextCell(numberOfPoints,pts);
       //++cellId)
  //{
    ////Progress and abort method support
    //if ( !(cellId % progressInterval) )
    //{
      //vtkDebugMacro(<<"Process cell #" << cellId);
      //this->UpdateProgress(static_cast<double>(cellId)/numCells);
    //}

    ////special code for nonlinear cells - rarely occurs, so right now it
    ////is slow.
    //switch (input->GetCellType(cellId))
    //{
      //case VTK_EMPTY_CELL:
        //break;

      //case VTK_VERTEX:
      //case VTK_POLY_VERTEX:
        //verts->InsertNextCell(numberOfPoints,pts);
        //vertCellIds.push_back(cellId);
        //break;

      //case VTK_LINE:
      //case VTK_POLY_LINE:
        //lines->InsertNextCell(numberOfPoints,pts);
        //lineCellIds.push_back(cellId);
        //break;

      //case VTK_TRIANGLE:
      //case VTK_QUAD:
      //case VTK_POLYGON:
        //polys->InsertNextCell(numberOfPoints,pts);
        //polyCellIds.push_back(cellId);
        //break;

      //case VTK_TRIANGLE_STRIP:
        //strips->InsertNextCell(numberOfPoints,pts);
        //stripCellIds.push_back(cellId);
        //break;

      //case VTK_TETRA:
        //for (faceId = 0; faceId < 4; faceId++)
        //{
          //faceIds->Reset();
          //faceVerts = vtkTetra::GetFaceArray(faceId);
          //faceIds->InsertNextId(pts[faceVerts[0]]);
          //faceIds->InsertNextId(pts[faceVerts[1]]);
          //faceIds->InsertNextId(pts[faceVerts[2]]);
          //numFacePts = 3;
          //input->GetCellNeighbors(cellId, faceIds, cellIds);
          //if ( cellIds->GetNumberOfIds() <= 0 )
          //{
            //polys->InsertNextCell(numFacePts);
            //for ( int i=0; i < numFacePts; i++)
            //{
              //polys->InsertCellPoint(pts[faceVerts[i]]);
            //}
            //polyCellIds.push_back(cellId);
          //}
        //}
        //break;

      //case VTK_HEXAHEDRON:
        //for (faceId = 0; faceId < 6; faceId++)
        //{
          //faceIds->Reset();
          //faceVerts = vtkHexahedron::GetFaceArray(faceId);
          //faceIds->InsertNextId(pts[faceVerts[0]]);
          //faceIds->InsertNextId(pts[faceVerts[1]]);
          //faceIds->InsertNextId(pts[faceVerts[2]]);
          //faceIds->InsertNextId(pts[faceVerts[3]]);
          //numFacePts = 4;
          //input->GetCellNeighbors(cellId, faceIds, cellIds);
          //if ( cellIds->GetNumberOfIds() <= 0 )
          //{
            //polys->InsertNextCell(numFacePts);
            //for ( int i=0; i < numFacePts; i++)
            //{
              //polys->InsertCellPoint(pts[faceVerts[i]]);
            //}
            //polyCellIds.push_back(cellId);
          //}
        //}
        //break;

      //case VTK_WEDGE:
        //for (faceId = 0; faceId < 5; faceId++)
        //{
          //faceIds->Reset();
          //faceVerts = vtkWedge::GetFaceArray(faceId);
          //faceIds->InsertNextId(pts[faceVerts[0]]);
          //faceIds->InsertNextId(pts[faceVerts[1]]);
          //faceIds->InsertNextId(pts[faceVerts[2]]);
          //numFacePts = 3;
          //if (faceVerts[3] >= 0)
          //{
            //faceIds->InsertNextId(pts[faceVerts[3]]);
            //numFacePts = 4;
          //}
          //input->GetCellNeighbors(cellId, faceIds, cellIds);
          //if ( cellIds->GetNumberOfIds() <= 0 )
          //{
            //polys->InsertNextCell(numFacePts);
            //for ( int i=0; i < numFacePts; i++)
            //{
              //polys->InsertCellPoint(pts[faceVerts[i]]);
            //}
            //polyCellIds.push_back(cellId);
          //}
        //}
        //break;

      //case VTK_PYRAMID:
        //for (faceId = 0; faceId < 5; faceId++)
        //{
          //faceIds->Reset();
          //faceVerts = vtkPyramid::GetFaceArray(faceId);
          //faceIds->InsertNextId(pts[faceVerts[0]]);
          //faceIds->InsertNextId(pts[faceVerts[1]]);
          //faceIds->InsertNextId(pts[faceVerts[2]]);
          //numFacePts = 3;
          //if (faceVerts[3] >= 0)
          //{
            //faceIds->InsertNextId(pts[faceVerts[3]]);
            //numFacePts = 4;
          //}
          //input->GetCellNeighbors(cellId, faceIds, cellIds);
          //if ( cellIds->GetNumberOfIds() <= 0 )
          //{
            //polys->InsertNextCell(numFacePts);
            //for ( int i=0; i < numFacePts; i++)
            //{
              //polys->InsertCellPoint(pts[faceVerts[i]]);
            //}
            //polyCellIds.push_back(cellId);
          //}
        //}
        //break;

      //case VTK_PENTAGONAL_PRISM:
        //for (faceId = 0; faceId < 7; faceId++)
        //{
          //faceIds->Reset();
          //faceVerts = vtkPentagonalPrism::GetFaceArray(faceId);
          //faceIds->InsertNextId(pts[faceVerts[0]]);
          //faceIds->InsertNextId(pts[faceVerts[1]]);
          //faceIds->InsertNextId(pts[faceVerts[2]]);
          //faceIds->InsertNextId(pts[faceVerts[3]]);
          //numFacePts = 4;
          //if (faceVerts[4] >= 0)
          //{
            //faceIds->InsertNextId(pts[faceVerts[4]]);
            //numFacePts = 5;
          //}
          //input->GetCellNeighbors(cellId, faceIds, cellIds);
          //if ( cellIds->GetNumberOfIds() <= 0 )
          //{
            //polys->InsertNextCell(numFacePts);
            //for ( int i=0; i < numFacePts; i++)
            //{
              //polys->InsertCellPoint(pts[faceVerts[i]]);
            //}
            //polyCellIds.push_back(cellId);
          //}
        //}
        //break;

      //case VTK_HEXAGONAL_PRISM:
        //for (faceId = 0; faceId < 8; faceId++)
        //{
          //faceIds->Reset();
          //faceVerts = vtkHexagonalPrism::GetFaceArray(faceId);
          //faceIds->InsertNextId(pts[faceVerts[0]]);
          //faceIds->InsertNextId(pts[faceVerts[1]]);
          //faceIds->InsertNextId(pts[faceVerts[2]]);
          //faceIds->InsertNextId(pts[faceVerts[3]]);
          //numFacePts = 4;
          //if (faceVerts[4] >= 0)
          //{
            //faceIds->InsertNextId(pts[faceVerts[4]]);
            //faceIds->InsertNextId(pts[faceVerts[5]]);
            //numFacePts = 6;
          //}
          //input->GetCellNeighbors(cellId, faceIds, cellIds);
          //if ( cellIds->GetNumberOfIds() <= 0 )
          //{
            //polys->InsertNextCell(numFacePts);
            //for ( int i=0; i < numFacePts; i++)
            //{
              //polys->InsertCellPoint(pts[faceVerts[i]]);
            //}
            //polyCellIds.push_back(cellId);
          //}
        //}
        //break;

      ////Quadratic cells
      //case VTK_QUADRATIC_EDGE:
      //case VTK_CUBIC_LINE:
      //case VTK_QUADRATIC_TRIANGLE:
      //case VTK_QUADRATIC_QUAD:
      //case VTK_QUADRATIC_POLYGON:
      //case VTK_QUADRATIC_TETRA:
      //case VTK_QUADRATIC_HEXAHEDRON:
      //case VTK_QUADRATIC_WEDGE:
      //case VTK_QUADRATIC_PYRAMID:
      //case VTK_QUADRATIC_LINEAR_QUAD:
      //case VTK_BIQUADRATIC_TRIANGLE:
      //case VTK_BIQUADRATIC_QUAD:
      //case VTK_TRIQUADRATIC_HEXAHEDRON:
      //case VTK_QUADRATIC_LINEAR_WEDGE:
      //case VTK_BIQUADRATIC_QUADRATIC_WEDGE:
      //case VTK_BIQUADRATIC_QUADRATIC_HEXAHEDRON:
      //{
        //input->GetCell(cellId,cell);

        //if ( cell->GetCellDimension() == 1 )
        //{
          //cell->Triangulate(0,ipts,coords);
          //for ( int i=0; i < ipts->GetNumberOfIds(); i+=2)
          //{
            //lines->InsertNextCell(2);
            //lines->InsertCellPoint(ipts->GetId(i));
            //lines->InsertCellPoint(ipts->GetId(i+1));
            //lineCellIds.push_back(cellId);
          //}
        //}
        //else if ( cell->GetCellDimension() == 2 )
        //{
          //cell->Triangulate(0,ipts,coords);
          //for ( int i=0; i < ipts->GetNumberOfIds(); i+=3)
          //{
            //polys->InsertNextCell(3);
            //polys->InsertCellPoint(ipts->GetId(i));
            //polys->InsertCellPoint(ipts->GetId(i+1));
            //polys->InsertCellPoint(ipts->GetId(i+2));
            //polyCellIds.push_back(cellId);
          //}
        //}
        //else //3D nonlinear cell
        //{
          //vtkCell *face;
          //for (int j=0; j < cell->GetNumberOfFaces(); j++)
          //{
            //face = cell->GetFace(j);
            //input->GetCellNeighbors(cellId, face->PointIds, icellIds);
            //if ( icellIds->GetNumberOfIds() <= 0)
            //{
              //face->Triangulate(0,ipts,coords);
              //for ( int i=0; i < ipts->GetNumberOfIds(); i+=3)
              //{
                //polys->InsertNextCell(3);
                //polys->InsertCellPoint(ipts->GetId(i));
                //polys->InsertCellPoint(ipts->GetId(i+1));
                //polys->InsertCellPoint(ipts->GetId(i+2));
                //polyCellIds.push_back(cellId);
              //}
            //}
          //}
        //} //3d cell
      //}
        //break; //done with quadratic cells
    //} //switch
  //} //for all cells

  //// Update ourselves and release memory
  ////
  //output->SetVerts(verts);
  //verts->Delete();
  //output->SetLines(lines);
  //lines->Delete();
  //output->SetPolys(polys);
  //polys->Delete();
  //output->SetStrips(strips);
  //strips->Delete();

  //// Copy the cell data in appropriate order : verts / lines / polys / strips
  //size_t offset = 0;
  //size_t size = vertCellIds.size();
  //for ( size_t i = 0; i < size; ++i )
  //{
    //outputCellData->CopyData(cellData, vertCellIds[i], static_cast<vtkIdType>(i) );
  //}
  //offset += size;
  //size = lineCellIds.size();
  //for ( size_t i = 0; i < size; ++i )
  //{
    //outputCellData->CopyData(cellData, lineCellIds[i], static_cast<vtkIdType>(i+offset) );
  //}
  //offset += size;
  //size = polyCellIds.size();
  //for ( size_t i = 0; i < size; ++i )
  //{
    //outputCellData->CopyData(cellData, polyCellIds[i], static_cast<vtkIdType>(i+offset) );
  //}
  //offset += size;
  //size = stripCellIds.size();
  //for ( size_t i = 0; i < size; ++i )
  //{
    //outputCellData->CopyData(cellData, stripCellIds[i], static_cast<vtkIdType>(i+offset) );
  //}

  //output->Squeeze();

  //vtkDebugMacro(<<"Extracted " << input->GetNumberOfPoints() << " points,"
    //<< output->GetNumberOfCells() << " cells.");

  //cellIds->Delete();
  //faceIds->Delete();
}

} // end namespace itk

#endif // itkMeshToPolyDataFilter_hxx
