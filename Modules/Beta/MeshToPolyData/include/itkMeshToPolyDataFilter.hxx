/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
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

#include "itkVertexCell.h"
#include "itkLineCell.h"
#include "itkPolyLineCell.h"
#include "itkMesh.h"
#include "itkTriangleCell.h"
#include "itkQuadrilateralCell.h"
#include "itkPolygonCell.h"
#include "itkTetrahedronCell.h"
#include "itkHexahedronCell.h"

namespace
{

template< typename TMesh, typename TPolyData >
class VisitCellsClass
{
public:
  using MeshType = TMesh;
  using PolyDataType = TPolyData;
  // typedef the itk cells we are interested in
  using CellInterfaceType = itk::CellInterface< typename MeshType::PixelType, typename MeshType::CellTraits >;

  using CellsContainerType = typename TPolyData::CellsContainer;

  using VertexCellType = itk::VertexCell<CellInterfaceType>;
  using LineCellType = itk::LineCell<CellInterfaceType>;
  using PolyLineCellType = itk::PolyLineCell<CellInterfaceType>;
  using TriangleCellType = itk::TriangleCell<CellInterfaceType>;
  using QuadrilateralCellType = itk::QuadrilateralCell<CellInterfaceType>;
  using PolygonCellType = itk::PolygonCell<CellInterfaceType>;
  using TetrahedronCellType = itk::TetrahedronCell<CellInterfaceType>;
  using HexahedronCellType = itk::HexahedronCell<CellInterfaceType>;

  // Set the output polydata vertices container
  void SetVertices(CellsContainerType* vertices)
  {
    m_Vertices = vertices;
  }

  // Set the output polydata polygons container
  void SetLines(CellsContainerType* lines)
  {
    m_Lines = lines;
  }

  // Set the output polydata polygons container
  void SetPolygons(CellsContainerType* polygons)
  {
    m_Polygons = polygons;
  }

  // Set the output polydata triangleStrips container
  void SetTriangleStrips(CellsContainerType* triangleStrips)
  {
    m_TriangleStrips = triangleStrips;
  }

  // Set the associated input cell id container
  void SetVerticesCellIds(CellsContainerType* cellIds)
  {
    m_VerticesCellIds = cellIds;
  }

  // Set the associated input cell id container
  void SetLinesCellIds(CellsContainerType* cellIds)
  {
    m_LinesCellIds = cellIds;
  }

  // Set the associated input cell id container
  void SetPolygonsCellIds(CellsContainerType* cellIds)
  {
    m_PolygonsCellIds = cellIds;
  }

  // Set the associated input cell id container
  void SetTriangleStripsCellIds(CellsContainerType* cellIds)
  {
    m_TriangleStripsCellIds = cellIds;
  }

  // Visit a vertex and create a vertex in the output
  void Visit(unsigned long cellId, VertexCellType* cell)
  {
    constexpr unsigned int numberOfPoints = VertexCellType::NumberOfPoints;
    m_Vertices->push_back( numberOfPoints );
    m_Vertices->push_back( cell->GetPointId() );
    m_VerticesCellIds->push_back( static_cast< unsigned int >( cellId ) );
  }

  // Visit a line and create a line in the output
  void Visit(unsigned long cellId, LineCellType* cell)
  {
    constexpr unsigned int numberOfPoints = LineCellType::NumberOfPoints;
    m_Lines->push_back( numberOfPoints );
    const typename LineCellType::PointIdConstIterator pointIdEnd = cell->PointIdsEnd();
    for( typename LineCellType::PointIdConstIterator pointIdIt = cell->PointIdsBegin(); pointIdIt != pointIdEnd; ++pointIdIt )
    {
      m_Lines->push_back( *pointIdIt );
    }
    m_LinesCellIds->push_back( static_cast< unsigned int >( cellId ) );
  }

  // Visit a polyline and create a polyline in the output
  void Visit(unsigned long cellId, PolyLineCellType* cell)
  {
    int numberOfPoints = cell->GetNumberOfPoints();
    m_Lines->push_back( numberOfPoints );
    const typename PolyLineCellType::PointIdConstIterator pointIdEnd = cell->PointIdsEnd();
    for( typename PolyLineCellType::PointIdConstIterator pointIdIt = cell->PointIdsBegin(); pointIdIt != pointIdEnd; ++pointIdIt )
    {
      m_Lines->push_back( *pointIdIt );
    }
    m_LinesCellIds->push_back( static_cast< unsigned int >( cellId ) );
  }

  // Visit a triangle and create a triangle in the output
  void Visit(unsigned long cellId, TriangleCellType* cell)
  {
    constexpr unsigned int numberOfPoints = TriangleCellType::NumberOfPoints;
    m_Polygons->push_back( numberOfPoints );
    const typename TriangleCellType::PointIdConstIterator pointIdEnd = cell->PointIdsEnd();
    for( typename TriangleCellType::PointIdConstIterator pointIdIt = cell->PointIdsBegin(); pointIdIt != pointIdEnd; ++pointIdIt )
      {
      m_Polygons->push_back( *pointIdIt );
      }
    m_PolygonsCellIds->push_back( static_cast< unsigned int >( cellId ) );
  }

  // Visit a quadrilateral and create a quadrilateral in the output
  void Visit(unsigned long cellId, QuadrilateralCellType* cell)
  {
    constexpr unsigned int numberOfPoints = QuadrilateralCellType::NumberOfPoints;
    m_Polygons->push_back( numberOfPoints );
    const typename QuadrilateralCellType::PointIdConstIterator pointIdEnd = cell->PointIdsEnd();
    for( typename QuadrilateralCellType::PointIdConstIterator pointIdIt = cell->PointIdsBegin(); pointIdIt != pointIdEnd; ++pointIdIt )
      {
      m_Polygons->push_back( *pointIdIt );
      }
    m_PolygonsCellIds->push_back( static_cast< unsigned int >( cellId ) );
  }

  // Visit a polygon and create a polygon in the output
  void Visit(unsigned long cellId, PolygonCellType* cell)
  {
    const unsigned int numberOfPoints = cell->GetNumberOfPoints();
    m_Polygons->push_back( numberOfPoints );
    const typename PolygonCellType::PointIdConstIterator pointIdEnd = cell->PointIdsEnd();
    for( typename PolygonCellType::PointIdConstIterator pointIdIt = cell->PointIdsBegin(); pointIdIt != pointIdEnd; ++pointIdIt )
      {
      m_Polygons->push_back( *pointIdIt );
      }
    m_PolygonsCellIds->push_back( static_cast< unsigned int >( cellId ) );
  }

  //// Visit a tetrahedron and create a tetrahedron in the output
  //void Visit(unsigned long, TetrahedronCellType* cell)
  //{
    //constexpr unsigned int numberOfPoints = TetrahedronCellType::NumberOfPoints;
    //constexpr unsigned int numberOfFaces = TetrahedronCellType::NumberOfFaces;
    //constexpr unsigned int numberOfEdges = TetrahedronCellType::NumberOfFaces;
    //for( unsigned int faceId = 0; faceId < numberOfFaces; ++faceId )
      //{
      //typename TetrahedronCellType::FaceAutoPointer facePointer;
      //cell->GetFace( faceId, facePointer );
      //// TODO
      ////faceIds = vtkIdList::New();
      ////case VTK_TETRA:
        ////for (faceId = 0; faceId < 4; faceId++)
        ////{
          ////faceIds->Reset();
          ////faceVerts = vtkTetra::GetFaceArray(faceId);
          ////faceIds->InsertNextId(pts[faceVerts[0]]);
          ////faceIds->InsertNextId(pts[faceVerts[1]]);
          ////faceIds->InsertNextId(pts[faceVerts[2]]);
          ////numFacePts = 3;
          ////input->GetCellNeighbors(cellId, faceIds, cellIds);
          ////if ( cellIds->GetNumberOfIds() <= 0 )
          ////{
            ////polys->InsertNextCell(numFacePts);
            ////for ( int i=0; i < numFacePts; i++)
            ////{
              ////polys->InsertCellPoint(pts[faceVerts[i]]);
            ////}
            ////polyCellIds.push_back(cellId);
          ////}
        ////}
        ////break;
      //}
  //}

  //// Visit a hexahedron and create a hexahedron in the output
  //void Visit(unsigned long, HexahedronCellType* cell)
  //{
      //// TODO
      ////case VTK_HEXAHEDRON:
        ////for (faceId = 0; faceId < 6; faceId++)
        ////{
          ////faceIds->Reset();
          ////faceVerts = vtkHexahedron::GetFaceArray(faceId);
          ////faceIds->InsertNextId(pts[faceVerts[0]]);
          ////faceIds->InsertNextId(pts[faceVerts[1]]);
          ////faceIds->InsertNextId(pts[faceVerts[2]]);
          ////faceIds->InsertNextId(pts[faceVerts[3]]);
          ////numFacePts = 4;
          ////input->GetCellNeighbors(cellId, faceIds, cellIds);
          ////if ( cellIds->GetNumberOfIds() <= 0 )
          ////{
            ////polys->InsertNextCell(numFacePts);
            ////for ( int i=0; i < numFacePts; i++)
            ////{
              ////polys->InsertCellPoint(pts[faceVerts[i]]);
            ////}
            ////polyCellIds.push_back(cellId);
          ////}
        ////}
        ////break;
  //}

private:
  CellsContainerType * m_Vertices;
  CellsContainerType * m_Lines;
  CellsContainerType * m_Polygons;
  CellsContainerType * m_TriangleStrips;

  CellsContainerType * m_VerticesCellIds;
  CellsContainerType * m_LinesCellIds;
  CellsContainerType * m_PolygonsCellIds;
  CellsContainerType * m_TriangleStripsCellIds;
};

} // end anonymous namespace

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

  using MeshPointsContainerType = typename InputMeshType::PointsContainer;
  using PolyDataPointsContainerType = typename PolyDataType::PointsContainer;
  const MeshPointsContainerType * inputPoints = inputMesh->GetPoints();
  typename PolyDataPointsContainerType::Pointer outputPoints = PolyDataPointsContainerType::New();
  outputPoints->resize( inputPoints->Size() );
  // 2D Mesh -> 3D PolyData, third dimension point locations defaults to 0.0.
  typename PolyDataType::PointType nullPoint{ 0.0f };
  outputPoints->assign( inputPoints->Size(), nullPoint );

  typename MeshPointsContainerType::ConstIterator inputPointItr = inputPoints->Begin();
  typename MeshPointsContainerType::ConstIterator inputPointEnd = inputPoints->End();

  typename PolyDataPointsContainerType::Iterator outputPointItr = outputPoints->Begin();
  while ( inputPointItr != inputPointEnd )
    {
    for( unsigned int ii = 0; ii < InputMeshType::PointDimension; ++ii )
      {
      outputPointItr.Value()[ii] = inputPointItr.Value()[ii];
      }
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

  GenerateDataDispatch< TInputMesh >();
}


template< typename TInputMesh >
template < typename TInputMeshDispatch, typename std::enable_if< !HasCellTraits<TInputMeshDispatch>::value, int>::type >
void
MeshToPolyDataFilter< TInputMesh >
::GenerateDataDispatch()
{
  // Nothing else to do
}


template< typename TInputMesh >
template < typename TInputMeshDispatch, typename std::enable_if< HasCellTraits<TInputMeshDispatch>::value, int>::type >
void
MeshToPolyDataFilter< TInputMesh >
::GenerateDataDispatch()
{
  // Also propagate cells and cell data

  const InputMeshType * inputMesh = this->GetInput();
  PolyDataType * outputPolyData = this->GetOutput();

  const IdentifierType numberOfCells = inputMesh->GetNumberOfCells();

  using CellsContainerType = typename PolyDataType::CellsContainer;
  typename CellsContainerType::Pointer vertices = CellsContainerType::New();
  vertices->reserve( numberOfCells / 4 + 1 );
  typename CellsContainerType::Pointer lines = CellsContainerType::New();
  lines->reserve( numberOfCells / 4 + 1 );
  typename CellsContainerType::Pointer polylines = CellsContainerType::New();
  polylines->reserve( numberOfCells / 4 + 1 );
  typename CellsContainerType::Pointer polygons = CellsContainerType::New();
  polygons->reserve( numberOfCells / 4 + 1 );
  //typename CellsContainerType::Pointer triangleStrips = CellsContainerType::New();
  //triangleStrips->reserve( numberOfCells / 4 + 1 );

  // These store the cell ids of the input that map to the
  // new vert/line/poly/strip cells, for copying cell data
  // in appropriate order.
  typename CellsContainerType::Pointer verticesCellIds = CellsContainerType::New();
  verticesCellIds->Reserve( numberOfCells / 4 + 1 );
  typename CellsContainerType::Pointer linesCellIds = CellsContainerType::New();
  linesCellIds->Reserve( numberOfCells / 4 + 1 );
  typename CellsContainerType::Pointer polygonsCellIds = CellsContainerType::New();
  polygonsCellIds->Reserve( numberOfCells / 4 + 1 );
  //typename CellsContainerType::Pointer triangleStripsCellIds = CellsContainerType::New();
  //triangleStripsCellIds->Reserve( numberOfCells / 4 + 1 );

  using CellTraits = typename InputMeshType::CellTraits;
  using PixelType = typename InputMeshType::PixelType;

  // Setup the vertex visitor
  typedef CellInterfaceVisitorImplementation<
    PixelType, CellTraits,
    VertexCell< CellInterface< PixelType, CellTraits > >,
    VisitCellsClass< InputMeshType, PolyDataType > > VertexVisitor;
  typename VertexVisitor::Pointer vertexVisitor = VertexVisitor::New();
  vertexVisitor->SetVertices( vertices );
  vertexVisitor->SetLines( lines );
  vertexVisitor->SetPolygons( polygons );
  //vertexVisitor->SetTriangleStrips( triangleStrips );
  vertexVisitor->SetVerticesCellIds( verticesCellIds );
  vertexVisitor->SetLinesCellIds( linesCellIds );
  vertexVisitor->SetPolygonsCellIds( polygonsCellIds );
  //vertexVisitor->SetTriangleStripsCellIds( triangleStripsCellIds );

  // Setup the poly line visitor
  typedef CellInterfaceVisitorImplementation<
    PixelType, CellTraits,
    PolyLineCell< CellInterface< PixelType, CellTraits > >,
    VisitCellsClass< InputMeshType, PolyDataType > > PolyLineVisitor;
  typename PolyLineVisitor::Pointer polyLineVisitor = PolyLineVisitor::New();
  polyLineVisitor->SetVertices( vertices );
  polyLineVisitor->SetLines( polylines );
  polyLineVisitor->SetPolygons( polygons );
  //lineVisitor->SetTriangleStrips( triangleStrips );
  polyLineVisitor->SetVerticesCellIds( verticesCellIds );
  polyLineVisitor->SetLinesCellIds( linesCellIds );
  polyLineVisitor->SetPolygonsCellIds( polygonsCellIds );
  //lineVisitor->SetTriangleStripsCellIds( triangleStripsCellIds );

  // Setup the line visitor
  typedef CellInterfaceVisitorImplementation<
    PixelType, CellTraits,
    LineCell< CellInterface< PixelType, CellTraits > >,
    VisitCellsClass< InputMeshType, PolyDataType > > LineVisitor;
  typename LineVisitor::Pointer lineVisitor = LineVisitor::New();
  lineVisitor->SetVertices( vertices );
  lineVisitor->SetLines( lines );
  lineVisitor->SetPolygons( polygons );
  //lineVisitor->SetTriangleStrips( triangleStrips );
  lineVisitor->SetVerticesCellIds( verticesCellIds );
  lineVisitor->SetLinesCellIds( linesCellIds );
  lineVisitor->SetPolygonsCellIds( polygonsCellIds );
  //lineVisitor->SetTriangleStripsCellIds( triangleStripsCellIds );

  // Setup the triangle visitor
  typedef CellInterfaceVisitorImplementation<
    PixelType, CellTraits,
    TriangleCell< CellInterface< PixelType, CellTraits > >,
    VisitCellsClass< InputMeshType, PolyDataType > > TriangleVisitor;
  typename TriangleVisitor::Pointer triangleVisitor = TriangleVisitor::New();
  triangleVisitor->SetVertices( vertices );
  triangleVisitor->SetLines( lines );
  triangleVisitor->SetPolygons( polygons );
  //triangleVisitor->SetTriangleStrips( triangleStrips );
  triangleVisitor->SetVerticesCellIds( verticesCellIds );
  triangleVisitor->SetLinesCellIds( linesCellIds );
  triangleVisitor->SetPolygonsCellIds( polygonsCellIds );
  //triangleVisitor->SetTriangleStripsCellIds( triangleStripsCellIds );

  // Setup the quadrilateral visitor
  typedef CellInterfaceVisitorImplementation<
    PixelType, CellTraits,
    QuadrilateralCell< CellInterface< PixelType, CellTraits > >,
    VisitCellsClass< InputMeshType, PolyDataType > > QuadrilateralVisitor;
  typename QuadrilateralVisitor::Pointer quadrilateralVisitor = QuadrilateralVisitor::New();
  quadrilateralVisitor->SetVertices( vertices );
  quadrilateralVisitor->SetLines( lines );
  quadrilateralVisitor->SetPolygons( polygons );
  //quadrilateralVisitor->SetTriangleStrips( triangleStrips );
  quadrilateralVisitor->SetVerticesCellIds( verticesCellIds );
  quadrilateralVisitor->SetLinesCellIds( linesCellIds );
  quadrilateralVisitor->SetPolygonsCellIds( polygonsCellIds );
  //quadrilateralVisitor->SetTriangleStripsCellIds( triangleStripsCellIds );

  // Setup the polygon visitor
  typedef CellInterfaceVisitorImplementation<
    PixelType, CellTraits,
    PolygonCell< CellInterface< PixelType, CellTraits > >,
    VisitCellsClass< InputMeshType, PolyDataType > > PolygonVisitor;
  typename PolygonVisitor::Pointer polygonVisitor = PolygonVisitor::New();
  polygonVisitor->SetVertices( vertices );
  polygonVisitor->SetLines( lines );
  polygonVisitor->SetPolygons( polygons );
  //polygonVisitor->SetTriangleStrips( triangleStrips );
  polygonVisitor->SetVerticesCellIds( verticesCellIds );
  polygonVisitor->SetLinesCellIds( linesCellIds );
  polygonVisitor->SetPolygonsCellIds( polygonsCellIds );
  //polygonVisitor->SetTriangleStripsCellIds( triangleStripsCellIds );

  // TODO
  // Setup the tetrahedron visitor
  //typedef CellInterfaceVisitorImplementation<
    //PixelType, CellTraits,
    //TetrahedronCell< CellInterface< PixelType, CellTraits > >,
    //VisitCellsClass< InputMeshType, PolyDataType > > TetrahedronVisitor;
  //typename TetrahedronVisitor::Pointer tetrahedronVisitor = TetrahedronVisitor::New();
  //tetrahedronVisitor->SetVertices( vertices );
  //tetrahedronVisitor->SetLines( lines );
  //tetrahedronVisitor->SetPolygons( polygons );
  //tetrahedronVisitor->SetTriangleStrips( triangleStrips );
  //tetrahedronVisitor->SetVerticesCellIds( verticesCellIds );
  //tetrahedronVisitor->SetLinesCellIds( linesCellIds );
  //tetrahedronVisitor->SettetrahedronsCellIds( tetrahedronsCellIds );
  //tetrahedronVisitor->SetTriangleStripsCellIds( triangleStripsCellIds );


  typename InputMeshType::CellType::MultiVisitor::Pointer multiVisitor = InputMeshType::CellType::MultiVisitor::New();
  multiVisitor->AddVisitor(vertexVisitor.GetPointer());
  multiVisitor->AddVisitor(lineVisitor.GetPointer());
  multiVisitor->AddVisitor(polyLineVisitor.GetPointer());
  multiVisitor->AddVisitor(triangleVisitor.GetPointer());
  multiVisitor->AddVisitor(quadrilateralVisitor.GetPointer());
  multiVisitor->AddVisitor(polygonVisitor.GetPointer());
  //multiVisitor->AddVisitor(tetrahedronVisitor.GetPointer());

  // Now ask the mesh to accept the multivisitor which
  // will Call Visit for each cell in the mesh that matches the
  // cell types of the visitors added to the MultiVisitor
  if( numberOfCells )
  {
    inputMesh->Accept(multiVisitor);
  }

  vertices->shrink_to_fit();
  outputPolyData->SetVertices( vertices );

  // Append lines to polylines before calling SetLines
  lines->shrink_to_fit();
  polylines->shrink_to_fit();

  auto iterator_start_lines = lines->begin();
  auto iterator_end_lines = lines->end();
  auto iterator_end_polylines = polylines->end();
  polylines->insert(iterator_end_polylines,  iterator_start_lines, iterator_end_lines);
  
  outputPolyData->SetLines( polylines);
  polygons->shrink_to_fit();
  outputPolyData->SetPolygons( polygons );
  //triangleStrips->shrink_to_fit();
  //outputPolyData->SetTriangleStrips( triangleStrips );

  using CellDataContainerType = typename PolyDataType::CellDataContainer;
  const CellDataContainerType * inputCellData = inputMesh->GetCellData();
  if( inputCellData && inputCellData->Size() )
  {
    typename CellDataContainerType::Pointer outputCellData = CellDataContainerType::New();
    outputCellData->Reserve( inputCellData->Size() );
    SizeValueType offset = 0;
    SizeValueType size = verticesCellIds->Size();

    // Copy the cell data in appropriate order : verts / lines / polys / strips
    for( SizeValueType ii = 0; ii < verticesCellIds->Size(); ++ii )
      {
      outputCellData->InsertElement( ii,
                                     inputCellData->ElementAt( verticesCellIds->ElementAt( ii ) ) );
      }
    offset += size;
    size = linesCellIds->Size();
    for( SizeValueType ii = 0; ii < size; ++ii )
      {
      outputCellData->InsertElement( offset + ii,
                                     inputCellData->ElementAt( linesCellIds->ElementAt( ii ) ) );
      }
    offset += size;
    size = polygonsCellIds->Size();
    for( SizeValueType ii = 0; ii < size; ++ii )
      {
      outputCellData->InsertElement( offset + ii,
                                     inputCellData->ElementAt( polygonsCellIds->ElementAt( ii ) ) );
      }
    offset += size;
    //size = triangleStripsCellIds->Size();
    //for( SizeValueType ii = 0; ii < size; ++ii )
      //{
      //outputCellData->InsertElement( offset + ii,
                                     //inputCellData->ElementAt( triangleStripsCellIds->ElementAt( ii ) ) );
      //}

    outputPolyData->SetCellData( outputCellData );
    }

}


} // end namespace itk

#endif // itkMeshToPolyDataFilter_hxx
