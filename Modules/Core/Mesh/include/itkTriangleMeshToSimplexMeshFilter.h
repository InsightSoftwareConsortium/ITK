/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkTriangleMeshToSimplexMeshFilter_h
#define itkTriangleMeshToSimplexMeshFilter_h

#include "itkMapContainer.h"
#include "itkSimplexMesh.h"
#include "itkMeshToMeshFilter.h"
#include "itkVectorContainer.h"

#include "itkPolygonCell.h"
namespace itk
{
/**
 * \class TriangleMeshToSimplexMeshFilter
 * \brief This filter converts a triangle mesh into a 2-simplex mesh.
 *
 * The triangle cell centers build the points of the dual simplex mesh
 * each center is connected with the center of each neighboring triangle.
 * This creates the simplex structure.
 *
 * Finally the neighbors of the points are reordered counter-clockwise
 * for geometry computation.
 *
 * \author Thomas Boettger. Division Medical and Biological Informatics, German Cancer Research Center, Heidelberg.
 *
 * \ingroup ITKMesh
 */
template <typename TInputMesh, typename TOutputMesh>
class ITK_TEMPLATE_EXPORT TriangleMeshToSimplexMeshFilter : public MeshToMeshFilter<TInputMesh, TOutputMesh>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(TriangleMeshToSimplexMeshFilter);

  /** Standard "Self" type alias. */
  using Self = TriangleMeshToSimplexMeshFilter;

  /** Standard "Superclass" type alias. */
  using Superclass = MeshToMeshFilter<TInputMesh, TOutputMesh>;

  /** Smart pointer type alias support */
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method of creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(TriangleMeshToSimplexMeshFilter, MeshToMeshFilter);

  using InputMeshType = TInputMesh;
  using InputMeshPointer = typename TInputMesh::Pointer;
  using InputPointType = typename TInputMesh::PointType;

  using InputBoundaryAssignmentsContainerPointer = typename TInputMesh::BoundaryAssignmentsContainerPointer;

  using InputPointsContainer = typename TInputMesh::PointsContainer;
  using InputPointsContainerPointer = typename InputPointsContainer::Pointer;
  using InputPointsContainerIterator = typename InputPointsContainer::Iterator;
  using InputPointsContainerConstIterator = typename InputPointsContainer::ConstIterator;

  using OutputMeshPointer = typename TOutputMesh::Pointer;
  using OutputPointType = typename TOutputMesh::PointType;
  using OutputPixelType = typename TOutputMesh::PixelType;
  using OutputPointsContainer = typename TOutputMesh::PointsContainer;
  using OutputPointsContainerPointer = typename OutputPointsContainer::Pointer;
  using OutputPointsContainerIterator = typename TOutputMesh::PointsContainer::Iterator;

  using InputBoundnaryAssignmentIdentifier = typename TInputMesh::BoundaryAssignmentIdentifier;

  using InputCellType = typename TInputMesh::CellType;
  using InputCellAutoPointer = typename InputCellType::CellAutoPointer;
  using CellAutoPointer = typename TInputMesh::CellAutoPointer;
  using LineType = itk::LineCell<InputCellType>;
  using PolygonType = itk::PolygonCell<InputCellType>;
  using TriangleType = itk::TriangleCell<InputCellType>;
  using VertexType = itk::VertexCell<InputCellType>;

  using CellIdentifier = typename TOutputMesh::CellIdentifier;
  using PointIdentifier = typename TOutputMesh::PointIdentifier;
  using CellFeatureIdentifier = typename TOutputMesh::CellFeatureIdentifier;

  using EdgeIdentifierType = std::pair<CellIdentifier, CellIdentifier>;
  using IndexSetType = std::set<CellIdentifier>;

  using EdgeNeighborListType = itk::MapContainer<CellIdentifier, EdgeIdentifierType>;
  using LineCellIndexType = itk::MapContainer<EdgeIdentifierType, CellIdentifier>;

  using VertexNeighborListType = itk::MapContainer<PointIdentifier, IndexSetType>;
  using EdgeMapType = itk::MapContainer<EdgeIdentifierType, CellIdentifier>;
  using EdgeMapPointer = typename EdgeMapType::Pointer;

  using IdVectorType = itk::VectorContainer<CellIdentifier, CellIdentifier>;
  using IdVectorPointer = typename IdVectorType::Pointer;

  using OutputCellType = typename TOutputMesh::CellType;
  using OutputCellAutoPointer = typename TOutputMesh::CellAutoPointer;
  using OutputLineType = itk::LineCell<OutputCellType>;
  using OutputPolygonType = itk::PolygonCell<OutputCellType>;

protected:
  TriangleMeshToSimplexMeshFilter();
  ~TriangleMeshToSimplexMeshFilter() override;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /**
   * Override from ProcessObject
   */
  void
  GenerateData() override;

  /**
   * Initializes all necessary datastructures
   */
  void
  Initialize();

  /**
   * Method inserts the new computed simplex points into the output mesh
   */
  void
  CreateSimplexPoints();

  /**
   * Method creates a new edge, which from the centers of
   * two neighboring triangles of the input mesh over the
   * edge the both triangles have in common.
   */
  void
  CreateEdgeForTrianglePair(CellIdentifier pointIndex, CellIdentifier boundaryId, TOutputMesh * outputMesh);

  /**
   * Constructs the neighborhood relations for all simplex mesh points
   * It also reorders the neighbors for easy normals computation
   */
  void
  CreateSimplexNeighbors();

  /**
   * This method creates all the cells of the dual simplex mesh
   */
  void
  CreateCells();

  /**
   * \brief add edge cells to the input mesh
   */
  void
  CreateNewEdge(CellIdentifier        currentCellId,
                CellFeatureIdentifier featureId,
                PointIdentifier       startPointId,
                PointIdentifier       endPointId,
                const InputMeshType * input);


  /**
   *  Computes the center of a face
   */
  InputPointType
  ComputeFaceCenter(CellIdentifier faceId, const InputMeshType * inputMesh);

  /**
   * \brief stores all faces (triangles) of the input mesh
   */
  IndexSetType * m_FaceSet;

  /**
   * \brief stores all edges of the input mesh.
   *
   * the key is the index of the edge cell, and the element is the
   * index pair of the two points of the edge.
   */
  EdgeMapPointer m_Edges;

  /**
   * \brief stores the two neighboring cells (faces) of an edge
   */
  typename EdgeNeighborListType::Pointer m_EdgeNeighborList;

  /**
   * \brief stores all edges starting from a vertex
   */
  typename VertexNeighborListType::Pointer m_VertexNeighborList;

  /**
   * stores line indices for conversion algorithm
   */
  typename LineCellIndexType::Pointer m_LineCellIndices;

  /**
   * offset for ids of new simplex polygon cells
   */
  CellIdentifier m_CellIdxOffset;

  /**
   * offset for point ids
   */
  PointIdentifier m_IdOffset;

  /**
   * offset for edge cell ids
   */
  CellIdentifier m_EdgeCellId;

  /**
   * stores algorithmic data
   */
  IdVectorPointer m_HandledEdgeIds;

  /**
   * autopointer definition for creation of new cells in the input mesh
   */
  InputCellAutoPointer m_NewInputMeshCellPointer;

  /**
   * autopointer definition for creation of new simplex cells
   */
  OutputCellAutoPointer m_NewSimplexCellPointer;
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkTriangleMeshToSimplexMeshFilter.hxx"
#endif

#endif
