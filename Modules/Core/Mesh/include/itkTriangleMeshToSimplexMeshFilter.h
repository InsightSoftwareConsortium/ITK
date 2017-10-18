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
template< typename TInputMesh, typename TOutputMesh >
class ITK_TEMPLATE_EXPORT TriangleMeshToSimplexMeshFilter:public MeshToMeshFilter< TInputMesh, TOutputMesh >
{
public:
  /** Standard "Self" typedef. */
  typedef TriangleMeshToSimplexMeshFilter Self;

  /** Standard "Superclass" typedef. */
  typedef MeshToMeshFilter< TInputMesh, TOutputMesh > Superclass;

  /** Smart pointer typedef support */
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method of creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(TriangleMeshToSimplexMeshFilter, MeshToMeshFilter);

  typedef TInputMesh                     InputMeshType;
  typedef typename TInputMesh::Pointer   InputMeshPointer;
  typedef typename TInputMesh::PointType InputPointType;

  typedef typename TInputMesh::BoundaryAssignmentsContainerPointer InputBoundaryAssignmentsContainerPointer;

  typedef typename TInputMesh::PointsContainer         InputPointsContainer;
  typedef typename InputPointsContainer::Pointer       InputPointsContainerPointer;
  typedef typename InputPointsContainer::Iterator      InputPointsContainerIterator;
  typedef typename InputPointsContainer::ConstIterator InputPointsContainerConstIterator;

  typedef typename TOutputMesh::Pointer                   OutputMeshPointer;
  typedef typename TOutputMesh::PointType                 OutputPointType;
  typedef typename TOutputMesh::PixelType                 OutputPixelType;
  typedef typename TOutputMesh::PointsContainer           OutputPointsContainer;
  typedef typename OutputPointsContainer::Pointer         OutputPointsContainerPointer;
  typedef typename TOutputMesh::PointsContainer::Iterator OutputPointsContainerIterator;

  typedef typename TInputMesh::BoundaryAssignmentIdentifier InputBoundnaryAssignmentIdentifier;

  typedef typename TInputMesh::CellType               InputCellType;
  typedef typename InputCellType::CellAutoPointer     InputCellAutoPointer;
  typedef typename TInputMesh::CellAutoPointer        CellAutoPointer;
  typedef          itk::LineCell< InputCellType >     LineType;
  typedef          itk::PolygonCell< InputCellType >  PolygonType;
  typedef          itk::TriangleCell< InputCellType > TriangleType;
  typedef          itk::VertexCell< InputCellType >   VertexType;

  typedef typename TOutputMesh::CellIdentifier        CellIdentifier;
  typedef typename TOutputMesh::PointIdentifier       PointIdentifier;
  typedef typename TOutputMesh::CellFeatureIdentifier CellFeatureIdentifier;

  typedef          std::pair< CellIdentifier, CellIdentifier >     EdgeIdentifierType;
  typedef          std::set< CellIdentifier >                      IndexSetType;

  typedef          itk::MapContainer< CellIdentifier, EdgeIdentifierType > EdgeNeighborListType;
  typedef          itk::MapContainer< EdgeIdentifierType, CellIdentifier > LineCellIndexType;

  typedef          itk::MapContainer< PointIdentifier, IndexSetType >       VertexNeighborListType;
  typedef          itk::MapContainer< EdgeIdentifierType, CellIdentifier >  EdgeMapType;
  typedef typename EdgeMapType::Pointer                                     EdgeMapPointer;

  typedef          itk::VectorContainer< CellIdentifier, CellIdentifier >   IdVectorType;
  typedef typename IdVectorType::Pointer                                    IdVectorPointer;

  typedef typename TOutputMesh::CellType              OutputCellType;
  typedef typename TOutputMesh::CellAutoPointer       OutputCellAutoPointer;
  typedef          itk::LineCell< OutputCellType >    OutputLineType;
  typedef          itk::PolygonCell< OutputCellType > OutputPolygonType;

protected:

  TriangleMeshToSimplexMeshFilter();
  ~TriangleMeshToSimplexMeshFilter() ITK_OVERRIDE;
  TriangleMeshToSimplexMeshFilter(const Self &) {}

  void operator=(const Self &) {}

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /**
   * Override from ProcessObject
   */
  virtual void GenerateData() ITK_OVERRIDE;

  /**
   * Initializes all necessary datastructures
   */
  void Initialize();

  /**
   * Method inserts the new computed simplex points into the output mesh
   */
  void CreateSimplexPoints();

  /**
   * Method creates a new edge, which from the centers of
   * two neighboring triangles of the input mesh over the
   * edge the both triangles have in common.
   */
  void CreateEdgeForTrianglePair(CellIdentifier pointIndex,
                                 CellIdentifier boundaryId,
                                 TOutputMesh *outputMesh);

  /**
   * Constructs the neighborhood relations for all simplex mesh points
   * It also reorders the neighbors for easy normals computation
   */
  void CreateSimplexNeighbors();

  /**
   * This method creates all the cells of the dual simplex mesh
   */
  void CreateCells();

  /**
   * \brief add edge cells to the input mesh
   */
  void CreateNewEdge(CellIdentifier currentCellId, CellFeatureIdentifier featureId,
                     PointIdentifier startPointId, PointIdentifier endPointId,
                     const InputMeshType *input);


  /**
   *  Computes the center of a face
   */
  InputPointType ComputeFaceCenter(CellIdentifier faceId, const InputMeshType *inputMesh);

  /**
   * \brief stores all faces (triangles) of the input mesh
   */
  IndexSetType *m_FaceSet;

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
} //end of namespace

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkTriangleMeshToSimplexMeshFilter.hxx"
#endif

#endif
