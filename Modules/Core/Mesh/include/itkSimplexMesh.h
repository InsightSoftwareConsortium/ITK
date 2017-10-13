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
#ifndef itkSimplexMesh_h
#define itkSimplexMesh_h

#include "itkMesh.h"
#include "itkSimplexMeshGeometry.h"
#include "itkVertexCell.h"
#include "itkTriangleCell.h"
#include "itkFixedArray.h"
#include <vector>
#include <algorithm>
#include <set>

namespace itk
{
/** \class SimplexMesh
 * \brief The class represents a 2-simplex mesh.
 *
 * A simplex mesh can be used for deformable model segmentation of 3D image data.
 * To create a simplex mesh one needs a triangle mesh, which can be converted
 * to using the class itkTriangleMeshToSimplexMeshFilter. The back filtering
 * (from simplex to trinagle mesh)is done through a itkSimplexMeshToTriangleMeshFilter.
 *
 * \author Thomas Boettger. Division Medical and Biological Informatics, German Cancer Research Center, Heidelberg.
 * \ingroup ITKMesh
 */
template< typename TPixelType, unsigned int VDimension = 3,
          typename TMeshTraits =
            DefaultStaticMeshTraits< TPixelType, VDimension, VDimension, TPixelType, TPixelType, TPixelType >
          >
class ITK_TEMPLATE_EXPORT SimplexMesh:public Mesh< TPixelType, VDimension, TMeshTraits >
{
public:
  /** Standard typedefs. */
  typedef SimplexMesh Self;

  /** Standard typedefs. */
  typedef Mesh< TPixelType, VDimension, TMeshTraits > Superclass;

  /** Standard typedefs. */
  typedef SmartPointer< Self > Pointer;

  /** Standard typedefs. */
  typedef SmartPointer< const Self > ConstPointer;

  /** definition for array of indices. */
  typedef typename SimplexMeshGeometry::IndexArray IndexArray;

  /** definition for a set of neighbor indices */
  typedef std::set< SizeValueType > NeighborSetType;

  /** */
  typedef typename NeighborSetType::iterator NeighborSetIterator;

  /** */
  typedef std::vector< SizeValueType > NeighborListType;

  /** */
  typedef typename TMeshTraits::PointType PointType;

  /** */
  typedef typename TMeshTraits::PointIdentifier PointIdentifier;

  /** */
  typedef typename PointType::VectorType VectorType;

  /** */
  typedef CovariantVector< typename VectorType::ValueType, 3 > CovariantVectorType;

  /** */
  typedef typename Superclass::CellType CellType;

  /** */
  typedef typename CellType::CellAutoPointer CellAutoPointer;
  /** */
  typedef itk::LineCell< CellType > LineType;

  /** map containing a SimplexMeshGeometry data object for each mesh
   * point */
  typedef itk::MapContainer< SizeValueType, SimplexMeshGeometry * > GeometryMapType;

  /** smartpointer def for the geometry map */
  typedef typename GeometryMapType::Pointer GeometryMapPointer;

  /** iterator definition for iterating over a geometry map */
  typedef typename GeometryMapType::Iterator        GeometryMapIterator;
  typedef typename GeometryMapType::ConstIterator   GeometryMapConstIterator;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Standard part of every itk Object. */
  itkTypeMacro(SimplexMesh, Mesh);

  /** Hold on to the type information specified by the template parameters. */
  typedef TMeshTraits                                       MeshTraits;
  typedef typename MeshTraits::PixelType                    PixelType;
  typedef typename MeshTraits::PointsContainer              PointsContainer;
  typedef typename Superclass::PointsContainerPointer       PointsContainerPointer;
  typedef typename Superclass::PointsContainer::Iterator    PointsContainerIterator;
  typedef typename Superclass::PointsContainerConstIterator PointsContainerConstIterator;
  typedef typename Superclass::CellsContainerPointer        CellsContainerPointer;
  typedef typename Superclass::CellsContainerConstPointer   CellsContainerConstPointer;
  typedef typename Superclass::CellsContainerIterator       CellsContainerIterator;
  typedef typename Superclass::CellsContainerConstIterator  CellsContainerConstIterator;
  typedef typename Superclass::CellIdentifier               CellIdentifier;

  /** set the map of geometrydata to the new pointer */
  itkSetMacro(GeometryData, GeometryMapPointer);

  /** returns the current map of geometrydata */
  itkGetConstReferenceMacro(GeometryData, GeometryMapPointer);

  /** Get the first free id for new cells */
  itkSetMacro(LastCellId, CellIdentifier);

  /** Set the id value valid for new cells */
  itkGetConstMacro(LastCellId, CellIdentifier);

  /**
   * copy all necessary information from passed object
   * to the mesh
   */
  virtual void CopyInformation(const DataObject *data) ITK_OVERRIDE;

  /**
   * Add a new edge to the simplex mesh by specifying the ids of the start
   * and end point of the edge
   * Note: This can destroy the simplex mesh structure! Better use the
   * simplex mesh modification or creation filters
   */
  CellIdentifier AddEdge(PointIdentifier startPointId, PointIdentifier endPointId);

  /**
   * Add a new simplex mesh cell to the mesh by passing an AutoPointer of a
   * previously created simplex mesh cell
   *
   * Note: This can destroy the simplex mesh structure! You should use the
   * simplex mesh modification or creation filters.
   */
  CellIdentifier AddFace(CellAutoPointer & cellPointer);

  /**
   * Replaces the cell specified by replaceIndex with the new cell passed by its
   * AutoPointer
   */
  CellIdentifier ReplaceFace(CellIdentifier replaceIndex, CellAutoPointer & cellPointer);

  /**
   * Get the three direct neighbors of a point
   */
  IndexArray GetNeighbors(PointIdentifier pointId) const;

  /**
   * Get all neighbor points with a specified radius
   */
  NeighborListType * GetNeighbors(PointIdentifier pointId, unsigned int radius, NeighborListType *list = ITK_NULLPTR) const;

  /**
   * Add a neighbor to a point.
   * Note: This can destroy the simplex mesh topology!
   * Better use te simplex mesh creation filters.
   */
  void AddNeighbor(PointIdentifier pointId, PointIdentifier neighborId);

  /**
   * Replace a neighbor of a specific point by a new one
   */
  void ReplaceNeighbor(PointIdentifier pointId, PointIdentifier oldNeighborId, PointIdentifier newNeighborIdx);

  /**
   * Swap the order of two neighbors
   */
  void SwapNeighbors(PointIdentifier pointId, PointIdentifier firstNeighborId, PointIdentifier secondNeighborId);

  /**
   * Set the geometry data for a specified point
   */
  void SetGeometryData(PointIdentifier pointId, SimplexMeshGeometry *);

  /**
   * Set the geometry data for a specified point
   */
  void SetBarycentricCoordinates(PointIdentifier idx, PointType values);

  /**
   * Set the barycentric coordinates for a specified point
   */
  PointType GetBarycentricCoordinates(PointIdentifier idx) const;

  /**
   * Set the reference metrics for a specified point
   */
  void SetReferenceMetrics(PointIdentifier idx, PointType values);

  /**
   *  Return the reference metrics for the specified point
   */
  PointType GetReferenceMetrics(PointIdentifier idx) const;

  /**
   * Set the simplex angle for the specified point
   */
  void SetPhi(PointIdentifier idx, double values);

  /**
   * Get the simplex angle for the specified point
   */
  double GetPhi(PointIdentifier idx) const;

  /**
   * Set the mean curvature for the specified point
   */
  void SetMeanCurvature(PointIdentifier idx, double values);

  /**
   * Get the mean curvature for the specified point
   */
  double GetMeanCurvature(PointIdentifier idx) const;

  /**
   * Set the circum circles radius for the specified point
   */
  void SetRadius(PointIdentifier idx, double values);

  /**
   * Get the circum circles radius for the specified point
   */
  double GetRadius(PointIdentifier idx) const;

  /**
   * Set the distance to the foot point for the specified point
   */
  void SetDistance(PointIdentifier idx, double values);

  /**
   * Get the distance to the foot point for the specified point
   */
  double GetDistance(PointIdentifier idx) const;

  /** compute the normal vector in the specified mesh point */
  CovariantVectorType ComputeNormal(PointIdentifier idx) const;

protected:
  //  /** Constructor for use by New() method. */
  SimplexMesh();
  virtual ~SimplexMesh() ITK_OVERRIDE;
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /**
   * The map stores a SimplexMeshGeometry object for each mesh point
   */
  GeometryMapPointer m_GeometryData;

  /**
   * The last cell id is the index which is used for insertion of new
   * cells. It increases during mesh creation. This is done because
   * one cannot rely on the size of the map or the highest index when
   * cells are removed.
   */
  CellIdentifier m_LastCellId;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(SimplexMesh);
}; // End Class:  SimplexMesh
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSimplexMesh.hxx"
#endif

#endif
