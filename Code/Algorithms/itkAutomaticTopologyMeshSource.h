/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAutomaticTopologyMeshSource.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkAutomaticTopologyMeshSource_h
#define __itkAutomaticTopologyMeshSource_h

#include "itkArray.h"
#include "itkDefaultStaticMeshTraits.h"
#include "itk_hash_map.h"
#include "itkHexahedronCell.h"
#include "itkLineCell.h"
#include "itkMesh.h"
#include "itkMeshSource.h"
#include "itkQuadrilateralCell.h"
#include "itkStructHashFunction.h"
#include "itkTriangleCell.h"
#include "itkTetrahedronCell.h"
#include "itkVertexCell.h"

namespace itk
{

class IdentifierArrayHashFunction
{
public:
  unsigned long operator()(
    Array< unsigned long > identifierArray
    ) const;
};

class IdentifierArrayEqualsFunction
{
public:
  bool operator()(
    Array< unsigned long > identifierArray1,
    Array< unsigned long > identifierArray2
    ) const;
};

/** \class itkAutomaticTopologyMeshSource.h
 * \brief Convenience class for generating simple meshes.  
 *
 * This generates an N-dimensional mesh consisting of some combination
 * of vertices, line segments, triangles, quadrilaterals, tetrahedra,
 * and hexahedra.  Identifiers for the cells are automatically added,
 * and topological connectivity is automatically computed.  When a
 * cell is added, all of its boundary features are determined and
 * added as well.
 *
 * The main methods are of the form Add<it>Object</it>, where
 * <it>Object</it> can be Point, Vertex, Triangle, Quadrilateral,
 * Tetrahedron, or Hexahedron.  Each of these methods has several
 * overloaded forms, permitting multiple ways to specify the object
 * being added.  When called, each of these methods first checks to
 * see if the object has already been added.  If it has not, then a
 * new identifier is generated (the smallest one so far unused), the
 * object is added with that identifier, and the ID is returned.  If
 * the object has already been added, then the ID it already has is
 * returned and nothing else is done.
 *
 * When a cell is added, all of its boundary elements are also added,
 * and boundary assignments are set.  A cell can be specified using
 * IDs of points already added, or using Point objects that may or may
 * not already be in the mesh.  If a cell is specified using Point
 * objects, then the points are added to the mesh if necessary.
 *
 * The different ways of specifying a cell are
 *  -# An IdentifierArrayType (= itk::Array<IdentifierType>) of point identifiers.
 *     These point identifiers are the ones returned by calls to
 *     AddPoint().
 *  -# A parameter list of point identifiers (for instance, <tt>this->AddLine(0,
 *     1)</tt>, if 0 and 1 are point identifiers).
 *  -# A parameter list of itk::Point objects (the function then
 *     generates the identifiers).
 *  -# A parameter list of C-style arrays, with each such array giving
 *     the coordinates of one point.  This form is useful for copying
 *     in geometry from foreign data structures.
 * 
 * For meshes generated using this filter, only one cell can be added
 * for any given set of vertices.  If a, b, c, and d are identifiers
 * for four points in R^3, Then (a, b, c, d) and (a, c, d, b)
 * determine two different quadrilaterals (at least one of which is
 * either degenerate or nonplanar).  If you call
 * \begincode
 *   AddQuadrilateral(a, b, c, d);
 *   AddQuadrilateral(a, c, d, b);
 * \endcode
 * then only the first quadrilateral will actually be added.
 *
 * To add the topological information to an already constructed mesh
 * (for efficiency of traversal), use this class to generate a copy of
 * the original mesh.
 *
 * \b Example: The following code generates a mesh consisting of two
 * triangles sharing an edge.
 * \code
 *  typedef itk::AutomaticTopologyMeshSource< MeshType >  MeshSourceType;
 *  MeshSourceType::Pointer meshSource = MeshSourceType::New();
 *  meshSource->AddTriangle(
 *    meshSource->AddPoint(0, 0, 0),
 *    meshSource->AddPoint(1, 0, 0),
 *    meshSource->AddPoint(0, 1, 0) );
 *  meshSource->AddTriangle(
 *    meshSource->AddPoint(0, 0, 0),
 *    meshSource->AddPoint(1, 0, 0),
 *    meshSource->AddPoint(0, 0, 1) );
 * \endcode
 *
 * This class inherits from itk::MeshSource so it fits conveniently into a
 * pipeline, but GetOutput() is always valid after every
 * Add[Something]() call, and Update() is a no-op.  It is <b>not
 * thread safe</b>.
 */
template <class TOutputMesh>
class ITK_EXPORT AutomaticTopologyMeshSource : public MeshSource<TOutputMesh>
{
public:
  /** Standard "Self" typedef. */
  typedef AutomaticTopologyMeshSource   Self;
  typedef MeshSource<TOutputMesh>       Superclass;
  typedef SmartPointer<Self>            Pointer;
  typedef SmartPointer<const Self>      ConstPointer;

  /** Hold on to the type information specified by the template parameters. */
  typedef          TOutputMesh                 MeshType;
  typedef typename MeshType::PointType         PointType;
  typedef typename MeshType::CellType          CellType;
  typedef typename MeshType::Pointer           MeshPointer;
  typedef typename PointType::CoordRepType     CoordinateType;
  typedef typename CellType::CellAutoPointer   CellAutoPointer;

  /** Different kinds of cells. */
  typedef VertexCell< CellType >               VertexCell;
  typedef LineCell< CellType >                 LineCell;
  typedef TriangleCell< CellType >             TriangleCell;
  typedef QuadrilateralCell< CellType >        QuadrilateralCell;
  typedef TetrahedronCell< CellType >          TetrahedronCell;
  typedef HexahedronCell< CellType >           HexahedronCell;

  /** This class requires that the mesh being built use unsigned long
   * as the identifier type for all its elements. */
  typedef unsigned long IdentifierType;

  /** Array of IdentifierType objects used to specify cells. */
  typedef Array< IdentifierType >              IdentifierArrayType;

  /** hash_map typedefs. */

  typedef itk::hash_map<
    PointType,
    IdentifierType,
    StructHashFunction< PointType > >          PointHashMap;

  /** The dimension of the output mesh. */
  itkStaticConstMacro(PointDimension, unsigned int,
                      MeshType::PointDimension);
  itkStaticConstMacro(MaxTopologicalDimension, unsigned int,
                      MeshType::MaxTopologicalDimension);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);  

  /** Run-time type information (and related methods). */
  itkTypeMacro(BinaryMask3DMeshSource, MeshSource);

  /** Add the point p0 if it's not already there, and return its ID. */
  IdentifierType AddPoint( const PointType& p0 );
  IdentifierType AddPoint( const CoordinateType p0[ PointDimension ] );

  /** Add the point with coordinates (x0, ..., xN) where N =
   *  PointDimension - 1.  If N < 5, then any parameters after xN are
   *  ignored.  If PointDimension > 6, then a point is generated with
   *  the first six coordinates equal to x0, ..., x5, and the rest set
   *  to 0. */
  IdentifierType AddPoint( CoordinateType x0 = 0, CoordinateType x1 = 0,
                           CoordinateType x2 = 0, CoordinateType x3 = 0,
                           CoordinateType x4 = 0, CoordinateType x5 = 0 );

  /** Add a vertex located at the given point, and return its ID. */
  IdentifierType AddVertex( const IdentifierArrayType& pointIds );
  IdentifierType AddVertex( IdentifierType pointId0 );
  IdentifierType AddVertex( const PointType& p0 );
  IdentifierType AddVertex( const CoordinateType p0[ PointDimension ] );

  /** Add the line specified by the two points, and return its ID.
   * The endpoints and their associated vertices are associated to the
   * line in the order that they are specified the first time the
   * function is called. */
  IdentifierType AddLine( const IdentifierArrayType& pointIds );
  IdentifierType AddLine(
      IdentifierType pointId0, IdentifierType pointId1 );
  IdentifierType AddLine( const PointType& p0, const PointType& p1 );
  IdentifierType AddLine( const CoordinateType p0[ PointDimension ],
                          const CoordinateType p1[ PointDimension ] );

  /** Add the triangle specified by the three points, and return its
   * ID. If the points are p0, p1, and p2, then the following
   * additional cells (represented here as ordered tuples) are created
   * (if they don't already exist) and associated as boundaries, in
   * the order given:
   *
   * Vertices: (p0), (p1), (p2).
   *
   * Lines: (p0, p1), (p1, p2), (p2, p0).
   * */
  IdentifierType AddTriangle(      const IdentifierArrayType& pointIds );
  IdentifierType AddTriangle(
      IdentifierType pointId0, IdentifierType pointId1,
      IdentifierType pointId2 );
  IdentifierType AddTriangle( const PointType& p0, const PointType& p1,
                            const PointType& p2 );
  IdentifierType AddTriangle( const CoordinateType p0[ PointDimension ],
                              const CoordinateType p1[ PointDimension ],
                              const CoordinateType p2[ PointDimension ] );

  /** Add the quadrilateral specified by the four points, and return its
   * ID. If the points are p0, p1, p2, and p3, then the following
   * additional cells (represented here as ordered tuples) are created
   * (if they don't already exist) and associated as boundaries, in
   * the order given:
   *
   * Vertices: (p0), (p1), (p2), (p3).
   *
   * Lines: (p0, p1), (p2, p3), (p0, p2), (p1, p3).
   *
   * In particular, if the points are arranged geometrically as follows
   \verbatim
     p0  p1

     p2  p3
   \endverbatim
   *
   * then you would call, for instance,
   * <tt>meshSource->AddQuadrilateral(p0, p1, p2, p3)</tt>.
   * */
  IdentifierType AddQuadrilateral( const IdentifierArrayType& pointIds );
  IdentifierType AddQuadrilateral(
      IdentifierType pointId0, IdentifierType pointId1,
      IdentifierType pointId2, IdentifierType pointId3 );
  IdentifierType AddQuadrilateral( const PointType& p0, const PointType& p1,
                                 const PointType& p2, const PointType& p3 );
  IdentifierType AddQuadrilateral( const CoordinateType p0[ PointDimension ],
                                   const CoordinateType p1[ PointDimension ],
                                   const CoordinateType p2[ PointDimension ],
                                   const CoordinateType p3[ PointDimension ] );

  /** Add the tetrahedron specified by the three points, and return its
   * ID. If the points are p0, p1, and p2, then the following
   * additional cells (represented here as ordered tuples) are created
   * (if they don't already exist) and associated as boundaries, in
   * the order given:
   *
   * Vertices: (p0), (p1), (p2), (p3).
   *
   * Lines: (p0, p1), (p0, p2), (p0, p3), (p1, p2), (p1, p3), (p2, p3).
   *
   * Triangles: (p0, p1, p2), (p0, p1, p3), (p0, p2, p3), (p1, p2, * p3).
   * */
  IdentifierType AddTetrahedron(   const IdentifierArrayType& pointIds );
  IdentifierType AddTetrahedron(
      IdentifierType pointId0, IdentifierType pointId1,
      IdentifierType pointId2, IdentifierType pointId3 );
  IdentifierType AddTetrahedron( const PointType& p0, const PointType& p1,
                               const PointType& p2, const PointType& p3 );
  IdentifierType AddTetrahedron( const CoordinateType p0[ PointDimension ],
                                 const CoordinateType p1[ PointDimension ],
                                 const CoordinateType p2[ PointDimension ],
                                 const CoordinateType p3[ PointDimension ] );

  /** Add the hexahedron specified by the four points, and return its
   * ID. If the points are p0, p1, p2, and p3, then the following
   * additional cells (represented here as ordered tuples) are created
   * (if they don't already exist) and associated as boundaries, in
   * the order given:
   *
   * Vertices: (p0), (p1), (p2), (p3), (p4), (p5), (p6), (p7).
   *
   * Lines: (p0, p1), (p2, p3), (p4, p5), (p6, p7), (p0, p2), (p1,
   * p3), (p4, p6), (p5, p7), (p0, p4), (p1, p5), (p2, p6), (p3, p7).
   *
   * Quadrilaterals: (0, 1, 2, 3), (4, 5, 6, 7), (0, 1, 4, 5), (2, 3,
   * 6, 7), (0, 2, 4, 6), (1, 3, 5, 7),
   *
   * In particular, if the points are connected topologically as follows
   \verbatim
     p4------------p5
     | \          / |
     |  p0------p1  |
     |  |       |   |
     |  |       |   |
     |  p2------p3  |
     | /          \ |
     p6------------p7
   \endverbatim
   * then you would call, for instance,
   * <tt>meshSource->AddQuadrilateral(p0, p1, p2, p3, p4, p5, p6,
   * p7)</tt>. */
  IdentifierType AddHexahedron(    const IdentifierArrayType& pointIds );
  IdentifierType AddHexahedron(
      IdentifierType pointId0, IdentifierType pointId1,
      IdentifierType pointId2, IdentifierType pointId3,
      IdentifierType pointId4, IdentifierType pointId5,
      IdentifierType pointId6, IdentifierType pointId7 );
  IdentifierType AddHexahedron(
    const PointType& p0, const PointType& p1, const PointType& p2,
    const PointType& p3, const PointType& p4, const PointType& p5,
    const PointType& p6, const PointType& p7
    );
  IdentifierType AddHexahedron( const CoordinateType p0[ PointDimension ],
                                const CoordinateType p1[ PointDimension ],
                                const CoordinateType p2[ PointDimension ],
                                const CoordinateType p3[ PointDimension ],
                                const CoordinateType p4[ PointDimension ],
                                const CoordinateType p5[ PointDimension ],
                                const CoordinateType p6[ PointDimension ],
                                const CoordinateType p7[ PointDimension ] );

  /** The update method is a no-op for this source.  The value of the
   *  output only changes in response to calls of the Add[object]()
   *  methods above. */
  void Update() {};

protected:
  AutomaticTopologyMeshSource();
  ~AutomaticTopologyMeshSource();
  void PrintSelf(std::ostream& os, Indent indent) const;

  // void GenerateData();

private:
  AutomaticTopologyMeshSource(const Self&);  //purposely not implemented
  void operator=(const Self&);               //purposely not implemented

  typedef itk::hash_map<
    Array< IdentifierType >,
    IdentifierType,
    IdentifierArrayHashFunction,
    IdentifierArrayEqualsFunction >          CellHashMap;

  PointHashMap m_PointsHashTable;
  CellHashMap  m_CellsHashTable;
  MeshPointer  m_OutputMesh;   // Retained for convenience.
  
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkAutomaticTopologyMeshSource.txx"
#endif

#endif // __itkAutomaticTopologyMeshSource_h
