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
#ifndef __itkBinaryMask3DMeshSource_h
#define __itkBinaryMask3DMeshSource_h

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
 * not already be in the mesh.  In the latter case, the points are
 * also added if necessary.
 *
 * To add the topological information to an already constructed mesh
 * (for efficiency of traversal), use this class to generate a copy of
 * the original mesh.
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
  typedef typename MeshType::MeshTraits        MeshTraits;
  typedef typename MeshType::PointType         PointType;
  typedef typename MeshTraits::PixelType       PixelType;  
  typedef typename MeshType::CellTraits        CellTraits;
  typedef typename MeshType::PointsContainer   PointsContainer;
  typedef typename MeshType::CellsContainer    CellsContainer;
  typedef typename MeshType::CellType          CellType;
  typedef typename PointType::CoordRepType     CoordinateType;
  typedef typename CellType::CellAutoPointer   CellAutoPointer;

  /** These pointers are typedef'ed because they need the "typename"
   * specification. */
  typedef typename MeshType::Pointer           MeshPointer;
  typedef typename PointsContainer::Pointer    PointsContainerPointer;
  typedef typename CellsContainer::Pointer     Cellscontainerpointer;

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
  typedef Array< IdentifierType >              CellArrayType;

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

  /** Add p0 if it's not already there, and return its ID. */
  IdentifierType AddPoint( const PointType& p0 );

  /** Add a Cell, specified as an itk::Array of point IDs.  The size
   * of the array is equal to the number of points needed to specify
   * the cell. */
  IdentifierType AddVertex( const CellArrayType& cellArray );
  IdentifierType AddLine( const CellArrayType& cellArray );

  /** Add a Cell, specified as a C-style array of point IDs. */
  IdentifierType AddLine( const IdentifierType cellArray[2] );

  /** Add a the Cell given by the ordered sequence of pointIds. */
  IdentifierType AddVertex( IdentifierType pointId );

  IdentifierType AddVertex( const PointType& p0 );
  IdentifierType AddVertex( const CoordinateType p0[ PointDimension ] );

  IdentifierType AddLine( const PointType& p0, const PointType& p1 );

  IdentifierType AddTriangle( const PointType& p0, const PointType& p1,
                            const PointType& p2 );

  IdentifierType AddQuadrilateral( const PointType& p0, const PointType& p1,
                                 const PointType& p2, const PointType& p3 );

  IdentifierType AddTetrahedron( const PointType& p0, const PointType& p1,
                               const PointType& p2, const PointType& p3 );

  IdentifierType AddHexahedron(
    const PointType& p0, const PointType& p1, const PointType& p2,
    const PointType& p3, const PointType& p4, const PointType& p5,
    const PointType& p6, const PointType& p7
    );

  IdentifierType AddPoint( const CoordinateType p0[ PointDimension ] );

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

#endif // ndef __itkBinaryMask3DMeshSource_h
