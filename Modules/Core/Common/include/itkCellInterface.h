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
#ifndef itkCellInterface_h
#define itkCellInterface_h

#include "itkObject.h"
#include <map>
#include "itkCellInterfaceVisitor.h"
#include "itkAutoPointer.h"
#include "itkArray.h"

// Define a macro for CellInterface sub-classes to use
// to define the Accept and GetTopologyId virtuals used
// by the MultiVisitor class
#define itkCellVisitMacro(TopologyId)                                                                \
  static int GetTopologyId()                                                                         \
    {                                                                                                \
    return TopologyId;                                                                               \
    }                                                                                                \
  virtual void Accept(CellIdentifier cellid, typename CellInterface< PixelType,                      \
                      CellTraits >::MultiVisitor * mv) ITK_OVERRIDE                                  \
    {                                                                                                \
    typename CellInterfaceVisitor< PixelType, CellTraits >::Pointer v =                              \
      mv->GetVisitor(TopologyId);                                                                    \
    if ( v )                                                                                         \
      {                                                                                              \
      v->VisitFromCell(cellid, this);                                                                \
      }                                                                                              \
    }

// Define a macro for the common typedefs required by the
// classes deriving form CellInterface (included).
#define itkCellCommonTypedefs(celltype)                   \
  typedef celltype                  Self;                 \
  typedef AutoPointer< const Self > ConstSelfAutoPointer; \
  typedef AutoPointer< Self >       SelfAutoPointer;      \
  typedef Self *                    RawPointer;           \
  typedef const Self *ConstRawPointer

// Define a macro for the common typedefs required by the
// classes deriving form CellInterface (excluded).
#define itkCellInheritedTypedefs(superclassArg)                             \
  typedef superclassArg                             Superclass;             \
  typedef typename Superclass::PixelType            PixelType;              \
  typedef typename Superclass::CellType             CellType;               \
  typedef typename Superclass::CellAutoPointer      CellAutoPointer;        \
  typedef typename Superclass::CellConstAutoPointer CellConstAutoPointer;   \
  typedef typename Superclass::CellRawPointer       CellRawPointer;         \
  typedef typename Superclass::CellConstRawPointer  CellConstRawPointer;    \
  typedef typename Superclass::CellTraits           CellTraits;             \
  typedef typename Superclass::CoordRepType         CoordRepType;           \
  typedef typename Superclass::InterpolationWeightType                      \
  InterpolationWeightType;                                                  \
  typedef typename Superclass::PointIdentifier       PointIdentifier;       \
  typedef typename Superclass::PointIdIterator       PointIdIterator;       \
  typedef typename Superclass::PointIdConstIterator  PointIdConstIterator;  \
  typedef typename Superclass::CellIdentifier        CellIdentifier;        \
  typedef typename Superclass::CellFeatureIdentifier CellFeatureIdentifier; \
  typedef typename Superclass::CellFeatureIdentifier CellFeatureCount;      \
  typedef typename Superclass::PointType             PointType;             \
  typedef typename Superclass::VectorType            VectorType;            \
  typedef typename Superclass::PointsContainer       PointsContainer;       \
  typedef typename Superclass::UsingCellsContainer   UsingCellsContainer;   \
  typedef typename Superclass::CellGeometry          CellGeometry;          \
  typedef typename Superclass::ParametricCoordArrayType                     \
  ParametricCoordArrayType;                                                 \
  typedef typename Superclass::ShapeFunctionsArrayType                      \
  ShapeFunctionsArrayType;                                                  \
  itkStaticConstMacro(PointDimension, unsigned int, Superclass::PointDimension)

namespace itk
{
/** \class CellInterface
 *  \brief An abstract interface for cells.
 *
 * Define an abstract interface for cells.  Actual cell types derive from
 * this class.
 *
 * \tparam TPixelType The type stored with an entity (cell, point, or boundary).
 * \tparam TCellTraits Type information for cell.
 *
 * \ingroup MeshObjects
 * \ingroup ITKCommon
 */
template<
  typename TPixelType,
  typename TCellTraits
  >
class ITK_TEMPLATE_EXPORT CellInterface
{
public:

  /** Standard class typedefs. */
  itkCellCommonTypedefs(CellInterface);

  /** Save the PixelType template parameter. */
  typedef TPixelType PixelType;

  /** Save the CellTraits template parameter. */
  typedef TCellTraits CellTraits;

  /** Save type information for this cell. */
  typedef typename CellTraits::CoordRepType            CoordRepType;
  typedef typename CellTraits::InterpolationWeightType InterpolationWeightType;
  typedef typename CellTraits::PointIdentifier         PointIdentifier;
  typedef typename CellTraits::PointIdIterator         PointIdIterator;
  typedef typename CellTraits::PointIdConstIterator    PointIdConstIterator;
  typedef typename CellTraits::CellIdentifier          CellIdentifier;
  typedef typename CellTraits::CellFeatureIdentifier   CellFeatureIdentifier;
  typedef typename CellTraits::PointType               PointType;
  typedef typename CellTraits::PointsContainer         PointsContainer;
  typedef typename CellTraits::UsingCellsContainer     UsingCellsContainer;

  ///NOTE: it should normally be defined in the traits
  typedef typename PointType::VectorType VectorType;

  /** Save the dimension from the template parameters. */
  itkStaticConstMacro(PointDimension, unsigned int, CellTraits::PointDimension);

  /** An iterator through the UsingCellsContainer. */
  typedef typename UsingCellsContainer::iterator UsingCellsContainerIterator;

  /** Give this and all derived classes quick access to the base cell type. */
  typedef CellInterface        CellType;
  typedef SelfAutoPointer      CellAutoPointer;
  typedef ConstSelfAutoPointer CellConstAutoPointer;
  typedef RawPointer           CellRawPointer;
  typedef ConstRawPointer      CellConstRawPointer;

  /** A useful rename. */
  typedef CellFeatureIdentifier CellFeatureCount;

  /**  Cell Visitor interfaces */
  enum CellGeometry { VERTEX_CELL = 0, LINE_CELL, TRIANGLE_CELL,
                      QUADRILATERAL_CELL, POLYGON_CELL, TETRAHEDRON_CELL, HEXAHEDRON_CELL,
                      QUADRATIC_EDGE_CELL, QUADRATIC_TRIANGLE_CELL,
                      LAST_ITK_CELL, MAX_ITK_CELLS = 255 };

  /** Types needed to contour the cells */
  typedef Array< CoordRepType >            ParametricCoordArrayType;
  typedef Array< InterpolationWeightType > ShapeFunctionsArrayType;

//  static int GetNextUserCellId(); // never return > MAX_INTERFACE

  /** \class MultiVisitor
   * \brief A visitor that can visit different cell types in a mesh.
   * CellInterfaceVisitor instances can be registered for each
   * type of cell that needs to be visited.
   *
   * \ingroup MeshAccess
   * \ingroup ITKCommon
   */
  class MultiVisitor:public LightObject
  {
public:
    /**  Visitor type, because VisualC++ 6.0 does not like
     *  Visitor being a nested type of CellInterfaceVisitor   */
    typedef CellInterfaceVisitor< TPixelType, TCellTraits > VisitorType;

    /** Standard class typedefs.   */
    typedef MultiVisitor         Self;
    typedef SmartPointer< Self > Pointer;

    /** Method for creation through the object factory.   */
    //itkNewMacro(Self);
    static Pointer New(void) { Pointer smartPtr = new Self; smartPtr->UnRegister(); return smartPtr; }

    /** Run-time type information (and related methods).   */
    itkTypeMacro(MultiVisitor, LightObject);

    /** Typedefs for the visitor class.   */
    typedef typename VisitorType::Pointer VisitorPointer;
    typedef typename std::map< int, VisitorPointer >::value_type
    VisitorPointerValueType;

public:
    VisitorType * GetVisitor(int id)
    {
      if ( id < LAST_ITK_CELL )
        {
        return m_Visitors[id];
        }
      else
        {
        typename std::map< int, typename VisitorType::Pointer >::iterator
        pos = m_UserDefined.find(id);
        if ( pos != m_UserDefined.end() )
          {
          return ( *pos ).second;
          }
        }
      return ITK_NULLPTR;
    }

    void AddVisitor(VisitorType *v)
    {
      int id = v->GetCellTopologyId();

      if ( id < LAST_ITK_CELL )
        {
        m_Visitors[id] = v;
        }
      else
        {
        m_UserDefined.insert( VisitorPointerValueType(id, v) );
        }
    }

    virtual ~MultiVisitor() ITK_OVERRIDE {}

protected:
    VisitorPointer m_Visitors[LAST_ITK_CELL];      // fixed array set to the
                                                   // size
                                                   // from the enum
    std::map< int, VisitorPointer > m_UserDefined; // user defined cell types
                                                   // go here
  };

  /** This must be implemented by all sub-classes of CellInterface */
  virtual void Accept(CellIdentifier cellId, MultiVisitor *) = 0;

  /**  Return the type of the cell (one of the CellGeometry enums
   *   listed above). */
  virtual CellGeometry GetType(void) const = 0;

  /** Create a new copy of this cell.  This is provided so that a copy can
   * be made without knowing the cell type. */
  virtual void MakeCopy(CellAutoPointer &) const = 0;

  /** Get the topological dimension of this cell. */
  virtual unsigned int GetDimension(void) const = 0;

  /** Get the interpolation order of the cell.  Usually linear. */
  virtual unsigned int GetInterpolationOrder() const;

  /** Get the number of points required to define the cell. */
  virtual unsigned int GetNumberOfPoints(void) const = 0;

  /** Get the number of boundary features of a given dimension on this cell. */
  virtual CellFeatureCount GetNumberOfBoundaryFeatures(int dimension) const = 0;

  /** Get the boundary feature corresponding to the given dimension and Id. */
  virtual bool GetBoundaryFeature(int dimension, CellFeatureIdentifier,
                                  CellAutoPointer &) = 0;

  /** Get the point id list used by the cell in a form suitable to pass to
   * SetPointIds(first) on another cell.  This is equivalent to
   * PointIdsBegin() const. */
  virtual PointIdConstIterator GetPointIds() const;

  /** Set the point id list used by the cell.  It is assumed that the given
   * iterator can be incremented and safely de-referenced enough times to
   * get all the point ids needed by the cell. */
  virtual void SetPointIds(PointIdConstIterator first) = 0;

  /** Set the point id list used by the cell.  It is assumed that the range
   * of iterators [first, last) contains the correct number of points needed to
   * define the cell.  The position *last is NOT referenced, so it can safely
   * be one beyond the end of an array or other container. */
  virtual void SetPointIds(PointIdConstIterator first,
                           PointIdConstIterator last) = 0;

  /** Set the point identifier for a given spot in the point list
   *  for the cell. */
  virtual void SetPointId(int localId, PointIdentifier) = 0;

  /** Get a begin iterator to the list of point identifiers used by the cell. */
  virtual PointIdIterator PointIdsBegin(void) = 0;

  /** Get a const begin iterator to the list of point identifiers used
   * by the cell. */
  virtual PointIdConstIterator PointIdsBegin(void) const = 0;

  /** Get an end iterator to the list of point identifiers used by the cell. */
  virtual PointIdIterator PointIdsEnd(void) = 0;

  /** Get a const end iterator to the list of point identifiers used
   * by the cell. */
  virtual PointIdConstIterator PointIdsEnd(void) const = 0;

  /** Get/Set the point id list used by the cell */
  typedef itk::Array<PointIdentifier> PointIdentifierContainerType;
  PointIdentifierContainerType GetPointIdsContainer() const;
  void SetPointIdsContainer( const PointIdentifierContainerType & );

  /** Given the parametric coordinates of a point in the cell
   * (pCoords[CellDimension]), get the closest cell boundary feature of
   * topological dimension CellDimension-1.  If the "inside" pointer is not
   * ITK_NULLPTR, the flag is set to indicate whether the point is inside the cell. */
  virtual bool GetClosestBoundary(CoordRepType[], bool *, CellAutoPointer &)
  { return false; }

  /** Given the geometric coordinates of a point (coord[PointDimension]),
   * return whether it is inside the cell.  Also perform the following
   * calculations, if the corresponding result pointers are not ITK_NULLPTR:
   *
   *  - Find the closest point in or on the cell to the given point
   *     (Returns through pointer to array: closestPoint[PointDimension]).
   *
   *  - Get the cell's parametric coordinates for the given point
   *     (Returns through pointer to array: pCoords[CellDimension]).
   *
   *  - Get the square of the distance between the point and the cell
   *     (this is the distance from the point to the closest point,
   *      returned through "dist2" pointer).
   *
   *  - Get the interpolation weights for the cell
   *     (Returns through pointer to array: weights[NumberOfPoints]). */
  virtual bool EvaluatePosition(CoordRepType *,
                                PointsContainer *,
                                CoordRepType *,
                                CoordRepType[],
                                double *,
                                InterpolationWeightType *)
  { return bool(); }

  /** Given the parametric coordinates of a point in the cell
   *  determine the value of its Shape Functions
   *  returned through an itkArray<InterpolationWeightType>).  */
  virtual void EvaluateShapeFunctions(
    const ParametricCoordArrayType &,
    ShapeFunctionsArrayType  &) const {}

  /** Intersect the cell with a line given by an origin (origin[PointDimension])
   * and direction (direction[PointDimension]).  The intersection point
   * found will be within the given tolerance of the real intersection.
   * Get the following results if the corresponding pointers are not ITK_NULLPTR:
   *
   *  - The intersection point's geometric coordinates (returned through
   *     pointer to array: coords[PointDimension]).
   *
   *  - The line's parametric coordinate of the intersection point
   *     (returned through "t" pointer).
   *
   *  - The cell's parametric coordinates of the intersection point
   *     (returned through pointer to array: pCoords[CellDimension]).
   *
   * Returns whether an intersection exists within the given tolerance. */
  virtual bool IntersectWithLine(CoordRepType[PointDimension],
                                 CoordRepType[PointDimension],
                                 CoordRepType,
                                 CoordRepType[PointDimension],
                                 CoordRepType *,
                                 CoordRepType[]) { return bool(); }

  /** Compute cell bounding box and store in the user-provided array.
   * Array is ordered (xmin, xmax,  ymin, ymax, ....).  A pointer to the
   * array is returned for convenience.  This allows code like:
   * "CoordRep* bounds = cell->GetBoundingBox(new CoordRep[6]);". */
  CoordRepType * GetBoundingBox(CoordRepType[PointDimension * 2]) { return ITK_NULLPTR; }

  /** Compute the square of the diagonal length of the bounding box. */
  CoordRepType GetBoundingBoxDiagonalLength2(void) { return NumericTraits< CoordRepType >::ZeroValue(); }

  /** Intersect the given bounding box (bounds[PointDimension*2]) with a line
   * given by an origin (origin[PointDimension]) and direction
   * (direction[PointDimension]). Get the following results if the
   * corresponding pointers are not ITK_NULLPTR:
   *
   *  - The intersection point's geometric coordinates (returned through
   *     pointer to array: coords[PointDimension]).
   *
   *  - The line's parametric coordinate of the intersection point
   *     (returned through "t" pointer).
   *
   * Returns whether an intersection exists. */
  virtual bool IntersectBoundingBoxWithLine(CoordRepType[PointDimension * 2],
                                            CoordRepType[PointDimension],
                                            CoordRepType[PointDimension],
                                            CoordRepType[PointDimension],
                                            CoordRepType *) { return bool(); }

  /** Interface to the boundary form of the cell to set/get UsingCells.
   * See the boundary wrapper source for more information. */

  /** Returns true if the cell has been explicitly assigned as a
   *  boundary, false otherwise. */
  virtual bool IsExplicitBoundary();

  /**
   * Register the fact that this cell is a part of the boundary of the
   * cell \a cellId, by adding \a cellId to the UsingCellsContainer.
   */
  virtual void AddUsingCell(CellIdentifier cellId);

  /**
   * Remove a cell from the UsingCellsContainer.
   */
  virtual void RemoveUsingCell(CellIdentifier cellId);

  /**
   * Test if a cell is in the UsingCellsContainer.  A result of \c true
   * indicates that this cell is part of the boundary of the cell \a
   * cellId, assuming that boundary information has been recorded.
   */
  virtual bool IsUsingCell(CellIdentifier cellId);

  /**
   * Get the number of cells in the UsingCellsContainer.
   */
  virtual unsigned int GetNumberOfUsingCells();

#if !defined( ITK_WRAPPING_PARSER )
  /**
   * Get a begin iterator for the UsingCellsContainer.
   */
  virtual UsingCellsContainerIterator UsingCellsBegin();

  /**
   * Get an end iterator for the UsingCellsContainer.
   */
  virtual UsingCellsContainerIterator UsingCellsEnd();

#endif

  /** Standard part of every itk Object. */
  itkTypeMacroNoParent(CellInterface);

public:
  CellInterface() {}
  virtual ~CellInterface() {}
  /** Cell internal utility routines. */

  /** Get the geometric position of a point. */
//  bool GetPointPosition(PointsContainer*, int localId, Point*)=0;

protected:
  /** Store the set of cells using this boundary. */
  UsingCellsContainer m_UsingCells;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(CellInterface);
};

/** \class CellTraitsInfo
 * \brief A simple utility class to define the cell type inside a mesh type
 * structure definition.  This just makes a copy of existing type information
 * that is needed for a cell type template parameter.
 *
 * During a mesh type definition, after the appropriate types and values
 * have been defined, just have the line:
 \verbatim
 typedef itkMakeCellTraitsMacro  CellTraits;
 \endverbatim
 *
 * itkMakeCellTraitsMacro is a macro front-end to automatically fill in the
 * template parameters for the CellTraitsInfo structure inside a mesh
 * type structure definition.
 *
 * \ingroup MeshObjects
 * \ingroup ITKCommon
 */
template< int VPointDimension, typename TCoordRep,
          typename TInterpolationWeight, typename TPointIdentifier,
          typename TCellIdentifier, typename TCellFeatureIdentifier,
          typename TPoint, typename TPointsContainer,
          typename TUsingCellsContainer >
class ITK_TEMPLATE_EXPORT CellTraitsInfo
{
public:
  itkStaticConstMacro(PointDimension, unsigned int, VPointDimension);
  typedef TCoordRep              CoordRepType;
  typedef TInterpolationWeight   InterpolationWeightType;
  typedef TPointIdentifier       PointIdentifier;
  typedef TCellIdentifier        CellIdentifier;
  typedef TCellFeatureIdentifier CellFeatureIdentifier;
  typedef TPoint                 PointType;
  typedef TPointsContainer       PointsContainer;
  typedef TUsingCellsContainer   UsingCellsContainer;
  typedef PointIdentifier *      PointIdIterator;

  typedef const PointIdentifier *PointIdConstIterator;
};

#define itkMakeCellTraitsMacro                                           \
  CellTraitsInfo < itkGetStaticConstMacro(PointDimension), CoordRepType, \
  InterpolationWeightType,                                               \
  PointIdentifier, CellIdentifier, CellFeatureIdentifier,                \
  PointType, PointsContainer, UsingCellsContainer >
} // end namespace itk

#if !defined( ITK_WRAPPING_PARSER )
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkCellInterface.hxx"
#endif
#endif

#endif
