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
#ifndef itkCellInterface_h
#define itkCellInterface_h

#include "itkObject.h"
#include "itkCellInterfaceVisitor.h"
#include "itkAutoPointer.h"
#include "itkArray.h"
#include "itkCommonEnums.h"

#include <map>

// Define a macro for CellInterface sub-classes to use
// to define the Accept and GetTopologyId virtuals used
// by the MultiVisitor class
#define itkCellVisitMacro(TopologyId)                                                                                  \
  static constexpr CellGeometryEnum GetTopologyId() { return TopologyId; }                                             \
  virtual void Accept(CellIdentifier cellid, typename CellInterface<PixelType, CellTraits>::MultiVisitor * mv)         \
    override                                                                                                           \
  {                                                                                                                    \
    typename CellInterfaceVisitor<PixelType, CellTraits>::Pointer v = mv->GetVisitor(TopologyId);                      \
    if (v)                                                                                                             \
    {                                                                                                                  \
      v->VisitFromCell(cellid, this);                                                                                  \
    }                                                                                                                  \
  }

// Define a macro for the common type alias required by the
// classes deriving form CellInterface (included).
#define itkCellCommonTypedefs(celltype)                                                                                \
  using Self = celltype;                                                                                               \
  using ConstSelfAutoPointer = AutoPointer<const Self>;                                                                \
  using SelfAutoPointer = AutoPointer<Self>;                                                                           \
  using RawPointer = Self *;                                                                                           \
  using ConstRawPointer = const Self *

// Define a macro for the common type alias required by the
// classes deriving form CellInterface (excluded).
#define itkCellInheritedTypedefs(superclassArg)                                                                        \
  using Superclass = superclassArg;                                                                                    \
  using PixelType = typename Superclass::PixelType;                                                                    \
  using CellType = typename Superclass::CellType;                                                                      \
  using CellAutoPointer = typename Superclass::CellAutoPointer;                                                        \
  using CellConstAutoPointer = typename Superclass::CellConstAutoPointer;                                              \
  using CellRawPointer = typename Superclass::CellRawPointer;                                                          \
  using CellConstRawPointer = typename Superclass::CellConstRawPointer;                                                \
  using CellTraits = typename Superclass::CellTraits;                                                                  \
  using CoordRepType = typename Superclass::CoordRepType;                                                              \
  using InterpolationWeightType = typename Superclass::InterpolationWeightType;                                        \
  using PointIdentifier = typename Superclass::PointIdentifier;                                                        \
  using PointIdIterator = typename Superclass::PointIdIterator;                                                        \
  using PointIdConstIterator = typename Superclass::PointIdConstIterator;                                              \
  using CellIdentifier = typename Superclass::CellIdentifier;                                                          \
  using CellFeatureIdentifier = typename Superclass::CellFeatureIdentifier;                                            \
  using CellFeatureCount = typename Superclass::CellFeatureIdentifier;                                                 \
  using PointType = typename Superclass::PointType;                                                                    \
  using VectorType = typename Superclass::VectorType;                                                                  \
  using PointsContainer = typename Superclass::PointsContainer;                                                        \
  using UsingCellsContainer = typename Superclass::UsingCellsContainer;                                                \
  using ParametricCoordArrayType = typename Superclass::ParametricCoordArrayType;                                      \
  using ShapeFunctionsArrayType = typename Superclass::ShapeFunctionsArrayType;                                        \
  static constexpr unsigned int PointDimension = Superclass::PointDimension

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
template <typename TPixelType, typename TCellTraits>
class ITK_TEMPLATE_EXPORT CellInterface
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(CellInterface);

  /** Standard class type aliases. */
  itkCellCommonTypedefs(CellInterface);

  /** Save the PixelType template parameter. */
  using PixelType = TPixelType;

  /** Save the CellTraits template parameter. */
  using CellTraits = TCellTraits;

  /** Save type information for this cell. */
  using CoordRepType = typename CellTraits::CoordRepType;
  using InterpolationWeightType = typename CellTraits::InterpolationWeightType;
  using PointIdentifier = typename CellTraits::PointIdentifier;
  using PointIdIterator = typename CellTraits::PointIdIterator;
  using PointIdConstIterator = typename CellTraits::PointIdConstIterator;
  using CellIdentifier = typename CellTraits::CellIdentifier;
  using CellFeatureIdentifier = typename CellTraits::CellFeatureIdentifier;
  using PointType = typename CellTraits::PointType;
  using PointsContainer = typename CellTraits::PointsContainer;
  using UsingCellsContainer = typename CellTraits::UsingCellsContainer;

  /// NOTE: it should normally be defined in the traits
  using VectorType = typename PointType::VectorType;

  /** Save the dimension from the template parameters. */
  static constexpr unsigned int PointDimension = CellTraits::PointDimension;

  /** An iterator through the UsingCellsContainer. */
  using UsingCellsContainerIterator = typename UsingCellsContainer::iterator;

  /** Give this and all derived classes quick access to the base cell type. */
  using CellType = CellInterface;
  using CellAutoPointer = SelfAutoPointer;
  using CellConstAutoPointer = ConstSelfAutoPointer;
  using CellRawPointer = RawPointer;
  using CellConstRawPointer = ConstRawPointer;

  /** A useful rename. */
  using CellFeatureCount = CellFeatureIdentifier;

  /** Types needed to contour the cells */
  using ParametricCoordArrayType = Array<CoordRepType>;
  using ShapeFunctionsArrayType = Array<InterpolationWeightType>;

  //  static int GetNextUserCellId(); // never return > MAX_INTERFACE

  /** \class MultiVisitor
   * \brief A visitor that can visit different cell types in a mesh.
   * CellInterfaceVisitor instances can be registered for each
   * type of cell that needs to be visited.
   *
   * \ingroup MeshAccess
   * \ingroup ITKCommon
   */
  class MultiVisitor : public LightObject
  {
  public:
    /**  Visitor type, because VisualC++ 6.0 does not like
     *  Visitor being a nested type of CellInterfaceVisitor   */
    using VisitorType = CellInterfaceVisitor<TPixelType, TCellTraits>;

    /** Standard class type aliases.   */
    using Self = MultiVisitor;
    using Pointer = SmartPointer<Self>;

    /** Method for creation through the object factory.   */
    // itkNewMacro(Self);
    static Pointer
    New()
    {
      Pointer smartPtr = new Self;
      smartPtr->UnRegister();
      return smartPtr;
    }

    /** Run-time type information (and related methods).   */
    itkTypeMacro(MultiVisitor, LightObject);

    /** Typedefs for the visitor class.   */
    using VisitorPointer = typename VisitorType::Pointer;
    using VisitorPointerValueType = typename std::map<CellGeometryEnum, VisitorPointer>::value_type;

  public:
    VisitorType *
    GetVisitor(CellGeometryEnum id)
    {
      if (id < CellGeometryEnum::LAST_ITK_CELL)
      {
        return m_Visitors[static_cast<int>(id)];
      }
      else
      {
        auto pos = m_UserDefined.find(id);
        if (pos != m_UserDefined.end())
        {
          return (*pos).second;
        }
      }
      return nullptr;
    }

    void
    AddVisitor(VisitorType * v)
    {
      CellGeometryEnum id = v->GetCellTopologyId();

      if (id < CellGeometryEnum::LAST_ITK_CELL)
      {
        m_Visitors[static_cast<int>(id)] = v;
      }
      else
      {
        m_UserDefined.insert(VisitorPointerValueType(id, v));
      }
    }

    ~MultiVisitor() override = default;

  protected:
    VisitorPointer m_Visitors[static_cast<int>(CellGeometryEnum::LAST_ITK_CELL)]; // fixed array set to the
                                                                                  // size
                                                                                  // from the enum
    std::map<CellGeometryEnum, VisitorPointer> m_UserDefined;                     // user defined cell types
                                                                                  // go here
  };

  /** This must be implemented by all sub-classes of CellInterface */
  virtual void
  Accept(CellIdentifier cellId, MultiVisitor *) = 0;

  /**  Return the type of the cell (one of the CellGeometryEnum enums
   *   listed above). */
  virtual ::itk::CommonEnums::CellGeometry
  GetType() const = 0;

  /** Create a new copy of this cell.  This is provided so that a copy can
   * be made without knowing the cell type. */
  virtual void
  MakeCopy(CellAutoPointer &) const = 0;

  /** Get the topological dimension of this cell. */
  virtual unsigned int
  GetDimension() const = 0;

  /** Get the interpolation order of the cell.  Usually linear. */
  virtual unsigned int
  GetInterpolationOrder() const;

  /** Get the number of points required to define the cell. */
  virtual unsigned int
  GetNumberOfPoints() const = 0;

  /** Get the number of boundary features of a given dimension on this cell. */
  virtual CellFeatureCount
  GetNumberOfBoundaryFeatures(int dimension) const = 0;

  /** Get the boundary feature corresponding to the given dimension and Id. */
  virtual bool
  GetBoundaryFeature(int dimension, CellFeatureIdentifier, CellAutoPointer &) = 0;

  /** Get the point id list used by the cell in a form suitable to pass to
   * SetPointIds(first) on another cell.  This is equivalent to
   * PointIdsBegin() const. */
  virtual PointIdConstIterator
  GetPointIds() const;

  /** Set the point id list used by the cell.  It is assumed that the given
   * iterator can be incremented and safely de-referenced enough times to
   * get all the point ids needed by the cell. */
  virtual void
  SetPointIds(PointIdConstIterator first) = 0;

  /** Set the point id list used by the cell.  It is assumed that the range
   * of iterators [first, last) contains the correct number of points needed to
   * define the cell.  The position *last is NOT referenced, so it can safely
   * be one beyond the end of an array or other container. */
  virtual void
  SetPointIds(PointIdConstIterator first, PointIdConstIterator last) = 0;

  /** Set the point identifier for a given spot in the point list
   *  for the cell. */
  virtual void
  SetPointId(int localId, PointIdentifier) = 0;

  /** Get a begin iterator to the list of point identifiers used by the cell. */
  virtual PointIdIterator
  PointIdsBegin() = 0;

  /** Get a const begin iterator to the list of point identifiers used
   * by the cell. */
  virtual PointIdConstIterator
  PointIdsBegin() const = 0;

  /** Get an end iterator to the list of point identifiers used by the cell. */
  virtual PointIdIterator
  PointIdsEnd() = 0;

  /** Get a const end iterator to the list of point identifiers used
   * by the cell. */
  virtual PointIdConstIterator
  PointIdsEnd() const = 0;

  /** Get/Set the point id list used by the cell */
  using PointIdentifierContainerType = itk::Array<PointIdentifier>;
  PointIdentifierContainerType
  GetPointIdsContainer() const;
  void
  SetPointIdsContainer(const PointIdentifierContainerType &);

  /** Given the parametric coordinates of a point in the cell
   * (pCoords[CellDimension]), get the closest cell boundary feature of
   * topological dimension CellDimension-1.  If the "inside" pointer is not
   * nullptr, the flag is set to indicate whether the point is inside the cell. */
  virtual bool
  GetClosestBoundary(CoordRepType[], bool *, CellAutoPointer &)
  {
    return false;
  }

  /** Given the geometric coordinates of a point (coord[PointDimension]),
   * return whether it is inside the cell.  Also perform the following
   * calculations, if the corresponding result pointers are not nullptr:
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
  virtual bool
  EvaluatePosition(CoordRepType *,
                   PointsContainer *,
                   CoordRepType *,
                   CoordRepType[],
                   double *,
                   InterpolationWeightType *)
  {
    return bool();
  }

  /** Given the parametric coordinates of a point in the cell
   *  determine the value of its Shape Functions
   *  returned through an itkArray<InterpolationWeightType>).  */
  virtual void
  EvaluateShapeFunctions(const ParametricCoordArrayType &, ShapeFunctionsArrayType &) const
  {}

  /** Intersect the cell with a line given by an origin (origin[PointDimension])
   * and direction (direction[PointDimension]).  The intersection point
   * found will be within the given tolerance of the real intersection.
   * Get the following results if the corresponding pointers are not nullptr:
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
  virtual bool
  IntersectWithLine(CoordRepType[PointDimension],
                    CoordRepType[PointDimension],
                    CoordRepType,
                    CoordRepType[PointDimension],
                    CoordRepType *,
                    CoordRepType[])
  {
    return bool();
  }

  /** Compute cell bounding box and store in the user-provided array.
   * Array is ordered (xmin, xmax,  ymin, ymax, ....).  A pointer to the
   * array is returned for convenience.  This allows code like:
   * "CoordRep* bounds = cell->GetBoundingBox(new CoordRep[6]);". */
  CoordRepType * GetBoundingBox(CoordRepType[PointDimension * 2]) { return nullptr; }

  /** Compute the square of the diagonal length of the bounding box. */
  CoordRepType
  GetBoundingBoxDiagonalLength2()
  {
    return NumericTraits<CoordRepType>::ZeroValue();
  }

  /** Intersect the given bounding box (bounds[PointDimension*2]) with a line
   * given by an origin (origin[PointDimension]) and direction
   * (direction[PointDimension]). Get the following results if the
   * corresponding pointers are not nullptr:
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
                                            CoordRepType *)
  {
    return bool();
  }

  /** Interface to the boundary form of the cell to set/get UsingCells.
   * See the boundary wrapper source for more information. */

  /** Returns true if the cell has been explicitly assigned as a
   *  boundary, false otherwise. */
  virtual bool
  IsExplicitBoundary();

  /**
   * Register the fact that this cell is a part of the boundary of the
   * cell \a cellId, by adding \a cellId to the UsingCellsContainer.
   */
  virtual void
  AddUsingCell(CellIdentifier cellId);

  /**
   * Remove a cell from the UsingCellsContainer.
   */
  virtual void
  RemoveUsingCell(CellIdentifier cellId);

  /**
   * Test if a cell is in the UsingCellsContainer.  A result of \c true
   * indicates that this cell is part of the boundary of the cell \a
   * cellId, assuming that boundary information has been recorded.
   */
  virtual bool
  IsUsingCell(CellIdentifier cellId);

  /**
   * Get the number of cells in the UsingCellsContainer.
   */
  virtual unsigned int
  GetNumberOfUsingCells();

#if !defined(ITK_WRAPPING_PARSER)
  /**
   * Get a begin iterator for the UsingCellsContainer.
   */
  virtual UsingCellsContainerIterator
  UsingCellsBegin();

  /**
   * Get an end iterator for the UsingCellsContainer.
   */
  virtual UsingCellsContainerIterator
  UsingCellsEnd();

#endif

  /** Standard part of every itk Object. */
  itkTypeMacroNoParent(CellInterface);

public:
  CellInterface() = default;
  virtual ~CellInterface() = default;
  /** Cell internal utility routines. */

  /** Get the geometric position of a point. */
  //  bool GetPointPosition(PointsContainer*, int localId, Point*)=0;

#if !defined(ITK_LEGACY_REMOVE)
  /** Expose old names for backwards compatibility*/
  constexpr static CommonEnums::CellGeometry VERTEX_CELL = CommonEnums::CellGeometry::VERTEX_CELL;
  constexpr static CommonEnums::CellGeometry LINE_CELL = CommonEnums::CellGeometry::LINE_CELL;
  constexpr static CommonEnums::CellGeometry TRIANGLE_CELL = CommonEnums::CellGeometry::TRIANGLE_CELL;
  constexpr static CommonEnums::CellGeometry QUADRILATERAL_CELL = CommonEnums::CellGeometry::QUADRILATERAL_CELL;
  constexpr static CommonEnums::CellGeometry POLYGON_CELL = CommonEnums::CellGeometry::POLYGON_CELL;
  constexpr static CommonEnums::CellGeometry TETRAHEDRON_CELL = CommonEnums::CellGeometry::TETRAHEDRON_CELL;
  constexpr static CommonEnums::CellGeometry HEXAHEDRON_CELL = CommonEnums::CellGeometry::HEXAHEDRON_CELL;
  constexpr static CommonEnums::CellGeometry QUADRATIC_EDGE_CELL = CommonEnums::CellGeometry::QUADRATIC_EDGE_CELL;
  constexpr static CommonEnums::CellGeometry QUADRATIC_TRIANGLE_CELL =
    CommonEnums::CellGeometry::QUADRATIC_TRIANGLE_CELL;
  constexpr static CommonEnums::CellGeometry LAST_ITK_CELL = CommonEnums::CellGeometry::LAST_ITK_CELL;
  constexpr static CommonEnums::CellGeometry MAX_ITK_CELLS = CommonEnums::CellGeometry::MAX_ITK_CELLS;
#endif

protected:
  /** Store the set of cells using this boundary. */
  UsingCellsContainer m_UsingCells;
};

/** \class CellTraitsInfo
 * \brief A simple utility class to define the cell type inside a mesh type
 * structure definition.  This just makes a copy of existing type information
 * that is needed for a cell type template parameter.
 *
 * During a mesh type definition, after the appropriate types and values
 * have been defined, just have the line:
 \verbatim
 using CellTraits = itkMakeCellTraitsMacro;
 \endverbatim
 *
 * itkMakeCellTraitsMacro is a macro front-end to automatically fill in the
 * template parameters for the CellTraitsInfo structure inside a mesh
 * type structure definition.
 *
 * \ingroup MeshObjects
 * \ingroup ITKCommon
 */
template <int VPointDimension,
          typename TCoordRep,
          typename TInterpolationWeight,
          typename TPointIdentifier,
          typename TCellIdentifier,
          typename TCellFeatureIdentifier,
          typename TPoint,
          typename TPointsContainer,
          typename TUsingCellsContainer>
class ITK_TEMPLATE_EXPORT CellTraitsInfo
{
public:
  static constexpr unsigned int PointDimension = VPointDimension;
  using CoordRepType = TCoordRep;
  using InterpolationWeightType = TInterpolationWeight;
  using PointIdentifier = TPointIdentifier;
  using CellIdentifier = TCellIdentifier;
  using CellFeatureIdentifier = TCellFeatureIdentifier;
  using PointType = TPoint;
  using PointsContainer = TPointsContainer;
  using UsingCellsContainer = TUsingCellsContainer;
  using PointIdIterator = PointIdentifier *;

  using PointIdConstIterator = const PointIdentifier *;
};

#define itkMakeCellTraitsMacro                                                                                         \
  CellTraitsInfo<Self::PointDimension,                                                                                 \
                 CoordRepType,                                                                                         \
                 InterpolationWeightType,                                                                              \
                 PointIdentifier,                                                                                      \
                 CellIdentifier,                                                                                       \
                 CellFeatureIdentifier,                                                                                \
                 PointType,                                                                                            \
                 PointsContainer,                                                                                      \
                 UsingCellsContainer>
} // end namespace itk

#if !defined(ITK_WRAPPING_PARSER)
#  ifndef ITK_MANUAL_INSTANTIATION
#    include "itkCellInterface.hxx"
#  endif
#endif

#endif
