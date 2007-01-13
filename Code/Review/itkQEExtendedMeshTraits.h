// -------------------------------------------------------------------------
// itkQEExtendedMeshTraits.h
// $Revision: 1.2 $
// $Author: ibanez $
// $Name:  $
// $Date: 2007-01-13 12:42:15 $
// -------------------------------------------------------------------------
// This code is an implementation of the well known quad edge (QE) data
// structure in the ITK library. Although the original QE can handle non
// orientable 2-manifolds and its dual and its mirror, this implementation
// is specifically dedicated to handle orientable 2-manifolds along with
// their dual.
//
// Any comment, criticism and/or donation is welcome.
//
// Please contact any member of the team:
//
// - The frog master (Eric Boix)       eboix@ens-lyon.fr
// - The duck master (Alex Gouaillard) alexandre.gouaillard@sun.com
// - The cow  master (Leonardo Florez) florez@creatis.insa-lyon.fr
// -------------------------------------------------------------------------

#ifndef __ITKQUADEDGEMESH__EXTENDEDMESHTRAITS__H__
#define __ITKQUADEDGEMESH__EXTENDEDMESHTRAITS__H__

#include "itkQEMeshTraits.h" // For CellTraitsInfo definition
#include <set>

namespace itkQE
{

/**
 * \class ExtendedMeshTraits
 * \brief Extended traits for a \ref itkQE::Mesh.
 *
 * ExtendedMeshTraits is a simple structure that holds type information
 * for a \ref itkQE::Mesh and its cells. It is used to avoid the passing
 * of many template parameters while still enjoying the benefits of generic
 * programming.
 *
 * @param TCoordRep
 *    Numerical type with which to represent each coordinate value.
 *
 * @param VPointDimension
 *    Geometric dimension of space.
 *
 * @param VMaxTopologicalDimension
 *    Max topological dimension of a cell that can be inserted into this mesh.
 *
 * @param TPixelType
 *    The type stored as data for vertices.
 *
 * @param TPData
 *    The type stored as data for the primal edges.
 *
 * @param TDData
 *    The type stored as data for the dual edges.
 *
 * @param TCellPixelType
 *     The type associated with every cell.
 *
 */
template< 
  typename TCoordRep = float,
  unsigned int VPointDimension = 3,
  unsigned int VMaxTopologicalDimension = VPointDimension,
  typename TPixelType = float,
  typename TPData = bool,
  typename TDData = bool,
  typename TCellPixelType = TPixelType >
class ExtendedMeshTraits
{
   public:
   typedef ExtendedMeshTraits Self;
   /** Save the template parameters. */
   typedef TCoordRep       CoordRepType;
   typedef TPixelType      PixelType;
   typedef TPData          PrimalDataType;
   typedef TDData          DualDataType;
   typedef TCellPixelType  CellPixelType;

   /** Save all the template parameters. */
   itkStaticConstMacro( PointDimension, unsigned int, VPointDimension );
   itkStaticConstMacro( MaxTopologicalDimension, unsigned int,
                        VPointDimension );

   typedef TPixelType      InterpolationWeightType;

   /** The type to be used to identify a point.  This should be the index type
    * to the PointsContainer. */
   typedef unsigned long PointIdentifier;

   /** The type to be used to identify a cell.  This should be the index type
    * to the CellsContainer. */
   typedef unsigned long CellIdentifier;

   /** A type that can be used to identifiy individual boundary features on
    * the cells.  Since this will probably be an index into a static array,
    * this will probably never change from an integer setting. */
   typedef unsigned long CellFeatureIdentifier;

   /** The container type that will be used to store boundary links
    * back to cells.  This must conform to the STL "set" interface. */
   typedef std::set< CellIdentifier > UsingCellsContainer;

   /** The CellLinks container should be a container of PointCellLinksContainer,
    * which should be a container conforming to the STL "set" interface. */
   typedef std::set< CellIdentifier > PointCellLinksContainer;

   /** Quad edge typedefs. */
   typedef GeometricalQuadEdge< PointIdentifier, CellIdentifier,
                         PrimalDataType, DualDataType > QEPrimal;
   typedef typename QEPrimal::Dual           QEDual;
   typedef typename QEPrimal::OrgRefType     VertexRefType;
   typedef typename QEPrimal::DualOrgRefType FaceRefType;

   /** The type of point used by the mesh. This should never change from 
    * this setting, regardless of the mesh type. Points have an entry
    * in the Onext ring */
   typedef Point< CoordRepType, VPointDimension, QEPrimal >  PointType;

   /** The container type for use in storing points. It must conform to
    * the IndexedContainer interface. */
   typedef itk::MapContainer< PointIdentifier, PointType >   PointsContainer;

   /** Standard itk cell interface. */
   typedef CellTraitsInfo< VPointDimension,         CoordRepType,
                           InterpolationWeightType, PointIdentifier,
                           CellIdentifier,          CellFeatureIdentifier,
                           PointType,               PointsContainer,
                           UsingCellsContainer,     QEPrimal > CellTraits;

   /** The interface to cells to be used by the mesh. */
   typedef itk::CellInterface< CellPixelType, CellTraits > CellType;
   typedef typename CellType::CellAutoPointer              CellAutoPointer;

   /** Containers types. */
   typedef itk::MapContainer< PointIdentifier,
                              PointCellLinksContainer >     CellLinksContainer;
   typedef itk::MapContainer< CellIdentifier, CellType* >   CellsContainer;
   typedef itk::MapContainer< PointIdentifier, PixelType >  PointDataContainer;
   typedef itk::MapContainer< CellIdentifier, CellPixelType > CellDataContainer;

   /** Other useful types. */
   typedef typename PointType::VectorType VectorType;
};

} // enamespace

#endif // __ITKQUADEDGEMESH__EXTENDEDMESHTRAITS__H__

// eof - itkQEExtendedMeshTraits.h
