/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkQuadEdgeCellTraitsInfo.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkQuadEdgeCellTraitsInfo_h
#define __itkQuadEdgeCellTraitsInfo_h

#include "itkQuadEdgeMeshPoint.h"
#include "itkMapContainer.h"
#include <set>
#include "itkGeometricalQuadEdge.h"

namespace itk
{
/** \class QuadEdgeMeshCellTraitsInfo
 *  \brief Helper class holding the traits of QuadEdge cells.
 *
 * \author Alexandre Gouaillard, Leonardo Florez-Valencia, Eric Boix
 *
 * This implementation was contributed as a paper to the Insight Journal
 * http://insight-journal.org/midas/handle.php?handle=1926/306
 *
 */
template< int VPointDimension,
          typename TCoordRep = float,
          typename TInterpolationWeight = float,
          typename TPointIdentifier = unsigned long,
          typename TCellIdentifier = unsigned long,
          typename TCellFeatureIdentifier = unsigned char,
          typename TPoint = QuadEdgeMeshPoint< TCoordRep, VPointDimension >,
          typename TPointsContainer = MapContainer< unsigned long, TPoint >,
          typename TUsingCellsContainer = std::set< TPointIdentifier >,
          typename TQE = GeometricalQuadEdge< unsigned long, unsigned long, bool, bool, true > >
class QuadEdgeMeshCellTraitsInfo
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

  /** Iterator types. */
  typedef PointIdentifier *               PointIdIterator;
  typedef const PointIdentifier *         PointIdConstIterator;
  typedef TQE                             QuadEdgeType;
  typedef typename TQE::IteratorGeom      PointIdInternalIterator;
  typedef typename TQE::ConstIteratorGeom PointIdInternalConstIterator;
};
}
#endif
