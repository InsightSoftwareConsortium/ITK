/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPoint.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkPoint_h
#define __itkPoint_h

#include "itkMacro.h"

namespace itk
{

/** \class Point
 * \brief Represent the coordinates of a point in n-dimensional space.
 *
 * Point simply represents the geometric coordinates of one point in
 * N-dimensional space. Point is used by various types of containers
 * (e.g., VectorContainer) to represent the points in a mesh or cell.
 *
 * Template parameters for Point:
 *
 * VPointDimension =
 *     Geometric dimension of space.
 * TCoordRep =
 *     Numerical type to store each coordinate value.
 */

template <
  int VPointDimension,
  typename TCoordRep = float
  >
class Point
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef Point  Self;

  /**
   * Save template parameter information.
   */
  enum { PointDimension = VPointDimension };
  typedef TCoordRep CoordRep;

  /**
   * Constructors.
   */
  Point();
  Point(CoordRep coords[PointDimension]);
  
  /**
   * Allow run-time point dimension access.
   */
  int GetPointDimension(void) const
    {
    return PointDimension;
    }
  
  /**
   * Access routines.
   */
  void SetCoords(CoordRep coords[PointDimension]);
  void GetCoords(CoordRep coords[PointDimension]) const;
  const CoordRep* GetCoords() const 
    { return m_Coords;} ;

protected:
  /**
   * Actually store the point's geometrical information.
   */
  CoordRep m_Coords[PointDimension];
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkPoint.txx"
#endif

#endif
