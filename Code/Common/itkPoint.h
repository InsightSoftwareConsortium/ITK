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
/**
 * Point simply represents the geometric coordinates of one point in
 * N-dimensional space.
 */

#ifndef __itkPoint_h
#define __itkPoint_h

namespace itk
{

/**
 * Template parameters for Point:
 *
 * VPointDimension =
 *     Geometric dimension of space.
 * TCoordRep =
 *     Numerical type to store each coordinate value.
 */

template <
  int VPointDimension,
  typename TCoordRep = double
  >
class Point
{
public:
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
  void GetCoords(CoordRep coords[PointDimension]);
  
protected:
  /**
   * Actually store the point's geometrical information.
   */
  CoordRep m_Coords[PointDimension];
};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkPoint.txx"
#endif

#endif
