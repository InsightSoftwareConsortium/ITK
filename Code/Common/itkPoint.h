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

/**
 * itkPoint simply represents the geometric coordinates of one point in
 * N-dimensional space.
 */

template <
  /**
   * Geometrical dimension of space.
   */
  int VPointDimension,
  
  /**
   * Numerical type to store each coordinate value.
   */
  typename TCoordRep = double
  >
class itkPoint
{
public:
  typedef itkPoint               Self;

  enum { PointDimension = VPointDimension };
  typedef TCoordRep CoordRep;

  /**
   * Constructors.
   */
  itkPoint();
  itkPoint(CoordRep coords[PointDimension]);
  
  
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

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkPoint.cxx"
#endif

#endif
