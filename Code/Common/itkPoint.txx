/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPoint.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "itkPoint.h"

ITK_NAMESPACE_BEGIN

/**
 * Default constructor.
 */
template <int VPointDimension, typename TCoordRep>
Point< VPointDimension , TCoordRep >
::Point()
{
  for(int i=0; i < PointDimension ; ++i)
    {
    m_Coords[i] = CoordRep();
    }
}


/**
 * Constructor which takes a coordinate set.
 */
template <int VPointDimension, typename TCoordRep>
Point< VPointDimension , TCoordRep >
::Point(CoordRep coords[PointDimension])
{
  for(int i=0; i < PointDimension ; ++i)
    {
    m_Coords[i] = coords[i];
    }
}


/**
 * Set coordinates of the point.
 */
template <int VPointDimension, typename TCoordRep>
void
Point< VPointDimension , TCoordRep >
::SetCoords(CoordRep coords[PointDimension])
{
  for(int i=0; i < PointDimension ; ++i)
    {
    m_Coords[i] = coords[i];
    }
}


/**
 * Get coordinates of the point.
 */
template <int VPointDimension, typename TCoordRep>
void
Point< VPointDimension , TCoordRep >
::GetCoords(CoordRep coords[PointDimension]) const
{
  for(int i=0; i < PointDimension ; ++i)
    {
    coords[i] = m_Coords[i];
    }
}

ITK_NAMESPACE_END
