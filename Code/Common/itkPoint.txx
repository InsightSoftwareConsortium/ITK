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

namespace itk
{

/**
 * Default constructor.
 */
template <int VPointDimension, typename TCoordRep>
Point< VPointDimension , TCoordRep >
::Point()
{
  for(int i=0; i < PointDimension ; ++i)
    {
    m_Coords[i] = CoordRepType();
    }
}


/**
 * Constructor which takes a coordinate set.
 */
template <int VPointDimension, typename TCoordRep>
Point< VPointDimension , TCoordRep >
::Point(CoordRepType coords[PointDimension])
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
::SetCoords(CoordRepType coords[PointDimension])
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
::GetCoords(CoordRepType coords[PointDimension]) const
{
  for(int i=0; i < PointDimension ; ++i)
    {
    coords[i] = m_Coords[i];
    }
}

} // end namespace itk
