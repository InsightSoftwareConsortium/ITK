/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPoint.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
// #include "itkMesh.h"

/**
 * Default constructor.
 */
template <int VPointDimension, typename TCoordRep>
itkPoint< VPointDimension , TCoordRep >
::itkPoint()
{
  for(int i=0; i < PointDimension ; ++i)
    m_Coords[i] = CoordRep();
}


/**
 * Constructor which takes coordinate set.
 */
template <int VPointDimension, typename TCoordRep>
itkPoint< VPointDimension , TCoordRep >
::itkPoint(CoordRep coords[PointDimension])
{
  for(int i=0; i < PointDimension ; ++i)
    m_Coords[i] = coords[i];
}


/**
 * Set coordinates of point.
 */
template <int VPointDimension, typename TCoordRep>
void
itkPoint< VPointDimension , TCoordRep >
::SetCoords(CoordRep coords[PointDimension])
{
  for(int i=0; i < PointDimension ; ++i)
    m_Coords[i] = coords[i];
}


/**
 * Get coordinates of point.
 */
template <int VPointDimension, typename TCoordRep>
void
itkPoint< VPointDimension , TCoordRep >
::GetCoords(CoordRep coords[PointDimension])
{
  for(int i=0; i < PointDimension ; ++i)
    coords[i] = m_Coords[i];
}
