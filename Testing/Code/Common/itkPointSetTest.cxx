/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkPointSetTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


Copyright (c) 2000 National Library of Medicine
All rights reserved.

See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include <iostream>
#include <string>

#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif

#include "itkPointSet.h"
#include "vnl/vnl_sample.h"

/**
 * Define a PointSet type that stores a PixelType of "int".  Use the defaults
 * for the other template parameters.
 */
typedef itk::PointSet<int>  PointSet;
typedef PointSet::PointType  PointType;

/**
 * The point set that is created consists of a 100 random points.
 */

int main(void)
{
  /**
   * Define the 3d geometric positions for 8 points in a cube.
   */
  PointSet::CoordRepType testPointCoords[3];
  
  /**
   * Create the point set through its object factory.
   */
  PointSet::Pointer pset(PointSet::New());  

  /**
   * Add our test points to the mesh.
   * pset->SetPoint(pointId, point)
   * Note that the constructor for Point is public, and takes an array
   * of coordinates for the point.
   */
  for(int i=0; i < 100 ; ++i)
    {
    testPointCoords[0] = (PointSet::CoordRepType) 
      vnl_sample_uniform((double)-1.0,(double)1.0);
    testPointCoords[1] = (PointSet::CoordRepType) 
      vnl_sample_uniform((double)-1.0,(double)1.0);
    testPointCoords[2] = (PointSet::CoordRepType) 
      vnl_sample_uniform((double)-1.0,(double)1.0);
    pset->SetPoint(i, PointType(testPointCoords));
    }

  /**
   * Perform some geometric operations (coordinate transformations)
   * to see if they are working.
   */
  PointSet::CoordRepType coords[PointSet::PointDimension];
  PointSet::PointIdentifier pointId;
  pset->FindClosestPoint(coords,&pointId);

  /**
   * Compute the bounding box of the mesh
   */
  typedef itk::BoundingBox<PointSet::PointIdentifier,PointSet::PointDimension,
    PointSet::CoordRepType,PointSet::PointsContainer> BoundingBox;

  BoundingBox::Pointer bbox(BoundingBox::New());
  bbox->SetPoints(pset->GetPoints());
  bbox->ComputeBoundingBox();
  std::cout << bbox << std::endl;

  return 0;  
}

