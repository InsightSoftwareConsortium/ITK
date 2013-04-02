/*=========================================================================
 *
 *  Copyright Insight Software Consortium
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

#include "itkPointSet.h"
#include "vnl/vnl_sample.h"

#include <iostream>

/**
 * Define a PointSet type that stores a PixelType of "int".  Use the defaults
 * for the other template parameters.
 */
typedef itk::PointSet<int>  PointSet;
typedef PointSet::PointType PointType;

/**
 * The point set that is created consists of a 100 random points.
 */

int itkPointSetTest(int, char* [] )
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

  try
    {
    for(int i=0; i < 100; ++i)
      {
      testPointCoords[0] = (PointSet::CoordRepType)
        vnl_sample_uniform((double)-1.0,(double)1.0);
      testPointCoords[1] = (PointSet::CoordRepType)
        vnl_sample_uniform((double)-1.0,(double)1.0);
      testPointCoords[2] = (PointSet::CoordRepType)
        vnl_sample_uniform((double)-1.0,(double)1.0);
      pset->SetPoint(i, PointType(testPointCoords));
      }
    }
  catch(...)
    {
    std::cerr << "Error setting points." << std::endl;
    return EXIT_FAILURE;
    }
  return EXIT_SUCCESS;
}
