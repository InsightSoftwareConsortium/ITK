/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkPointSetTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

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

