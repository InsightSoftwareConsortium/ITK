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

#include "itkPolygonGroupSpatialObject.h"

#include <iostream>
#include "itkMath.h"

static float testPoints[11][2] =
  {
    {1,1},{1,2},{1.25,2},{1.25,1.25},{1.75,1.25},
    {1.75,1.5},{1.5,1.5},{1.5,2},{2,2},{2,1},{1,1}
  };

typedef itk::PolygonGroupSpatialObject<3> PolygonGroup3DType;
typedef PolygonGroup3DType::Pointer       PolygonGroup3DPointer;

int
buildPolygonGroup(PolygonGroup3DPointer &PolygonGroup)
{
  try
    {
    for(float z = 0.0; z <= 10.0; z += 1.0)
      {
      itk::PolygonSpatialObject<3>::Pointer strand
        = itk::PolygonSpatialObject<3>::New();
      strand->Print(std::cout);

      if(!PolygonGroup->AddStrand(strand))
        {
        std::cerr << "Error adding point" << std::endl;
        return -1;
        }
      strand->SetThickness(1.0);
      //
      // add all points to this strand.
      for(int i = 0; i < 11; i++)
        {
        double pos[3];
        pos[0] = testPoints[i][0];
        pos[1] = testPoints[i][1];
        pos[2] = z;
        itk::PolygonSpatialObject<3>::PointType curpoint(pos);
        if(!strand->AddPoint(curpoint))
          {
          std::cerr << "Error adding point" << std::endl;
          return -1;
          }
        }
      //
      // check measurements for consistency.
      double area, volume, perimeter;
      area = strand->MeasureArea();
      volume = strand->MeasureVolume();
      perimeter = strand->MeasurePerimeter();
      // since a slice is 1 unit thick, volume should equal area
      if(itk::Math::NotAlmostEquals(area, volume))
        {
        std::cerr << "Polygon: area(" << area << ") != volume("
                  << volume << ")" << std::endl;
        return -1;
        }
      if(z == 0.0)         // just do unit testing for one strand
        {
        //
        // try deleting this strand
        PolygonGroup->DeleteStrand(strand);
        //
        // put it back.
        PolygonGroup->AddStrand(strand);
        //
        // try replacing it.
        PolygonGroup->ReplaceStrand(strand,strand);

        if(!strand->IsClosed())
          {
          std::cerr << "Strand should be closed" << std::endl;
          return -1;
          }
        std::cerr << "Area = " << area
                  << " Volume = " << volume
                  << " Perimeter = " << perimeter
                  << std::endl;

        double pos[3];
        pos[0] = testPoints[5][0];
        pos[1] = testPoints[5][1];
        pos[2] = z;
        itk::PolygonSpatialObject<3>::PointType testpoint(pos);
        if(!strand->DeletePoint(testpoint))
          {
          std::cerr << "Polygon: Delete Point failed" << std::endl;
          }
        //
        // put the same point back
        double p2[3];
        p2[0] = testPoints[4][0];
        p2[1] = testPoints[4][1];
        p2[2] = z;
        itk::PolygonSpatialObject<3>::PointType insertafter(p2);
        if(!strand->InsertPoint(insertafter,testpoint))
          {
          std::cerr << "Polygon: Insert Point failed" << std::endl;
          }
        //
        // try replacing the point.
        if(!strand->ReplacePoint(insertafter,insertafter))
          {
          std::cerr << "Polygon: Replace Point failed" << std::endl;
          }
        //
        // try deleting a segment
        double p3[3];
        p3[0] = testPoints[10][0];
        p3[1] = testPoints[10][1];
        p3[2] = z;
        itk::PolygonSpatialObject<3>::PointType endpt(p3);
        if(!strand->RemoveSegment(testpoint,endpt))
          {
          std::cerr << "Polygon: RemoveSegment failed" << std::endl;
          }
        if(strand->NumberOfPoints() != 5)
          {
          std::cerr << "Polygon: should have 5 points, "
                    << strand->NumberOfPoints() << " returned." << std::endl;
          }
        //
        // put the same points back.
        for(int i = 5; i < 11; i++)
          {
          pos[0] = testPoints[i][0];
          pos[1] = testPoints[i][1];
          pos[2] = z;
          itk::PolygonSpatialObject<3>::PointType curpoint(pos);

          if(!strand->AddPoint(curpoint))
            {
            std::cerr << "Error adding point" << std::endl;
            return -1;
            }
          }
        if(strand->NumberOfPoints() != 11)
          {
          std::cerr << "Polygon: should have 11 points, "
                    << strand->NumberOfPoints() << " returned." << std::endl;
          return -1;
          }
        double area2 =  strand->MeasureArea();
        double perimeter2 = strand->MeasurePerimeter();
        if(itk::Math::NotExactlyEquals(area, area2))
          {
          std::cerr << "Area shouldn't have changed; old = "
                    << area << " new = " << area2 << std::endl;
          return -1;
          }
        if(itk::Math::NotExactlyEquals(perimeter, perimeter2))
          {
          std::cerr << "Perimeter shouldn't have changed; old = "
                    << perimeter << " new = " << perimeter2 << std::endl;
          return -1;
          }
        //
        // check IsInside
        double insidepos[3];
        insidepos[0] = 1.75;
        insidepos[1] = 1.75;
        insidepos[2] = z;
        itk::PolygonSpatialObject<3>::PointType insidepoint(insidepos);

        double outsidepos[3];
        outsidepos[0] = 1.6;
        outsidepos[1] = 1.3;
        outsidepos[2] = z;
        itk::PolygonSpatialObject<3>::PointType outsidepoint(outsidepos);
        if(!strand->IsInside(insidepoint))
          {
          std::cerr << " 1.75, 1.75, 0 should be inside strand" << std::endl;
          return -1;

          }
        if(strand->IsInside(outsidepoint))
          {
          std::cerr << " 1.6, 1.3, 0 should be outside strand" << std::endl;
          return -1;

          }
        }
      }
    }
  catch(itk::ExceptionObject &)
    {
    std::cerr << "Error creating PolygonGroup" << std::endl;
    return -1;
    }
  return 0;
}

int itkPolygonGroupSpatialObjectTest(int, char *[])
{
  PolygonGroup3DPointer PolygonGroup = PolygonGroup3DType::New();

  if(PolygonGroup->NumberOfStrands() != 0)
    {
    std::cerr << "PolygonGroup->NumberOfStrands() returns non zero fo empty PolygonGroup"
              << std::endl;
    return 1;
    }
  if(buildPolygonGroup(PolygonGroup) != 0)
    {
    return 1;
    }
  if(PolygonGroup->NumberOfStrands() != 11)
    {
    std::cerr << "PolygonGroup->NumberOfStrands() returns " << PolygonGroup->NumberOfStrands()
              << " should return 11"
              << std::endl;
    return 1;
    }
  if(!PolygonGroup->IsClosed())
    {
    std::cerr << "PolygonGroup->IsClosed() returned false, should be true"
              << std::endl;
    return 1;
    }
  double vol = PolygonGroup->Volume();
  std::cerr << "PolygonGroup Volume = " << vol << std::endl;
  double insidepos[3];
  insidepos[0] = 1.75;
  insidepos[1] = 1.75;
  insidepos[2] = 5.0;
  itk::PolygonSpatialObject<3>::PointType insidepoint(insidepos);
  if(!PolygonGroup->IsInside(insidepoint))
    {
    std::cerr << "1.75,1.75,5 is inside PolygonGroup, IsInside returns false"
              << std::endl;
    }


  double outsidepos[3];
  outsidepos[0] = 1.6;
  outsidepos[1] = 1.3;
  outsidepos[2] = 11.5;
  itk::PolygonSpatialObject<3>::PointType outsidepoint(outsidepos);
  if(PolygonGroup->IsInside(outsidepoint))
    {
    std::cerr << "1.6,1.3,11.5" << "is not inside PolygonGroup, IsInside returns true"
              << std::endl;
    }

  return 0;
}
