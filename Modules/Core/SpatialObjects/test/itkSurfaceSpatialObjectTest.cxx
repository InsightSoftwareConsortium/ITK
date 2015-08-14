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
// Disable warning for long symbol names in this file only

/**
 * This is a test file for the itkSurfaceSpatialObject class.
 */

#include "itkSurfaceSpatialObject.h"
#include "itkMath.h"

int itkSurfaceSpatialObjectTest(int, char* [])
{
  typedef itk::SurfaceSpatialObject<3>      SurfaceType;
  typedef SurfaceType::Pointer              SurfacePointer;
  typedef itk::SurfaceSpatialObjectPoint<3> SurfacePointType;
  typedef itk::CovariantVector<double,3>    VectorType;

  std::cout<<"=================================="<<std::endl;
  std::cout<<"Testing SurfaceSpatialObject:"<<std::endl<<std::endl;

  SurfaceType::PointListType list;
  unsigned int i;
  for(i=0; i<10; i++)
  {
    SurfacePointType p;
    p.SetPosition(i,i+1,i+2);
    VectorType normal;
    for(unsigned int j=0;j<3;j++)
    {
      normal[j]=j;
    }
    p.SetNormal(normal);
    list.push_back(p);
  }

   // For coverage
  SurfacePointType p;
  p.SetPosition(1,2,3);
  p.Print(std::cout);

  // Create a Surface Spatial Object
  SurfacePointer Surface = SurfaceType::New();
  Surface->GetProperty()->SetName("Surface 1");
  Surface->SetId(1);
  Surface->SetPoints(list);

  Surface->ComputeBoundingBox();

 // Number of points
  std::cout << "Testing Consistency: " << std::endl;
  std::cout << "Number of Points: ";

  if(Surface->GetPoints().size() != 10)
  {
    std::cout<<"[FAILED]"<<std::endl;
    return EXIT_FAILURE;
  }
  else
  {
    std::cout<<"[PASSED]"<<std::endl;
  }

  // Point consistency
  std::cout << "Point consistency: ";

  SurfaceType::PointListType::const_iterator it = Surface->GetPoints().begin();

  i=0;
  while(it != Surface->GetPoints().end())
    {
    for(unsigned int d=0;d<3;d++)
      {
      if(itk::Math::NotExactlyEquals((*it).GetPosition()[d], i+d))
        {
        std::cout<<"[FAILED]"<<std::endl;
        return EXIT_FAILURE;
        }

      if(itk::Math::NotExactlyEquals((*it).GetNormal()[d], d))
        {
        std::cout<<"[FAILED]"<<std::endl;
        return EXIT_FAILURE;
        }
      }
    it++;
    i++;
  }

  std::cout<<"[PASSED]"<<std::endl;

  // Point consistency
  std::cout << "Is Inside: ";
  itk::Point<double,3> in;
  in[0]=1;in[1]=2;in[2]=3;
  itk::Point<double,3> out;
  out[0]=0;out[1]=0;out[2]=0;

  if(!Surface->IsInside(in))
  {
    std::cout<<"[FAILED]"<<std::endl;
    return EXIT_FAILURE;
  }

  if(Surface->IsInside(out))
  {
    std::cout<<"[FAILED]"<<std::endl;
    return EXIT_FAILURE;
  }

  std::cout<<"[PASSED]"<<std::endl;

  // Testing IsEvaluableAt()
  std::cout << "IsEvaluableAt: ";
  if(!Surface->IsEvaluableAt(in) || Surface->IsEvaluableAt(out))
  {
     std::cout<<"[FAILED]"<<std::endl;
     return EXIT_FAILURE;
  }
  std::cout<<"[PASSED]"<<std::endl;


  // Testing IsEvaluableAt()
  std::cout << "ValueAt: ";

  double value;
  if(!Surface->ValueAt(in,value))
  {
     std::cout<<"[FAILED]"<<std::endl;
     return EXIT_FAILURE;
  }

  if(itk::Math::NotExactlyEquals(value, 1))
  {
     std::cout<<"[FAILED]"<<std::endl;
     return EXIT_FAILURE;
  }
  std::cout<<"[PASSED]"<<std::endl;

  return EXIT_SUCCESS;

}
