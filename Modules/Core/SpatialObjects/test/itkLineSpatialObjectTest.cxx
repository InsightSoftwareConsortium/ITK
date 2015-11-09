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

/**
 * This is a test file for the itkLineSpatialObject class.
 */

#include "itkLineSpatialObject.h"
#include "itkMath.h"

int itkLineSpatialObjectTest(int, char* [])
{
  typedef itk::LineSpatialObject<3>      LineType;
  typedef LineType::Pointer              LinePointer;
  typedef LineType::LinePointType        LinePointType;
  typedef itk::CovariantVector<double,3> VectorType;

  std::cout<<"=================================="<<std::endl;
  std::cout<<"Testing LineSpatialObject:"<<std::endl<<std::endl;

  LineType::PointListType list;
  unsigned int i;
  for(i=0; i<10; i++)
  {
    LinePointType p;
    p.SetPosition(i,i+1,i+2);
    VectorType normal1;
    VectorType normal2;
    for(unsigned int j=0;j<3;j++)
    {
      normal1[j]=j;
      normal2[j]=j*2;
    }

    p.SetNormal(normal1,0);
    p.SetNormal(normal2,1);
    list.push_back(p);
  }

  // For coverage
  LinePointType p;
  p.SetPosition(0,1,2);
  p.Print(std::cout);

  // Create a Line Spatial Object
  LinePointer Line = LineType::New();
  Line->GetProperty()->SetName("Line 1");
  Line->SetId(1);
  Line->SetPoints(list);
  Line->ComputeBoundingBox();

 // Number of points
  std::cout << "Testing Consistency: " << std::endl;
  std::cout << "Number of Points: ";

  if(Line->GetPoints().size() != 10)
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

  LineType::PointListType::const_iterator it = Line->GetPoints().begin();

  i=0;
  while(it != Line->GetPoints().end())
    {
    for(unsigned int d=0;d<3;d++)
      {
      if(itk::Math::NotExactlyEquals((*it).GetPosition()[d], i+d))
        {
        std::cout<<"[FAILED]"<<std::endl;
        return EXIT_FAILURE;
        }

      if(itk::Math::NotExactlyEquals(((*it).GetNormal(0))[d], d))
        {
        std::cout<<"[FAILED]"<<std::endl;
        return EXIT_FAILURE;
        }

      if(itk::Math::NotExactlyEquals(((*it).GetNormal(1))[d], 2*d))
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

  if(!Line->IsInside(in))
  {
    std::cout<<"[FAILED]"<<std::endl;
    return EXIT_FAILURE;
  }

  if(Line->IsInside(out))
  {
    std::cout<<"[FAILED]"<<std::endl;
    return EXIT_FAILURE;
  }
  std::cout<<"[PASSED]"<<std::endl;

  // Testing IsEvaluableAt()
  std::cout << "IsEvaluableAt: ";
  if(!Line->IsEvaluableAt(in) || Line->IsEvaluableAt(out))
  {
     std::cout<<"[FAILED]"<<std::endl;
     return EXIT_FAILURE;
  }
  std::cout<<"[PASSED]"<<std::endl;


  // Testing IsEvaluableAt()
  std::cout << "ValueAt: ";

  double value;
  if(!Line->ValueAt(in,value))
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
