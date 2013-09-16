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

#include "itkTimeProbesCollectorBase.h"
#include "itkImage.h"

#include <iostream>
#include <fstream>

template <typename T>
void TestTransformIndexToPhysicalPoint(T * image)
{
  typename T::IndexType index3D;
  typename T::PointType point3D;

    for (int k = 0; k < 10; k++)
    {
    index3D[2] = k;
    for (int j = 0; j < 1000; j++)
      {
      index3D[1] = j;
      for (int i = 0; i < 1000; i++)
        {
        index3D[0] = i;
        image->TransformIndexToPhysicalPoint(index3D, point3D);
        }
      }
    if (k == 5) std::cout << point3D << std::endl;
    }
}
template <typename T>
void TestTransformPhysicalPointToIndex(T * image)
{
  typename T::IndexType index3D;
  typename T::PointType point3D;

    for (double k = 0; k < 10; k++)
    {
    point3D[2] = k;
    for (double j = 0; j < 1000; j++)
      {
      point3D[1] = j;
      for (double i = 0; i < 1000; i++)
        {
        point3D[0] = i;
        image->TransformPhysicalPointToIndex(point3D, index3D);
        }
      }
    if (k == 5) std::cout << point3D << std::endl;
    }
}
//-------------------------
//
//   This file test the interface to the TimeProbe classes
//
//-------------------------
int itkTimeProbesTest(int, char* [] )
{

  itk::TimeProbesCollectorBase   collector;

  const unsigned int N =  1000L;
  const unsigned int M = 10000L;

  collector.Start("Loop1"); // label that identify the range of the probe

  // Do slow stuff here...
  {
  for(unsigned int i=0; i<N; i++)
    {
    char * dummy = new char [ M ];
    for(unsigned int j=0; j<M; j++)
      {
      dummy[j] = j;
      }
    delete[] dummy;
    }
  }
  collector.Stop("Loop1");

  collector.Start("Loop2"); // label that identify the range of the probe

  // Do other slow stuff here...
  {
  for(unsigned int i=0; i<M; i++)
    {
    char * dummy = new char [ N ];
    for(unsigned int j=0; j<N; j++)
      {
      dummy[j] = j;
      }
    delete[] dummy;
    }
  }
  collector.Stop("Loop2");

  typedef itk::Image<float,3> Image3DType;

  Image3DType::Pointer image3D = Image3DType::New();
  Image3DType::Pointer orientedImage3D = Image3DType::New();

  Image3DType::PointType point3D;

  typedef itk::ImageRegion< 3 >   Region3DType;
  typedef Region3DType::IndexType Index3DType;
  typedef Region3DType::SizeType  Size3DType;
  Region3DType region3D;
  Size3DType size3D = {{ 1000, 1000, 1000 }};

  region3D.SetSize(  size3D );
  collector.Start("i->TransformIndexToPhysicalPoint");
  TestTransformIndexToPhysicalPoint<Image3DType> (image3D);
  collector.Stop("i->TransformIndexToPhysicalPoint");

  collector.Start("i->TransformPhysicalPointToIndex");
  TestTransformPhysicalPointToIndex<Image3DType> (image3D);
  collector.Stop("i->TransformPhysicalPointToIndex");

  collector.Start("o->TransformIndexToPhysicalPoint");
  TestTransformIndexToPhysicalPoint<Image3DType> (orientedImage3D);
  collector.Stop("o->TransformIndexToPhysicalPoint");

  collector.Start("o->TransformPhysicalPointToIndex");
  TestTransformPhysicalPointToIndex<Image3DType> (orientedImage3D);
  collector.Stop("o->TransformPhysicalPointToIndex");

  // Print the results of the time probes
  collector.Report();

  // Test writing to a ostream
  std::ofstream  logfile;
  logfile.open("itkTimeProbesTest.txt");
  collector.Report( logfile );
  logfile.close();

  // Print to the standar error
  collector.Report( std::cerr );


  return EXIT_SUCCESS;

}
