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

#include <iostream>
#include "itkPhasedArray3DSpecialCoordinatesImage.h"
#include "itkWindowedSincInterpolateImageFunction.h"

int itkPhasedArray3DSpecialCoordinatesImageTest(int, char* [] )
{
  bool passed = true;

  typedef itk::PhasedArray3DSpecialCoordinatesImage<float>  Image;
  typedef Image::RegionType                                 RegionType;
  typedef Image::SizeType                                   SizeType;
  typedef Image::PointType                                  PointType;
  typedef Image::IndexType                                  IndexType;
  typedef itk::ContinuousIndex<itk::SpacePrecisionType,3>   ContinuousIndexType;

  Image::Pointer image = Image::New();
  //image->DebugOn();
  //image->GetSource();
  SizeType size;
  size.Fill(5); // 5x5x5 sampling of data
  RegionType region;
  region.SetSize(size);
  image->SetRegions(region);
  image->DisconnectPipeline();

  /**  Set the number of radians between each azimuth unit.   **/
  image->SetAzimuthAngularSeparation( 15.0*2.0*itk::Math::pi/360.0 );

  /**  Set the number of radians between each elevation unit.   **/
  image->SetElevationAngularSeparation( 15.0*2.0*itk::Math::pi/360.0 );

  /**  Set the number of cartesian units between each unit along the R .  **/
  image->SetRadiusSampleSize( 1 );

  /**  Set the distance to add to the radius. */
  image->SetFirstSampleDistance( 2 );

  image->Print(std::cout);
  std::cout<<std::endl;


  PointType point;
  IndexType index;
  ContinuousIndexType continuousIndex;

  point.Fill(0.05); point[2]=3.1;
  image->TransformPhysicalPointToIndex(point, index);
  std::cout<<"Point "<<point<<" -> Index "<<index<<std::endl;
  image->TransformPhysicalPointToContinuousIndex(point, continuousIndex);
  std::cout<<"Point "<<point<<" -> Continuous Index "<<continuousIndex<<std::endl;

  if(     index[0] != 2
      ||  index[1] != 2
      ||  index[2] != 1 )
  {
    passed = false;
  }

  index.Fill(1);
  image->TransformIndexToPhysicalPoint(index, point);
  std::cout<<"Index "<<index<<" -> Point "<<point<<std::endl;

  continuousIndex.Fill(3.5);
  image->TransformContinuousIndexToPhysicalPoint(continuousIndex, point);
  std::cout<<"Continuous Index "<<continuousIndex<<" -> Point "<<point<<std::endl;

  continuousIndex.Fill(2.0);
  image->TransformContinuousIndexToPhysicalPoint(continuousIndex, point);
  std::cout<<"Continuous Index "<<continuousIndex<<" -> Point "<<point<<std::endl;

  if(     point[0] < -0.001 || point[0] > 0.001
      ||  point[1] < -0.001 || point[1] > 0.001
      ||  point[2] < -4.001 || point[2] > 4.001 )
  {
    passed = false;
  }

  typedef itk::WindowedSincInterpolateImageFunction< Image, 3 > WindowedSincInterpolatorType;
  WindowedSincInterpolatorType::Pointer interpolator = WindowedSincInterpolatorType::New();
  interpolator->SetInputImage( image );

  std::cout<<std::endl;

  if (passed)
  {
    std::cout << "PhasedArray3DSpecialCoordinatesImage tests passed" << std::endl;
    return EXIT_SUCCESS;
  }
  else
  {
    std::cout << "PhasedArray3DSpecialCoordinatesImage tests failed" << std::endl;
    return EXIT_FAILURE;
  }
}
