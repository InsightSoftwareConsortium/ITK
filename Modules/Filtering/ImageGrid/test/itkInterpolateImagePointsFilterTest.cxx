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
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/

#include <iostream>

#include "itkInterpolateImagePointsFilter.h"
#include "itkStdStreamStateSave.h"

#include "itkGaussianImageSource.h"
#include "itkImageToImageFilter.h"
#include "itkMath.h"
#include "itkTestingMacros.h"


  typedef double InputPixelType;
  typedef double CoordValueType;


  // Setup for 2D Images
  enum { ImageDimension2D = 2 };

  typedef itk::Image< InputPixelType, ImageDimension2D > ImageType2D;
  typedef ImageType2D::Pointer                           ImageType2DPointer;
  typedef ImageType2D::SizeType                          ImageType2DSizeType;
  typedef itk::InterpolateImagePointsFilter<ImageType2D,ImageType2D,CoordValueType>
                                                          InterpolatorType2D;

  typedef itk::Image< CoordValueType, ImageDimension2D > CoordImageType2D;
  typedef CoordImageType2D::Pointer                      CoordImageType2DPointer;
  typedef CoordImageType2D::SizeType                     CoordImage2DSizeType;

  void set2DInterpolateImagePointsFilterData(ImageType2D::Pointer);

  // Setup for 3D Images
  enum { ImageDimension3D = 3 };

  typedef itk::Image< InputPixelType, ImageDimension3D > ImageType3D;
  typedef ImageType3D::Pointer                           ImageTypePtr3D;
  typedef ImageType3D::SizeType                          SizeType3D;
  typedef ImageType3D::IndexType                         IndexType3D;
  typedef itk::InterpolateImagePointsFilter<ImageType3D,ImageType3D,CoordValueType>
                                                          InterpolatorType3D;

  typedef itk::Image< CoordValueType, ImageDimension3D > CoordImageType3D;
  typedef CoordImageType3D::Pointer                      CoordImageType3DPointer;
  typedef CoordImageType3D::SizeType                     CoordSizeType3D;
  typedef CoordImageType3D::IndexType                    CoordIndexType3D;

  ImageTypePtr3D set3DData();


/** test2DInterpolateImagePointsFilter() Tests InterpolateImagePointsFilter for
   * expected results at a handful of index locations.
   */
int test2DInterpolateImagePointsFilter()
{
  int testStatus = EXIT_SUCCESS;

  std::cout << "Testing 2D InterpolateImagePointsFilter at sample index locations.\n ";

  // Initialize input image
  ImageType2DPointer image = ImageType2D::New();
  set2DInterpolateImagePointsFilterData(image);
  // Using Index Coordinates so setting of origin and spacing should
  // not change results.
  double origin [] = { 5.5, 1.0 };
  double spacing[] = { 5.1, 0.5 };
  image->SetOrigin(origin);
  image->SetSpacing(spacing);

  // Initialize the sample data
  const int NPOINTS2 = 4;  // number of points
  const double DEFAULTPIXELVALUE = 1.23;  // Arbitrary value to test setting

  double xcoord[NPOINTS2] = { 0.1, 3.4, 4.0, 2.0};
  double ycoord[NPOINTS2] = { 0.2, 5.8, 6.0, 7.0};
  double truth[NPOINTS2] = {151.650316034, 22.411473093, 36.2, DEFAULTPIXELVALUE};

  // Place continuous index coordinates into an image data structure
  CoordImageType2DPointer index1 = CoordImageType2D::New();
  CoordImageType2DPointer index2 = CoordImageType2D::New();

  CoordImage2DSizeType size = {{2,2}};
  CoordImageType2D::RegionType region;
  region.SetSize( size );

  // x coordinates
  index1->SetLargestPossibleRegion( region );
  index1->SetBufferedRegion( region );
  index1->Allocate();

  // y coordinates
  index2->SetLargestPossibleRegion( region );
  index2->SetBufferedRegion( region );
  index2->Allocate();

  // Setup copy iterators
  typedef itk::ImageRegionIterator<CoordImageType2D>  InputIterator;
  InputIterator inIter1( index1, region );
  InputIterator inIter2( index2, region );

  // copy coordinate values into image data types
  unsigned int j = 0;
  while( !inIter1.IsAtEnd() )
    {
    inIter1.Set(xcoord[j]);
    inIter2.Set(ycoord[j]);
    ++inIter1;
    ++inIter2;
    ++j;
    }

  // Initialize InterpolateImagePointsFilter
  InterpolatorType2D::Pointer resamp = InterpolatorType2D::New();

  EXERCISE_BASIC_OBJECT_METHODS( resamp, InterpolateImagePointsFilter, ImageToImageFilter );

  unsigned int splineOrder = 3;
  resamp->GetInterpolator()->SetSplineOrder(splineOrder);
  resamp->SetInputImage(image);
  resamp->SetInterpolationCoordinate(index1, 0);
  resamp->SetInterpolationCoordinate(index2, 1);

  InterpolatorType2D::PixelType defaultPixelValue = DEFAULTPIXELVALUE;
  resamp->SetDefaultPixelValue( defaultPixelValue );

  TEST_SET_GET_VALUE( defaultPixelValue, resamp->GetDefaultPixelValue() );

  resamp->Update();
  resamp->Print(std::cout);

  // Get results and compare for accuracy
  ImageType2DPointer outputImage;
  outputImage = resamp->GetOutput();
  InputIterator outIter(outputImage,region);
  int i = 0;
  double epsilon = 1e-9;
  while ( !outIter.IsAtEnd() )
    {
    double value = outIter.Get();
    std::cout.width(10);
    std::cout.precision( unsigned( itk::Math::abs( std::log10( epsilon ) ) ) );
    std::cout << "Checking image value: " << value << std::endl;
    if( !itk::Math::FloatAlmostEqual( value, truth[i], 10, epsilon ) )
      {
      std::cout << "*** Error: value should be " << truth[i] << std::endl;
      testStatus = EXIT_FAILURE;
      }
    else
      {
      std::cout << "*** test2DInterpolateImagePointsFilter() Passed.\n" << std::endl;
      }
    ++outIter;
    ++i;
    }
  std::cout << std::endl;

  return testStatus;
}

int test3DInterpolateImagePointsFilter()
{
  int testStatus = EXIT_SUCCESS;

  std::cout << "Testing 3D InterpolateImagePointsFilter.\n ";

  // Initialize input image
  ImageTypePtr3D image = set3DData();

  // Initialize InterpolateImagePointsFilter and set input image
  InterpolatorType3D::Pointer resamp = InterpolatorType3D::New();

  EXERCISE_BASIC_OBJECT_METHODS( resamp, InterpolateImagePointsFilter, ImageToImageFilter );

  unsigned int splineOrder = 3;
  resamp->GetInterpolator()->SetSplineOrder(splineOrder);
  resamp->SetInputImage(image);

  // Generate coordinates at original index locations
  SizeType3D size = image->GetLargestPossibleRegion().GetSize();
  CoordImageType3DPointer coord[ImageDimension3D]; // = CoordImageType2D::New();
  CoordImageType3D::RegionType region;
  region.SetSize(size);
  for (int i = 0; i < ImageDimension3D; i++)
    {
    CoordImageType3DPointer temp = CoordImageType3D::New();
    coord[i] = temp;
    (coord[i])->SetLargestPossibleRegion( region );
    (coord[i])->SetBufferedRegion( region );
    (coord[i])->Allocate();
    }

  CoordIndexType3D index;
    for (unsigned int i0 = 0; i0 < size[0]; i0++)
      {
      index[0] = i0;
      for (unsigned int i1 = 0; i1 < size[1]; i1++)
        {
        index[1] = i1;
        for (unsigned int i2 = 0; i2 < size[2]; i2++)
          {
          index[2] = i2;
          (coord[0])->SetPixel(index, i0);
          (coord[1])->SetPixel(index, i1);
          (coord[2])->SetPixel(index, i2);
          }
        }
      }
    for (unsigned int i = 0; i < ImageDimension3D; i++)
      {
      resamp->SetInterpolationCoordinate(coord[i],i);
      }

  resamp->Update();

  // Get results and compare for accuracy
  ImageTypePtr3D outputImage;
  outputImage = resamp->GetOutput();

  // Calculate RMSE
  // First set up iterators
  typedef itk::ImageRegionIterator<ImageType3D>      InputIterator;
  typedef itk::ImageRegionIterator<CoordImageType3D> OutputIterator;
  InputIterator inIter(image,region);
  OutputIterator outIter(outputImage,region);
  double rmse;
  rmse = 0.0;
  while ( !outIter.IsAtEnd() )
    {
    double temp = inIter.Get() - outIter.Get();
    rmse += temp*temp;
    ++outIter;
    ++inIter;
    }
  rmse = std::sqrt( (rmse / size[0] / size[1] / size[2] ) );

  // Write home and let mom & dad know how we're doing.
  std::cout << "rmse of image is " << rmse << "\n ";
  double epsilon = 1e-7;
  std::cout.precision( unsigned( itk::Math::abs( std::log10( epsilon ) ) ) );
  if ( !itk::Math::FloatAlmostEqual( rmse, (double)0 , 10, epsilon ) )
    {
    std::cout << "*** Error: rmse is larger than expected." << std::endl;
    testStatus = EXIT_FAILURE;
    }
  else
    {
    std::cout << "*** test3DInterpolateImagePointsFilter() Passed.\n" << std::endl;
    }

  return testStatus;
}

int itkInterpolateImagePointsFilterTest( int, char * [] )
{
  // Save the format stream variables for std::cout
  // They will be restored when coutState goes out of scope
  // scope.
  itk::StdStreamStateSave coutState(std::cout);

  int testStatus = EXIT_SUCCESS;

  std::cout << "Testing InterpolateImagePointsFilter class:\n";

  testStatus += test2DInterpolateImagePointsFilter();
  testStatus += test3DInterpolateImagePointsFilter();

  // Return results of test
  if (testStatus != 0)
    {
    std::cout << "\n*** " << testStatus << " tests failed" << std::endl;
    return EXIT_FAILURE;
    }
  else
    {
    std::cout << "\nAll tests successfully passed\n" << std::endl;
    return EXIT_SUCCESS;
    }

}

void set2DInterpolateImagePointsFilterData(ImageType2D::Pointer imgPtr)
{
  ImageType2DSizeType size = {{7,7}};
  double mydata[ 49 ] = {
    154.5000,   82.4000,   30.9000,         0,  -10.3000,         0,   30.9000 ,
    117.0000,   62.4000,   23.4000,         0,   -7.8000,         0,   23.4000 ,
     18.0000,    9.6000,    3.6000,         0,   -1.2000,         0,    3.6000 ,
   -120.0000,  -64.0000,  -24.0000,         0,    8.0000,         0,  -24.0000 ,
   -274.5000, -146.4000,  -54.9000,         0,   18.3000,         0,  -54.9000 ,
   -423.0000, -225.6000,  -84.6000,         0,   28.2000,         0,  -84.6000 ,
   -543.0000, -289.6000, -108.6000,         0,   36.2000,         0, -108.6000 };

  ImageType2D::RegionType region;
  region.SetSize( size );

  imgPtr->SetLargestPossibleRegion( region );
  imgPtr->SetBufferedRegion( region );
  imgPtr->Allocate();

  typedef itk::ImageRegionIterator<ImageType2D>  InputIterator;

  InputIterator inIter( imgPtr, region );

  int j = 0;
  while( !inIter.IsAtEnd() )
    {
    inIter.Set(mydata[j]);
    ++inIter;
    ++j;
    }
}


ImageTypePtr3D set3DData()
{
  // Create a Gaussian image source
  typedef itk::GaussianImageSource< ImageType3D > GaussianSourceType;
  GaussianSourceType::Pointer pSource = GaussianSourceType::New();

  ImageType3D::SpacingValueType spacing[] = { 1.2f, 1.3f, 1.4f };
  ImageType3D::PointValueType origin[] = { 1.0f, 4.0f, 2.0f };
  ImageType3D::SizeValueType    size[] = { 65, 75, 60};

  GaussianSourceType::ArrayType mean;
  mean[0] = size[0]/2.0f + origin[0];
  mean[1] = size[1]/2.0f + origin[1];
  mean[2] = size[2]/2.0f + origin[2];

  GaussianSourceType::ArrayType sigma;
  sigma[0] = 12.5f;
  sigma[1] = 17.5f;
  sigma[2] = 27.5f;

  pSource->SetSize( size );
  pSource->SetOrigin( origin );
  pSource->SetSpacing( spacing );
  pSource->SetMean( mean );
  pSource->SetSigma( sigma );

  // Get the output of the source
  ImageTypePtr3D pImage = pSource->GetOutput();

  // Run the pipeline
  pSource->Update();

  return pImage;

}
