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
#include <iomanip>

#include "itkMath.h"
#include "itkPhasedArray3DSpecialCoordinatesImage.h"
#include "itkResampleImageFilter.h"


enum {NDimensions = 3};

typedef float                                                 PixelType;
typedef itk::PhasedArray3DSpecialCoordinatesImage<PixelType>  InputImageType;
typedef itk::Image<PixelType, NDimensions>                    ImageType;
typedef InputImageType::Pointer             InputImagePointerType;
typedef ImageType::Pointer                  ImagePointerType;
typedef ImageType::RegionType               ImageRegionType;
typedef ImageType::SizeType                 ImageSizeType;
typedef ImageType::IndexType                ImageIndexType;
typedef double                              CoordRepType;


int itkResamplePhasedArray3DSpecialCoordinatesImageTest(int, char* [] )
{
  // Create and configure an image
  InputImagePointerType image = InputImageType::New();
  ImageIndexType  index = {{0,  0,  0}};
  ImageSizeType   size  = {{13, 13, 9}};
  ImageRegionType region;
  region.SetSize ( size );
  region.SetIndex( index );
  image->SetLargestPossibleRegion( region );
  image->SetBufferedRegion( region );
  image->SetAzimuthAngularSeparation( 5.0*2.0*itk::Math::pi/360.0 );
  image->SetElevationAngularSeparation( 5.0*2.0*itk::Math::pi/360.0 );
  image->SetRadiusSampleSize( 0.5 );
  image->SetFirstSampleDistance( 2 );
  image->Allocate();

  // Fill image with isoshells
  std::cout << "\nOriginal 3D Phased Array Data" << std::endl;
  itk::ImageRegionIteratorWithIndex<InputImageType> iter(image, region);
  PixelType value;
  for (; !iter.IsAtEnd(); ++iter) {
    index = iter.GetIndex();
    //value = index[0] + index[1] + index[2];
    value = index[2];
    iter.Set(value);
    // display one slice of the volume
    if ( index[0] == int(size[0]-1)/2 ) {
      std::cout << std::setw(14) << value;
      if ( int(index[1]) == int(size[1]-1) ) {
        std::cout << std::endl;
      }
    }
  }

  // Create and configure a resampling filter
  itk::ResampleImageFilter< InputImageType, ImageType >::Pointer resample;
  resample = itk::ResampleImageFilter< InputImageType, ImageType >::New();
  resample->SetInput(image);

  ImageSizeType cubeSize;
  cubeSize.Fill(7);
  resample->SetSize(cubeSize);

//  ImageType::SpacingType spacing;
//  spacing.Fill( 2.0/3.0 ); // try this w/ a cubeSize.Fill(10) above
//  resample->SetOutputSpacing( spacing );

  ImageType::PointType origin;
  origin[0] = -3.0;
  origin[1] = -3.0;
  origin[2] = 1.0;
  resample->SetOutputOrigin( origin );

//  index.Fill( 0 );
//  resample->SetOutputStartIndex( index );

  // Run the resampling filter
  resample->Update();

  // Check if desired results were obtained
  bool passed = true;
  std::cout << "\nData resampled onto a translated Cartesian image grid." << std::endl;
  ImageType::RegionType region2;
  region2 = resample->GetOutput()->GetRequestedRegion();
  itk::ImageRegionIteratorWithIndex<ImageType>
      iter2(resample->GetOutput(), region2);
  for (; !iter2.IsAtEnd(); ++iter2) {
    index  = iter2.GetIndex();
    value  = iter2.Get();
    // display one slice of the volume
    if ( index[0] == int(cubeSize[0]-1)/2 ) {
      std::cout << std::setw(14) << value;
      if ( int(index[1]) == int(cubeSize[1]-1) ) {
        std::cout << std::endl;
      }
      // check the values down a portion of the z-axis
      if( index[1] == int(cubeSize[1]-1)/2 && 2 <= index[2] && index[2] <= 5 ) {
        if( itk::Math::NotAlmostEquals( value, (index[2]-1)*2 ) ) {
          std::cout << " (Error in resampled image: Pixel " << index
                    << " = " << value
                    << ", expecting " << (index[2]-1)*2 << ")"
                    << std::endl;
          passed = false;
        }
      }
    }
  }
  if( !passed )
    std::cout << "Forward Sampling Failed!!!\n\n" << std::endl;
  else
    std::cout << "Forward Sampling Passed\n" << std::endl;

  // Create and configure an image to be a restored "copy" of the input
  InputImagePointerType image2;
  // Create and configure a back-resampling filter
  itk::ResampleImageFilter< ImageType, InputImageType >::Pointer backResample;
  backResample = itk::ResampleImageFilter< ImageType, InputImageType >::New();
  backResample->SetInput(resample->GetOutput());
  backResample->SetSize(size);
  // ResampleImageFilter was not designed for special-coordinates images, so we
  // MUST provide the physical-spacing parameters ourselves before calling Update.
  image2 = backResample->GetOutput();
  image2->SetAzimuthAngularSeparation( 5.0*2.0*itk::Math::pi/360.0 );
  image2->SetElevationAngularSeparation( 5.0*2.0*itk::Math::pi/360.0 );
  image2->SetRadiusSampleSize( 0.5 );
  image2->SetFirstSampleDistance( 2 );

  // Run the back-resampling filter
  backResample->Update();
  // Check how close we were to the original image
  std::cout << "\nResampled recovery of 3D Phased Array Data from Cartesian space" << std::endl;
  itk::ImageRegionIteratorWithIndex<InputImageType>
      iter3(backResample->GetOutput(), region);
  for (; !iter3.IsAtEnd(); ++iter3) {
    index  = iter3.GetIndex();
    value  = iter3.Get();
    // display one slice of the volume
    if ( index[0] == int(size[0]-1)/2 ) {
      std::cout << std::setw(14) << value;
      if ( int(index[1]) == int(size[1]-1) ) {
        std::cout << std::endl;
      }
      // check the values down the z-axis
      if( index[1] == int(size[1]-1)/2 ) {
        if( itk::Math::NotAlmostEquals( value, index[2] ) ) {
          std::cout << " (Error in resampled image: Pixel " << index
                    << " = " << value
                    << ", expecting " << index[2] << ")"
                    << std::endl;
          passed = false;
        }
      }
    }
  }
  if( !passed )
    std::cout << "Resampling back to Phased Array coordinates Failed!!!\n\n" << std::endl;
  else
    std::cout << "Resampling back to Phased Array coordinates Passed\n" << std::endl;

  if (!passed) {
    std::cout << "Resampling PhasedArray3DSpecialCoordinatesImage test failed" << std::endl;
    return EXIT_FAILURE;
  }

 std::cout << "Test passed." << std::endl;
 return EXIT_SUCCESS;

}
