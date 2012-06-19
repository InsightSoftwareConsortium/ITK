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

#include "itkAffineTransform.h"
#include "itkResampleImageFilter.h"
#include "itkTimeProbe.h"

int itkResampleImageTest4(int argc, char * argv [] )
{

  const unsigned int NDimensions = 2;

  typedef float                  PixelType;

  typedef itk::Image<PixelType, NDimensions>  ImageType;
  typedef ImageType::IndexType                ImageIndexType;
  typedef ImageType::Pointer                  ImagePointerType;
  typedef ImageType::RegionType               ImageRegionType;
  typedef ImageType::SizeType                 ImageSizeType;

  typedef double                  CoordRepType;

  typedef itk::AffineTransform<CoordRepType,NDimensions>   AffineTransformType;

  typedef itk::LinearInterpolateImageFunction<ImageType,CoordRepType>  InterpolatorType;


  float scaling = 10.0;
  if (argc > 1)
    {
    scaling = atof( argv[1] );
    }

  // Create and configure an image
  ImagePointerType image = ImageType::New();
  ImageIndexType  index = {{0,  0}};
  ImageSizeType   size  = {{64,64}};
  ImageRegionType region;
  region.SetSize ( size );
  region.SetIndex( index );
  image->SetLargestPossibleRegion( region );
  image->SetBufferedRegion( region );
  image->Allocate();

  unsigned int newDims = static_cast<unsigned int>( 64*scaling );
  ImageSizeType osize = {{newDims, newDims}};

  ImageType::SpacingType spacing;
  spacing[0] = size[0] / static_cast<double>(osize[0]);
  spacing[1] = size[1] / static_cast<double>(osize[1]);

  // Fill image with a ramp
  itk::ImageRegionIteratorWithIndex<ImageType> iter(image, region);
  PixelType value;
  for (iter.GoToBegin(); !iter.IsAtEnd(); ++iter)
    {
    index = iter.GetIndex();
    value = index[0] + index[1];
    iter.Set(value);
    }

  // Create an affine transformation
  AffineTransformType::Pointer aff = AffineTransformType::New();
  aff->Scale(0.9);

  // Create a linear interpolation image function
  InterpolatorType::Pointer interp = InterpolatorType::New();
  interp->SetInputImage(image);

  // Create and configure a resampling filter
  itk::ResampleImageFilter< ImageType, ImageType >::Pointer resample;
  resample = itk::ResampleImageFilter< ImageType, ImageType >::New();
  resample->SetInput(image);
  resample->SetSize(osize);
  resample->SetTransform(aff);
  resample->SetInterpolator(interp);

  index.Fill( 0 );
  resample->SetOutputStartIndex( index );

  ImageType::PointType origin;
  origin.Fill( 0.0 );
  resample->SetOutputOrigin( origin );
  resample->SetOutputSpacing( spacing );

  // Run the resampling filter
  itk::TimeProbe clock;
  clock.Start();
  resample->Update();
  clock.Stop();

  std::cout << "Resampling from " << size << " to " << osize << " took " << clock.GetMean() << " s" << std::endl;

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;

}
