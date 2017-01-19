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
#include "itkImageFileWriter.h"
#include "itkResampleImageFilter.h"
#include "itkTimeProbe.h"
#include "itkTestingMacros.h"

int itkResampleImageTest6(int argc, char * argv [] )
{

  // Resample a Vector image
  const unsigned int NDimensions = 2;

  typedef unsigned char                ValueType;

  typedef itk::VectorImage<ValueType, 2 >    ImageType;
  typedef ImageType::PixelType               PixelType;

  typedef ImageType::IndexType         ImageIndexType;
  typedef ImageType::Pointer           ImagePointerType;
  typedef ImageType::RegionType        ImageRegionType;
  typedef ImageType::SizeType          ImageSizeType;

  typedef double                       CoordRepType;

  typedef itk::AffineTransform<CoordRepType,NDimensions>
                                       AffineTransformType;
  typedef itk::LinearInterpolateImageFunction<ImageType,CoordRepType>
                                       InterpolatorType;
  typedef itk::ImageFileWriter<ImageType>
                                       WriterType;

  if (argc < 2)
    {
    std::cout << "Usage: " << argv[0]
              << " scaling outputFilename" << std::endl;
    return EXIT_FAILURE;
    }

  float scaling = atof( argv[1] );

  // Create and configure an image
  ImagePointerType image = ImageType::New();
  ImageIndexType  index = {{0,  0}};
  ImageSizeType   size  = {{64,64}};
  ImageRegionType region;
  region.SetSize ( size );
  region.SetIndex( index );
  image->SetLargestPossibleRegion( region );
  image->SetBufferedRegion( region );
  image->SetVectorLength( 3 );
  image->Allocate();

  unsigned int newDims = static_cast<unsigned int>( 64*scaling );
  ImageSizeType osize = {{newDims, newDims}};

  ImageType::SpacingType spacing;
  spacing[0] = size[0] / static_cast<double>(osize[0]);
  spacing[1] = size[1] / static_cast<double>(osize[1]);

  // Fill image with a ramp
  std::cout << "init image..." << std::flush;
  itk::ImageRegionIteratorWithIndex<ImageType> iter(image, region);
  PixelType value;
  value.SetSize( 3 );

  for (iter.GoToBegin(); !iter.IsAtEnd(); ++iter)
    {
    index = iter.GetIndex();
    value[0] = index[0];
    value[1] = index[1];
    value[2] = index[0] + index[1];
    iter.Set(value);
    }
  std::cout << "Done." << std::endl;

  // Create an affine transformation
  AffineTransformType::Pointer aff = AffineTransformType::New();
  aff->Scale(0.9);

  // Create a linear interpolation image function
  InterpolatorType::Pointer interp = InterpolatorType::New();
  interp->SetInputImage(image);

  // Create and configure a resampling filter
  itk::ResampleImageFilter< ImageType, ImageType >::Pointer resample =
    itk::ResampleImageFilter< ImageType, ImageType >::New();

  EXERCISE_BASIC_OBJECT_METHODS( resample, ResampleImageFilter, ImageToImageFilter );

  resample->SetInput(image);
  TEST_SET_GET_VALUE( image, resample->GetInput() );

  resample->SetSize(osize);
  TEST_SET_GET_VALUE( osize, resample->GetSize() );

  resample->SetTransform(aff);
  TEST_SET_GET_VALUE( aff, resample->GetTransform() );

  resample->SetInterpolator(interp);
  TEST_SET_GET_VALUE( interp, resample->GetInterpolator() );

  index.Fill( 0 );
  resample->SetOutputStartIndex( index );
  TEST_SET_GET_VALUE( index, resample->GetOutputStartIndex() );

  ImageType::PointType origin;
  origin.Fill( 0.0 );
  resample->SetOutputOrigin( origin );
  TEST_SET_GET_VALUE( origin, resample->GetOutputOrigin() );

  resample->SetOutputSpacing( spacing );
  TEST_SET_GET_VALUE( spacing, resample->GetOutputSpacing() );

  // Run the resampling filter
  itk::TimeProbe clock;
  std::cout << "Resample..." << std::flush;
  clock.Start();
  resample->Update();
  std::cout << "Done. " << std::endl;
  clock.Stop();

  std::cout << "Resampling from " << size
            << " to " << osize
            << " took " << clock.GetMean() << " s" << std::endl;

  WriterType::Pointer writer = WriterType::New();
  writer->SetInput(resample->GetOutput());
  writer->SetFileName(argv[2]);
  writer->Update();

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;

}
