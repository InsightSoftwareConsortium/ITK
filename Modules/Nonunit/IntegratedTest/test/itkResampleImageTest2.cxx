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
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif


#include <iostream>

#include "itkAffineTransform.h"
#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkResampleImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"


int itkResampleImageTest2(int argc, char * argv [] )
{

  if( argc < 4 )
    {
    std::cerr << "Missing arguments ! " << std::endl;
    std::cerr << "Usage : " << std::endl;
    std::cerr << argv[0] << "inputImage referenceImage resampledImage";
    std::cerr << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int NDimensions = 2;

  typedef unsigned char                          PixelType;
  typedef itk::Image<PixelType, NDimensions>     ImageType;
  typedef ImageType::IndexType                   ImageIndexType;
  typedef ImageType::Pointer                     ImagePointerType;
  typedef ImageType::RegionType                  ImageRegionType;
  typedef ImageType::SizeType                    ImageSizeType;
  typedef double                  CoordRepType;
  typedef itk::AffineTransform<CoordRepType,NDimensions>   AffineTransformType;
  typedef itk::LinearInterpolateImageFunction<ImageType,CoordRepType>  InterpolatorType;

  typedef itk::ImageFileReader< ImageType > ReaderType;
  typedef itk::ImageFileWriter< ImageType > WriterType;

  ReaderType::Pointer reader1 = ReaderType::New();
  ReaderType::Pointer reader2 = ReaderType::New();

  WriterType::Pointer writer = WriterType::New();

  reader1->SetFileName( argv[1] );
  reader2->SetFileName( argv[2] );

  writer->SetFileName( argv[3] );

  // Create an affine transformation
  AffineTransformType::Pointer affineTransform = AffineTransformType::New();
  affineTransform->Scale(2.0);

  // Create a linear interpolation image function
  InterpolatorType::Pointer interpolator = InterpolatorType::New();

  // Create and configure a resampling filter
  typedef itk::ResampleImageFilter< ImageType, ImageType > ResampleFilterType;

  ResampleFilterType::Pointer resample = ResampleFilterType::New();

  resample->SetInput( reader1->GetOutput() );
  resample->SetReferenceImage( reader2->GetOutput() );
  resample->UseReferenceImageOn();
  resample->SetTransform( affineTransform );
  resample->SetInterpolator( interpolator );

  writer->SetInput( resample->GetOutput() );

  // Check GetReferenceImage
  if( resample->GetReferenceImage() != reader2->GetOutput() )
    {
    std::cerr << "GetReferenceImage() failed ! " << std::endl;
    return EXIT_FAILURE;
    }


  // Run the resampling filter
  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  // Check UseReferenceImage methods
  resample->UseReferenceImageOff();
  if( resample->GetUseReferenceImage() )
    {
    std::cerr << "GetUseReferenceImage() or UseReferenceImageOff() failed ! ";
    std::cerr << std::endl;
    return EXIT_FAILURE;
    }

  // Check UseReferenceImage methods
  resample->UseReferenceImageOn();
  if( !resample->GetUseReferenceImage() )
    {
    std::cerr << "GetUseReferenceImage() or UseReferenceImageOn() failed ! ";
    std::cerr << std::endl;
    return EXIT_FAILURE;
    }

  // Check UseReferenceImage methods
  resample->SetUseReferenceImage( false );
  if( resample->GetUseReferenceImage() )
    {
    std::cerr << "GetUseReferenceImage() or SetUseReferenceImage() failed ! ";
    std::cerr << std::endl;
    return EXIT_FAILURE;
    }


 std::cout << "Test passed." << std::endl;
 return EXIT_SUCCESS;

}
