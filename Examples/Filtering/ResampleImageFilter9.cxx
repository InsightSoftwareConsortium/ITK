/*=========================================================================
 *
 *  Copyright NumFOCUS
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

//  Software Guide : BeginLatex
//
//  Resampling can also be performed in multi-component images.
//  This example compares nearest neighbor resampling using the Nearest
//  neighbor and the linear interpolators for vector images.
//
//  Try
//    ResampleImageFilter9 Examples/Data/VisibleWomanEyeSlice.png
//        SliceNearestNeighbor.png SliceLinear.png
//
//  \index{itk::ResampleImageFilter!Image internal transform}
//
//  Software Guide : EndLatex


#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkResampleImageFilter.h"
#include "itkIdentityTransform.h"
#include "itkNearestNeighborInterpolateImageFunction.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkRGBPixel.h"


int
main(int argc, char * argv[])
{
  if (argc < 4)
  {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  inputImageFile  outputImageFile_NearestNeighbor"
              << "  outputImageFile_Linear " << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int Dimension = 2;
  using PixelComponentType = unsigned char;
  using PixelType = itk::RGBPixel<PixelComponentType>;

  using ImageType = itk::Image<PixelType, Dimension>;


  using ReaderType = itk::ImageFileReader<ImageType>;
  using WriterType = itk::ImageFileWriter<ImageType>;

  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writerNearest = WriterType::New(); // writer for nearest neighbor
  WriterType::Pointer writerLinear = WriterType::New();  // writer for linear

  reader->SetFileName(argv[1]);
  writerNearest->SetFileName(argv[2]);
  writerLinear->SetFileName(argv[3]);

  using FilterType = itk::ResampleImageFilter<ImageType, ImageType>;

  FilterType::Pointer nearestFilter = FilterType::New();
  FilterType::Pointer linearFilter = FilterType::New();

  // Interpolators
  using NearestInterpolatorType =
    itk::NearestNeighborInterpolateImageFunction<ImageType, double>;
  NearestInterpolatorType::Pointer interpolatorNearest = NearestInterpolatorType::New();

  using LinearInterpolatorType = itk::LinearInterpolateImageFunction<ImageType, double>;
  LinearInterpolatorType::Pointer interpolatorLinear = LinearInterpolatorType::New();

  nearestFilter->SetInterpolator(interpolatorNearest);
  linearFilter->SetInterpolator(interpolatorLinear);

  using TransformType = itk::IdentityTransform<double, Dimension>;
  TransformType::Pointer transform = TransformType::New();

  nearestFilter->SetTransform(transform);
  linearFilter->SetTransform(transform);

  // Software Guide : BeginCodeSnippet
  PixelType defaultValue;
  defaultValue.Fill(50);
  nearestFilter->SetDefaultPixelValue(defaultValue);
  linearFilter->SetDefaultPixelValue(defaultValue);
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginCodeSnippet
  ImageType::SpacingType spacing;
  spacing[0] = .35; // pixel spacing in millimeters along X
  spacing[1] = .35; // pixel spacing in millimeters along Y

  nearestFilter->SetOutputSpacing(spacing);
  linearFilter->SetOutputSpacing(spacing);
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginCodeSnippet
  ImageType::PointType origin;
  origin[0] = 0.4; // X space coordinate of origin
  origin[1] = 0.4; // Y space coordinate of origin
  nearestFilter->SetOutputOrigin(origin);
  linearFilter->SetOutputOrigin(origin);
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginCodeSnippet
  ImageType::DirectionType direction;
  direction.SetIdentity();
  nearestFilter->SetOutputDirection(direction);
  linearFilter->SetOutputDirection(direction);
  // Software Guide : EndCodeSnippet


  ImageType::SizeType size;

  size[0] = 300; // number of pixels along X
  size[1] = 300; // number of pixels along Y

  nearestFilter->SetSize(size);
  linearFilter->SetSize(size);

  nearestFilter->SetInput(reader->GetOutput());
  linearFilter->SetInput(reader->GetOutput());
  writerNearest->SetInput(nearestFilter->GetOutput());
  writerLinear->SetInput(linearFilter->GetOutput());


  try
  {
    writerNearest->Update();
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << "Exception thrown " << std::endl;
    std::cerr << excp << std::endl;
  }

  try
  {
    writerLinear->Update();
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << "Exception thrown " << std::endl;
    std::cerr << excp << std::endl;
  }

  return EXIT_SUCCESS;
}
