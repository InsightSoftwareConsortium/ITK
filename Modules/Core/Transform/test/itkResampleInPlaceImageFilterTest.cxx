/* =========================================================================
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
/* =========================================================================
 *  Created by Wei Lu on 10/14/10.
 *  Copyright 2010 The University of Iowa
 *=========================================================================*/

#include "itkResampleInPlaceImageFilter.h"

#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkVersorRigid3DTransform.h"
#include "itkImageRegionConstIterator.h"

#include <iostream>

// return true if it fails the validation
inline bool
Validate(double input, double desired, double tolerance)
{
  return std::abs<double>(input - desired) > tolerance * std::abs<double>(desired);
}

int
main(int argc, char * argv[])
{
  // Simple parameter check
  if (argc < 3)
  {
    std::cerr << "Wrong arguments!" << std::endl;
    std::cerr << "Usage: ./" << argv[0] << " inputImage baselineImage outputImage" << std::endl;
    return EXIT_FAILURE;
  }

  bool   result = false; // test result default = no failure
  double tol = 1.e-3;    // tolerance

  // Image, filter, transform typedef
  constexpr unsigned int LocalImageDimension = 3;
  using PixelType = short;

  using ImageType = itk::Image<PixelType, LocalImageDimension>;
  using ImagePointer = ImageType::Pointer;
  using ImagePointType = ImageType::PointType;
  using ImageDirectionType = ImageType::DirectionType;
  using ImageSpacingType = ImageType::SpacingType;

  using ImageConstIterator = itk::ImageRegionConstIterator<ImageType>;
  using ReaderType = itk::ImageFileReader<ImageType>;
  using TransformType = itk::VersorRigid3DTransform<double>;

  using FilterType = itk::ResampleInPlaceImageFilter<ImageType, ImageType>;

  // Read in input test image
  ImagePointer inputImage;
  {
    ReaderType::Pointer reader = ReaderType::New();
    reader->SetFileName(argv[1]);
    try
    {
      reader->Update();
    }
    catch (itk::ExceptionObject & err)
    {
      std::cerr << " Error while reading image file(s) with ITK:\n " << err << std::endl;
    }
    inputImage = reader->GetOutput();
  }

  // Set up transforms
  itk::Vector<double, 3> rotationAxis;
  rotationAxis.Fill(0.);
  rotationAxis[0] = 1.;
  double                 rotationAngle = .5; // in rad
  itk::Vector<double, 3> translation;
  translation.Fill(0.);
  translation[1] = 300.; // in mm along P-axis
  TransformType::Pointer transform = TransformType::New();
  transform->SetIdentity();
  transform->SetRotation(rotationAxis, rotationAngle);
  transform->Translate(translation, true);

  // Set up the resample filter
  FilterType::Pointer filter = FilterType::New();
  filter->SetInputImage(inputImage);
  filter->SetRigidTransform(transform);
  filter->Update();
  ImagePointer outputImage = filter->GetOutput();

  // Get image info
  ImagePointType     origin = outputImage->GetOrigin();
  ImageDirectionType direction = outputImage->GetDirection();
  ImageSpacingType   spacing = outputImage->GetSpacing();

  // Read in baseline image
  ImagePointer baselineImage = nullptr;
  {
    ReaderType::Pointer reader = ReaderType::New();
    reader->SetFileName(argv[2]);
    try
    {
      reader->Update();
    }
    catch (itk::ExceptionObject & err)
    {
      std::cerr << " Error while reading image file(s) with ITK:\n " << err << std::endl;
    }
    baselineImage = reader->GetOutput();
  }
  ImagePointType     origin_d = baselineImage->GetOrigin();
  ImageDirectionType direction_d = baselineImage->GetDirection();
  ImageSpacingType   spacing_d = baselineImage->GetSpacing();
  // Image info validation
  for (unsigned int i = 0; i < LocalImageDimension; ++i)
  {
    result = (result || Validate(origin[i], origin_d[i], tol));
    result = (result || Validate(spacing[i], spacing_d[i], tol));
    for (unsigned int j = 0; j < LocalImageDimension; ++j)
    {
      result = (result || Validate(direction(i, j), direction_d(i, j), tol));
    }
  }

  // Voxel contents validation
  ImageConstIterator it1(outputImage, outputImage->GetRequestedRegion());
  ImageConstIterator it2(baselineImage, baselineImage->GetRequestedRegion());
  it1.GoToBegin();
  it2.GoToBegin();
  while (!it1.IsAtEnd())
  {
    result = (result || Validate(it1.Get(), it2.Get(), tol));
    ++it1;
    ++it2;
  }

  using WriterType = itk::ImageFileWriter<ImageType>;
  WriterType::Pointer writer = WriterType::New();
  writer->SetFileName(argv[3]);
  writer->SetInput(outputImage);
  writer->Update();

  return result;
}
