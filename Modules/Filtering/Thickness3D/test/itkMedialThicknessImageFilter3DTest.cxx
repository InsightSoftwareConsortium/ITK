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
#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkMedialThicknessImageFilter3D.h"
#include "itkTestingMacros.h"

#include <iostream>

int
itkMedialThicknessImageFilter3DTest(int argc, char * argv[])
{
  // Verify the number of parameters in the command line
  if (argc <= 2)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << " inputImageFile outputImageFile" << std::endl;
    return EXIT_FAILURE;
  }
  char * infilename = argv[1];
  char * outfilename = argv[2];

  const unsigned int                             Dimension = 3;
  typedef unsigned char                          InputPixelType;
  typedef float                                  OutputPixelType;
  typedef itk::Image<InputPixelType, Dimension>  InputImageType;
  typedef itk::Image<OutputPixelType, Dimension> OutputImageType;

  // Read image
  typedef itk::ImageFileReader<InputImageType> ReaderType;
  ReaderType::Pointer                          reader = ReaderType::New();
  reader->SetFileName(infilename);

  ITK_TRY_EXPECT_NO_EXCEPTION(reader->Update());


  // Define the thinning filter
  typedef itk::MedialThicknessImageFilter3D<InputImageType, OutputImageType> FilterType;
  FilterType::Pointer                                                        medialThicknessFilter = FilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(medialThicknessFilter, MedialThicknessImageFilter3D, ImageToImageFilter);

  medialThicknessFilter->SetInput(reader->GetOutput());

  ITK_TRY_EXPECT_NO_EXCEPTION(medialThicknessFilter->Update());


  // output to file
  typedef itk::ImageFileWriter<OutputImageType> WriterType;
  WriterType::Pointer                           writer = WriterType::New();
  writer->SetInput(medialThicknessFilter->GetOutput());
  writer->SetFileName(outfilename);

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  std::cout << "Test finished.";
  return EXIT_SUCCESS;
}
