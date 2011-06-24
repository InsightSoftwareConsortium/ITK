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
#include "itkMedianImageFilter.h"
#include "itkSubtractImageFilter.h"

#include "itksys/SystemTools.hxx"

#include <sstream>

#include "QuickView.h"

int itkVtkMedianFilterTest(int argc, char * argv[])
{
  // Verify command line arguments
  if( argc < 2 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << " InputImageFile [radius]" << std::endl;
    return EXIT_FAILURE;
    }
  std::string inputFilename = argv[1];

  // Setup types
  typedef itk::Image<float, 2 >                         ImageType;
  typedef itk::ImageFileReader<ImageType>               ReaderType;
  typedef itk::MedianImageFilter<ImageType, ImageType > FilterType;
  typedef itk::SubtractImageFilter<ImageType>           SubtractType;

  // Create and setup a reader
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( inputFilename );

  // Create and setup a median filter
  FilterType::Pointer medianFilter = FilterType::New();
  FilterType::InputSizeType radius;
  radius.Fill(2);
  if (argc > 2)
    {
    radius.Fill(atoi(argv[2]));
    }

  medianFilter->SetRadius(radius);
  medianFilter->SetInput( reader->GetOutput() );

  SubtractType::Pointer diff = SubtractType::New();
  diff->SetInput1(reader->GetOutput());
  diff->SetInput2(medianFilter->GetOutput());

  QuickView viewer;
  viewer.AddImage(
    reader->GetOutput(),true,
    itksys::SystemTools::GetFilenameName(inputFilename));

  std::stringstream desc;
  desc << "MedianImageFilter, radius = " << radius;
  viewer.AddImage(
    medianFilter->GetOutput(),
    true,
    desc.str());

  std::stringstream desc2;
  desc2 << "Original - Median";
  viewer.AddImage(
    diff->GetOutput(),
    true,
    desc2.str());

  // For testing, turn off interaction
  viewer.Visualize(false);

  return EXIT_SUCCESS;
}
