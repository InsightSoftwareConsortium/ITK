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
#include "itkRunLengthTextureFeaturesImageFilter.h"

#include "itkImage.h"
#include "itkVector.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkNeighborhood.h"
#include "itkVectorIndexSelectionCastImageFilter.h"
#include "itkTestingMacros.h"

int
RunLengthTextureFeaturesImageFilterTestSeparateFeatures(int argc, char * argv[])
{
  if (argc < 4)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << argv[0] << " inputImageFile"
              << " maskImageFile"
              << " outputImageFile"
              << " [numberOfBinsPerAxis]"
              << " [pixelValueMin]"
              << " [pixelValueMax]"
              << " [minDistance]"
              << " [maxDistance]"
              << " [neighborhoodRadius]" << std::endl;
    return EXIT_FAILURE;
  }

  const unsigned int ImageDimension = 3;
  const unsigned int VectorComponentDimension = 10;

  // Declare types
  typedef float                                                           InputPixelType;
  typedef float                                                           OutputPixelComponentType;
  typedef itk::Vector<OutputPixelComponentType, VectorComponentDimension> OutputPixelType;

  typedef itk::Image<InputPixelType, ImageDimension>                                   InputImageType;
  typedef itk::Image<OutputPixelType, ImageDimension>                                  OutputImageType;
  typedef itk::ImageFileReader<InputImageType>                                         ReaderType;
  typedef itk::Neighborhood<InputImageType::PixelType, InputImageType::ImageDimension> NeighborhoodType;


  // Create and set up a reader
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  // Create and set up a maskReader
  ReaderType::Pointer maskReader = ReaderType::New();
  maskReader->SetFileName(argv[2]);

  // Create the filter
  typedef itk::Statistics::RunLengthTextureFeaturesImageFilter<InputImageType, OutputImageType, InputImageType>
                      FilterType;
  FilterType::Pointer filter = FilterType::New();

  filter->SetInput(reader->GetOutput());
  filter->SetMaskImage(maskReader->GetOutput());

  if (argc >= 5)
  {
    unsigned int numberOfBinsPerAxis = std::atoi(argv[4]);
    filter->SetNumberOfBinsPerAxis(numberOfBinsPerAxis);

    FilterType::PixelType pixelValueMin = std::atof(argv[5]);
    FilterType::PixelType pixelValueMax = std::atof(argv[6]);
    filter->SetHistogramValueMinimum(pixelValueMin);
    filter->SetHistogramValueMaximum(pixelValueMax);

    FilterType::RealType minDistance = std::atof(argv[7]);
    FilterType::RealType maxDistance = std::atof(argv[8]);
    filter->SetHistogramDistanceMinimum(minDistance);
    filter->SetHistogramDistanceMaximum(maxDistance);


    NeighborhoodType::SizeValueType neighborhoodRadius = std::atoi(argv[9]);
    NeighborhoodType                hood;
    hood.SetRadius(neighborhoodRadius);
    filter->SetNeighborhoodRadius(hood.GetRadius());
  }

  TRY_EXPECT_NO_EXCEPTION(filter->Update());


  typedef itk::Image<OutputPixelComponentType, ImageDimension>                        FeatureImageType;
  typedef itk::VectorIndexSelectionCastImageFilter<OutputImageType, FeatureImageType> IndexSelectionType;
  IndexSelectionType::Pointer indexSelectionFilter = IndexSelectionType::New();
  indexSelectionFilter->SetInput(filter->GetOutput());

  for (unsigned int i = 0; i < VectorComponentDimension; i++)
  {
    indexSelectionFilter->SetIndex(i);

    // Create and set up a writer
    typedef itk::ImageFileWriter<FeatureImageType> WriterType;
    WriterType::Pointer                            writer = WriterType::New();
    std::string                                    outputFilename = argv[3];
    std::ostringstream                             ss;
    ss << i;
    std::string s = ss.str();
    writer->SetFileName(outputFilename + "_" + s + ".nrrd");
    writer->SetInput(indexSelectionFilter->GetOutput());

    TRY_EXPECT_NO_EXCEPTION(writer->Update());
  }


  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
