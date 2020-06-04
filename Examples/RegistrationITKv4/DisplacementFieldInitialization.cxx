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
//  In order to initialize deformable registration algorithm it is often
//  convenient to generate a displacemnt field from a set of feature
//  correspondances provided by the user. The following example illustrates
//  how to use the \doxygen{itkLandmarkDisplacementFieldSource} class in order
//  to generate a displacement field from the specification of two sets of
//  landmarks. Landmarks from one set are associated one-to-one to the
//  landmarks in the other set. Each landmark pair defines one displacement
//  vector. This class interpolates the values of all other displacement
//  vectors using \doxygen{KernelBasedTransform}
//
//
//  \index{Registration!Finite Element-Based}
//
//  Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include "itkImage.h"
#include "itkVector.h"
#include "itkLandmarkDisplacementFieldSource.h"
#include "itkImageFileWriter.h"

#include <fstream>


int
main(int argc, char * argv[])
{

  if (argc < 3)
  {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " landmarksFile fixedImage outputDisplacementField"
              << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int Dimension = 2;
  using VectorComponentType = float;

  using VectorType = itk::Vector<VectorComponentType, Dimension>;

  using DisplacementFieldType = itk::Image<VectorType, Dimension>;


  using PixelType = unsigned char;
  using FixedImageType = itk::Image<PixelType, Dimension>;

  using FixedReaderType = itk::ImageFileReader<FixedImageType>;


  FixedReaderType::Pointer fixedReader = FixedReaderType::New();

  fixedReader->SetFileName(argv[2]);

  try
  {
    fixedReader->Update();
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << "Exception thrown " << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
  }


  FixedImageType::ConstPointer fixedImage = fixedReader->GetOutput();

  using FilterType =
    itk::LandmarkDisplacementFieldSource<DisplacementFieldType>;

  FilterType::Pointer filter = FilterType::New();

  filter->SetOutputSpacing(fixedImage->GetSpacing());
  filter->SetOutputOrigin(fixedImage->GetOrigin());
  filter->SetOutputRegion(fixedImage->GetLargestPossibleRegion());
  filter->SetOutputDirection(fixedImage->GetDirection());

  //  Create source and target landmarks.
  //
  using LandmarkContainerType = FilterType::LandmarkContainer;
  using LandmarkPointType = FilterType::LandmarkPointType;

  LandmarkContainerType::Pointer sourceLandmarks =
    LandmarkContainerType::New();
  LandmarkContainerType::Pointer targetLandmarks =
    LandmarkContainerType::New();
  std::ifstream pointsFile;
  pointsFile.open(argv[1]);

  LandmarkPointType sourcePoint;
  pointsFile >> sourcePoint;
  LandmarkPointType targetPoint;
  pointsFile >> targetPoint;

  unsigned int pointId = 0;
  while (!pointsFile.fail())
  {
    sourceLandmarks->InsertElement(pointId, sourcePoint);
    targetLandmarks->InsertElement(pointId, targetPoint);
    pointId++;

    pointsFile >> sourcePoint;
    pointsFile >> targetPoint;
  }
  pointsFile.close();
  filter->SetSourceLandmarks(sourceLandmarks);
  filter->SetTargetLandmarks(targetLandmarks);

  try
  {
    filter->UpdateLargestPossibleRegion();
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << "Exception thrown " << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
  }

  // Write an image for regression testing
  using WriterType = itk::ImageFileWriter<DisplacementFieldType>;

  WriterType::Pointer writer = WriterType::New();
  writer->SetInput(filter->GetOutput());
  writer->SetFileName(argv[3]);
  filter->Print(std::cout);
  try
  {
    writer->Update();
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << "Exception thrown by writer" << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
  }
  return EXIT_SUCCESS;
  //  Software Guide : EndLatex
}
