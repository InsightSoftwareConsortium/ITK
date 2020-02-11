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
//  This example illustrates how to deform an image using a KernelBase spline
//  and two sets of landmarks.
//
//  \index{WarpImageFilter}
//  \index{LandmarkDisplacementFieldSource}
//
//  In addition to standard headers included in previous examples, this example
//  requires the following includes:
//
//  Software Guide : EndLatex

#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkDisplacementFieldTransform.h"
#include "itkResampleImageFilter.h"
// Software Guide : BeginCodeSnippet
#include "itkVector.h"
#include "itkLandmarkDisplacementFieldSource.h"
#include <fstream>
// Software Guide : EndCodeSnippet

int
main(int argc, char * argv[])
{

  if (argc < 3)
  {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " landmarksFile fixedImage ";
    std::cerr << "movingImage deformedMovingImage" << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int Dimension = 2;
  using VectorComponentType = float;

  using VectorType = itk::Vector<VectorComponentType, Dimension>;

  using DisplacementFieldType = itk::Image<VectorType, Dimension>;

  using PixelType = unsigned char;
  using FixedImageType = itk::Image<PixelType, Dimension>;
  using MovingImageType = itk::Image<PixelType, Dimension>;

  using FixedReaderType = itk::ImageFileReader<FixedImageType>;
  using MovingReaderType = itk::ImageFileReader<MovingImageType>;

  using MovingWriterType = itk::ImageFileWriter<MovingImageType>;


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


  MovingReaderType::Pointer movingReader = MovingReaderType::New();
  MovingWriterType::Pointer movingWriter = MovingWriterType::New();

  movingReader->SetFileName(argv[3]);
  movingWriter->SetFileName(argv[4]);


  FixedImageType::ConstPointer fixedImage = fixedReader->GetOutput();

  //  Software Guide : BeginLatex
  //
  //  After reading in the fixed and moving images, the \code{deformer} object is
  //  instantiated from the \code{itk::LandmarkDisplacementFieldSource} class, and
  //  parameters of the image space and orientation are set.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet

  using DisplacementSourceType =
    itk::LandmarkDisplacementFieldSource<DisplacementFieldType>;

  DisplacementSourceType::Pointer deformer = DisplacementSourceType::New();

  deformer->SetOutputSpacing(fixedImage->GetSpacing());
  deformer->SetOutputOrigin(fixedImage->GetOrigin());
  deformer->SetOutputRegion(fixedImage->GetLargestPossibleRegion());
  deformer->SetOutputDirection(fixedImage->GetDirection());

  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  Source and target landmarks are then created, and the points themselves are
  //  read in from a file stream.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet

  using LandmarkContainerType = DisplacementSourceType::LandmarkContainer;
  using LandmarkPointType = DisplacementSourceType::LandmarkPointType;

  LandmarkContainerType::Pointer sourceLandmarks = LandmarkContainerType::New();
  LandmarkContainerType::Pointer targetLandmarks = LandmarkContainerType::New();

  LandmarkPointType sourcePoint;
  LandmarkPointType targetPoint;

  std::ifstream pointsFile;
  pointsFile.open(argv[1]);

  unsigned int pointId = 0;

  pointsFile >> sourcePoint;
  pointsFile >> targetPoint;

  while (!pointsFile.fail())
  {
    sourceLandmarks->InsertElement(pointId, sourcePoint);
    targetLandmarks->InsertElement(pointId, targetPoint);
    ++pointId;

    pointsFile >> sourcePoint;
    pointsFile >> targetPoint;
  }

  pointsFile.close();

  //  Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  The source and target landmark objects are then assigned to \code{deformer}.
  //
  //  Software Guide : EndLatex

  //  Software Guide : BeginCodeSnippet

  deformer->SetSourceLandmarks(sourceLandmarks);
  deformer->SetTargetLandmarks(targetLandmarks);

  //  Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  After calling \code{UpdateLargestPossibleRegion()} on the \code{deformer},
  //  the displacement field may be obtained via the \code{GetOutput()} method.
  //
  //  Software Guide : EndLatex

  try
  {
    deformer->UpdateLargestPossibleRegion();
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << "Exception thrown " << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
  }

  DisplacementFieldType::Pointer displacementField = deformer->GetOutput();

  using InterpolatorPrecisionType = double;
  using TransformPrecisionType = float;
  using FilterType = itk::ResampleImageFilter<MovingImageType,
                                              MovingImageType,
                                              InterpolatorPrecisionType,
                                              TransformPrecisionType>;
  FilterType::Pointer warper = FilterType::New();

  using InterpolatorType =
    itk::LinearInterpolateImageFunction<MovingImageType, InterpolatorPrecisionType>;

  InterpolatorType::Pointer interpolator = InterpolatorType::New();

  warper->SetInterpolator(interpolator);

  using DisplacementFieldTransformType =
    itk::DisplacementFieldTransform<TransformPrecisionType, Dimension>;
  auto displacementTransform = DisplacementFieldTransformType::New();
  displacementTransform->SetDisplacementField(displacementField);
  warper->SetTransform(displacementTransform);

  warper->SetOutputSpacing(displacementField->GetSpacing());
  warper->SetOutputOrigin(displacementField->GetOrigin());

  warper->SetOutputDirection(displacementField->GetDirection());
  warper->SetSize(displacementField->GetLargestPossibleRegion().GetSize());

  warper->SetInput(movingReader->GetOutput());

  movingWriter->SetInput(warper->GetOutput());

  try
  {
    movingWriter->Update();
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << "Exception thrown " << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
