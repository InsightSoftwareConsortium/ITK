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

#include "itkCastImageFilter.h"
#include "itkImageFileWriter.h"
#include "itkPointSet.h"
#include "itkBSplineScatteredDataPointSetToImageFilter.h"
#include "itkTestingMacros.h"

#include <fstream>

/**
 * In this test, we approximate a sequence of 3D points with a parametric
 * curve described by B-Splines
 */
int
itkBSplineScatteredDataPointSetToImageFilterTest3(int argc, char * argv[])
{

  if (argc < 3)
  {
    std::cerr << "Missing arguments" << std::endl;
    std::cerr << "Usage:" << std::endl;
    std::cerr << argv[0] << "inputPointsFile.txt outputImage" << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int ParametricDimension = 1;
  constexpr unsigned int DataDimension = 3;

  using RealType = double;
  using OutputPixelType = unsigned char;
  using VectorType = itk::Vector<RealType, DataDimension>;
  using OutputVectorType = itk::Vector<OutputPixelType, DataDimension>;
  using ImageType = itk::Image<VectorType, ParametricDimension>;
  using OutputImageType = itk::Image<OutputVectorType, ParametricDimension>;
  using PointSetPixelType = VectorType;

  using PointSetType = itk::PointSet<PointSetPixelType, ParametricDimension>;

  PointSetType::Pointer pointSet = PointSetType::New();

  // Read the input points
  std::ifstream inputFile;
  inputFile.open(argv[1]);

  // The actual data to be approximated
  VectorType P;

  // Parameter of the curve
  PointSetType::PointType parameterPosition;

  unsigned int pointCounter = 0;

  inputFile >> P;

  //  FIXME: add parameterization of the input points, in the range [0:1]
  double t = 0.0;

  while (!inputFile.eof())
  {
    parameterPosition[0] = t;
    t += 0.01; // FIXME

    pointSet->SetPoint(pointCounter, parameterPosition);
    pointSet->SetPointData(pointCounter, P);

    pointCounter++;
    inputFile >> P;
  }

  inputFile.close();

  // Instantiate the filter and set the parameters
  using FilterType = itk::BSplineScatteredDataPointSetToImageFilter<PointSetType, ImageType>;

  FilterType::Pointer filter = FilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, BSplineScatteredDataPointSetToImageFilter, PointSetToImageFilter);

  // Define the parametric domain
  ImageType::SpacingType spacing;
  spacing.Fill(0.001);
  ImageType::SizeType size;
  // Adding 0.5 to avoid rounding errors
  size.Fill(static_cast<unsigned int>(1.0 / spacing[0] + .5) + 1);
  ImageType::PointType origin;
  origin.Fill(0.0);

  filter->SetSize(size);
  filter->SetOrigin(origin);
  filter->SetSpacing(spacing);
  filter->SetInput(pointSet);

  filter->SetSplineOrder(3);
  FilterType::ArrayType ncps;
  ncps.Fill(4);
  filter->SetNumberOfControlPoints(ncps);

  // We set an extreme number of levels to show how this
  // fails because of the choice of B-spline epsilon
  filter->SetNumberOfLevels(15);

  bool generateOutputImage = true;
  filter->SetGenerateOutputImage(generateOutputImage);
  ITK_TEST_SET_GET_VALUE(generateOutputImage, filter->GetGenerateOutputImage());

  filter->GenerateOutputImageOff();
  ITK_TEST_SET_GET_VALUE(false, filter->GetGenerateOutputImage());

  filter->GenerateOutputImageOn();
  ITK_TEST_SET_GET_VALUE(true, filter->GetGenerateOutputImage());


  ITK_TRY_EXPECT_NO_EXCEPTION(filter->Update());

  // Get the filter output
  ImageType::Pointer outputImage = filter->GetOutput();

  // Cast the output image
  using CastImageFilterType = itk::CastImageFilter<ImageType, OutputImageType>;

  CastImageFilterType::Pointer caster = CastImageFilterType::New();

  caster->SetInput(outputImage);

  // Write the result image
  using WriterType = itk::ImageFileWriter<OutputImageType>;

  WriterType::Pointer writer = WriterType::New();

  writer->SetFileName(argv[2]);

  writer->SetInput(caster->GetOutput());

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  return EXIT_SUCCESS;
}
