/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include "itkResampleImageFilter.h"

#include "itkBSplineTransform.h"
#include "itkBSplineTransformInitializer.h"

#include "itkObject.h"
#include "itkTestingMacros.h"

#include <fstream>

int
itkBSplineTransformInitializerTest1(int argc, char * argv[])
{

  if (argc < 5)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " coefficientsFile fixedImage";
    std::cerr << " movingImage deformedMovingImage" << std::endl;
    std::cerr << " [deformationField]" << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int ImageDimension = 2;

  using PixelType = unsigned char;
  using FixedImageType = itk::Image<PixelType, ImageDimension>;
  using MovingImageType = itk::Image<PixelType, ImageDimension>;

  using FixedReaderType = itk::ImageFileReader<FixedImageType>;
  using MovingReaderType = itk::ImageFileReader<MovingImageType>;

  auto fixedReader = FixedReaderType::New();
  fixedReader->SetFileName(argv[2]);

  ITK_TRY_EXPECT_NO_EXCEPTION(fixedReader->Update());


  auto movingReader = MovingReaderType::New();

  movingReader->SetFileName(argv[3]);

  FixedImageType::ConstPointer fixedImage = fixedReader->GetOutput();

  using FilterType = itk::ResampleImageFilter<MovingImageType, FixedImageType>;

  auto resampler = FilterType::New();

  using InterpolatorType = itk::LinearInterpolateImageFunction<MovingImageType, double>;

  auto interpolator = InterpolatorType::New();

  resampler->SetInterpolator(interpolator);

  FixedImageType::SpacingType   fixedSpacing = fixedImage->GetSpacing();
  FixedImageType::PointType     fixedOrigin = fixedImage->GetOrigin();
  FixedImageType::DirectionType fixedDirection = fixedImage->GetDirection();

  resampler->SetOutputSpacing(fixedSpacing);
  resampler->SetOutputOrigin(fixedOrigin);
  resampler->SetOutputDirection(fixedDirection);

  FixedImageType::RegionType fixedRegion = fixedImage->GetBufferedRegion();
  FixedImageType::SizeType   fixedSize = fixedRegion.GetSize();
  resampler->SetSize(fixedSize);
  resampler->SetOutputStartIndex(fixedRegion.GetIndex());

  resampler->SetInput(movingReader->GetOutput());

  const unsigned int     SpaceDimension = ImageDimension;
  constexpr unsigned int SplineOrder = 3;
  using CoordinateRepType = double;

  using TransformType = itk::BSplineTransform<CoordinateRepType, SpaceDimension, SplineOrder>;

  auto bsplineTransform = TransformType::New();

  using InitializerType = itk::BSplineTransformInitializer<TransformType, FixedImageType>;
  auto transformInitializer = InitializerType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(transformInitializer, BSplineTransformInitializer, Object);

  TransformType::MeshSizeType meshSize;
  meshSize.Fill(4);

  transformInitializer->SetTransform(bsplineTransform);
  ITK_TEST_SET_GET_VALUE(bsplineTransform, transformInitializer->GetTransform());

  transformInitializer->SetImage(fixedImage);
  ITK_TEST_SET_GET_VALUE(fixedImage, transformInitializer->GetImage());

  transformInitializer->SetTransformDomainMeshSize(meshSize);
  ITK_TEST_SET_GET_VALUE(meshSize, transformInitializer->GetTransformDomainMeshSize());

  transformInitializer->InitializeTransform();

  using ParametersType = TransformType::ParametersType;

  const unsigned int numberOfParameters = bsplineTransform->GetNumberOfParameters();

  const unsigned int numberOfNodes = numberOfParameters / SpaceDimension;

  ParametersType parameters(numberOfParameters);

  std::ifstream infile;

  infile.open(argv[1]);
  for (unsigned int n = 0; n < numberOfNodes; ++n)
  {
    infile >> parameters[n];
    infile >> parameters[n + numberOfNodes];
  }
  infile.close();

  bsplineTransform->SetParameters(parameters);

  resampler->SetTransform(bsplineTransform);

  ITK_TRY_EXPECT_NO_EXCEPTION(itk::WriteImage(resampler->GetOutput(), argv[4]));


  using VectorType = itk::Vector<float, ImageDimension>;
  using DeformationFieldType = itk::Image<VectorType, ImageDimension>;

  auto field = DeformationFieldType::New();
  field->SetRegions(fixedRegion);
  field->SetOrigin(fixedOrigin);
  field->SetSpacing(fixedSpacing);
  field->SetDirection(fixedDirection);
  field->Allocate();

  using FieldIterator = itk::ImageRegionIterator<DeformationFieldType>;
  FieldIterator fi(field, fixedRegion);

  fi.GoToBegin();

  TransformType::InputPointType   fixedPoint;
  TransformType::OutputPointType  movingPoint;
  TransformType::JacobianType     jacobian;
  DeformationFieldType::IndexType index;

  VectorType displacement;

  while (!fi.IsAtEnd())
  {
    index = fi.GetIndex();
    field->TransformIndexToPhysicalPoint(index, fixedPoint);
    movingPoint = bsplineTransform->TransformPoint(fixedPoint);
    bsplineTransform->ComputeJacobianWithRespectToParameters(fixedPoint, jacobian);
    displacement = movingPoint - fixedPoint;
    fi.Set(displacement);
    ++fi;
  }

  if (argc >= 6)
  {
    ITK_TRY_EXPECT_NO_EXCEPTION(itk::WriteImage(field, argv[5]));
  }

  return EXIT_SUCCESS;
}
