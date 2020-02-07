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
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
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

  using FilterType = itk::ResampleImageFilter<MovingImageType, FixedImageType>;

  FilterType::Pointer resampler = FilterType::New();

  using InterpolatorType = itk::LinearInterpolateImageFunction<MovingImageType, double>;

  InterpolatorType::Pointer interpolator = InterpolatorType::New();

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

  movingWriter->SetInput(resampler->GetOutput());

  const unsigned int     SpaceDimension = ImageDimension;
  constexpr unsigned int SplineOrder = 3;
  using CoordinateRepType = double;

  using TransformType = itk::BSplineTransform<CoordinateRepType, SpaceDimension, SplineOrder>;

  TransformType::Pointer bsplineTransform = TransformType::New();

  using InitializerType = itk::BSplineTransformInitializer<TransformType, FixedImageType>;
  InitializerType::Pointer transformInitializer = InitializerType::New();

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

  using VectorType = itk::Vector<float, ImageDimension>;
  using DeformationFieldType = itk::Image<VectorType, ImageDimension>;

  DeformationFieldType::Pointer field = DeformationFieldType::New();
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

  using FieldWriterType = itk::ImageFileWriter<DeformationFieldType>;
  FieldWriterType::Pointer fieldWriter = FieldWriterType::New();

  fieldWriter->SetInput(field);

  if (argc >= 6)
  {
    fieldWriter->SetFileName(argv[5]);
    try
    {
      fieldWriter->Update();
    }
    catch (const itk::ExceptionObject & excp)
    {
      std::cerr << "Exception thrown " << std::endl;
      std::cerr << excp << std::endl;
      return EXIT_FAILURE;
    }
  }

  return EXIT_SUCCESS;
}
