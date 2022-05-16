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
#include "itkTestingMacros.h"

#include <fstream>

//  The following section of code implements a Command observer
//  used to monitor the evolution of the registration process.
//
class CommandProgressUpdate : public itk::Command
{
public:
  using Self = CommandProgressUpdate;
  using Superclass = itk::Command;
  using Pointer = itk::SmartPointer<Self>;
  itkNewMacro(Self);

protected:
  CommandProgressUpdate() = default;

public:
  void
  Execute(itk::Object * caller, const itk::EventObject & event) override
  {
    Execute((const itk::Object *)caller, event);
  }

  void
  Execute(const itk::Object * object, const itk::EventObject & event) override
  {
    const auto * filter = dynamic_cast<const itk::ProcessObject *>(object);
    if (!itk::ProgressEvent().CheckEvent(&event))
    {
      return;
    }
    std::cout << filter->GetProgress() << std::endl;
  }
};


template <unsigned int VSplineOrder>
class BSplineTransformTest2Helper
{
public:
  static int
  RunTest(int argc, char * argv[])
  {
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

    typename FixedImageType::ConstPointer fixedImage = fixedReader->GetOutput();


    using FilterType = itk::ResampleImageFilter<MovingImageType, FixedImageType>;

    auto resampler = FilterType::New();

    using InterpolatorType = itk::LinearInterpolateImageFunction<MovingImageType, double>;

    auto interpolator = InterpolatorType::New();

    resampler->SetInterpolator(interpolator);

    typename FixedImageType::SpacingType   fixedSpacing = fixedImage->GetSpacing();
    typename FixedImageType::PointType     fixedOrigin = fixedImage->GetOrigin();
    typename FixedImageType::DirectionType fixedDirection = fixedImage->GetDirection();

    resampler->SetOutputSpacing(fixedSpacing);
    resampler->SetOutputOrigin(fixedOrigin);
    resampler->SetOutputDirection(fixedDirection);


    typename FixedImageType::RegionType fixedRegion = fixedImage->GetBufferedRegion();
    typename FixedImageType::SizeType   fixedSize = fixedRegion.GetSize();
    resampler->SetSize(fixedSize);
    resampler->SetOutputStartIndex(fixedRegion.GetIndex());


    resampler->SetInput(movingReader->GetOutput());

    const unsigned int SpaceDimension = ImageDimension;
    using CoordinateRepType = double;

    using TransformType = itk::BSplineTransform<CoordinateRepType, SpaceDimension, VSplineOrder>;

    auto bsplineTransform = TransformType::New();

    using MeshSizeType = typename TransformType::MeshSizeType;
    MeshSizeType meshSize;
    meshSize.Fill(4);

    using PhysicalDimensionsType = typename TransformType::PhysicalDimensionsType;
    PhysicalDimensionsType fixedDimensions;
    for (unsigned int d = 0; d < ImageDimension; ++d)
    {
      fixedDimensions[d] = fixedSpacing[d] * (fixedSize[d] - 1.0);
    }

    bsplineTransform->SetTransformDomainOrigin(fixedOrigin);
    bsplineTransform->SetTransformDomainDirection(fixedDirection);
    bsplineTransform->SetTransformDomainPhysicalDimensions(fixedDimensions);
    bsplineTransform->SetTransformDomainMeshSize(meshSize);

    using ParametersType = typename TransformType::ParametersType;
    const unsigned int numberOfParameters = bsplineTransform->GetNumberOfParameters();
    ParametersType     parameters(numberOfParameters);

    std::ifstream infile;

    infile.open(argv[1]);

    const unsigned int numberOfNodes = numberOfParameters / SpaceDimension;
    for (unsigned int n = 0; n < numberOfNodes; ++n)
    {
      infile >> parameters[n];
      infile >> parameters[n + numberOfNodes];
    }

    infile.close();

    bsplineTransform->SetParameters(parameters);

    auto observer = CommandProgressUpdate::New();

    resampler->AddObserver(itk::ProgressEvent(), observer);

    resampler->SetTransform(bsplineTransform);

    ITK_TRY_EXPECT_NO_EXCEPTION(itk::WriteImage(resampler->GetOutput(), argv[4]));


    using VectorType = itk::Vector<float, ImageDimension>;
    using DeformationFieldType = itk::Image<VectorType, ImageDimension>;

    auto field = DeformationFieldType::New();
    field->SetRegions(fixedRegion);
    field->SetOrigin(fixedOrigin);
    field->SetSpacing(fixedSpacing);
    field->Allocate();

    using FieldIterator = itk::ImageRegionIterator<DeformationFieldType>;
    FieldIterator fi(field, fixedRegion);

    fi.GoToBegin();

    typename TransformType::InputPointType   fixedPoint;
    typename TransformType::OutputPointType  movingPoint;
    typename DeformationFieldType::IndexType index;

    VectorType displacement;

    while (!fi.IsAtEnd())
    {
      index = fi.GetIndex();
      field->TransformIndexToPhysicalPoint(index, fixedPoint);
      movingPoint = bsplineTransform->TransformPoint(fixedPoint);
      displacement[0] = movingPoint[0] - fixedPoint[0];
      displacement[1] = movingPoint[1] - fixedPoint[1];
      fi.Set(displacement);
      ++fi;
    }

    if (argc >= 6)
    {
      ITK_TRY_EXPECT_NO_EXCEPTION(itk::WriteImage(field, argv[5]));
    }
    return EXIT_SUCCESS;
  }
};


int
itkBSplineTransformTest2(int argc, char * argv[])
{

  if (argc < 5)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " coefficientsFile fixedImage ";
    std::cerr << "movingImage deformedMovingImage" << std::endl;
    std::cerr << "[deformationField][spline order 2,3]" << std::endl;
    return EXIT_FAILURE;
  }

  unsigned int splineOrder = 3;

  if (argc > 6)
  {
    splineOrder = std::stoi(argv[6]);
  }

  int status = 0;

  switch (splineOrder)
  {
    case 1:
    {
      status |= BSplineTransformTest2Helper<1>::RunTest(argc, argv);
      break;
    }
    case 2:
    {
      status |= BSplineTransformTest2Helper<2>::RunTest(argc, argv);
      break;
    }
    case 3:
    {
      status |= BSplineTransformTest2Helper<3>::RunTest(argc, argv);
      break;
    }
  }

  if (status)
  {
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
