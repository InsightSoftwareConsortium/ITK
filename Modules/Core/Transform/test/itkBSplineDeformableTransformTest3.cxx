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

#include "itkBSplineDeformableTransform.h"
#include "itkSimilarity2DTransform.h"

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
class BSplineDeformableTransformTest3Helper
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

    typename FixedReaderType::Pointer fixedReader = FixedReaderType::New();
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


    typename MovingReaderType::Pointer movingReader = MovingReaderType::New();

    movingReader->SetFileName(argv[3]);

    typename FixedImageType::ConstPointer fixedImage = fixedReader->GetOutput();


    using FilterType = itk::ResampleImageFilter<MovingImageType, FixedImageType>;

    typename FilterType::Pointer resampler = FilterType::New();

    using InterpolatorType = itk::LinearInterpolateImageFunction<MovingImageType, double>;

    typename InterpolatorType::Pointer interpolator = InterpolatorType::New();

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

    using TransformType = itk::BSplineDeformableTransform<CoordinateRepType, SpaceDimension, VSplineOrder>;

    typename TransformType::Pointer bsplineTransform = TransformType::New();


    using RegionType = typename TransformType::RegionType;
    RegionType                    bsplineRegion;
    typename RegionType::SizeType size;

    const unsigned int numberOfGridNodesOutsideTheImageSupport = VSplineOrder;

    constexpr unsigned int numberOfGridNodesInsideTheImageSupport = 5;

    const unsigned int numberOfGridNodes =
      numberOfGridNodesInsideTheImageSupport + numberOfGridNodesOutsideTheImageSupport;

    const unsigned int numberOfGridCells = numberOfGridNodesInsideTheImageSupport - 1;

    size.Fill(numberOfGridNodes);
    bsplineRegion.SetSize(size);

    using SpacingType = typename TransformType::SpacingType;
    SpacingType spacing;

    using OriginType = typename TransformType::OriginType;
    OriginType origin;

    spacing[0] = fixedSpacing[0] * fixedSize[0] / numberOfGridCells;
    spacing[1] = fixedSpacing[1] * fixedSize[1] / numberOfGridCells;

    const unsigned int orderShift = VSplineOrder / 2;

    origin[0] = fixedOrigin[0] - orderShift * spacing[0] - fixedSpacing[0] / 2.0;
    origin[1] = fixedOrigin[1] - orderShift * spacing[1] - fixedSpacing[1] / 2.0;

    bsplineTransform->SetGridSpacing(spacing);
    bsplineTransform->SetGridOrigin(origin);
    bsplineTransform->SetGridRegion(bsplineRegion);
    bsplineTransform->SetGridDirection(fixedImage->GetDirection());

    using BulkTransformType = itk::Similarity2DTransform<CoordinateRepType>;
    BulkTransformType::Pointer bulkTransform = BulkTransformType::New();
    bulkTransform->SetIdentity();

    BulkTransformType::ParametersType bulkParameters = bulkTransform->GetParameters();
    bulkParameters[0] = 0.5; // half the scale.
    bulkTransform->SetParameters(bulkParameters);
    std::cout << " parameters " << bulkTransform->GetParameters() << "\n";
    bsplineTransform->SetBulkTransform(bulkTransform);

    using ParametersType = typename TransformType::ParametersType;

    const unsigned int numberOfParameters = bsplineTransform->GetNumberOfParameters();

    const unsigned int numberOfNodes = numberOfParameters / SpaceDimension;

    ParametersType parameters(numberOfParameters);


    std::ifstream infile;

    infile.open(argv[1]);

    for (unsigned int n = 0; n < numberOfNodes; n++)
    {
      infile >> parameters[n];
      infile >> parameters[n + numberOfNodes];
    }

    infile.close();


    bsplineTransform->SetParameters(parameters);


    typename CommandProgressUpdate::Pointer observer = CommandProgressUpdate::New();

    resampler->AddObserver(itk::ProgressEvent(), observer);


    resampler->SetTransform(bsplineTransform);

    try
    {
      itk::WriteImage(resampler->GetOutput(), argv[4]);
    }
    catch (const itk::ExceptionObject & excp)
    {
      std::cerr << "Exception thrown " << std::endl;
      std::cerr << excp << std::endl;
      return EXIT_FAILURE;
    }


    using VectorType = itk::Vector<float, ImageDimension>;
    using DeformationFieldType = itk::Image<VectorType, ImageDimension>;

    typename DeformationFieldType::Pointer field = DeformationFieldType::New();
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
      try
      {
        itk::WriteImage(field, argv[5]);
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
};


int
itkBSplineDeformableTransformTest3(int argc, char * argv[])
{

  if (argc < 7)
  {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " coefficientsFile fixedImage ";
    std::cerr << "movingImage deformedMovingImage" << std::endl;
    std::cerr << "[deformationField][multithreader use #threads]" << std::endl;
    return EXIT_FAILURE;
  }

  const int numberOfThreads = std::stoi(argv[6]);

  int status = 0;
  switch (numberOfThreads)
  {
    case 0:
    {
      // Don't invoke MultiThreader at all.
      status |= BSplineDeformableTransformTest3Helper<3>::RunTest(argc, argv);
      break;
    }
    default:
    {
      // Use MultiThreader with argv[6] threads
      itk::MultiThreaderBase::SetGlobalDefaultNumberOfThreads(numberOfThreads);
      itk::MultiThreaderBase::SetGlobalMaximumNumberOfThreads(numberOfThreads);
      status |= BSplineDeformableTransformTest3Helper<3>::RunTest(argc, argv);
      break;
    }
  }

  if (status)
  {
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
