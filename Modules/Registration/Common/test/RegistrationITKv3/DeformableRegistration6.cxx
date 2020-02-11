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

//
// This example illustrates the use of the \doxygen{BSplineTransform}
// class in a manually controlled multi-resolution scheme. Here we define two
// transforms at two different resolution levels. A first registration is
// performed with the spline grid of low resolution, and the results are then
// used for initializing a higher resolution grid. Since this example is quite
// similar to the previous example on the use of the
// \code{BSplineTransform} we omit here most of the details already
// discussed and will focus on the aspects related to the multi-resolution
// approach.
//
// \index{itk::BSplineTransform}
// \index{itk::BSplineTransform!DeformableRegistration}
// \index{itk::LBFGSOptimizer}
//
//

#include "itkImageRegistrationMethod.h"
#include "itkMeanSquaresImageToImageMetric.h"

//
//  We include the header files for the transform, optimizer and adaptor.
//
//  \index{itk::BSplineTransform!header}
//  \index{itk::LBFGSOptimizer!header}
//

#include "itkBSplineTransform.h"
#include "itkLBFGSOptimizer.h"


#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include "itkResampleImageFilter.h"
#include "itkCastImageFilter.h"
#include "itkSquaredDifferenceImageFilter.h"

#include "itkBSplineResampleImageFunction.h"
#include "itkIdentityTransform.h"
#include "itkBSplineDecompositionImageFilter.h"

// NOTE: the LBFGSOptimizer does not invoke events


#include "itkTestDriverIncludeRequiredIOFactories.h"
int
main(int argc, char * argv[])
{
  RegisterRequiredFactories();
  if (argc < 4)
  {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " fixedImageFile  movingImageFile outputImagefile  ";
    std::cerr << " [differenceOutputfile] [differenceBeforeRegistration] ";
    std::cerr << " [deformationField] ";
    return EXIT_FAILURE;
  }

  constexpr unsigned int ImageDimension = 2;
  using PixelType = float;

  using FixedImageType = itk::Image<PixelType, ImageDimension>;
  using MovingImageType = itk::Image<PixelType, ImageDimension>;


  //
  //  We instantiate the type of the \code{BSplineTransform} using
  //  as template parameters the type for coordinates representation, the
  //  dimension of the space, and the order of the BSpline.
  //
  //  \index{BSplineTransform!New}
  //  \index{BSplineTransform!Instantiation}
  //

  const unsigned int     SpaceDimension = ImageDimension;
  constexpr unsigned int SplineOrder = 3;
  using CoordinateRepType = double;

  using TransformType = itk::BSplineTransform<CoordinateRepType, SpaceDimension, SplineOrder>;


  using OptimizerType = itk::LBFGSOptimizer;


  using MetricType = itk::MeanSquaresImageToImageMetric<FixedImageType, MovingImageType>;

  using InterpolatorType = itk::LinearInterpolateImageFunction<MovingImageType, double>;

  using RegistrationType = itk::ImageRegistrationMethod<FixedImageType, MovingImageType>;

  MetricType::Pointer       metric = MetricType::New();
  OptimizerType::Pointer    optimizer = OptimizerType::New();
  InterpolatorType::Pointer interpolator = InterpolatorType::New();
  RegistrationType::Pointer registration = RegistrationType::New();


  registration->SetMetric(metric);
  registration->SetOptimizer(optimizer);
  registration->SetInterpolator(interpolator);


  //
  //  We construct two transform objects, each one will be configured for a resolution level.
  //  Notice than in this multi-resolution scheme we are not modifying the
  //  resolution of the image, but rather the flexibility of the deformable
  //  transform itself.
  //
  //  \index{itk::RegistrationMethod!SetTransform()}
  //

  TransformType::Pointer transformLow = TransformType::New();
  registration->SetTransform(transformLow);

  using FixedImageReaderType = itk::ImageFileReader<FixedImageType>;
  using MovingImageReaderType = itk::ImageFileReader<MovingImageType>;

  FixedImageReaderType::Pointer  fixedImageReader = FixedImageReaderType::New();
  MovingImageReaderType::Pointer movingImageReader = MovingImageReaderType::New();

  fixedImageReader->SetFileName(argv[1]);
  movingImageReader->SetFileName(argv[2]);

  FixedImageType::ConstPointer fixedImage = fixedImageReader->GetOutput();

  registration->SetFixedImage(fixedImage);
  registration->SetMovingImage(movingImageReader->GetOutput());

  fixedImageReader->Update();

  FixedImageType::RegionType fixedRegion = fixedImage->GetBufferedRegion();

  registration->SetFixedImageRegion(fixedRegion);

  unsigned int numberOfGridNodes = 8;

  TransformType::PhysicalDimensionsType fixedPhysicalDimensions;
  TransformType::MeshSizeType           meshSize;
  TransformType::OriginType             fixedOrigin;

  for (unsigned int i = 0; i < SpaceDimension; i++)
  {
    fixedOrigin[i] = fixedImage->GetOrigin()[i];
    fixedPhysicalDimensions[i] =
      fixedImage->GetSpacing()[i] * static_cast<double>(fixedImage->GetLargestPossibleRegion().GetSize()[i] - 1);
  }
  meshSize.Fill(numberOfGridNodes - SplineOrder);

  transformLow->SetTransformDomainOrigin(fixedOrigin);
  transformLow->SetTransformDomainPhysicalDimensions(fixedPhysicalDimensions);
  transformLow->SetTransformDomainMeshSize(meshSize);
  transformLow->SetTransformDomainDirection(fixedImage->GetDirection());


  using ParametersType = TransformType::ParametersType;

  const unsigned int numberOfParameters = transformLow->GetNumberOfParameters();

  ParametersType parametersLow(numberOfParameters);

  parametersLow.Fill(0.0);

  transformLow->SetParameters(parametersLow);

  //
  //  We now pass the parameters of the current transform as the initial
  //  parameters to be used when the registration process starts.
  //

  registration->SetInitialTransformParameters(transformLow->GetParameters());


  optimizer->SetGradientConvergenceTolerance(0.05);
  optimizer->SetLineSearchAccuracy(0.9);
  optimizer->SetDefaultStepLength(1.5);
  optimizer->TraceOn();
  optimizer->SetMaximumNumberOfFunctionEvaluations(1000);
  std::cout << "Starting Registration with low resolution transform" << std::endl;

  try
  {
    registration->Update();
    std::cout << "Optimizer stop condition = " << registration->GetOptimizer()->GetStopConditionDescription()
              << std::endl;
  }
  catch (const itk::ExceptionObject & err)
  {
    std::cerr << "ExceptionObject caught !" << std::endl;
    std::cerr << err << std::endl;
    return EXIT_FAILURE;
  }


  //
  //  Once the registration has finished with the low resolution grid, we
  //  proceed to instantiate a higher resolution
  //  \code{BSplineTransform}.
  //

  TransformType::Pointer transformHigh = TransformType::New();

  numberOfGridNodes = 12;

  for (unsigned int i = 0; i < SpaceDimension; i++)
  {
    fixedOrigin[i] = fixedImage->GetOrigin()[i];
    fixedPhysicalDimensions[i] =
      fixedImage->GetSpacing()[i] * static_cast<double>(fixedImage->GetLargestPossibleRegion().GetSize()[i] - 1);
  }
  meshSize.Fill(numberOfGridNodes - SplineOrder);

  transformHigh->SetTransformDomainOrigin(fixedOrigin);
  transformHigh->SetTransformDomainPhysicalDimensions(fixedPhysicalDimensions);
  transformHigh->SetTransformDomainMeshSize(meshSize);
  transformHigh->SetTransformDomainDirection(fixedImage->GetDirection());

  ParametersType parametersHigh(transformHigh->GetNumberOfParameters());
  parametersHigh.Fill(0.0);

  //
  //  Now we need to initialize the BSpline coefficients of the higher resolution
  //  transform. This is done by first computing the actual deformation field
  //  at the higher resolution from the lower resolution BSpline coefficients.
  //  Then a BSpline decomposition is done to obtain the BSpline coefficient of
  //  the higher resolution transform.
  //

  unsigned int counter = 0;

  for (unsigned int k = 0; k < SpaceDimension; k++)
  {
    using ParametersImageType = TransformType::ImageType;
    using ResamplerType = itk::ResampleImageFilter<ParametersImageType, ParametersImageType>;
    ResamplerType::Pointer upsampler = ResamplerType::New();

    using FunctionType = itk::BSplineResampleImageFunction<ParametersImageType, double>;
    FunctionType::Pointer function = FunctionType::New();

    using IdentityTransformType = itk::IdentityTransform<double, SpaceDimension>;
    IdentityTransformType::Pointer identity = IdentityTransformType::New();

    upsampler->SetInput(transformLow->GetCoefficientImages()[k]);
    upsampler->SetInterpolator(function);
    upsampler->SetTransform(identity);
    upsampler->SetSize(transformHigh->GetCoefficientImages()[k]->GetLargestPossibleRegion().GetSize());
    upsampler->SetOutputSpacing(transformHigh->GetCoefficientImages()[k]->GetSpacing());
    upsampler->SetOutputOrigin(transformHigh->GetCoefficientImages()[k]->GetOrigin());
    upsampler->SetOutputDirection(fixedImage->GetDirection());

    using DecompositionType = itk::BSplineDecompositionImageFilter<ParametersImageType, ParametersImageType>;
    DecompositionType::Pointer decomposition = DecompositionType::New();

    decomposition->SetSplineOrder(SplineOrder);
    decomposition->SetInput(upsampler->GetOutput());
    decomposition->Update();

    ParametersImageType::Pointer newCoefficients = decomposition->GetOutput();

    // copy the coefficients into the parameter array
    using Iterator = itk::ImageRegionIterator<ParametersImageType>;
    Iterator it(newCoefficients, transformHigh->GetCoefficientImages()[k]->GetLargestPossibleRegion());
    while (!it.IsAtEnd())
    {
      parametersHigh[counter++] = it.Get();
      ++it;
    }
  }

  transformHigh->SetParameters(parametersHigh);

  //
  //  We now pass the parameters of the high resolution transform as the initial
  //  parameters to be used in a second stage of the registration process.
  //

  std::cout << "Starting Registration with high resolution transform" << std::endl;

  registration->SetInitialTransformParameters(transformHigh->GetParameters());
  registration->SetTransform(transformHigh);
  //
  //  Typically, we will also want to tighten the optimizer parameters
  //  when we move from lower to higher resolution grid.
  //
  optimizer->SetGradientConvergenceTolerance(0.01);
  optimizer->SetDefaultStepLength(0.25);
  try
  {
    registration->Update();
  }
  catch (const itk::ExceptionObject & err)
  {
    std::cerr << "ExceptionObject caught !" << std::endl;
    std::cerr << err << std::endl;
    return EXIT_FAILURE;
  }

  // Finally we use the last transform parameters in order to resample the image.
  //
  transformHigh->SetParameters(registration->GetLastTransformParameters());

  using ResampleFilterType = itk::ResampleImageFilter<MovingImageType, FixedImageType>;

  ResampleFilterType::Pointer resample = ResampleFilterType::New();

  resample->SetTransform(transformHigh);
  resample->SetInput(movingImageReader->GetOutput());

  resample->SetSize(fixedImage->GetLargestPossibleRegion().GetSize());
  resample->SetOutputOrigin(fixedImage->GetOrigin());
  resample->SetOutputSpacing(fixedImage->GetSpacing());
  resample->SetOutputDirection(fixedImage->GetDirection());
  resample->SetDefaultPixelValue(100);

  using OutputPixelType = unsigned char;

  using OutputImageType = itk::Image<OutputPixelType, ImageDimension>;

  using CastFilterType = itk::CastImageFilter<FixedImageType, OutputImageType>;

  using WriterType = itk::ImageFileWriter<OutputImageType>;


  WriterType::Pointer     writer = WriterType::New();
  CastFilterType::Pointer caster = CastFilterType::New();


  writer->SetFileName(argv[3]);


  caster->SetInput(resample->GetOutput());
  writer->SetInput(caster->GetOutput());


  try
  {
    writer->Update();
  }
  catch (const itk::ExceptionObject & err)
  {
    std::cerr << "ExceptionObject caught !" << std::endl;
    std::cerr << err << std::endl;
    return EXIT_FAILURE;
  }

  using DifferenceFilterType = itk::SquaredDifferenceImageFilter<FixedImageType, FixedImageType, OutputImageType>;

  DifferenceFilterType::Pointer difference = DifferenceFilterType::New();

  WriterType::Pointer writer2 = WriterType::New();
  writer2->SetInput(difference->GetOutput());


  // Compute the difference image between the
  // fixed and resampled moving image.
  if (argc >= 5)
  {
    difference->SetInput1(fixedImageReader->GetOutput());
    difference->SetInput2(resample->GetOutput());
    writer2->SetFileName(argv[4]);
    try
    {
      writer2->Update();
    }
    catch (const itk::ExceptionObject & err)
    {
      std::cerr << "ExceptionObject caught !" << std::endl;
      std::cerr << err << std::endl;
      return EXIT_FAILURE;
    }
  }


  // Compute the difference image between the
  // fixed and moving image before registration.
  if (argc >= 6)
  {
    writer2->SetFileName(argv[5]);
    difference->SetInput1(fixedImageReader->GetOutput());
    difference->SetInput2(movingImageReader->GetOutput());
    try
    {
      writer2->Update();
    }
    catch (const itk::ExceptionObject & err)
    {
      std::cerr << "ExceptionObject caught !" << std::endl;
      std::cerr << err << std::endl;
      return EXIT_FAILURE;
    }
  }

  // Generate the explicit deformation field resulting from
  // the registration.

  using VectorType = itk::Vector<float, ImageDimension>;
  using DisplacementFieldType = itk::Image<VectorType, ImageDimension>;

  DisplacementFieldType::Pointer field = DisplacementFieldType::New();
  field->SetRegions(fixedRegion);
  field->SetOrigin(fixedImage->GetOrigin());
  field->SetSpacing(fixedImage->GetSpacing());
  field->SetDirection(fixedImage->GetDirection());
  field->Allocate();

  using FieldIterator = itk::ImageRegionIterator<DisplacementFieldType>;
  FieldIterator fi(field, fixedRegion);

  fi.GoToBegin();

  TransformType::InputPointType    fixedPoint;
  TransformType::OutputPointType   movingPoint;
  DisplacementFieldType::IndexType index;

  VectorType displacement;

  while (!fi.IsAtEnd())
  {
    index = fi.GetIndex();
    field->TransformIndexToPhysicalPoint(index, fixedPoint);
    movingPoint = transformHigh->TransformPoint(fixedPoint);
    displacement = movingPoint - fixedPoint;
    fi.Set(displacement);
    ++fi;
  }

  using FieldWriterType = itk::ImageFileWriter<DisplacementFieldType>;
  FieldWriterType::Pointer fieldWriter = FieldWriterType::New();

  fieldWriter->SetInput(field);

  if (argc >= 7)
  {
    fieldWriter->SetFileName(argv[6]);
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
