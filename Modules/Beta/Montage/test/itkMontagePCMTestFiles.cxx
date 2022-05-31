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
#include "itkArray.h"
#include "itkCastImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkPhaseCorrelationOptimizer.h"
#include "itkPhaseCorrelationImageRegistrationMethod.h"
#include "itkResampleImageFilter.h"

namespace itk
{

template <unsigned int VDimension, typename TFixedImagePixel, typename TMovingImagePixel>
int
PhaseCorrelationRegistrationFiles(int argc, char * argv[])
{
  bool pass = true;

  constexpr unsigned int Dimension = VDimension;
  using FixedPixelType = TFixedImagePixel;
  using MovingPixelType = TMovingImagePixel;
  using FixedImageType = itk::Image<FixedPixelType, Dimension>;
  using MovingImageType = itk::Image<MovingPixelType, Dimension>;

  itkAssertOrThrowMacro(argc == 4 + 2 * Dimension, "Wrong number of parameters");

  using FixedReaderType = itk::ImageFileReader<FixedImageType>;
  typename FixedReaderType::Pointer fixedReader = FixedReaderType::New();
  fixedReader->SetFileName(argv[1]);
  const FixedImageType * fixedImage = fixedReader->GetOutput();

  using MovingReaderType = itk::ImageFileReader<MovingImageType>;
  typename MovingReaderType::Pointer movingReader = MovingReaderType::New();
  movingReader->SetFileName(argv[2]);
  typename MovingImageType::Pointer movingImage = movingReader->GetOutput();
  movingImage->Update();
  movingImage->DisconnectPipeline();

  typename MovingImageType::SpacingType spacing = movingImage->GetSpacing();
  typename MovingImageType::PointType   origin = movingImage->GetOrigin();
  for (unsigned d = 0; d < VDimension; d++)
  {
    origin[d] = std::stod(argv[4 + d]);
  }
  movingImage->SetOrigin(origin);

  // Registration method
  using PhaseCorrelationMethodType = itk::PhaseCorrelationImageRegistrationMethod<FixedImageType, MovingImageType>;
  typename PhaseCorrelationMethodType::Pointer phaseCorrelationMethod = PhaseCorrelationMethodType::New();
  phaseCorrelationMethod->SetFixedImage(fixedImage);
  phaseCorrelationMethod->SetMovingImage(movingImage);
  typename PhaseCorrelationMethodType::SizeType pad;
  pad.Fill(8 * sizeof(TFixedImagePixel));
  phaseCorrelationMethod->SetObligatoryPadding(pad);
  phaseCorrelationMethod->DebugOn();

  // Operator type
  using OperatorType =
    itk::PhaseCorrelationOperator<typename PhaseCorrelationMethodType::InternalPixelType, VDimension>;
  typename OperatorType::Pointer pcmOperator = OperatorType::New();
  phaseCorrelationMethod->SetOperator(pcmOperator);

  // Optimizer type
  using OptimizerType =
    itk::PhaseCorrelationOptimizer<typename PhaseCorrelationMethodType::InternalPixelType, VDimension>;
  typename OptimizerType::Pointer pcmOptimizer = OptimizerType::New();
  phaseCorrelationMethod->SetOptimizer(pcmOptimizer);

  // Transform type
  using TransformType = typename PhaseCorrelationMethodType::TransformType;
  using ParametersType = typename TransformType::ParametersType;

  using PadMethod = typename PhaseCorrelationMethodType::PaddingMethodEnum;
  for (auto padMethod : { PadMethod::Zero, PadMethod::Mirror, PadMethod::MirrorWithExponentialDecay })
  {
    phaseCorrelationMethod->SetPaddingMethod(padMethod);
    std::cout << "Padding method " << static_cast<int>(padMethod) << std::endl;
    phaseCorrelationMethod->Update();

    // Get registration result and validate it.
    ParametersType finalParameters = phaseCorrelationMethod->GetTransformParameters();
    ParametersType transformParameters = phaseCorrelationMethod->GetOutput()->Get()->GetParameters();

    const unsigned int numberOfParameters = finalParameters.Size();
    ParametersType     actualParameters(numberOfParameters);
    for (unsigned int ii = 4 + VDimension; ii < 4 + VDimension + numberOfParameters; ++ii)
    {
      actualParameters[ii - 4 - VDimension] = std::stod(argv[ii]);
    }


    const double tolerance = 1.0; // equivalent to 1 pixel.

    // Validate first two parameters (introduced by image source)
    for (unsigned int ii = 0; ii < numberOfParameters; ++ii)
    {
      // the parameters are negated in order to get the inverse transformation.
      // this only works for comparing translation parameters....
      std::cout << finalParameters[ii] << " == " << actualParameters[ii] << " == " << transformParameters[ii]
                << std::endl;

      if ((itk::Math::abs(finalParameters[ii] - actualParameters[ii]) > tolerance * spacing[ii]) ||
          (itk::Math::abs(transformParameters[ii] - actualParameters[ii]) > tolerance * spacing[ii]))
      {
        std::cerr << "Tolerance exceeded at component " << ii << std::endl;
        pass = false;
      }
    }

    using WriterType = itk::ImageFileWriter<typename PhaseCorrelationMethodType::RealImageType>;
    typename WriterType::Pointer writer = WriterType::New();
    writer->SetFileName(argv[3]);
    writer->SetInput(phaseCorrelationMethod->GetPhaseCorrelationImage());
    writer->Update();
  }

  std::cout << std::endl;
  if (!pass)
  {
    std::cout << "Test FAILED." << std::endl;
    return EXIT_FAILURE;
  }

  std::cout << "Test PASSED." << std::endl;
  return EXIT_SUCCESS;
}

} // namespace itk


int
itkMontagePCMTestFiles(int argc, char * argv[])
{
  if (argc < 7)
  {
    std::cerr << "Usage: " << argv[0];
    std::cerr << "  <fixedImageFile> <movingImageFile> <phaseCorrelationImage>";
    std::cerr << "  initialX initialY [initialZ]  trueX trueY [trueZ]" << std::endl;
    return EXIT_FAILURE;
  }

  try
  {
    itk::ImageIOBase::Pointer imageIO = itk::ImageIOFactory::CreateImageIO(argv[1], itk::IOFileModeEnum::ReadMode);
    imageIO->SetFileName(argv[1]);
    imageIO->ReadImageInformation();
    // const itk::ImageIOBase::IOComponentType pixelType = imageIO->GetComponentType();
    const size_t numDimensions = imageIO->GetNumberOfDimensions();
    if (numDimensions <= 2)
    {
      return itk::PhaseCorrelationRegistrationFiles<2, unsigned short, unsigned short>(argc, argv);
    }
    else if (numDimensions == 3)
    {
      return itk::PhaseCorrelationRegistrationFiles<3, unsigned short, unsigned short>(argc, argv);
    }
    else
    {
      std::cerr << "Only 2D and 3D scalar images are supported!" << std::endl;
      return EXIT_FAILURE;
    }
  }
  catch (itk::ExceptionObject & error)
  {
    std::cerr << error << std::endl;
    return EXIT_FAILURE;
  }
}
