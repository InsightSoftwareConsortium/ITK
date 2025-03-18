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
#include "itkImageFileWriter.h"
#include "itkPhaseCorrelationOptimizer.h"
#include "itkMontageTestHelper.hxx" //for WriteTransform
#include "itkNumericTraits.h"
#include "itkPhaseCorrelationImageRegistrationMethod.h"
#include <array>

namespace itk
{
template <typename TPixel, unsigned int VDimension>
class HyperSphereImageSource : public itk::Object
{
public:
  using Self = HyperSphereImageSource;
  using Superclass = Object;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using ParametersType = Array<double>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkOverrideGetNameOfClassMacro(HyperSphereImageSource);

  using ImageType = itk::Image<TPixel, VDimension>;

  ImageType *
  GenerateImage()
  {
    m_Image = ImageType::New();

    typename ImageType::IndexType index;
    index.Fill(0);
    typename ImageType::RegionType region;
    region.SetSize(m_ImageSize);
    region.SetIndex(index);

    m_Image->SetLargestPossibleRegion(region);
    m_Image->SetBufferedRegion(region);
    m_Image->SetRequestedRegion(region);
    m_Image->Allocate();

    m_Image->SetSpacing(m_ImageSpacing);
    m_Image->SetOrigin(m_ImageOrigin);
    m_Image->SetDirection(m_ImageDirection);

    multiThreader->ParallelizeImageRegion<VDimension>(
      region,
      [&](const typename ImageType::RegionType & fragment) {
        itk::Point<double, VDimension>               p;
        TPixel                                       value;
        itk::ImageRegionIteratorWithIndex<ImageType> it(m_Image, fragment);
        while (!it.IsAtEnd())
        {
          m_Image->TransformIndexToPhysicalPoint(it.GetIndex(), p);
          if (m_SphereCenter.EuclideanDistanceTo(p) > m_SphereRadius)
          {
            value = itk::NumericTraits<TPixel>::ZeroValue();
          }
          else
          {
            value = itk::NumericTraits<TPixel>::OneValue();
          }
          it.Set(value);
          ++it;
        }
      },
      nullptr);

    return m_Image.GetPointer();
  }

protected:
  HyperSphereImageSource()
  {
    m_SphereRadius = 50.0;
    m_SphereCenter.Fill(50.0);
    m_ImageOrigin.Fill(0.0);
    m_ImageSize.Fill(100);
    m_ImageSpacing.Fill(1.0);
    m_ImageDirection.SetIdentity();
  }

private:
  typename ImageType::Pointer m_Image;

  typename MultiThreaderBase::Pointer multiThreader = MultiThreaderBase::New();

public:
  double                            m_SphereRadius;
  typename ImageType::PointType     m_SphereCenter;
  typename ImageType::PointType     m_ImageOrigin;
  typename ImageType::SizeType      m_ImageSize;
  typename ImageType::SpacingType   m_ImageSpacing;
  typename ImageType::DirectionType m_ImageDirection;
};
} // namespace itk


template <unsigned int VDimension, typename TFixedImagePixel, typename TMovingImagePixel>
int
PhaseCorrelationRegistration(int argc, char * argv[])
{
  if (argc < 6)
  {
    std::cerr << "Usage: " << argv[0] << " <<dimension><fixedTypeChar><movingTypeChar>>";
    std::cerr << " <phaseCorrelationFile> <transformFile> <startSize> <endSize> [movingImageSpacings]" << std::endl;
    std::cerr << "e.g.\n\t" << argv[0] << " 2cf phase.nrrd transform.tfm 17 31 1.0 1.0" << std::endl;
    return EXIT_FAILURE;
  }
  const char * phaseCorrelationFile = argv[2];
  unsigned     startSize = std::stoul(argv[4]);
  unsigned     endSize = std::stoul(argv[5]);

  using FixedImageType = itk::Image<TFixedImagePixel, VDimension>;
  using MovingImageType = itk::Image<TMovingImagePixel, VDimension>;
  using SizeType = typename MovingImageType::SizeType;
  using FixedImageSourceType = itk::HyperSphereImageSource<typename FixedImageType::PixelType, VDimension>;
  using MovingImageSourceType = itk::HyperSphereImageSource<typename MovingImageType::PixelType, VDimension>;
  using PCMType = itk::PhaseCorrelationImageRegistrationMethod<FixedImageType, MovingImageType>;
  using OperatorType = itk::PhaseCorrelationOperator<typename PCMType::InternalPixelType, VDimension>;
  using OptimizerType = itk::PhaseCorrelationOptimizer<typename PCMType::InternalPixelType, VDimension>;
  using TransformType = typename PCMType::TransformType;
  using ParametersType = typename TransformType::ParametersType;

  typename OperatorType::Pointer  pcmOperator = OperatorType::New();
  typename OptimizerType::Pointer pcmOptimizer = OptimizerType::New();
  typename PCMType::Pointer       pcm = PCMType::New();
  pcm->SetOperator(pcmOperator);
  pcm->SetOptimizer(pcmOptimizer);
  pcm->DebugOn();

  typename FixedImageSourceType::Pointer  fixedImageSource = FixedImageSourceType::New();
  typename MovingImageSourceType::Pointer movingImageSource = MovingImageSourceType::New();

  bool pass = true;
  itk::ObjectFactoryBase::RegisterFactory(itk::TxtTransformIOFactory::New());

  using TestCoefficientsType = std::array<double, 5>;
  std::vector<TestCoefficientsType> testCoefficients = { { 2.0, -0.1, 0.05, 0.1, -2.1 },
                                                         { 2.5, -0.3, 0.05, 0.15, 2.15 } };

  for (unsigned size1 = startSize; size1 <= endSize; size1++)
  {
    std::cout << "\nSize " << size1 << std::endl;
    for (const auto & coef : testCoefficients)
    {
      std::cout << "Coefficient set " << (&coef - &testCoefficients[0]) << std::endl;
      fixedImageSource->m_SphereRadius = size1 / coef[0];
      fixedImageSource->m_SphereCenter.Fill(size1 / 2.0);
      movingImageSource->m_SphereRadius = size1 / coef[0];
      movingImageSource->m_SphereCenter.Fill(size1 / 2.0);

      SizeType size;
      size.Fill(size1);
      fixedImageSource->m_ImageSize = size;
      typename FixedImageType::ConstPointer fixedImage = fixedImageSource->GenerateImage();

      ParametersType                        actualParameters(VDimension);
      typename MovingImageType::SizeType    movingSize;
      typename MovingImageType::PointType   movingOrigin;
      typename MovingImageType::SpacingType movingSpacing;
      movingSpacing.Fill(1.0);

      for (unsigned int i = 0; i < VDimension; i++)
      {
        actualParameters[i] = coef[1] + i * (1 + size1 * coef[2]);
        movingImageSource->m_SphereCenter[i] += actualParameters[i];
        movingOrigin[i] = size1 * coef[3] + i * coef[4];
        // movingSpacing[i] = 1.0 / (0.8 + i); //test different spacing (unsupported)
        if (argc > 6 + int(i))
        {
          movingSpacing[i] = std::stod(argv[6 + i]);
        }
        movingSize[i] = (unsigned long)(size[i] / movingSpacing[i] + 3 * std::pow(-1, i));
        movingSize[i] = std::max<itk::SizeValueType>(movingSize[i], 7u);
      }

      // modify the size of the moving image
      // this tests the ability of PCM to padd the images to the same real size
      movingImageSource->m_ImageSize = movingSize;
      movingImageSource->m_ImageSpacing = movingSpacing;

      // shift the origin of the moving image
      // this tests the ability of PCM to introduce between-image origin offset
      // into the transformation (so the final parameters can be directly used to
      // resample the two images into the same coordinate system)
      movingImageSource->m_ImageOrigin = movingOrigin;
      typename MovingImageType::ConstPointer movingImage = movingImageSource->GenerateImage();

      pcm->SetFixedImage(fixedImage);
      pcm->SetMovingImage(movingImage);

      using PadMethod = typename PCMType::PaddingMethodEnum;
      for (auto padMethod : { PadMethod::Zero, PadMethod::Mirror, PadMethod::MirrorWithExponentialDecay })
      {
        pcm->SetPaddingMethod(padMethod);
        std::cout << "Padding method " << static_cast<int>(padMethod) << std::endl;

        using PeakMethod = typename OptimizerType::PeakInterpolationMethodEnum;
        for (auto peakMethod : { PeakMethod::None, PeakMethod::Parabolic, PeakMethod::Cosine })
        {
          pcmOptimizer->SetPeakInterpolationMethod(peakMethod);
          pcm->Modified(); // optimizer is not an "input" to PCM
          // so its modification does not cause a pipeline update automatically
          std::cout << "Peak interpolation method " << static_cast<int>(peakMethod) << std::endl;

          try
          {
            pcm->Update();
            if (pcm->GetFixedImageFFT()->GetLargestPossibleRegion().GetSize(0) == 0)
            {
              std::cout << "Fixed FFT cache's size[0] must be positive!" << std::endl;
              pass = false;
            }
            if (pcm->GetMovingImageFFT()->GetLargestPossibleRegion().GetSize(0) == 0)
            {
              std::cout << "Moving FFT cache's size[0] must be positive!" << std::endl;
              pass = false;
            }
          }
          catch (itk::ExceptionObject & e)
          {
            std::cerr << e << std::endl;
            pass = false;
          }

          // Get registration result and validate it.
          ParametersType finalParameters = pcm->GetTransformParameters();
          ParametersType transformParameters = pcm->GetOutput()->Get()->GetParameters();

          const unsigned int numberOfParameters = actualParameters.Size();
          const double       tolerance = 1.0 + 1e-6;

          // Validate the translation parameters
          for (unsigned int i = 0; i < numberOfParameters; i++)
          {
            std::cout << finalParameters[i] << " == " << actualParameters[i] << " == " << transformParameters[i];

            if ((itk::Math::abs(finalParameters[i] - actualParameters[i]) > tolerance) ||
                (itk::Math::abs(transformParameters[i] - actualParameters[i]) > tolerance))
            {
              std::cout << "    Tolerance exceeded at component " << i << std::endl;
              pass = false;
            }
            else
            {
              std::cout << std::endl;
            }
          }

          // All other parameters must be 0
          for (unsigned int i = numberOfParameters; i < VDimension; i++)
          {
            if ((itk::Math::abs(finalParameters[i]) > tolerance) ||
                (itk::Math::abs(transformParameters[i]) > tolerance))
            {
              std::cout << "Tolerance exceeded at component " << i << std::endl;
              pass = false;
            }
          }

          using WriterType = itk::ImageFileWriter<typename PCMType::RealImageType>;
          typename WriterType::Pointer writer = WriterType::New();
          writer->SetFileName(phaseCorrelationFile);
          writer->SetInput(pcm->GetPhaseCorrelationImage());
          try
          {
            writer->Update();
          }
          catch (itk::ExceptionObject & e)
          {
            std::cerr << e << std::endl;
            pass = false;
          }

          try
          {
            WriteTransform(pcm->GetOutput()->Get(), argv[3]);
          }
          catch (itk::ExceptionObject & e)
          {
            std::cerr << e << std::endl;
            pass = false;
          }
        } // for peakMethod
      } // for padMethod
    } // for testCoefficients
  } // for size1

  std::cout << *pcm;

  if (!pass)
  {
    std::cout << "Test FAILED." << std::endl;
    return EXIT_FAILURE;
  }

  std::cout << "Test PASSED." << std::endl;
  return EXIT_SUCCESS;
}


int
itkMontagePCMTestSynthetic(int argc, char * argv[])
{
  if (argc < 2)
  {
    std::cerr << "Usage: " << argv[0] << " <<Dimension><FixedPixelTypeCharacter><MovingImageTypeCharacter>>"
              << std::endl;
    return EXIT_FAILURE;
  }

  if (!strcmp(argv[1], "2cc"))
  {
    return PhaseCorrelationRegistration<2, signed char, signed char>(argc, argv);
  }
  else if (!strcmp(argv[1], "2ff"))
  {
    return PhaseCorrelationRegistration<2, float, float>(argc, argv);
  }
  else if (!strcmp(argv[1], "2dd"))
  {
    return PhaseCorrelationRegistration<2, double, double>(argc, argv);
  }
  else if (!strcmp(argv[1], "2cf"))
  {
    return PhaseCorrelationRegistration<2, signed char, float>(argc, argv);
  }
  else if (!strcmp(argv[1], "2fd"))
  {
    return PhaseCorrelationRegistration<2, float, double>(argc, argv);
  }
  else if (!strcmp(argv[1], "3cc"))
  {
    return PhaseCorrelationRegistration<3, signed char, signed char>(argc, argv);
  }
  else if (!strcmp(argv[1], "3ff"))
  {
    return PhaseCorrelationRegistration<3, float, float>(argc, argv);
  }
  else if (!strcmp(argv[1], "3dd"))
  {
    return PhaseCorrelationRegistration<3, double, double>(argc, argv);
  }
  else if (!strcmp(argv[1], "3cf"))
  {
    return PhaseCorrelationRegistration<3, signed char, float>(argc, argv);
  }
  else if (!strcmp(argv[1], "3fd"))
  {
    return PhaseCorrelationRegistration<3, float, double>(argc, argv);
  }

  std::cerr << "Unexpected Dimension/FixedPixelType/MovingPixelType!" << std::endl;
  return EXIT_FAILURE;
}
