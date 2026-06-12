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
#include "itkPocketFFTForwardFFTImageFilter.h"
#include "itkPocketFFTInverseFFTImageFilter.h"
#include "itkPocketFFTRealToHalfHermitianForwardFFTImageFilter.h"
#include "itkPocketFFTHalfHermitianToRealInverseFFTImageFilter.h"

#if defined(ITK_USE_FFTWF) || defined(ITK_USE_FFTWD)
#  include "itkFFTWForwardFFTImageFilter.h"
#  include "itkFFTWInverseFFTImageFilter.h"
#  include "itkFFTWRealToHalfHermitianForwardFFTImageFilter.h"
#  include "itkFFTWHalfHermitianToRealInverseFFTImageFilter.h"
#endif

#include "itkForwardInverseFFTTest.h"
#include "itkTestingMacros.h"

int
itkForwardInverseFFTImageFilterTest(int argc, char * argv[])
{
  if (argc < 2)
  {
    std::cerr << "Missing Parameters." << std::endl;
    std::cout << "Usage: " << itkNameOfTestExecutableMacro(argv) << " <input file> " << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int Dimension{ 2 };
  using FloatType = float;
  using DoubleType = double;
  using FloatImageType = itk::Image<FloatType, Dimension>;
  using DoubleImageType = itk::Image<DoubleType, Dimension>;

  bool success = true;

  using FloatPocketFFTFullFFTType = itk::PocketFFTForwardFFTImageFilter<FloatImageType>;
  using FloatPocketFFTFullIFFTType = itk::PocketFFTInverseFFTImageFilter<FloatPocketFFTFullFFTType::OutputImageType>;
  if (!ForwardInverseFullFFTTest<FloatPocketFFTFullFFTType, FloatPocketFFTFullIFFTType>(argv[1]))
  {
    success = false;
    std::cerr << "Test failed for FloatPocketFFTFullFFTType" << std::endl;
  }
  else
  {
    std::cout << "Test passed for FloatPocketFFTFullFFTType" << std::endl;
  }

  using DoublePocketFFTFullFFTType = itk::PocketFFTForwardFFTImageFilter<DoubleImageType>;
  using DoublePocketFFTFullIFFTType = itk::PocketFFTInverseFFTImageFilter<DoublePocketFFTFullFFTType::OutputImageType>;
  if (!ForwardInverseFullFFTTest<DoublePocketFFTFullFFTType, DoublePocketFFTFullIFFTType>(argv[1]))
  {
    success = false;
    std::cerr << "Test failed for DoublePocketFFTFullFFTType" << std::endl;
  }
  else
  {
    std::cout << "Test passed for DoublePocketFFTFullFFTType" << std::endl;
  }

#if defined(ITK_USE_FFTWF)
  using FloatFFTWFullFFTType = itk::FFTWForwardFFTImageFilter<FloatImageType>;
  using FloatFFTWFullIFFTType = itk::FFTWInverseFFTImageFilter<FloatFFTWFullFFTType::OutputImageType>;
  if (!ForwardInverseFullFFTTest<FloatFFTWFullFFTType, FloatFFTWFullIFFTType>(argv[1]))
  {
    success = false;
    std::cerr << "Test failed for FloatFFTWFullFFTType" << std::endl;
  }
  else
  {
    std::cout << "Test passed for FloatFFTWFullFFTType" << std::endl;
  }
#endif

#if defined(ITK_USE_FFTWD)
  using DoubleFFTWFullFFTType = itk::FFTWForwardFFTImageFilter<DoubleImageType>;
  using DoubleFFTWFullIFFTType = itk::FFTWInverseFFTImageFilter<DoubleFFTWFullFFTType::OutputImageType>;
  if (!ForwardInverseFullFFTTest<DoubleFFTWFullFFTType, DoubleFFTWFullIFFTType>(argv[1]))
  {
    success = false;
    std::cerr << "Test failed for DoubleFFTWFullFFTType" << std::endl;
  }
  else
  {
    std::cout << "Test passed for DoubleFFTWFullFFTType" << std::endl;
  }

#endif


  using FloatPocketFFTHalfFFTType = itk::PocketFFTRealToHalfHermitianForwardFFTImageFilter<FloatImageType>;
  using FloatPocketFFTHalfIFFTType =
    itk::PocketFFTHalfHermitianToRealInverseFFTImageFilter<FloatPocketFFTHalfFFTType::OutputImageType>;
  if (!ForwardInverseHalfFFTTest<FloatPocketFFTHalfFFTType, FloatPocketFFTHalfIFFTType>(argv[1]))
  {
    success = false;
    std::cerr << "Test failed for FloatPocketFFTHalfFFTType" << std::endl;
  }
  else
  {
    std::cout << "Test passed for FloatPocketFFTHalfFFTType" << std::endl;
  }

  using DoublePocketFFTHalfFFTType = itk::PocketFFTRealToHalfHermitianForwardFFTImageFilter<DoubleImageType>;
  using DoublePocketFFTHalfIFFTType =
    itk::PocketFFTHalfHermitianToRealInverseFFTImageFilter<DoublePocketFFTHalfFFTType::OutputImageType>;
  if (!ForwardInverseHalfFFTTest<DoublePocketFFTHalfFFTType, DoublePocketFFTHalfIFFTType>(argv[1]))
  {
    success = false;
    std::cerr << "Test failed for DoublePocketFFTHalfFFTType" << std::endl;
  }
  else
  {
    std::cout << "Test passed for DoublePocketFFTHalfFFTType" << std::endl;
  }

#if defined(ITK_USE_FFTWF)
  using FloatFFTWHalfFFTType = itk::FFTWRealToHalfHermitianForwardFFTImageFilter<FloatImageType>;
  using FloatFFTWHalfIFFTType =
    itk::FFTWHalfHermitianToRealInverseFFTImageFilter<FloatFFTWHalfFFTType::OutputImageType>;
  if (!ForwardInverseHalfFFTTest<FloatFFTWHalfFFTType, FloatFFTWHalfIFFTType>(argv[1]))
  {
    success = false;
    std::cerr << "Test failed for FloatFFTWHalfFFTType" << std::endl;
  }
  else
  {
    std::cout << "Test passed for FloatFFTWHalfFFTType" << std::endl;
  }
#endif

#if defined(ITK_USE_FFTWD)
  using DoubleFFTWHalfFFTType = itk::FFTWRealToHalfHermitianForwardFFTImageFilter<DoubleImageType>;
  using DoubleFFTWHalfIFFTType =
    itk::FFTWHalfHermitianToRealInverseFFTImageFilter<DoubleFFTWHalfFFTType::OutputImageType>;
  if (!ForwardInverseHalfFFTTest<DoubleFFTWHalfFFTType, DoubleFFTWHalfIFFTType>(argv[1]))
  {
    success = false;
    std::cerr << "Test failed for DoubleFFTWHalfFFTType" << std::endl;
  }
  else
  {
    std::cout << "Test passed for DoubleFFTWHalfFFTType" << std::endl;
  }
#endif

  return success ? EXIT_SUCCESS : EXIT_FAILURE;
}
