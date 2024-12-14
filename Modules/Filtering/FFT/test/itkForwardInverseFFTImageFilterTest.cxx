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
#include "itkVnlForwardFFTImageFilter.h"
#include "itkVnlInverseFFTImageFilter.h"
#include "itkVnlRealToHalfHermitianForwardFFTImageFilter.h"
#include "itkVnlHalfHermitianToRealInverseFFTImageFilter.h"

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
    std::cerr << "Missing Parameters." << '\n';
    std::cout << "Usage: " << itkNameOfTestExecutableMacro(argv) << " <input file> " << '\n';
    return EXIT_FAILURE;
  }

  constexpr unsigned int Dimension = 2;
  using FloatType = float;
  using DoubleType = double;
  using FloatImageType = itk::Image<FloatType, Dimension>;
  using DoubleImageType = itk::Image<DoubleType, Dimension>;

  bool success = true;

  using FloatVnlFullFFTType = itk::VnlForwardFFTImageFilter<FloatImageType>;
  using FloatVnlFullIFFTType = itk::VnlInverseFFTImageFilter<FloatVnlFullFFTType::OutputImageType>;
  if (!ForwardInverseFullFFTTest<FloatVnlFullFFTType, FloatVnlFullIFFTType>(argv[1]))
  {
    success = false;
    std::cerr << "Test failed for FloatVnlFullFFTType" << '\n';
  }
  else
  {
    std::cout << "Test passed for FloatVnlFullFFTType" << '\n';
  }

  using DoubleVnlFullFFTType = itk::VnlForwardFFTImageFilter<DoubleImageType>;
  using DoubleVnlFullIFFTType = itk::VnlInverseFFTImageFilter<DoubleVnlFullFFTType::OutputImageType>;
  if (!ForwardInverseFullFFTTest<DoubleVnlFullFFTType, DoubleVnlFullIFFTType>(argv[1]))
  {
    success = false;
    std::cerr << "Test failed for DoubleVnlFullFFTType" << '\n';
  }
  else
  {
    std::cout << "Test passed for DoubleVnlFullFFTType" << '\n';
  }

#if defined(ITK_USE_FFTWF)
  using FloatFFTWFullFFTType = itk::FFTWForwardFFTImageFilter<FloatImageType>;
  using FloatFFTWFullIFFTType = itk::FFTWInverseFFTImageFilter<FloatFFTWFullFFTType::OutputImageType>;
  if (!ForwardInverseFullFFTTest<FloatFFTWFullFFTType, FloatFFTWFullIFFTType>(argv[1]))
  {
    success = false;
    std::cerr << "Test failed for FloatFFTWFullFFTType" << '\n';
  }
  else
  {
    std::cout << "Test passed for FloatFFTWFullFFTType" << '\n';
  }
#endif

#if defined(ITK_USE_FFTWD)
  using DoubleFFTWFullFFTType = itk::FFTWForwardFFTImageFilter<DoubleImageType>;
  using DoubleFFTWFullIFFTType = itk::FFTWInverseFFTImageFilter<DoubleFFTWFullFFTType::OutputImageType>;
  if (!ForwardInverseFullFFTTest<DoubleFFTWFullFFTType, DoubleFFTWFullIFFTType>(argv[1]))
  {
    success = false;
    std::cerr << "Test failed for DoubleFFTWFullFFTType" << '\n';
  }
  else
  {
    std::cout << "Test passed for DoubleFFTWFullFFTType" << '\n';
  }

#endif


  using FloatVnlHalfFFTType = itk::VnlRealToHalfHermitianForwardFFTImageFilter<FloatImageType>;
  using FloatVnlHalfIFFTType = itk::VnlHalfHermitianToRealInverseFFTImageFilter<FloatVnlHalfFFTType::OutputImageType>;
  if (!ForwardInverseHalfFFTTest<FloatVnlHalfFFTType, FloatVnlHalfIFFTType>(argv[1]))
  {
    success = false;
    std::cerr << "Test failed for FloatVnlHalfFFTType" << '\n';
  }
  else
  {
    std::cout << "Test passed for FloatVnlHalfFFTType" << '\n';
  }

  using DoubleVnlHalfFFTType = itk::VnlRealToHalfHermitianForwardFFTImageFilter<DoubleImageType>;
  using DoubleVnlHalfIFFTType = itk::VnlHalfHermitianToRealInverseFFTImageFilter<DoubleVnlHalfFFTType::OutputImageType>;
  if (!ForwardInverseHalfFFTTest<DoubleVnlHalfFFTType, DoubleVnlHalfIFFTType>(argv[1]))
  {
    success = false;
    std::cerr << "Test failed for DoubleVnlHalfFFTType" << '\n';
  }
  else
  {
    std::cout << "Test passed for DoubleVnlHalfFFTType" << '\n';
  }

#if defined(ITK_USE_FFTWF)
  using FloatFFTWHalfFFTType = itk::FFTWRealToHalfHermitianForwardFFTImageFilter<FloatImageType>;
  using FloatFFTWHalfIFFTType =
    itk::FFTWHalfHermitianToRealInverseFFTImageFilter<FloatFFTWHalfFFTType::OutputImageType>;
  if (!ForwardInverseHalfFFTTest<FloatFFTWHalfFFTType, FloatFFTWHalfIFFTType>(argv[1]))
  {
    success = false;
    std::cerr << "Test failed for FloatFFTWHalfFFTType" << '\n';
  }
  else
  {
    std::cout << "Test passed for FloatFFTWHalfFFTType" << '\n';
  }
#endif

#if defined(ITK_USE_FFTWD)
  using DoubleFFTWHalfFFTType = itk::FFTWRealToHalfHermitianForwardFFTImageFilter<DoubleImageType>;
  using DoubleFFTWHalfIFFTType =
    itk::FFTWHalfHermitianToRealInverseFFTImageFilter<DoubleFFTWHalfFFTType::OutputImageType>;
  if (!ForwardInverseHalfFFTTest<DoubleFFTWHalfFFTType, DoubleFFTWHalfIFFTType>(argv[1]))
  {
    success = false;
    std::cerr << "Test failed for DoubleFFTWHalfFFTType" << '\n';
  }
  else
  {
    std::cout << "Test passed for DoubleFFTWHalfFFTType" << '\n';
  }
#endif

  return success ? EXIT_SUCCESS : EXIT_FAILURE;
}
