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

#include "itkGTest.h"
#include "itkConfigure.h"

#include <fftw3.h>

#include <array>

namespace
{
constexpr int signalSize{ 8 };
constexpr int numberOfThreads{ 2 };
} // namespace

#if defined(ITK_USE_FFTWD)
TEST(FFTWModule, DoublePrecisionImpulseHasFlatSpectrum)
{
  constexpr double tolerance{ 1e-12 };

  ASSERT_NE(fftw_init_threads(), 0);
  fftw_plan_with_nthreads(numberOfThreads);

  std::array<double, signalSize> impulse{ 1.0 };
  std::array<fftw_complex, signalSize / 2 + 1> spectrum{};
  const fftw_plan plan = fftw_plan_dft_r2c_1d(signalSize, impulse.data(), spectrum.data(), FFTW_ESTIMATE);
  ASSERT_NE(plan, nullptr);
  fftw_execute(plan);

  for (const auto & bin : spectrum)
  {
    EXPECT_NEAR(bin[0], 1.0, tolerance);
    EXPECT_NEAR(bin[1], 0.0, tolerance);
  }
  fftw_destroy_plan(plan);
  fftw_cleanup_threads();
}
#endif

#if defined(ITK_USE_FFTWF)
TEST(FFTWModule, SinglePrecisionImpulseHasFlatSpectrum)
{
  constexpr float tolerance{ 1e-6f };

  ASSERT_NE(fftwf_init_threads(), 0);
  fftwf_plan_with_nthreads(numberOfThreads);

  std::array<float, signalSize> impulse{ 1.0f };
  std::array<fftwf_complex, signalSize / 2 + 1> spectrum{};
  const fftwf_plan plan = fftwf_plan_dft_r2c_1d(signalSize, impulse.data(), spectrum.data(), FFTW_ESTIMATE);
  ASSERT_NE(plan, nullptr);
  fftwf_execute(plan);

  for (const auto & bin : spectrum)
  {
    EXPECT_NEAR(bin[0], 1.0f, tolerance);
    EXPECT_NEAR(bin[1], 0.0f, tolerance);
  }
  fftwf_destroy_plan(plan);
  fftwf_cleanup_threads();
}
#endif
