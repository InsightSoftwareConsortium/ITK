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
#ifndef itkFFTWCommonExtended_h
#define itkFFTWCommonExtended_h

#if defined(ITK_USE_FFTWF) || defined(ITK_USE_FFTWD)
#  if defined(ITK_USE_CUFFTW)
#    include "cufftw.h"
#  else
#    include "itkFFTWGlobalConfiguration.h"
#    include "fftw3.h"
#  endif
#endif

#include <mutex>

namespace itk
{

namespace fftw
{
/**
 * \class Interface
 * \brief Wrapper for FFTW API
 *
 * \ingroup ITKFFT
 * \ingroup FourierTransform
 */
template <typename TPixel>
class ComplexToComplexProxy
{
  // empty -- only double and float specializations work
protected:
  ComplexToComplexProxy() = default;
  ~ComplexToComplexProxy() = default;
};

#if defined(ITK_USE_FFTWF)

template <>
class ComplexToComplexProxy<float>
{
public:
  using PixelType = float;
  using ComplexType = fftwf_complex;
  using PlanType = fftwf_plan;
  using Self = ComplexToComplexProxy<float>;

  static PlanType
  Plan_dft_c2r_1d(int n, ComplexType * in, PixelType * out, unsigned int flags, int threads = 1)
  {
#  ifndef ITK_USE_CUFFTW
    std::lock_guard<FFTWGlobalConfiguration::MutexType> lock(FFTWGlobalConfiguration::GetLockMutex());
    fftwf_plan_with_nthreads(threads);
#  else
    (void)threads;
#  endif
    PlanType plan = fftwf_plan_dft_c2r_1d(n, in, out, flags);
    return plan;
  }
  static PlanType
  Plan_dft_c2r_2d(int nx, int ny, ComplexType * in, PixelType * out, unsigned int flags, int threads = 1)
  {
#  ifndef ITK_USE_CUFFTW
    std::lock_guard<FFTWGlobalConfiguration::MutexType> lock(FFTWGlobalConfiguration::GetLockMutex());
    fftwf_plan_with_nthreads(threads);
#  else
    (void)threads;
#  endif
    PlanType plan = fftwf_plan_dft_c2r_2d(nx, ny, in, out, flags);
    return plan;
  }
  static PlanType
  Plan_dft_c2r_3d(int nx, int ny, int nz, ComplexType * in, PixelType * out, unsigned int flags, int threads = 1)
  {
#  ifndef ITK_USE_CUFFTW
    std::lock_guard<FFTWGlobalConfiguration::MutexType> lock(FFTWGlobalConfiguration::GetLockMutex());
    fftwf_plan_with_nthreads(threads);
#  else
    (void)threads;
#  endif
    PlanType plan = fftwf_plan_dft_c2r_3d(nx, ny, nz, in, out, flags);
    return plan;
  }
  static PlanType
  Plan_dft_c2r(int rank, const int * n, ComplexType * in, PixelType * out, unsigned int flags, int threads = 1)
  {
#  ifndef ITK_USE_CUFFTW
    std::lock_guard<FFTWGlobalConfiguration::MutexType> lock(FFTWGlobalConfiguration::GetLockMutex());
    fftwf_plan_with_nthreads(threads);
#  else
    (void)threads;
#  endif
    PlanType plan = fftwf_plan_dft_c2r(rank, n, in, out, flags);
    return plan;
  }

  static PlanType
  Plan_dft_r2c_1d(int n, PixelType * in, ComplexType * out, unsigned int flags, int threads = 1)
  {
#  ifndef ITK_USE_CUFFTW
    std::lock_guard<FFTWGlobalConfiguration::MutexType> lock(FFTWGlobalConfiguration::GetLockMutex());
    fftwf_plan_with_nthreads(threads);
#  else
    (void)threads;
#  endif
    PlanType plan = fftwf_plan_dft_r2c_1d(n, in, out, flags);
    return plan;
  }
  static PlanType
  Plan_dft_r2c_2d(int nx, int ny, PixelType * in, ComplexType * out, unsigned int flags, int threads = 1)
  {
#  ifndef ITK_USE_CUFFTW
    std::lock_guard<FFTWGlobalConfiguration::MutexType> lock(FFTWGlobalConfiguration::GetLockMutex());
    fftwf_plan_with_nthreads(threads);
#  else
    (void)threads;
#  endif
    PlanType plan = fftwf_plan_dft_r2c_2d(nx, ny, in, out, flags);
    return plan;
  }
  static PlanType
  Plan_dft_r2c_3d(int nx, int ny, int nz, PixelType * in, ComplexType * out, unsigned int flags, int threads = 1)
  {
#  ifndef ITK_USE_CUFFTW
    std::lock_guard<FFTWGlobalConfiguration::MutexType> lock(FFTWGlobalConfiguration::GetLockMutex());
    fftwf_plan_with_nthreads(threads);
#  else
    (void)threads;
#  endif
    PlanType plan = fftwf_plan_dft_r2c_3d(nx, ny, nz, in, out, flags);
    return plan;
  }
  static PlanType
  Plan_dft_r2c(int rank, const int * n, PixelType * in, ComplexType * out, unsigned int flags, int threads = 1)
  {
#  ifndef ITK_USE_CUFFTW
    std::lock_guard<FFTWGlobalConfiguration::MutexType> lock(FFTWGlobalConfiguration::GetLockMutex());
    fftwf_plan_with_nthreads(threads);
#  else
    (void)threads;
#  endif
    PlanType plan = fftwf_plan_dft_r2c(rank, n, in, out, flags);
    return plan;
  }
  static PlanType
  Plan_dft_1d(const int n, ComplexType * in, ComplexType * out, int sign, unsigned int flags, int threads = 1)
  {
#  ifndef ITK_USE_CUFFTW
    std::lock_guard<FFTWGlobalConfiguration::MutexType> lock(FFTWGlobalConfiguration::GetLockMutex());
    fftwf_plan_with_nthreads(threads);
#  else
    (void)threads;
#  endif
    PlanType plan = fftwf_plan_dft_1d(n, in, out, sign, flags);
    return plan;
  }


  static void
  Execute(PlanType p)
  {
    fftwf_execute(p);
  }
  static void
  DestroyPlan(PlanType p)
  {
    fftwf_destroy_plan(p);
  }
};

#endif // USE_FFTWF


#if defined(ITK_USE_FFTWD)
template <>
class ComplexToComplexProxy<double>
{
public:
  using PixelType = double;
  using ComplexType = fftw_complex;
  using PlanType = fftw_plan;
  using Self = ComplexToComplexProxy<double>;

  static PlanType
  Plan_dft_c2r_1d(int n, ComplexType * in, PixelType * out, unsigned int flags, int threads = 1)
  {
#  ifndef ITK_USE_CUFFTW
    std::lock_guard<FFTWGlobalConfiguration::MutexType> lock(FFTWGlobalConfiguration::GetLockMutex());
    fftw_plan_with_nthreads(threads);
#  else
    (void)threads;
#  endif
    PlanType plan = fftw_plan_dft_c2r_1d(n, in, out, flags);
    return plan;
  }
  static PlanType
  Plan_dft_c2r_2d(int nx, int ny, ComplexType * in, PixelType * out, unsigned int flags, int threads = 1)
  {
#  ifndef ITK_USE_CUFFTW
    std::lock_guard<FFTWGlobalConfiguration::MutexType> lock(FFTWGlobalConfiguration::GetLockMutex());
    fftw_plan_with_nthreads(threads);
#  else
    (void)threads;
#  endif
    PlanType plan = fftw_plan_dft_c2r_2d(nx, ny, in, out, flags);
    return plan;
  }
  static PlanType
  Plan_dft_c2r_3d(int nx, int ny, int nz, ComplexType * in, PixelType * out, unsigned int flags, int threads = 1)
  {
#  ifndef ITK_USE_CUFFTW
    std::lock_guard<FFTWGlobalConfiguration::MutexType> lock(FFTWGlobalConfiguration::GetLockMutex());
    fftw_plan_with_nthreads(threads);
#  else
    (void)threads;
#  endif
    PlanType plan = fftw_plan_dft_c2r_3d(nx, ny, nz, in, out, flags);
    return plan;
  }
  static PlanType
  Plan_dft_c2r(int rank, const int * n, ComplexType * in, PixelType * out, unsigned int flags, int threads = 1)
  {
#  ifndef ITK_USE_CUFFTW
    std::lock_guard<FFTWGlobalConfiguration::MutexType> lock(FFTWGlobalConfiguration::GetLockMutex());
    fftw_plan_with_nthreads(threads);
#  else
    (void)threads;
#  endif
    PlanType plan = fftw_plan_dft_c2r(rank, n, in, out, flags);
    return plan;
  }

  static PlanType
  Plan_dft_r2c_1d(int n, PixelType * in, ComplexType * out, unsigned int flags, int threads = 1)
  {
#  ifndef ITK_USE_CUFFTW
    std::lock_guard<FFTWGlobalConfiguration::MutexType> lock(FFTWGlobalConfiguration::GetLockMutex());
    fftw_plan_with_nthreads(threads);
#  else
    (void)threads;
#  endif
    PlanType plan = fftw_plan_dft_r2c_1d(n, in, out, flags);
    return plan;
  }

  static PlanType
  Plan_dft_r2c_2d(int nx, int ny, PixelType * in, ComplexType * out, unsigned int flags, int threads = 1)
  {
#  ifndef ITK_USE_CUFFTW
    std::lock_guard<FFTWGlobalConfiguration::MutexType> lock(FFTWGlobalConfiguration::GetLockMutex());
    fftw_plan_with_nthreads(threads);
#  else
    (void)threads;
#  endif
    PlanType plan = fftw_plan_dft_r2c_2d(nx, ny, in, out, flags);
    return plan;
  }

  static PlanType
  Plan_dft_r2c_3d(int nx, int ny, int nz, PixelType * in, ComplexType * out, unsigned int flags, int threads = 1)
  {
#  ifndef ITK_USE_CUFFTW
    std::lock_guard<FFTWGlobalConfiguration::MutexType> lock(FFTWGlobalConfiguration::GetLockMutex());
    fftw_plan_with_nthreads(threads);
#  else
    (void)threads;
#  endif
    PlanType plan = fftw_plan_dft_r2c_3d(nx, ny, nz, in, out, flags);
    return plan;
  }

  static PlanType
  Plan_dft_r2c(int rank, const int * n, PixelType * in, ComplexType * out, unsigned int flags, int threads = 1)
  {
#  ifndef ITK_USE_CUFFTW
    std::lock_guard<FFTWGlobalConfiguration::MutexType> lock(FFTWGlobalConfiguration::GetLockMutex());
    fftw_plan_with_nthreads(threads);
#  else
    (void)threads;
#  endif
    PlanType plan = fftw_plan_dft_r2c(rank, n, in, out, flags);
    return plan;
  }
  static PlanType
  Plan_dft_1d(const int n, ComplexType * in, ComplexType * out, int sign, unsigned int flags, int threads = 1)
  {
#  ifndef ITK_USE_CUFFTW
    std::lock_guard<FFTWGlobalConfiguration::MutexType> lock(FFTWGlobalConfiguration::GetLockMutex());
    fftw_plan_with_nthreads(threads);
#  else
    (void)threads;
#  endif
    PlanType plan = fftw_plan_dft_1d(n, in, out, sign, flags);
    return plan;
  }

  static void
  Execute(PlanType p)
  {
    fftw_execute(p);
  }
  static void
  DestroyPlan(PlanType p)
  {
    fftw_destroy_plan(p);
  }
};

#endif
} // namespace fftw
} // namespace itk
#endif
