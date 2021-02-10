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
#ifndef itkFFTWCommon_h
#define itkFFTWCommon_h

#if defined(ITK_USE_FFTWF) || defined(ITK_USE_FFTWD)
#  if defined(ITK_USE_CUFFTW)
#    include "cufftw.h"
#  else
#    include "itkFFTWGlobalConfiguration.h"
#    include "fftw3.h"
#  endif
#  if !defined(FFTW_WISDOM_ONLY)
// FFTW_WISDOM_ONLY is a "beyond guru" option that is only available in fftw 3.2.2
// to be compatible with all the fftw 3.x API, we need to define this away here:
#    error "FFTW 3.3.2 or later is required so that FFTW_WISDOM_ONLY is defined."
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
 * This implementation was taken from the Insight Journal paper:
 * https://www.insight-journal.org/browse/publication/717
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * \ingroup ITKFFT
 */
template <typename TPixel>
class Proxy
{
  // empty -- only double and float specializations work

protected:
  Proxy() = default;
  ~Proxy() = default;
};

#if defined(ITK_USE_FFTWF)

template <>
class Proxy<float>
{
public:
  using PixelType = float;
  using ComplexType = fftwf_complex;
  using PlanType = fftwf_plan;
  using Self = Proxy<float>;

  // FFTW works with any data size, but is optimized for size decomposition with prime factors up to 13.
#  ifdef ITK_USE_CUFFTW
  static constexpr SizeValueType GREATEST_PRIME_FACTOR = 7;
#  else
  static constexpr SizeValueType GREATEST_PRIME_FACTOR = 13;
#  endif

  static PlanType
  Plan_dft_c2r_1d(int           n,
                  ComplexType * in,
                  PixelType *   out,
                  unsigned      flags,
                  int           threads = 1,
                  bool          canDestroyInput = false)
  {
    return Plan_dft_c2r(1, &n, in, out, flags, threads, canDestroyInput);
  }

  static PlanType
  Plan_dft_c2r_2d(int           nx,
                  int           ny,
                  ComplexType * in,
                  PixelType *   out,
                  unsigned      flags,
                  int           threads = 1,
                  bool          canDestroyInput = false)
  {
    auto * sizes = new int[2];
    sizes[0] = nx;
    sizes[1] = ny;
    PlanType plan = Plan_dft_c2r(2, sizes, in, out, flags, threads, canDestroyInput);
    delete[] sizes;
    return plan;
  }

  static PlanType
  Plan_dft_c2r_3d(int           nx,
                  int           ny,
                  int           nz,
                  ComplexType * in,
                  PixelType *   out,
                  unsigned      flags,
                  int           threads = 1,
                  bool          canDestroyInput = false)
  {
    auto * sizes = new int[3];
    sizes[0] = nx;
    sizes[1] = ny;
    sizes[2] = nz;
    PlanType plan = Plan_dft_c2r(3, sizes, in, out, flags, threads, canDestroyInput);
    delete[] sizes;
    return plan;
  }

  static PlanType
  Plan_dft_c2r(int           rank,
               const int *   n,
               ComplexType * in,
               PixelType *   out,
               unsigned      flags,
               int           threads = 1,
               bool          canDestroyInput = false)
  {
#  ifndef ITK_USE_CUFFTW
    std::lock_guard<FFTWGlobalConfiguration::MutexType> lock(FFTWGlobalConfiguration::GetLockMutex());
    fftwf_plan_with_nthreads(threads);
#  else
    (void)threads;
#  endif
    // don't add FFTW_WISDOM_ONLY if the plan rigor is FFTW_ESTIMATE
    // because FFTW_ESTIMATE guarantee to not destroy the input
    unsigned roflags = flags;
    if (!(flags & FFTW_ESTIMATE))
    {
      roflags = flags | FFTW_WISDOM_ONLY;
    }
    PlanType plan = fftwf_plan_dft_c2r(rank, n, in, out, roflags);
    if (plan == nullptr)
    {
      // no wisdom available for that plan
      if (canDestroyInput)
      {
        // just create the plan
        plan = fftwf_plan_dft_c2r(rank, n, in, out, flags);
      }
      else
      {
        // lets create a plan with a fake input to generate the wisdom
        int total = 1;
        for (int i = 0; i < rank; i++)
        {
          total *= n[i];
        }
        auto * din = new ComplexType[total];
        fftwf_plan_dft_c2r(rank, n, din, out, flags);
        delete[] din;
        // and then create the final plan - this time it shouldn't fail
        plan = fftwf_plan_dft_c2r(rank, n, in, out, roflags);
      }
#  ifndef ITK_USE_CUFFTW
      FFTWGlobalConfiguration::SetNewWisdomAvailable(true);
#  endif
    }
    itkAssertOrThrowMacro(plan != nullptr, "PLAN_CREATION_FAILED ");
    return plan;
  }


  static PlanType
  Plan_dft_r2c_1d(int           n,
                  PixelType *   in,
                  ComplexType * out,
                  unsigned      flags,
                  int           threads = 1,
                  bool          canDestroyInput = false)
  {
    return Plan_dft_r2c(1, &n, in, out, flags, threads, canDestroyInput);
  }

  static PlanType
  Plan_dft_r2c_2d(int           nx,
                  int           ny,
                  PixelType *   in,
                  ComplexType * out,
                  unsigned      flags,
                  int           threads = 1,
                  bool          canDestroyInput = false)
  {
    auto * sizes = new int[2];
    sizes[0] = nx;
    sizes[1] = ny;
    PlanType plan = Plan_dft_r2c(2, sizes, in, out, flags, threads, canDestroyInput);
    delete[] sizes;
    return plan;
  }

  static PlanType
  Plan_dft_r2c_3d(int           nx,
                  int           ny,
                  int           nz,
                  PixelType *   in,
                  ComplexType * out,
                  unsigned      flags,
                  int           threads = 1,
                  bool          canDestroyInput = false)
  {
    auto * sizes = new int[3];
    sizes[0] = nx;
    sizes[1] = ny;
    sizes[2] = nz;
    PlanType plan = Plan_dft_r2c(3, sizes, in, out, flags, threads, canDestroyInput);
    delete[] sizes;
    return plan;
  }

  static PlanType
  Plan_dft_r2c(int           rank,
               const int *   n,
               PixelType *   in,
               ComplexType * out,
               unsigned      flags,
               int           threads = 1,
               bool          canDestroyInput = false)
  {
    //
#  ifndef ITK_USE_CUFFTW
    std::lock_guard<FFTWGlobalConfiguration::MutexType> lock(FFTWGlobalConfiguration::GetLockMutex());
    fftwf_plan_with_nthreads(threads);
#  else
    (void)threads;
#  endif
    // don't add FFTW_WISDOM_ONLY if the plan rigor is FFTW_ESTIMATE
    // because FFTW_ESTIMATE guarantee to not destroy the input
    unsigned roflags = flags;
    if (!(flags & FFTW_ESTIMATE))
    {
      roflags = flags | FFTW_WISDOM_ONLY;
    }
    PlanType plan = fftwf_plan_dft_r2c(rank, n, in, out, roflags);
    if (plan == nullptr)
    {
      // no wisdom available for that plan
      if (canDestroyInput)
      {
        // just create the plan
        plan = fftwf_plan_dft_r2c(rank, n, in, out, flags);
      }
      else
      {
        // lets create a plan with a fake input to generate the wisdom
        int total = 1;
        for (int i = 0; i < rank; i++)
        {
          total *= n[i];
        }
        auto * din = new PixelType[total];
        fftwf_plan_dft_r2c(rank, n, din, out, flags);
        delete[] din;
        // and then create the final plan - this time it shouldn't fail
        plan = fftwf_plan_dft_r2c(rank, n, in, out, roflags);
      }
#  ifndef ITK_USE_CUFFTW
      FFTWGlobalConfiguration::SetNewWisdomAvailable(true);
#  endif
    }
    itkAssertOrThrowMacro(plan != nullptr, "PLAN_CREATION_FAILED ");
    return plan;
  }

  static PlanType
  Plan_dft_1d(int           n,
              ComplexType * in,
              ComplexType * out,
              int           sign,
              unsigned      flags,
              int           threads = 1,
              bool          canDestroyInput = false)
  {
    return Plan_dft(1, &n, in, out, sign, flags, threads, canDestroyInput);
  }

  static PlanType
  Plan_dft_2d(int           nx,
              int           ny,
              ComplexType * in,
              ComplexType * out,
              int           sign,
              unsigned      flags,
              int           threads = 1,
              bool          canDestroyInput = false)
  {
    auto * sizes = new int[2];
    sizes[0] = nx;
    sizes[1] = ny;
    PlanType plan = Plan_dft(2, sizes, in, out, sign, flags, threads, canDestroyInput);
    delete[] sizes;
    return plan;
  }

  static PlanType
  Plan_dft_3d(int           nx,
              int           ny,
              int           nz,
              ComplexType * in,
              ComplexType * out,
              int           sign,
              unsigned      flags,
              int           threads = 1,
              bool          canDestroyInput = false)
  {
    auto * sizes = new int[3];
    sizes[0] = nx;
    sizes[1] = ny;
    sizes[2] = nz;
    PlanType plan = Plan_dft(3, sizes, in, out, sign, flags, threads, canDestroyInput);
    delete[] sizes;
    return plan;
  }

  static PlanType
  Plan_dft(int           rank,
           const int *   n,
           ComplexType * in,
           ComplexType * out,
           int           sign,
           unsigned      flags,
           int           threads = 1,
           bool          canDestroyInput = false)
  {
#  ifndef ITK_USE_CUFFTW
    std::lock_guard<FFTWGlobalConfiguration::MutexType> lock(FFTWGlobalConfiguration::GetLockMutex());
    fftwf_plan_with_nthreads(threads);
#  else
    (void)threads;
#  endif
    // don't add FFTW_WISDOM_ONLY if the plan rigor is FFTW_ESTIMATE
    // because FFTW_ESTIMATE guarantee to not destroy the input
    unsigned roflags = flags;
    if (!(flags & FFTW_ESTIMATE))
    {
      roflags = flags | FFTW_WISDOM_ONLY;
    }
    PlanType plan = fftwf_plan_dft(rank, n, in, out, sign, roflags);
    if (plan == nullptr)
    {
      // no wisdom available for that plan
      if (canDestroyInput)
      {
        // just create the plan
        plan = fftwf_plan_dft(rank, n, in, out, sign, flags);
      }
      else
      {
        // lets create a plan with a fake input to generate the wisdom
        int total = 1;
        for (int i = 0; i < rank; i++)
        {
          total *= n[i];
        }
        auto * din = new ComplexType[total];
        fftwf_plan_dft(rank, n, din, out, sign, flags);
        delete[] din;
        // and then create the final plan - this time it shouldn't fail
        plan = fftwf_plan_dft(rank, n, in, out, sign, roflags);
      }
#  ifndef ITK_USE_CUFFTW
      FFTWGlobalConfiguration::SetNewWisdomAvailable(true);
#  endif
    }
    itkAssertOrThrowMacro(plan != nullptr, "PLAN_CREATION_FAILED ");
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
#  ifndef ITK_USE_CUFFTW
    std::lock_guard<FFTWGlobalConfiguration::MutexType> lock(FFTWGlobalConfiguration::GetLockMutex());
#  endif
    fftwf_destroy_plan(p);
  }
};

#endif // ITK_USE_FFTWF


#if defined(ITK_USE_FFTWD)
template <>
class Proxy<double>
{
public:
  using PixelType = double;
  using ComplexType = fftw_complex;
  using PlanType = fftw_plan;
  using Self = Proxy<double>;

  // FFTW works with any data size, but is optimized for size decomposition with prime factors up to 13.
#  ifdef ITK_USE_CUFFTW
  static constexpr SizeValueType GREATEST_PRIME_FACTOR = 7;
#  else
  static constexpr SizeValueType GREATEST_PRIME_FACTOR = 13;
#  endif

  static PlanType
  Plan_dft_c2r_1d(int           n,
                  ComplexType * in,
                  PixelType *   out,
                  unsigned      flags,
                  int           threads = 1,
                  bool          canDestroyInput = false)
  {
    return Plan_dft_c2r(1, &n, in, out, flags, threads, canDestroyInput);
  }

  static PlanType
  Plan_dft_c2r_2d(int           nx,
                  int           ny,
                  ComplexType * in,
                  PixelType *   out,
                  unsigned      flags,
                  int           threads = 1,
                  bool          canDestroyInput = false)
  {
    auto * sizes = new int[2];
    sizes[0] = nx;
    sizes[1] = ny;
    PlanType plan = Plan_dft_c2r(2, sizes, in, out, flags, threads, canDestroyInput);
    delete[] sizes;
    return plan;
  }

  static PlanType
  Plan_dft_c2r_3d(int           nx,
                  int           ny,
                  int           nz,
                  ComplexType * in,
                  PixelType *   out,
                  unsigned      flags,
                  int           threads = 1,
                  bool          canDestroyInput = false)
  {
    auto * sizes = new int[3];
    sizes[0] = nx;
    sizes[1] = ny;
    sizes[2] = nz;
    PlanType plan = Plan_dft_c2r(3, sizes, in, out, flags, threads, canDestroyInput);
    delete[] sizes;
    return plan;
  }

  static PlanType
  Plan_dft_c2r(int           rank,
               const int *   n,
               ComplexType * in,
               PixelType *   out,
               unsigned      flags,
               int           threads = 1,
               bool          canDestroyInput = false)
  {
#  ifndef ITK_USE_CUFFTW
    std::lock_guard<FFTWGlobalConfiguration::MutexType> lock(FFTWGlobalConfiguration::GetLockMutex());
    fftw_plan_with_nthreads(threads);
#  else
    (void)threads;
#  endif
    // don't add FFTW_WISDOM_ONLY if the plan rigor is FFTW_ESTIMATE
    // because FFTW_ESTIMATE guarantee to not destroy the input
    unsigned roflags = flags;
    if (!(flags & FFTW_ESTIMATE))
    {
      roflags = flags | FFTW_WISDOM_ONLY;
    }
    PlanType plan = fftw_plan_dft_c2r(rank, n, in, out, roflags);
    if (plan == nullptr)
    {
      // no wisdom available for that plan
      if (canDestroyInput)
      {
        // just create the plan
        plan = fftw_plan_dft_c2r(rank, n, in, out, flags);
      }
      else
      {
        // lets create a plan with a fake input to generate the wisdom
        int total = 1;
        for (int i = 0; i < rank; i++)
        {
          total *= n[i];
        }
        auto * din = new ComplexType[total];
        fftw_plan_dft_c2r(rank, n, din, out, flags);
        delete[] din;
        // and then create the final plan - this time it shouldn't fail
        plan = fftw_plan_dft_c2r(rank, n, in, out, roflags);
      }
#  ifndef ITK_USE_CUFFTW
      FFTWGlobalConfiguration::SetNewWisdomAvailable(true);
#  endif
    }
    itkAssertOrThrowMacro(plan != nullptr, "PLAN_CREATION_FAILED ");
    return plan;
  }


  static PlanType
  Plan_dft_r2c_1d(int           n,
                  PixelType *   in,
                  ComplexType * out,
                  unsigned      flags,
                  int           threads = 1,
                  bool          canDestroyInput = false)
  {
    return Plan_dft_r2c(1, &n, in, out, flags, threads, canDestroyInput);
  }

  static PlanType
  Plan_dft_r2c_2d(int           nx,
                  int           ny,
                  PixelType *   in,
                  ComplexType * out,
                  unsigned      flags,
                  int           threads = 1,
                  bool          canDestroyInput = false)
  {
    auto * sizes = new int[2];
    sizes[0] = nx;
    sizes[1] = ny;
    PlanType plan = Plan_dft_r2c(2, sizes, in, out, flags, threads, canDestroyInput);
    delete[] sizes;
    return plan;
  }

  static PlanType
  Plan_dft_r2c_3d(int           nx,
                  int           ny,
                  int           nz,
                  PixelType *   in,
                  ComplexType * out,
                  unsigned      flags,
                  int           threads = 1,
                  bool          canDestroyInput = false)
  {
    auto * sizes = new int[3];
    sizes[0] = nx;
    sizes[1] = ny;
    sizes[2] = nz;
    PlanType plan = Plan_dft_r2c(3, sizes, in, out, flags, threads, canDestroyInput);
    delete[] sizes;
    return plan;
  }

  static PlanType
  Plan_dft_r2c(int           rank,
               const int *   n,
               PixelType *   in,
               ComplexType * out,
               unsigned      flags,
               int           threads = 1,
               bool          canDestroyInput = false)
  {
#  ifndef ITK_USE_CUFFTW
    std::lock_guard<FFTWGlobalConfiguration::MutexType> lock(FFTWGlobalConfiguration::GetLockMutex());
    fftw_plan_with_nthreads(threads);
#  else
    (void)threads;
#  endif
    // don't add FFTW_WISDOM_ONLY if the plan rigor is FFTW_ESTIMATE
    // because FFTW_ESTIMATE guarantee to not destroy the input
    unsigned roflags = flags;
    if (!(flags & FFTW_ESTIMATE))
    {
      roflags = flags | FFTW_WISDOM_ONLY;
    }
    PlanType plan = fftw_plan_dft_r2c(rank, n, in, out, roflags);
    if (plan == nullptr)
    {
      // no wisdom available for that plan
      if (canDestroyInput)
      {
        // just create the plan
        plan = fftw_plan_dft_r2c(rank, n, in, out, flags);
      }
      else
      {
        // lets create a plan with a fake input to generate the wisdom
        int total = 1;
        for (int i = 0; i < rank; i++)
        {
          total *= n[i];
        }
        auto * din = new PixelType[total];
        fftw_plan_dft_r2c(rank, n, din, out, flags);
        delete[] din;
        // and then create the final plan - this time it shouldn't fail
        plan = fftw_plan_dft_r2c(rank, n, in, out, roflags);
      }
#  ifndef ITK_USE_CUFFTW
      FFTWGlobalConfiguration::SetNewWisdomAvailable(true);
#  endif
    }
    itkAssertOrThrowMacro(plan != nullptr, "PLAN_CREATION_FAILED ");
    return plan;
  }

  static PlanType
  Plan_dft_1d(int           n,
              ComplexType * in,
              ComplexType * out,
              int           sign,
              unsigned      flags,
              int           threads = 1,
              bool          canDestroyInput = false)
  {
    return Plan_dft(1, &n, in, out, sign, flags, threads, canDestroyInput);
  }

  static PlanType
  Plan_dft_2d(int           nx,
              int           ny,
              ComplexType * in,
              ComplexType * out,
              int           sign,
              unsigned      flags,
              int           threads = 1,
              bool          canDestroyInput = false)
  {
    auto * sizes = new int[2];
    sizes[0] = nx;
    sizes[1] = ny;
    PlanType plan = Plan_dft(2, sizes, in, out, sign, flags, threads, canDestroyInput);
    delete[] sizes;
    return plan;
  }

  static PlanType
  Plan_dft_3d(int           nx,
              int           ny,
              int           nz,
              ComplexType * in,
              ComplexType * out,
              int           sign,
              unsigned      flags,
              int           threads = 1,
              bool          canDestroyInput = false)
  {
    auto * sizes = new int[3];
    sizes[0] = nx;
    sizes[1] = ny;
    sizes[2] = nz;
    PlanType plan = Plan_dft(3, sizes, in, out, sign, flags, threads, canDestroyInput);
    delete[] sizes;
    return plan;
  }

  static PlanType
  Plan_dft(int           rank,
           const int *   n,
           ComplexType * in,
           ComplexType * out,
           int           sign,
           unsigned      flags,
           int           threads = 1,
           bool          canDestroyInput = false)
  {
#  ifndef ITK_USE_CUFFTW
    std::lock_guard<FFTWGlobalConfiguration::MutexType> lock(FFTWGlobalConfiguration::GetLockMutex());
    fftw_plan_with_nthreads(threads);
#  else
    (void)threads;
#  endif
    // don't add FFTW_WISDOM_ONLY if the plan rigor is FFTW_ESTIMATE
    // because FFTW_ESTIMATE guarantee to not destroy the input
    unsigned roflags = flags;
    if (!(flags & FFTW_ESTIMATE))
    {
      roflags = flags | FFTW_WISDOM_ONLY;
    }
    PlanType plan = fftw_plan_dft(rank, n, in, out, sign, roflags);
    if (plan == nullptr)
    {
      // no wisdom available for that plan
      if (canDestroyInput)
      {
        // just create the plan
        plan = fftw_plan_dft(rank, n, in, out, sign, flags);
      }
      else
      {
        // lets create a plan with a fake input to generate the wisdom
        int total = 1;
        for (int i = 0; i < rank; i++)
        {
          total *= n[i];
        }
        auto * din = new ComplexType[total];
        fftw_plan_dft(rank, n, din, out, sign, flags);
        delete[] din;
        // and then create the final plan - this time it shouldn't fail
        plan = fftw_plan_dft(rank, n, in, out, sign, roflags);
      }
#  ifndef ITK_USE_CUFFTW
      FFTWGlobalConfiguration::SetNewWisdomAvailable(true);
#  endif
    }
    itkAssertOrThrowMacro(plan != nullptr, "PLAN_CREATION_FAILED ");
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
#  ifndef ITK_USE_CUFFTW
    std::lock_guard<FFTWGlobalConfiguration::MutexType> lock(FFTWGlobalConfiguration::GetLockMutex());
#  endif
    fftw_destroy_plan(p);
  }
};

#endif
} // end namespace fftw
} // end namespace itk
#endif
