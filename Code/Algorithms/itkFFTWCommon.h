/*=========================================================================
 *
 *  Copyright Insight Software Consortium
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
#ifndef __itkFFTWCommon_h
#define __itkFFTWCommon_h

#include "itkFFTWGlobalConfiguration.h"
#if defined( USE_FFTWF ) || defined( USE_FFTWD )
#include "fftw3.h"
#endif

#if !defined(FFTW_WISDOM_ONLY)
// FFTW_WISDOM_ONLY is a "beyond guru" option that is only available in fftw 3.2.2
// to be compatible with all the fftw 3.x API, we need to define this away here:
#error "FFTW 3.3.2 or later is required so that FFTW_WISDOM_ONLY is defined."
#endif

namespace itk
{
namespace fftw
{
/**
 * \class Interface
 * \brief Wrapper for FFTW API
 *
 * This implementation was taken from the Insight Journal paper:
 * http://hdl.handle.net/10380/3154
 * or http://insight-journal.com/browse/publication/717
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 */
template< typename TPixel >
class Proxy
{
  // empty -- only double and float specializations work
protected:
  Proxy() {};
  ~Proxy() {};
};

#if defined( USE_FFTWF )

template< >
class Proxy< float >
{
public:
  typedef float         PixelType;
  typedef fftwf_complex ComplexType;
  typedef fftwf_plan    PlanType;
  typedef Proxy<float>  Self;

  static PlanType Plan_dft_c2r_1d(int n,
                                  ComplexType *in,
                                  PixelType *out,
                                  unsigned flags,
                                  int threads=1,
                                  bool canDestroyInput=false)
  {
    FFTWGlobalConfiguration::Lock();
    fftwf_plan_with_nthreads(threads);
    PlanType plan = fftwf_plan_dft_c2r_1d(n,in,out,flags | FFTW_WISDOM_ONLY);
    if( plan == NULL )
      {
      // no wisdom available for that plan
      if( canDestroyInput )
        {
        // just create the plan
        plan = fftwf_plan_dft_c2r_1d(n,in,out,flags);
        }
      else
        {
        // lets create a plan with a fake input to generate the wisdom
        ComplexType * din = new ComplexType[n];
        fftwf_plan_dft_c2r_1d(n,din,out,flags);
        delete [] din;
        // and then create the final plan - this time it shouldn't fail
        plan = fftwf_plan_dft_c2r_1d(n,in,out,flags | FFTW_WISDOM_ONLY);
        }
      FFTWGlobalConfiguration::SetNewWisdomAvailable(true);
      }
    FFTWGlobalConfiguration::Unlock();
    assert( plan != NULL );
    return plan;
  }

  static PlanType Plan_dft_c2r_2d(int nx,
                                  int ny,
                                  ComplexType *in,
                                  PixelType *out,
                                  unsigned flags,
                                  int threads=1,
                                  bool canDestroyInput=false)
  {
    FFTWGlobalConfiguration::Lock();
    fftwf_plan_with_nthreads(threads);
    PlanType plan = fftwf_plan_dft_c2r_2d(nx,ny,in,out,flags | FFTW_WISDOM_ONLY);
    if( plan == NULL )
      {
      // no wisdom available for that plan
      if( canDestroyInput )
        {
        // just create the plan
        plan = fftwf_plan_dft_c2r_2d(nx,ny,in,out,flags);
        }
      else
        {
        // lets create a plan with a fake input to generate the wisdom
        ComplexType * din = new ComplexType[nx*ny];
        fftwf_plan_dft_c2r_2d(nx,ny,din,out,flags);
        delete [] din;
        // and then create the final plan - this time it shouldn't fail
        plan = fftwf_plan_dft_c2r_2d(nx,ny,in,out,flags | FFTW_WISDOM_ONLY);
        }
      FFTWGlobalConfiguration::SetNewWisdomAvailable(true);
      }
    FFTWGlobalConfiguration::Unlock();
    assert( plan != NULL );
    return plan;
  }

  static PlanType Plan_dft_c2r_3d(int nx,
                                  int ny,
                                  int nz,
                                  ComplexType *in,
                                  PixelType *out,
                                  unsigned flags,
                                  int threads=1,
                                  bool canDestroyInput=false)
  {
    FFTWGlobalConfiguration::Lock();
    fftwf_plan_with_nthreads(threads);
    PlanType plan = fftwf_plan_dft_c2r_3d(nx,ny,nz,in,out,flags | FFTW_WISDOM_ONLY);
    if( plan == NULL )
      {
      // no wisdom available for that plan
      if( canDestroyInput )
        {
        // just create the plan
        plan = fftwf_plan_dft_c2r_3d(nx,ny,nz,in,out,flags);
        }
      else
        {
        // lets create a plan with a fake input to generate the wisdom
        ComplexType * din = new ComplexType[nx*ny*nz];
        fftwf_plan_dft_c2r_3d(nx,ny,nz,din,out,flags);
        delete [] din;
        // and then create the final plan - this time it shouldn't fail
        plan = fftwf_plan_dft_c2r_3d(nx,ny,nz,in,out,flags | FFTW_WISDOM_ONLY);
        }
      FFTWGlobalConfiguration::SetNewWisdomAvailable(true);
      }
    FFTWGlobalConfiguration::Unlock();
    assert( plan != NULL );
    return plan;
  }

  static PlanType Plan_dft_c2r(int rank,
                               const int *n,
                               ComplexType *in,
                               PixelType *out,
                               unsigned flags,
                               int threads=1,
                               bool canDestroyInput=false)
  {
    FFTWGlobalConfiguration::Lock();
    fftwf_plan_with_nthreads(threads);
    PlanType plan = fftwf_plan_dft_c2r(rank,n,in,out,flags | FFTW_WISDOM_ONLY);
    if( plan == NULL )
      {
      // no wisdom available for that plan
      if( canDestroyInput )
        {
        // just create the plan
        plan = fftwf_plan_dft_c2r(rank,n,in,out,flags);
        }
      else
        {
        // lets create a plan with a fake input to generate the wisdom
        int total = 0;
        for( int i=0; i<rank; i++ )
          {
          total += n[i];
          }
        ComplexType * din = new ComplexType[total];
        fftwf_plan_dft_c2r(rank,n,din,out,flags);
        delete [] din;
        // and then create the final plan - this time it shouldn't fail
        plan = fftwf_plan_dft_c2r(rank,n,in,out,flags | FFTW_WISDOM_ONLY);
        }
      FFTWGlobalConfiguration::SetNewWisdomAvailable(true);
      }
    FFTWGlobalConfiguration::Unlock();
    assert( plan != NULL );
    return plan;
  }


  static PlanType Plan_dft_r2c_1d(int n,
                                  PixelType *in,
                                  ComplexType *out,
                                  unsigned flags,
                                  int threads=1,
                                  bool canDestroyInput=false)
  {
    FFTWGlobalConfiguration::Lock();
    fftwf_plan_with_nthreads(threads);
    PlanType plan = fftwf_plan_dft_r2c_1d(n,in,out,flags | FFTW_WISDOM_ONLY);
    if( plan == NULL )
      {
      // no wisdom available for that plan
      if( canDestroyInput )
        {
        // just create the plan
        plan = fftwf_plan_dft_r2c_1d(n,in,out,flags);
        }
      else
        {
        // lets create a plan with a fake input to generate the wisdom
        PixelType * din = new PixelType[n];
        fftwf_plan_dft_r2c_1d(n,din,out,flags);
        delete [] din;
        // and then create the final plan - this time it shouldn't fail
        plan = fftwf_plan_dft_r2c_1d(n,in,out,flags | FFTW_WISDOM_ONLY);
        }
      FFTWGlobalConfiguration::SetNewWisdomAvailable(true);
      }
    FFTWGlobalConfiguration::Unlock();
    assert( plan != NULL );
    return plan;
  }

  static PlanType Plan_dft_r2c_2d(int nx,
                                  int ny,
                                  PixelType *in,
                                  ComplexType *out,
                                  unsigned flags,
                                  int threads=1,
                                  bool canDestroyInput=false)
  {
    FFTWGlobalConfiguration::Lock();
    fftwf_plan_with_nthreads(threads);
    PlanType plan = fftwf_plan_dft_r2c_2d(nx,ny,in,out,flags | FFTW_WISDOM_ONLY);
    if( plan == NULL )
      {
      // no wisdom available for that plan
      if( canDestroyInput )
        {
        // just create the plan
        plan = fftwf_plan_dft_r2c_2d(nx,ny,in,out,flags);
        }
      else
        {
        // lets create a plan with a fake input to generate the wisdom
        PixelType * din = new PixelType[nx*ny];
        fftwf_plan_dft_r2c_2d(nx,ny,din,out,flags);
        delete [] din;
        // and then create the final plan - this time it shouldn't fail
        plan = fftwf_plan_dft_r2c_2d(nx,ny,in,out,flags | FFTW_WISDOM_ONLY);
        }
      FFTWGlobalConfiguration::SetNewWisdomAvailable(true);
      }
    FFTWGlobalConfiguration::Unlock();
    assert( plan != NULL );
    return plan;
  }

  static PlanType Plan_dft_r2c_3d(int nx,
                                  int ny,
                                  int nz,
                                  PixelType *in,
                                  ComplexType *out,
                                  unsigned flags,
                                  int threads=1,
                                  bool canDestroyInput=false)
  {
    FFTWGlobalConfiguration::Lock();
    fftwf_plan_with_nthreads(threads);
    PlanType plan = fftwf_plan_dft_r2c_3d(nx,ny,nz,in,out,flags | FFTW_WISDOM_ONLY);
    if( plan == NULL )
      {
      // no wisdom available for that plan
      if( canDestroyInput )
        {
        // just create the plan
        plan = fftwf_plan_dft_r2c_3d(nx,ny,nz,in,out,flags);
        }
      else
        {
        // lets create a plan with a fake input to generate the wisdom
        PixelType * din = new PixelType[nx*ny*nz];
        fftwf_plan_dft_r2c_3d(nx,ny,nz,din,out,flags);
        delete [] din;
        // and then create the final plan - this time it shouldn't fail
        plan = fftwf_plan_dft_r2c_3d(nx,ny,nz,in,out,flags | FFTW_WISDOM_ONLY);
        }
      FFTWGlobalConfiguration::SetNewWisdomAvailable(true);
      }
    FFTWGlobalConfiguration::Unlock();
    assert( plan != NULL );
    return plan;
  }

  static PlanType Plan_dft_r2c(int rank,
                               const int *n,
                               PixelType *in,
                               ComplexType *out,
                               unsigned flags,
                               int threads=1,
                               bool canDestroyInput=false)
  {
    FFTWGlobalConfiguration::Lock();
    fftwf_plan_with_nthreads(threads);
    PlanType plan = fftwf_plan_dft_r2c(rank,n,in,out,flags | FFTW_WISDOM_ONLY);
    if( plan == NULL )
      {
      // no wisdom available for that plan
      if( canDestroyInput )
        {
        // just create the plan
        plan = fftwf_plan_dft_r2c(rank,n,in,out,flags);
        }
      else
        {
        // lets create a plan with a fake input to generate the wisdom
        int total = 0;
        for( int i=0; i<rank; i++ )
          {
          total += n[i];
          }
        PixelType * din = new PixelType[total];
        fftwf_plan_dft_r2c(rank,n,din,out,flags);
        delete [] din;
        // and then create the final plan - this time it shouldn't fail
        plan = fftwf_plan_dft_r2c(rank,n,in,out,flags | FFTW_WISDOM_ONLY);
        }
      FFTWGlobalConfiguration::SetNewWisdomAvailable(true);
      }
    FFTWGlobalConfiguration::Unlock();
    assert( plan != NULL );
    return plan;
  }

  static PlanType Plan_dft_1d(int n,
                                  ComplexType *in,
                                  ComplexType *out,
                                  int sign,
                                  unsigned flags,
                                  int threads=1,
                                  bool canDestroyInput=false)
  {
    FFTWGlobalConfiguration::Lock();
    fftwf_plan_with_nthreads(threads);
    PlanType plan = fftwf_plan_dft_1d(n,in,out,sign,flags | FFTW_WISDOM_ONLY);
    if( plan == NULL )
      {
      // no wisdom available for that plan
      if( canDestroyInput )
        {
        // just create the plan
        plan = fftwf_plan_dft_1d(n,in,out,sign,flags);
        }
      else
        {
        // lets create a plan with a fake input to generate the wisdom
        ComplexType * din = new ComplexType[n];
        fftwf_plan_dft_1d(n,din,out,sign,flags);
        delete [] din;
        // and then create the final plan - this time it shouldn't fail
        plan = fftwf_plan_dft_1d(n,in,out,sign,flags | FFTW_WISDOM_ONLY);
        }
      FFTWGlobalConfiguration::SetNewWisdomAvailable(true);
      }
    FFTWGlobalConfiguration::Unlock();
    assert( plan != NULL );
    return plan;
  }

  static PlanType Plan_dft_2d(int nx,
                                  int ny,
                                  ComplexType *in,
                                  ComplexType *out,
                                  int sign,
                                  unsigned flags,
                                  int threads=1,
                                  bool canDestroyInput=false)
  {
    FFTWGlobalConfiguration::Lock();
    fftwf_plan_with_nthreads(threads);
    PlanType plan = fftwf_plan_dft_2d(nx,ny,in,out,sign,flags | FFTW_WISDOM_ONLY);
    if( plan == NULL )
      {
      // no wisdom available for that plan
      if( canDestroyInput )
        {
        // just create the plan
        plan = fftwf_plan_dft_2d(nx,ny,in,out,sign,flags);
        }
      else
        {
        // lets create a plan with a fake input to generate the wisdom
        ComplexType * din = new ComplexType[nx*ny];
        fftwf_plan_dft_2d(nx,ny,din,out,sign,flags);
        delete [] din;
        // and then create the final plan - this time it shouldn't fail
        plan = fftwf_plan_dft_2d(nx,ny,in,out,sign,flags | FFTW_WISDOM_ONLY);
        }
      FFTWGlobalConfiguration::SetNewWisdomAvailable(true);
      }
    FFTWGlobalConfiguration::Unlock();
    assert( plan != NULL );
    return plan;
  }

  static PlanType Plan_dft_3d(int nx,
                                  int ny,
                                  int nz,
                                  ComplexType *in,
                                  ComplexType *out,
                                  int sign,
                                  unsigned flags,
                                  int threads=1,
                                  bool canDestroyInput=false)
  {
    FFTWGlobalConfiguration::Lock();
    fftwf_plan_with_nthreads(threads);
    PlanType plan = fftwf_plan_dft_3d(nx,ny,nz,in,out,sign,flags | FFTW_WISDOM_ONLY);
    if( plan == NULL )
      {
      // no wisdom available for that plan
      if( canDestroyInput )
        {
        // just create the plan
        plan = fftwf_plan_dft_3d(nx,ny,nz,in,out,sign,flags);
        }
      else
        {
        // lets create a plan with a fake input to generate the wisdom
        ComplexType * din = new ComplexType[nx*ny*nz];
        fftwf_plan_dft_3d(nx,ny,nz,din,out,sign,flags);
        delete [] din;
        // and then create the final plan - this time it shouldn't fail
        plan = fftwf_plan_dft_3d(nx,ny,nz,in,out,sign,flags | FFTW_WISDOM_ONLY);
        }
      FFTWGlobalConfiguration::SetNewWisdomAvailable(true);
      }
    FFTWGlobalConfiguration::Unlock();
    assert( plan != NULL );
    return plan;
  }

  static PlanType Plan_dft(int rank,
                               const int *n,
                               ComplexType *in,
                               ComplexType *out,
                               int sign,
                               unsigned flags,
                               int threads=1,
                               bool canDestroyInput=false)
  {
    FFTWGlobalConfiguration::Lock();
    fftwf_plan_with_nthreads(threads);
    PlanType plan = fftwf_plan_dft(rank,n,in,out,sign,flags | FFTW_WISDOM_ONLY);
    if( plan == NULL )
      {
      // no wisdom available for that plan
      if( canDestroyInput )
        {
        // just create the plan
        plan = fftwf_plan_dft(rank,n,in,out,sign,flags);
        }
      else
        {
        // lets create a plan with a fake input to generate the wisdom
        int total = 0;
        for( int i=0; i<rank; i++ )
          {
          total += n[i];
          }
        ComplexType * din = new ComplexType[total];
        fftwf_plan_dft(rank,n,din,out,sign,flags);
        delete [] din;
        // and then create the final plan - this time it shouldn't fail
        plan = fftwf_plan_dft(rank,n,in,out,sign,flags | FFTW_WISDOM_ONLY);
        }
      FFTWGlobalConfiguration::SetNewWisdomAvailable(true);
      }
    FFTWGlobalConfiguration::Unlock();
    assert( plan != NULL );
    return plan;
  }


  static void Execute(PlanType p)
  {
    fftwf_execute(p);
  }
  static void DestroyPlan(PlanType p)
  {
    fftwf_destroy_plan(p);
  }
};

#endif // USE_FFTWF


#if defined( USE_FFTWD )
template< >
class Proxy< double >
{
public:
  typedef double        PixelType;
  typedef fftw_complex  ComplexType;
  typedef fftw_plan     PlanType;
  typedef Proxy<double> Self;

  static PlanType Plan_dft_c2r_1d(int n,
                                  ComplexType *in,
                                  PixelType *out,
                                  unsigned flags,
                                  int threads=1,
                                  bool canDestroyInput=false)
  {
    FFTWGlobalConfiguration::Lock();
    fftw_plan_with_nthreads(threads);
    PlanType plan = fftw_plan_dft_c2r_1d(n,in,out,flags | FFTW_WISDOM_ONLY);
    if( plan == NULL )
      {
      // no wisdom available for that plan
      if( canDestroyInput )
        {
        // just create the plan
        plan = fftw_plan_dft_c2r_1d(n,in,out,flags);
        }
      else
        {
        // lets create a plan with a fake input to generate the wisdom
        ComplexType * din = new ComplexType[n];
        fftw_plan_dft_c2r_1d(n,din,out,flags);
        delete [] din;
        // and then create the final plan - this time it shouldn't fail
        plan = fftw_plan_dft_c2r_1d(n,in,out,flags | FFTW_WISDOM_ONLY);
        }
      FFTWGlobalConfiguration::SetNewWisdomAvailable(true);
      }
    FFTWGlobalConfiguration::Unlock();
    assert( plan != NULL );
    return plan;
  }

  static PlanType Plan_dft_c2r_2d(int nx,
                                  int ny,
                                  ComplexType *in,
                                  PixelType *out,
                                  unsigned flags,
                                  int threads=1,
                                  bool canDestroyInput=false)
  {
    FFTWGlobalConfiguration::Lock();
    fftw_plan_with_nthreads(threads);
    PlanType plan = fftw_plan_dft_c2r_2d(nx,ny,in,out,flags | FFTW_WISDOM_ONLY);
    if( plan == NULL )
      {
      // no wisdom available for that plan
      if( canDestroyInput )
        {
        // just create the plan
        plan = fftw_plan_dft_c2r_2d(nx,ny,in,out,flags);
        }
      else
        {
        // lets create a plan with a fake input to generate the wisdom
        ComplexType * din = new ComplexType[nx*ny];
        fftw_plan_dft_c2r_2d(nx,ny,din,out,flags);
        delete [] din;
        // and then create the final plan - this time it shouldn't fail
        plan = fftw_plan_dft_c2r_2d(nx,ny,in,out,flags | FFTW_WISDOM_ONLY);
        }
      FFTWGlobalConfiguration::SetNewWisdomAvailable(true);
      }
    FFTWGlobalConfiguration::Unlock();
    assert( plan != NULL );
    return plan;
  }

  static PlanType Plan_dft_c2r_3d(int nx,
                                  int ny,
                                  int nz,
                                  ComplexType *in,
                                  PixelType *out,
                                  unsigned flags,
                                  int threads=1,
                                  bool canDestroyInput=false)
  {
    FFTWGlobalConfiguration::Lock();
    fftw_plan_with_nthreads(threads);
    PlanType plan = fftw_plan_dft_c2r_3d(nx,ny,nz,in,out,flags | FFTW_WISDOM_ONLY);
    if( plan == NULL )
      {
      // no wisdom available for that plan
      if( canDestroyInput )
        {
        // just create the plan
        plan = fftw_plan_dft_c2r_3d(nx,ny,nz,in,out,flags);
        }
      else
        {
        // lets create a plan with a fake input to generate the wisdom
        ComplexType * din = new ComplexType[nx*ny*nz];
        fftw_plan_dft_c2r_3d(nx,ny,nz,din,out,flags);
        delete [] din;
        // and then create the final plan - this time it shouldn't fail
        plan = fftw_plan_dft_c2r_3d(nx,ny,nz,in,out,flags | FFTW_WISDOM_ONLY);
        }
      FFTWGlobalConfiguration::SetNewWisdomAvailable(true);
      }
    FFTWGlobalConfiguration::Unlock();
    assert( plan != NULL );
    return plan;
  }

  static PlanType Plan_dft_c2r(int rank,
                               const int *n,
                               ComplexType *in,
                               PixelType *out,
                               unsigned flags,
                               int threads=1,
                               bool canDestroyInput=false)
  {
    FFTWGlobalConfiguration::Lock();
    fftw_plan_with_nthreads(threads);
    PlanType plan = fftw_plan_dft_c2r(rank,n,in,out,flags | FFTW_WISDOM_ONLY);
    if( plan == NULL )
      {
      // no wisdom available for that plan
      if( canDestroyInput )
        {
        // just create the plan
        plan = fftw_plan_dft_c2r(rank,n,in,out,flags);
        }
      else
        {
        // lets create a plan with a fake input to generate the wisdom
        int total = 0;
        for( int i=0; i<rank; i++ )
          {
          total += n[i];
          }
        ComplexType * din = new ComplexType[total];
        fftw_plan_dft_c2r(rank,n,din,out,flags);
        delete [] din;
        // and then create the final plan - this time it shouldn't fail
        plan = fftw_plan_dft_c2r(rank,n,in,out,flags | FFTW_WISDOM_ONLY);
        }
      FFTWGlobalConfiguration::SetNewWisdomAvailable(true);
      }
    FFTWGlobalConfiguration::Unlock();
    assert( plan != NULL );
    return plan;
  }


  static PlanType Plan_dft_r2c_1d(int n,
                                  PixelType *in,
                                  ComplexType *out,
                                  unsigned flags,
                                  int threads=1,
                                  bool canDestroyInput=false)
  {
    FFTWGlobalConfiguration::Lock();
    fftw_plan_with_nthreads(threads);
    PlanType plan = fftw_plan_dft_r2c_1d(n,in,out,flags | FFTW_WISDOM_ONLY);
    if( plan == NULL )
      {
      // no wisdom available for that plan
      if( canDestroyInput )
        {
        // just create the plan
        plan = fftw_plan_dft_r2c_1d(n,in,out,flags);
        }
      else
        {
        // lets create a plan with a fake input to generate the wisdom
        PixelType * din = new PixelType[n];
        fftw_plan_dft_r2c_1d(n,din,out,flags);
        delete [] din;
        // and then create the final plan - this time it shouldn't fail
        plan = fftw_plan_dft_r2c_1d(n,in,out,flags | FFTW_WISDOM_ONLY);
        }
      FFTWGlobalConfiguration::SetNewWisdomAvailable(true);
      }
    FFTWGlobalConfiguration::Unlock();
    assert( plan != NULL );
    return plan;
  }

  static PlanType Plan_dft_r2c_2d(int nx,
                                  int ny,
                                  PixelType *in,
                                  ComplexType *out,
                                  unsigned flags,
                                  int threads=1,
                                  bool canDestroyInput=false)
  {
    FFTWGlobalConfiguration::Lock();
    fftw_plan_with_nthreads(threads);
    PlanType plan = fftw_plan_dft_r2c_2d(nx,ny,in,out,flags | FFTW_WISDOM_ONLY);
    if( plan == NULL )
      {
      // no wisdom available for that plan
      if( canDestroyInput )
        {
        // just create the plan
        plan = fftw_plan_dft_r2c_2d(nx,ny,in,out,flags);
        }
      else
        {
        // lets create a plan with a fake input to generate the wisdom
        PixelType * din = new PixelType[nx*ny];
        fftw_plan_dft_r2c_2d(nx,ny,din,out,flags);
        delete [] din;
        // and then create the final plan - this time it shouldn't fail
        plan = fftw_plan_dft_r2c_2d(nx,ny,in,out,flags | FFTW_WISDOM_ONLY);
        }
      FFTWGlobalConfiguration::SetNewWisdomAvailable(true);
      }
    FFTWGlobalConfiguration::Unlock();
    assert( plan != NULL );
    return plan;
  }

  static PlanType Plan_dft_r2c_3d(int nx,
                                  int ny,
                                  int nz,
                                  PixelType *in,
                                  ComplexType *out,
                                  unsigned flags,
                                  int threads=1,
                                  bool canDestroyInput=false)
  {
    FFTWGlobalConfiguration::Lock();
    fftw_plan_with_nthreads(threads);
    PlanType plan = fftw_plan_dft_r2c_3d(nx,ny,nz,in,out,flags | FFTW_WISDOM_ONLY);
    if( plan == NULL )
      {
      // no wisdom available for that plan
      if( canDestroyInput )
        {
        // just create the plan
        plan = fftw_plan_dft_r2c_3d(nx,ny,nz,in,out,flags);
        }
      else
        {
        // lets create a plan with a fake input to generate the wisdom
        PixelType * din = new PixelType[nx*ny*nz];
        fftw_plan_dft_r2c_3d(nx,ny,nz,din,out,flags);
        delete [] din;
        // and then create the final plan - this time it shouldn't fail
        plan = fftw_plan_dft_r2c_3d(nx,ny,nz,in,out,flags | FFTW_WISDOM_ONLY);
        }
      FFTWGlobalConfiguration::SetNewWisdomAvailable(true);
      }
    FFTWGlobalConfiguration::Unlock();
    assert( plan != NULL );
    return plan;
  }

  static PlanType Plan_dft_r2c(int rank,
                               const int *n,
                               PixelType *in,
                               ComplexType *out,
                               unsigned flags,
                               int threads=1,
                               bool canDestroyInput=false)
  {
    FFTWGlobalConfiguration::Lock();
    fftw_plan_with_nthreads(threads);
    PlanType plan = fftw_plan_dft_r2c(rank,n,in,out,flags | FFTW_WISDOM_ONLY);
    if( plan == NULL )
      {
      // no wisdom available for that plan
      if( canDestroyInput )
        {
        // just create the plan
        plan = fftw_plan_dft_r2c(rank,n,in,out,flags);
        }
      else
        {
        // lets create a plan with a fake input to generate the wisdom
        int total = 0;
        for( int i=0; i<rank; i++ )
          {
          total += n[i];
          }
        PixelType * din = new PixelType[total];
        fftw_plan_dft_r2c(rank,n,din,out,flags);
        delete [] din;
        // and then create the final plan - this time it shouldn't fail
        plan = fftw_plan_dft_r2c(rank,n,in,out,flags | FFTW_WISDOM_ONLY);
        }
      FFTWGlobalConfiguration::SetNewWisdomAvailable(true);
      }
    FFTWGlobalConfiguration::Unlock();
    assert( plan != NULL );
    return plan;
  }

  static PlanType Plan_dft_1d(int n,
                                  ComplexType *in,
                                  ComplexType *out,
                                  int sign,
                                  unsigned flags,
                                  int threads=1,
                                  bool canDestroyInput=false)
  {
    FFTWGlobalConfiguration::Lock();
    fftw_plan_with_nthreads(threads);
    PlanType plan = fftw_plan_dft_1d(n,in,out,sign,flags | FFTW_WISDOM_ONLY);
    if( plan == NULL )
      {
      // no wisdom available for that plan
      if( canDestroyInput )
        {
        // just create the plan
        plan = fftw_plan_dft_1d(n,in,out,sign,flags);
        }
      else
        {
        // lets create a plan with a fake input to generate the wisdom
        ComplexType * din = new ComplexType[n];
        fftw_plan_dft_1d(n,din,out,sign,flags);
        delete [] din;
        // and then create the final plan - this time it shouldn't fail
        plan = fftw_plan_dft_1d(n,in,out,sign,flags | FFTW_WISDOM_ONLY);
        }
      FFTWGlobalConfiguration::SetNewWisdomAvailable(true);
      }
    FFTWGlobalConfiguration::Unlock();
    assert( plan != NULL );
    return plan;
  }

  static PlanType Plan_dft_2d(int nx,
                                  int ny,
                                  ComplexType *in,
                                  ComplexType *out,
                                  int sign,
                                  unsigned flags,
                                  int threads=1,
                                  bool canDestroyInput=false)
  {
    FFTWGlobalConfiguration::Lock();
    fftw_plan_with_nthreads(threads);
    PlanType plan = fftw_plan_dft_2d(nx,ny,in,out,sign,flags | FFTW_WISDOM_ONLY);
    if( plan == NULL )
      {
      // no wisdom available for that plan
      if( canDestroyInput )
        {
        // just create the plan
        plan = fftw_plan_dft_2d(nx,ny,in,out,sign,flags);
        }
      else
        {
        // lets create a plan with a fake input to generate the wisdom
        ComplexType * din = new ComplexType[nx*ny];
        fftw_plan_dft_2d(nx,ny,din,out,sign,flags);
        delete [] din;
        // and then create the final plan - this time it shouldn't fail
        plan = fftw_plan_dft_2d(nx,ny,in,out,sign,flags | FFTW_WISDOM_ONLY);
        }
      FFTWGlobalConfiguration::SetNewWisdomAvailable(true);
      }
    FFTWGlobalConfiguration::Unlock();
    assert( plan != NULL );
    return plan;
  }

  static PlanType Plan_dft_3d(int nx,
                                  int ny,
                                  int nz,
                                  ComplexType *in,
                                  ComplexType *out,
                                  int sign,
                                  unsigned flags,
                                  int threads=1,
                                  bool canDestroyInput=false)
  {
    FFTWGlobalConfiguration::Lock();
    fftw_plan_with_nthreads(threads);
    PlanType plan = fftw_plan_dft_3d(nx,ny,nz,in,out,sign,flags | FFTW_WISDOM_ONLY);
    if( plan == NULL )
      {
      // no wisdom available for that plan
      if( canDestroyInput )
        {
        // just create the plan
        plan = fftw_plan_dft_3d(nx,ny,nz,in,out,sign,flags);
        }
      else
        {
        // lets create a plan with a fake input to generate the wisdom
        ComplexType * din = new ComplexType[nx*ny*nz];
        fftw_plan_dft_3d(nx,ny,nz,din,out,sign,flags);
        delete [] din;
        // and then create the final plan - this time it shouldn't fail
        plan = fftw_plan_dft_3d(nx,ny,nz,in,out,sign,flags | FFTW_WISDOM_ONLY);
        }
      FFTWGlobalConfiguration::SetNewWisdomAvailable(true);
      }
    FFTWGlobalConfiguration::Unlock();
    assert( plan != NULL );
    return plan;
  }

  static PlanType Plan_dft(int rank,
                               const int *n,
                               ComplexType *in,
                               ComplexType *out,
                               int sign,
                               unsigned flags,
                               int threads=1,
                               bool canDestroyInput=false)
  {
    FFTWGlobalConfiguration::Lock();
    fftw_plan_with_nthreads(threads);
    PlanType plan = fftw_plan_dft(rank,n,in,out,sign,flags | FFTW_WISDOM_ONLY);
    if( plan == NULL )
      {
      // no wisdom available for that plan
      if( canDestroyInput )
        {
        // just create the plan
        plan = fftw_plan_dft(rank,n,in,out,sign,flags);
        }
      else
        {
        // lets create a plan with a fake input to generate the wisdom
        int total = 0;
        for( int i=0; i<rank; i++ )
          {
          total += n[i];
          }
        ComplexType * din = new ComplexType[total];
        fftw_plan_dft(rank,n,din,out,sign,flags);
        delete [] din;
        // and then create the final plan - this time it shouldn't fail
        plan = fftw_plan_dft(rank,n,in,out,sign,flags | FFTW_WISDOM_ONLY);
        }
      FFTWGlobalConfiguration::SetNewWisdomAvailable(true);
      }
    FFTWGlobalConfiguration::Unlock();
    assert( plan != NULL );
    return plan;
  }


  static void Execute(PlanType p)
  {
    fftw_execute(p);
  }
  static void DestroyPlan(PlanType p)
  {
    fftw_destroy_plan(p);
  }
};

#endif
}
}
#endif
