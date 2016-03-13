// This is core/vnl/vnl_random.cxx
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma implementation
#endif
//:
//  \file

#include <ctime>
#include <cmath>
#include "vnl_random.h"
#include <vcl_compiler.h>
#include <vcl_cassert.h>

unsigned long vnl_random::linear_congruential_lrand32()
{
  return linear_congruential_previous = (linear_congruential_previous*linear_congruential_multiplier + 1)&0xffffffff;
}

//: Construct with seed
vnl_random::vnl_random(unsigned long seed)
  : linear_congruential_previous(seed), mz_array_position(0UL), mz_borrow(0), mz_previous_normal_flag(0)
{reseed(seed);}

//: Construct with seed
vnl_random::vnl_random(unsigned long seed[vnl_random_array_size])
  : mz_array_position(0UL), mz_borrow(0), mz_previous_normal_flag(0)
{reseed(seed);}

vnl_random::vnl_random(const vnl_random& r)
  : linear_congruential_previous(r.linear_congruential_previous)
  , mz_array_position(r.mz_array_position)
  , mz_borrow(r.mz_borrow)
  , mz_previous_normal_flag(r.mz_previous_normal_flag)
{
  for (unsigned int i=0;i<vnl_random_array_size;++i)
  {
    mz_seed_array[i] = r.mz_seed_array[i];
    mz_array[i] = r.mz_array[i];
  }
}

vnl_random& vnl_random::operator=(const vnl_random& r)
{
  linear_congruential_previous=r.linear_congruential_previous;
  mz_array_position=r.mz_array_position;
  mz_borrow=r.mz_borrow;
  mz_previous_normal_flag=r.mz_previous_normal_flag;
  for (unsigned int i=0;i<vnl_random_array_size;++i)
  {
    mz_seed_array[i] = r.mz_seed_array[i];
    mz_array[i] = r.mz_array[i];
  }
  return *this;
}

vnl_random::vnl_random() : mz_array_position(0UL), mz_borrow(0), mz_previous_normal_flag(0)
{
  reseed();
}

vnl_random::~vnl_random()
{
  for (unsigned int i=0;i<vnl_random_array_size;++i)
  {
    mz_seed_array[i] = 0;
    mz_array[i] = 0;
  }
}

void vnl_random::reseed()
{
  reseed((unsigned long)std::time(VXL_NULLPTR));
}

void vnl_random::reseed(unsigned long seed)
{
  mz_array_position = 0UL;
  mz_borrow = 0;

  linear_congruential_previous = seed;
  // Use the lc generator to fill the array
  for (unsigned int i=0;i<vnl_random_array_size;++i)
  {
    mz_seed_array[i] = linear_congruential_lrand32();
    mz_array[i] = mz_seed_array[i];
  }

  // Warm up with 1000 randoms
  for (int j=0;j<1000;j++) lrand32();
}

void vnl_random::reseed(unsigned long seed[vnl_random_array_size])
{
  mz_array_position = 0UL;
  mz_borrow = 0L;

  for (unsigned int i=0;i<vnl_random_array_size;++i)
  {
    mz_array[i] = seed[i];
    mz_seed_array[i] = seed[i];
  }
}

void vnl_random::restart()
{
  mz_array_position = 0UL;

  for (unsigned int i=0;i<vnl_random_array_size;++i)
  {
    mz_array[i] = mz_seed_array[i];
  }
}

double vnl_random::normal()
{
  if (mz_previous_normal_flag)
  {
    mz_previous_normal_flag = 0;
    return mz_previous_normal;
  }
  else
  {
    double x,y,r2;
    do
    {
      x = drand32(-1.0,1.0);
      y = drand32(-1.0,1.0);
      r2 = x*x+y*y;
    }
    while (r2 >=1.0 || r2 == 0.0);
    double fac = std::sqrt(-2.0*std::log(r2)/r2);
    mz_previous_normal = x*fac;
    mz_previous_normal_flag = 1;
    return y*fac;
  }
}


//: Random value from a unit normal distribution about zero
// Uses a drand64() as its underlying generator.
// Because the function uses a probability transform, the randomness (and
// quantisation) is non-linearly dependent on the value. The further the sample
// is from zero, the lower the number of bits on which it is random.
double vnl_random::normal64()
{
  if (mz_previous_normal_flag)
  {
    mz_previous_normal_flag = 0;
    return mz_previous_normal;
  }
  else
  {
    double x,y,r2;
    do
    {
      x = drand64(-1.0,1.0);
      y = drand64(-1.0,1.0);
      r2 = x*x+y*y;
    }
    while (r2 >=1.0 || r2 == 0.0);
    double fac = std::sqrt(-2.0*std::log(r2)/r2);
    mz_previous_normal = x*fac;
    mz_previous_normal_flag = 1;
    return y*fac;
  }
}

unsigned long vnl_random::lrand32()
{
  unsigned long p1 = mz_array[(vnl_random_array_size + mz_array_position - mz_previous1)%vnl_random_array_size];
  unsigned long p2 = (p1 - mz_array[mz_array_position] - mz_borrow)&0xffffffff;
  if (p2 < p1) mz_borrow = 0;
  if (p2 > p1) mz_borrow = 1;
  mz_array[mz_array_position] = p2;
  mz_array_position = (mz_array_position+1)%vnl_random_array_size;
  return p2;
}

int vnl_random::lrand32(int lower, int upper)
{
  assert(lower <= upper);

  // Note: we have to reject some numbers otherwise we get a very slight bias
  // towards the lower part of the range lower - upper. See below

  unsigned long range = upper-lower+1;
  unsigned long denom = 0xffffffff/range;
  unsigned long ran;
  while ((ran=lrand32()) >= denom*range) ;
  return lower + int(ran/denom);
}


int vnl_random::lrand32(int lower, int upper, int &count)
{
  assert(lower <= upper);

  // Note: we have to reject some numbers otherwise we get a very slight bias
  // towards the lower part of the range lower - upper. Hence this is a "count"
  // version of the above function that returns the number of lrand32()
  // calls made.

  unsigned long range = upper-lower+1;
  unsigned long denom = 0xffffffff/range;
  unsigned long ran;
  count = 1;
  while ((ran=lrand32())>=denom*range) ++count;
  return lower + int(ran/denom);
}

double vnl_random::drand32(double lower, double upper)
{
  assert(lower <= upper);
  return  (double(lrand32())/0xffffffff)*(upper-lower) + lower;
}

double vnl_random::drand64(double lower, double upper)
{
  assert(lower <= upper);
  return  (double(lrand32())/0xffffffff + double(lrand32())/(double(0xffffffff)*double(0xffffffff)))*(upper-lower) + lower;
}
