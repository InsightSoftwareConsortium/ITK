#ifndef _RANDOM_NUMBER_GENERATOR_H_
#define _RANDOM_NUMBER_GENERATOR_H_

#include <vnl/vnl_random.h>

/**
 * Random number generator, uniform and normal distributions.
 * This implementation wraps the vnl_random object and uses "nicer"
 * naming conventions, i.e. uniform instead of drand64.
 *
 * @author: Ziv Yaniv (zivy@isis.georgetown.edu)
 *
 */
class RandomNumberGenerator
{
public:
  RandomNumberGenerator() { generator = new vnl_random(); }


  RandomNumberGenerator(unsigned long seed) { generator = new vnl_random(seed); }


  ~RandomNumberGenerator() { delete generator; }


  /**
   * Get a random number uniformly distributed in (a,b).
   * @param a Lower bound for random numbers, default is 0.0.
   * @param b Upper bound for random numbers, default is 1.0.
   */
  virtual double
  uniform(double a = 0.0, double b = 1.0)
  {
    return generator->drand64(a, b);
  }

  /**
   * Get a random number normally distributed with given mean and standard deviation.
   * @param sigma Normal distributions standard deviation, default is one.
   * @param mu Normal distribution's mean, default is zero.
   */
  virtual double
  normal(double sigma = 1.0, double mu = 0.0)
  {
    return sigma * generator->normal64() + mu;
  }

private:
  vnl_random * generator;
};

#endif //_RANDOM_NUMBER_GENERATOR_H_
