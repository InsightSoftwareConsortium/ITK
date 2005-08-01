// This is core/vnl/vnl_random.h
#ifndef vnl_random_h
#define vnl_random_h
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
// \file
// \author Aaron Kotcheff (Manchester)
// \brief A superior random number generator

const int vnl_random_array_size = 37;

//: A superior random number generator.
// Implements a new random number generator that
// recently appeared in the literature. It generates 32 bit
// numbers with a higher degree of randomness than previous
// generators and has a cycle of 10^354 i.e. so huge that in
// practice it never cycles.
// For the mathematics behind it see:
// "A New Class of Random Number Generators" G. Marsaglia and A. Zaman,
// Annals of Applied Probability 1991, Vol. 1, No. 3, 462.
class vnl_random
{
    enum {linear_congruential_multiplier = 1664525, mz_previous1 = 24};
    unsigned long linear_congruential_previous;
    unsigned long mz_seed_array[vnl_random_array_size];
    unsigned long mz_array[vnl_random_array_size];
    int mz_array_position;
    int mz_borrow;
    unsigned long linear_congruential_lrand32();

    double mz_previous_normal;
    int mz_previous_normal_flag;

 public:
    //: Default constructor.
    // Initializes the random number generator non-deterministically.
    // i.e. it will generate a different series of random numbers each
    // time the program is run.
    vnl_random();

    //: Destructor
    ~vnl_random();

    //: Construct with seed.
    //  Initializes the random number generator deterministically
    //  using a single ulong as the 'seed'. A linear congruential
    //  generator is used to generate the 37 ulongs needed
    //  as the real seed. The same seed will produce the
    //  same series of random numbers.
    //
    //  9667566  is a good seed.
    vnl_random(unsigned long seed);

    //: Construct with seed.
    //  Initializes the random number generator deterministically
    //  using 37 ulongs as the 'seed'. The same seed will
    //  produce the same series of random numbers.
    vnl_random(unsigned long seed[vnl_random_array_size]);

    //: Copy constructor.
    //  Initializes/sets the random number generator to exactly
    //  the same state as the argument, i.e. both will generate exactly
    //  the same series of random numbers from then on.
    vnl_random(const vnl_random&);

    //: Copy operator.
    //  Initializes/sets the random number generator to exactly
    //  the same state as the argument, i.e. both will generate exactly
    //  the same series of random numbers from then on.
    vnl_random& operator=(const vnl_random&);

    //: Starts a new non-deterministic sequence from an already declared generator.
    void reseed();

    //: Starts a new deterministic sequence from an already declared generator using the provided seed.
    void reseed(unsigned long);

    //: Starts a new deterministic sequence from an already declared generator using the provided seed.
    void reseed(unsigned long[vnl_random_array_size]);

    //: This restarts the sequence of random numbers.
    //  Restarts so that it repeats
    //  from the point at which you declared the generator, last
    //  initialized it, or last called a 'reseed'.
    void restart();

    //: Generates a random unsigned 32-bit number.
    unsigned long lrand32();

    //: Generates a random unsigned long in [a,b]
    int lrand32(int a, int b);

    //: Generates a random unsigned long in [0,b]
    int lrand32(int b) {return lrand32(0, b);}

    //: Generates a random unsigned long in [a,b]
    int lrand32(int a, int b, int&);

    //:  Generates a random double in the range a <= x <= b with 32 bit randomness.
    //   drand32(1,0) is random down to about the 10th decimal place.
    double drand32(double a, double b);

    //: Generates a random unsigned integer in [0,n)
    // This function allows the random number generator to be used as
    // a functor, e.g. with vcl_random_shuffle()
    unsigned long operator()(unsigned n) { return lrand32(0, n-1); }

    //:  Generates a random double in the range 0 <= x <= b with 32 bit randomness.
    //   drand32(1.0) is random down to about the 10th decimal place.
    double drand32(double b) {return drand32(0.0, b);}

    //:  Generates a random double in the range 0 <= x <= 1 with 32 bit randomness.
    //   drand32() is random down to about the 10th decimal place.
    double drand32() {return drand32(0.0, 1.0);}

    //: Generates a random double in the range a <= x <= b with 64 bit randomness.
    //  Completely random down to the accuracy of an IEEE double.
    double drand64(double a, double b);

    //: Generates a random double in the range 0 <= x <= b with 64 bit randomness.
    //  Completely random down to the accuracy of an IEEE double.
    double drand64(double b) {return drand64(0.0, b);}

    //: Generates a random double in the range 0 <= x <= 1 with 64 bit randomness.
    //  Completely random down to the accuracy of an IEEE double.
    double drand64() {return drand64(0.0, 1.0);}

    //: Random value from a unit normal distribution about zero.
    // Uses a drand32() as its underlying generator.
    // Because the function uses a probability transform, the randomness (and
    // quantisation) is non-linearly dependent on the value. The further the
    // sample is from zero, the lower the number of bits on which it is random.
    double normal();

    //: Random value from a unit normal distribution about zero.
    // Uses a drand64() as its underlying generator.
    // Because the function uses a probability transform, the randomness (and
    // quantisation) is non-linearly dependent on the value. The further the
    // sample is from zero, the lower the number of bits on which it is random.
    double normal64();
};

#endif // vnl_random_h
