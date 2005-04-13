/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMersenneTwisterRandomVariateGeneratorTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include "itkMersenneTwisterRandomVariateGenerator.h"
#include "vnl/vnl_sample.h"
#include "vnl/vnl_math.h"
#include "itkTimeProbe.h"

bool ComputeMeanAndVariance( const unsigned long numberOfSamples );


int itkMersenneTwisterRandomVariateGeneratorTest(int, char* [] ) 
{
#if __CYGWIN__
  vnl_sample_reseed(0x1234abcd);
#endif

  std::cout << "MersenneTwisterRandomVariateGenerator Test" << std::endl; 

  bool pass = true;
  
  pass &= ComputeMeanAndVariance( 100000000UL );
  pass &= ComputeMeanAndVariance( 10000000UL );
  pass &= ComputeMeanAndVariance( 1000000UL );

  if( pass )
    {
    return EXIT_SUCCESS;
    }
  else 
    {
    return EXIT_FAILURE;
    }
}


bool ComputeMeanAndVariance(const unsigned long numberOfSamples)
{
  std::cout << "Test mean and variance of a uniform distribution [0, 10): " << std::endl;

  bool pass = true;  
  const double tolerance = 0.05;

  typedef  itk::Statistics::MersenneTwisterRandomVariateGenerator GeneratorType;
  GeneratorType::Pointer generator = GeneratorType::New();
  GeneratorType::IntegerType randomSeed = 14543 ; // any number to initialize the seed.
  generator->Initialize( randomSeed );

  
  double MTsum  = 0.0f;
  double MTsum2 = 0.0f;

  std::cout << "Compute Mean and variance with " << 
        numberOfSamples << " samples... " << std::endl;

  itk::TimeProbe clockMT1;
  clockMT1.Start(); 
  for(unsigned int i=0; i < numberOfSamples; i++)
    {
    generator->GetUniformVariate(0,10);
    }
  clockMT1.Stop();
    
  for(unsigned int i=0; i < numberOfSamples; i++)
    {
    double variate = generator->GetUniformVariate(0,10);
    MTsum  += variate;
    MTsum2 += variate * variate;
    }

  { 
    const double mean     = MTsum  / numberOfSamples;
    const double variance = MTsum2 / numberOfSamples - mean * mean;

    std::cout << "Mean [MersenneTwisterRandomVariateGenerator]     = " 
      << mean     << std::endl;
    std::cout << "Variance [MersenneTwisterRandomVariateGenerator] = " 
      << variance << std::endl;

    const double uniformVariance = 100.0/12.0;
    std::cout << "Testing Mean " << std::endl;
    if( fabs(mean - 5) > tolerance )
      {
      pass = false;
      std::cout << "[FAILED]" << std::endl;
      }
    else
      {
      std::cout << "[PASSED]" << std::endl;
      }
    std::cout << "Testing Variance " << std::endl;
    if( fabs(variance - uniformVariance) > tolerance )
      {
      pass = false;
      std::cout << "[FAILED]" << std::endl;
      }
    else
      {
      std::cout << "[PASSED]" << std::endl;
      }
    }


  double vnlSum  = 0.0f;
  double vnlSum2 = 0.0f;


  std::cout << "USING VNL " << std::endl;

  itk::TimeProbe clockVnl1;
  clockVnl1.Start(); 
  for(unsigned int i=0; i < numberOfSamples; i++)
    {
    vnl_sample_uniform(0,10);
    }
  clockVnl1.Stop();
  
  for(unsigned int i=0; i < numberOfSamples; i++)
    {
    double variate = vnl_sample_uniform(0,10);
    vnlSum  += variate;
    vnlSum2 += variate * variate;
    }

  { 
    const double mean     = vnlSum  / numberOfSamples;
    const double variance = vnlSum2 / numberOfSamples - mean * mean;

    std::cout << "Mean [VNL]     = " 
      << mean     << std::endl;
    std::cout << "Variance [VNL] = " 
      << variance << std::endl;

    const double uniformVariance = 100.0/12.0;
    std::cout << "Testing Mean " << std::endl;
    if( fabs(mean - 5) > tolerance )
      {
      pass = false;
      std::cout << "[FAILED]" << std::endl;
      }
    else
      {
      std::cout << "[PASSED]" << std::endl;
      }
    std::cout << "Testing Variance " << std::endl;
    if( fabs(variance - uniformVariance) > tolerance )
      {
      pass = false;
      std::cout << "[FAILED]" << std::endl;
      }
    else
      {
      std::cout << "[PASSED]" << std::endl;
      }
    }


  
  std::cout << 
    "MersenneTwisterRandomVariateGenerator: Computing "
    << numberOfSamples << " random numbers took " << 
    clockMT1.GetMeanTime() << " s" << std::endl;
  std::cout << 
    "vnl_sample                           : Computing "
    << numberOfSamples << " random numbers took " << 
    clockVnl1.GetMeanTime() << " s" << std::endl;

  return pass;

}

