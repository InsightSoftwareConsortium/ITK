/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVNLRoundProfileTest1.cxx
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

#include "vnl/vnl_math.h"
#include "itkTimeProbesCollectorBase.h"

double itkRoundTestHelperFunction( double x )
{
  if( x >= 0.0 )
    {
    return static_cast< int >( x + 0.5f );
    }
  
 return static_cast< int >( x - 0.5f );
}

template <class T>
class itkMathFunctor
{
public:
  static inline T Round(T x )
    {
    if( x >= 0.0 )
      {
      return static_cast< int >( x + 0.5f );
      }
    
   return static_cast< int >( x - 0.5f );

    }
};

int itkVNLRoundProfileTest1( int, char *[] )
{
  itk::TimeProbesCollectorBase  chronometer;

  typedef std::vector< double >   ArrayType;

  ArrayType input;
  ArrayType output1;
  ArrayType output2;
  ArrayType output3;

  const unsigned long numberOfValues = 1000000L;

  const double initialValue = -10.0;

  const double valueIncrement = (-initialValue-initialValue) / numberOfValues;

  std::cout << "Initial Value   = " << initialValue << std::endl;
  std::cout << "Value Increment = " << valueIncrement << std::endl;

  for( unsigned long i=0; i<numberOfValues; i++)
    {
    const double inputValue = initialValue + i * valueIncrement;
    input.push_back( inputValue );
    }

  output1.resize( input.size() );
  output2.resize( input.size() );
  output3.resize( input.size() );

  for(unsigned int tours=0; tours < 100; tours++)
    {
    //
    // Count the time of simply assigning values in an std::vector
    //
    //
    ArrayType::const_iterator  outItr1src = output1.begin();
    ArrayType::iterator        outItr2dst = output2.begin();

    ArrayType::const_iterator  outEnd1 = output1.end();

    chronometer.Start("std::vector");

    while( outItr1src != outEnd1 )
      {
      *outItr2dst = *outItr1src;
      ++outItr1src;
      ++outItr2dst;
      }

    chronometer.Stop("std::vector");

    ArrayType::const_iterator  inpItr   = input.begin();
    ArrayType::const_iterator  inputEnd = input.end();

    ArrayType::iterator        outItr1nc = output1.begin();

    //
    //  Count the time of rounding plus storing in container
    //
    chronometer.Start("if-round");

    while( inpItr != inputEnd )
      {
      *outItr1nc = itkRoundTestHelperFunction( *inpItr ); 
      ++outItr1nc;
      ++inpItr;
      }

    chronometer.Stop("if-round");


    inpItr   = input.begin();
    inputEnd = input.end();

    ArrayType::iterator        outItr3nc = output3.begin();

    //
    //  Count the time of rounding plus storing in container
    //
    chronometer.Start("Functor");

    while( inpItr != inputEnd )
      {
      *outItr3nc = itkMathFunctor<double>::Round( *inpItr ); 
      ++outItr3nc;
      ++inpItr;
      }

    chronometer.Stop("Functor");


    inpItr   = input.begin();
    inputEnd = input.end();

    ArrayType::iterator        outItr = output2.begin();

    //
    //  Count the time of rounding plus storing in container
    //
    chronometer.Start("vnl_math_rnd");

    while( inpItr != inputEnd )
      {
      *outItr = vnl_math_rnd( *inpItr ); 
      ++outItr;
      ++inpItr;
      }

    chronometer.Stop("vnl_math_rnd");

    }

  chronometer.Report( std::cout );

  //
  // Now test the correctness of the output
  //
  ArrayType::const_iterator inpItr   = input.begin();
  ArrayType::const_iterator inputEnd = input.end();

  ArrayType::const_iterator outItr1 = output1.begin();
  ArrayType::const_iterator outItr2 = output2.begin();

  const double tolerance = 1e-5;

  bool testFailed = false;

  std::cout << std::endl;
  std::cout << std::endl;

  while( inpItr != inputEnd )
    {
    if( vnl_math_abs( *outItr1 - *outItr2 ) > tolerance )
      {
      std::cerr << "Error in : " << *inpItr << " : " << *outItr1 << " : " << *outItr2 << std::endl;
      testFailed = true;
      }
    ++inpItr;
    ++outItr1;
    ++outItr2;
    }
  
  std::cout << std::endl;
  std::cout << "Tested " << output1.size() << " entries " << std::endl;
  std::cout << std::endl;

  if( testFailed )
    {
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
