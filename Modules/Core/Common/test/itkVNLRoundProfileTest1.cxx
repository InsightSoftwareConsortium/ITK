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

#include "itkMath.h"
#include "itkTimeProbesCollectorBase.h"

double itkVNLRoundTestHelperFunction( double x )
{
  if( x >= 0.0 )
    {
    return static_cast< int >( x + 0.5f );
    }

 return static_cast< int >( x - 0.5f );
}

#define itkRoundMacro( x, y )                 \
    if( x >= 0.0 )                            \
      {                                       \
      y = static_cast< int >( x + 0.5f );     \
      }                                       \
    else                                      \
      {                                       \
      y = static_cast< int >( x - 0.5f );     \
      }


int itkVNLRoundProfileTest1( int, char *[] )
{
  itk::TimeProbesCollectorBase  chronometer;

  typedef std::vector< double >   ArrayType;

  ArrayType input;
  ArrayType output1;
  ArrayType output2;
  ArrayType output3;
  ArrayType output4;

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


  //
  // Make sure that entries in the .5 locations are included
  //
  for( signed int k = -10; k <= 10; k++ )
    {
    const double value = k + 0.5;
    input.push_back( value );
    }


  output1.resize( input.size() );
  output2.resize( input.size() );
  output3.resize( input.size() );
  output4.resize( input.size() );

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
      *outItr1nc = itkVNLRoundTestHelperFunction( *inpItr );
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
      if( *inpItr >= 0.0 )
        {
        *outItr3nc =  static_cast< int >( *inpItr + 0.5f );
        }

      *outItr3nc = static_cast< int >( *inpItr - 0.5f );

      ++outItr3nc;
      ++inpItr;
      }

    chronometer.Stop("Functor");

    inpItr   = input.begin();
    inputEnd = input.end();

    ArrayType::iterator        outItr4nc = output4.begin();

    //
    //  Count the time of rounding plus storing in container
    //
    chronometer.Start("Macro");

    while( inpItr != inputEnd )
      {
      itkRoundMacro(*inpItr, *outItr4nc );
      ++outItr4nc;
      ++inpItr;
      }

    chronometer.Stop("Macro");


    inpItr   = input.begin();
    inputEnd = input.end();

    ArrayType::iterator        outItr = output2.begin();

    //
    //  Count the time of rounding plus storing in container
    //
    chronometer.Start("itk::Math::rnd");

    while( inpItr != inputEnd )
      {
      *outItr = itk::Math::rnd( *inpItr );
      ++outItr;
      ++inpItr;
      }

    chronometer.Stop("itk::Math::rnd");

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

  bool roundUp = true;
  bool roundMismatch = false;

  std::cout << std::endl;
  std::cout << std::endl;

  while( inpItr != inputEnd )
    {
    if( itk::Math::abs( *outItr1 - *outItr2 ) > tolerance )
      {
      std::cout << "Warning*** For input: " << *inpItr << " if-round: " << *outItr1 << " differs from itk::Math::rnd: " << *outItr2 << std::endl;
      if ((static_cast<int>(*outItr2) % 2) == 0)
        {
        roundUp = false;
        }
      else
        {
        roundMismatch = true;
        }
      }
    ++inpItr;
    ++outItr1;
    ++outItr2;
    }

  std::cout << std::endl;
  std::cout << "Tested " << output1.size() << " entries " << std::endl;
  std::cout << std::endl;

  if (!roundMismatch)
    {
    if( roundUp)
      {
      std::cout << "******* On this platform, itk::Math::rnd() rounds up ********" << std::endl;
      }
    else
      {
      std::cout << "******* On this platform, itk::Math::rnd() rounds to even ********" << std::endl;
      }
    }
  else
    {
    std::cout << "******* On this platform, itk::Math::rnd() neither rounds up nor rounds to even consistently ********" << std::endl;
    }
  if (roundMismatch)
    {
    return EXIT_FAILURE;
    }
  return EXIT_SUCCESS;
}
