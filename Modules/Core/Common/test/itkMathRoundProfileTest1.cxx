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

#include "itkTimeProbesCollectorBase.h"
#include "itkMath.h"

int itkMathRoundTestHelperFunction( double x )
{
  x += 0.5;
  return static_cast<int>(x>=0.?x:(itk::Math::ExactlyEquals(x,static_cast<int>(x))?x:x-1.));
}

#define itkRoundMacro( x, y )                 \
    if( x >= 0.5 )                            \
      {                                       \
      y = static_cast< int >( x + 0.5 );      \
      }                                       \
    else                                      \
      {                                       \
CLANG_PRAGMA_PUSH                             \
CLANG_SUPPRESS_Wfloat_equal                     \
      if( (x+0.5) == static_cast<int>(x+0.5) )  \
CLANG_PRAGMA_POP                              \
        {                                     \
        y = static_cast< int >( x + 0.5 );    \
        }                                     \
      else                                    \
        {                                     \
        y = static_cast< int >( x - 0.5 );    \
        }                                     \
      }


int itkMathRoundProfileTest1( int, char *[] )
{
  itk::TimeProbesCollectorBase  chronometer;

  typedef std::vector< double >   ArrayType;
  typedef std::vector< int >      IntArrayType;

  ArrayType input;
  IntArrayType output1;
  IntArrayType output2;
  IntArrayType output3;
  IntArrayType output4;

  const unsigned long numberOfValues = 1000L;

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
    IntArrayType::const_iterator  outItr1src = output1.begin();
    IntArrayType::iterator        outItr2dst = output2.begin();

    IntArrayType::const_iterator  outEnd1 = output1.end();

    chronometer.Start("std::vector");

    while( outItr1src != outEnd1 )
      {
      *outItr2dst++ = *outItr1src++;
      }

    chronometer.Stop("std::vector");

    ArrayType::const_iterator  inpItr   = input.begin();
    ArrayType::const_iterator  inputEnd = input.end();

    IntArrayType::iterator        outItr1nc = output1.begin();

    //
    //  Count the time of rounding plus storing in container
    //
    chronometer.Start("if-round");

    while( inpItr != inputEnd )
      {
      *outItr1nc++ = itkMathRoundTestHelperFunction( *inpItr++ );
      }

    chronometer.Stop("if-round");


    inpItr   = input.begin();
    inputEnd = input.end();

    IntArrayType::iterator        outItr3nc = output3.begin();

    //
    //  Count the time of rounding plus storing in container
    //
    chronometer.Start("Functor");

    while( inpItr != inputEnd )
      {
      const double x = (*inpItr++) + 0.5;
      *outItr3nc++ = static_cast<int>(x>=0.?x:(itk::Math::ExactlyEquals(x,static_cast<int>(x))?x:x-1.));
      }

    chronometer.Stop("Functor");

    inpItr   = input.begin();
    inputEnd = input.end();

    IntArrayType::iterator        outItr4nc = output4.begin();

    //
    //  Count the time of rounding plus storing in container
    //
    chronometer.Start("Macro");

    while( inpItr != inputEnd )
      {
      itkRoundMacro(*inpItr, *outItr4nc );
      ++inpItr;
      ++outItr4nc;
      }

    chronometer.Stop("Macro");


    inpItr   = input.begin();
    inputEnd = input.end();

    IntArrayType::iterator        outItr = output2.begin();

    //
    //  Count the time of rounding plus storing in container
    //
    chronometer.Start("itk::Math::Round");

    while( inpItr != inputEnd )
      {
      *outItr++ = itk::Math::Round<int>( *inpItr++ );
      }

    chronometer.Stop("itk::Math::Round");

    }

  chronometer.Report( std::cout );

  //
  // Now test the correctness of the output
  //
  ArrayType::const_iterator inpItr   = input.begin();
  ArrayType::const_iterator inputEnd = input.end();

  IntArrayType::const_iterator outItr1 = output1.begin();
  IntArrayType::const_iterator outItr2 = output2.begin();

  bool roundMismatch = false;

  std::cout << std::endl;
  std::cout << std::endl;

  while( inpItr != inputEnd )
    {
      if( (*outItr1) != (*outItr2) )
      {
      std::cout << "Warning*** For input: " << *inpItr << " if-round: " << *outItr1 << " differs from itk::Math::Round: " << *outItr2 << std::endl;

      roundMismatch = true;
      }
    ++inpItr;
    ++outItr1;
    ++outItr2;
    }

  std::cout << std::endl;
  std::cout << "Tested " << output1.size() << " entries " << std::endl;
  std::cout << std::endl;

  if (roundMismatch)
    {
    std::cout << "******* On this platform, itk::Math::Round() does not round half integers upward ********" << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
