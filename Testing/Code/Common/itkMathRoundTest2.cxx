/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMathRoundTest2.cxx
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

#include "itkMath.h"
#include <string>
#include <iostream>

namespace 
{
bool MathTestHelper(std::string str, bool test)
{
  if (!test)
    {
    std::cout<<"test ("<<str<<") failed"<<std::endl;
    }
  return test;
}

template <class T>
bool TemplatedRoundTest( void )
{
   bool ok = true;

  ok &= MathTestHelper("rnd(-8.4999)  == -8", itk::Math::Round<T>(-8.4999)  == -8);
  ok &= MathTestHelper("rnd(-8.4999f) == -8", itk::Math::Round<T>(-8.4999f) == -8);
  ok &= MathTestHelper("rnd(-8.50)    == -8", itk::Math::Round<T>(-8.50)    == -8);
  ok &= MathTestHelper("rnd(-8.50f)   == -8", itk::Math::Round<T>(-8.50f)   == -8);
  ok &= MathTestHelper("rnd(-8.5001)  == -9", itk::Math::Round<T>(-8.5001)  == -9);
  ok &= MathTestHelper("rnd(-8.5001f) == -9", itk::Math::Round<T>(-8.5001f) == -9);
  ok &= MathTestHelper("rnd(8.4999)   ==  8", itk::Math::Round<T>(8.4999)   ==  8);
  ok &= MathTestHelper("rnd(8.4999f)  ==  8", itk::Math::Round<T>(8.4999f)  ==  8);
  ok &= MathTestHelper("rnd(8.50)     ==  9", itk::Math::Round<T>(8.50)     ==  9);
  ok &= MathTestHelper("rnd(8.50f)    ==  9", itk::Math::Round<T>(8.50f)    ==  9);
  ok &= MathTestHelper("rnd(8.5001)   ==  9", itk::Math::Round<T>(8.5001)   ==  9);
  ok &= MathTestHelper("rnd(8.5001f)  ==  9", itk::Math::Round<T>(8.5001f)  ==  9);

  ok &= MathTestHelper("rnd(-9.4999)  == -9 ", itk::Math::Round<T>(-9.4999)   == -9);
  ok &= MathTestHelper("rnd(-9.4999f) == -9 ", itk::Math::Round<T>(-9.4999f)  == -9);
  ok &= MathTestHelper("rnd(-9.50)    == -9 ", itk::Math::Round<T>(-9.50)     == -9);
  ok &= MathTestHelper("rnd(-9.50f)   == -9 ", itk::Math::Round<T>(-9.50f)    == -9);
  ok &= MathTestHelper("rnd(-9.5001)  == -10", itk::Math::Round<T>(-9.5001)   == -10);
  ok &= MathTestHelper("rnd(-9.5001f) == -10", itk::Math::Round<T>(-9.5001f)  == -10);
  ok &= MathTestHelper("rnd(9.4999)   ==  9 ", itk::Math::Round<T>(9.4999)    ==  9);
  ok &= MathTestHelper("rnd(9.4999f)  ==  9 ", itk::Math::Round<T>(9.4999f)   ==  9);
  ok &= MathTestHelper("rnd(9.50)     ==  10", itk::Math::Round<T>(9.50)      ==  10);
  ok &= MathTestHelper("rnd(9.50f)    ==  10", itk::Math::Round<T>(9.50f)     ==  10);
  ok &= MathTestHelper("rnd(9.5001)   ==  10", itk::Math::Round<T>(9.5001)    ==  10);
  ok &= MathTestHelper("rnd(9.5001f)  ==  10", itk::Math::Round<T>(9.5001f)   ==  10);

  ok &= MathTestHelper("rnd_halfinttoeven(-8.4999)  == -8", itk::Math::RoundHalfIntegerToEven<T>(-8.4999) == -8);
  ok &= MathTestHelper("rnd_halfinttoeven(-8.4999f) == -8", itk::Math::RoundHalfIntegerToEven<T>(-8.4999f)== -8);
  ok &= MathTestHelper("rnd_halfinttoeven(-8.50)    == -8", itk::Math::RoundHalfIntegerToEven<T>(-8.50)   == -8);
  ok &= MathTestHelper("rnd_halfinttoeven(-8.50f)   == -8", itk::Math::RoundHalfIntegerToEven<T>(-8.50f)  == -8);
  ok &= MathTestHelper("rnd_halfinttoeven(-8.5001)  == -9", itk::Math::RoundHalfIntegerToEven<T>(-8.5001) == -9);
  ok &= MathTestHelper("rnd_halfinttoeven(-8.5001f) == -9", itk::Math::RoundHalfIntegerToEven<T>(-8.5001f)== -9);
  ok &= MathTestHelper("rnd_halfinttoeven(8.4999)   ==  8", itk::Math::RoundHalfIntegerToEven<T>(8.4999)  ==  8);
  ok &= MathTestHelper("rnd_halfinttoeven(8.4999f)  ==  8", itk::Math::RoundHalfIntegerToEven<T>(8.4999f) ==  8);
  ok &= MathTestHelper("rnd_halfinttoeven(8.50)     ==  8", itk::Math::RoundHalfIntegerToEven<T>(8.50)    ==  8);
  ok &= MathTestHelper("rnd_halfinttoeven(8.50f)    ==  8", itk::Math::RoundHalfIntegerToEven<T>(8.50f)   ==  8);
  ok &= MathTestHelper("rnd_halfinttoeven(8.5001)   ==  9", itk::Math::RoundHalfIntegerToEven<T>(8.5001)  ==  9);
  ok &= MathTestHelper("rnd_halfinttoeven(8.5001f)  ==  9", itk::Math::RoundHalfIntegerToEven<T>(8.5001f) ==  9);

  ok &= MathTestHelper("rnd_halfinttoeven(-9.4999)  == -9 ", itk::Math::RoundHalfIntegerToEven<T>(-9.4999) == -9);
  ok &= MathTestHelper("rnd_halfinttoeven(-9.4999f) == -9 ", itk::Math::RoundHalfIntegerToEven<T>(-9.4999f)== -9);
  ok &= MathTestHelper("rnd_halfinttoeven(-9.50)    == -10", itk::Math::RoundHalfIntegerToEven<T>(-9.50)   == -10);
  ok &= MathTestHelper("rnd_halfinttoeven(-9.50f)   == -10", itk::Math::RoundHalfIntegerToEven<T>(-9.50f)  == -10);
  ok &= MathTestHelper("rnd_halfinttoeven(-9.5001)  == -10", itk::Math::RoundHalfIntegerToEven<T>(-9.5001) == -10);
  ok &= MathTestHelper("rnd_halfinttoeven(-9.5001f) == -10", itk::Math::RoundHalfIntegerToEven<T>(-9.5001f)== -10);
  ok &= MathTestHelper("rnd_halfinttoeven(9.4999)   ==  9 ", itk::Math::RoundHalfIntegerToEven<T>(9.4999)  ==  9);
  ok &= MathTestHelper("rnd_halfinttoeven(9.4999f)  ==  9 ", itk::Math::RoundHalfIntegerToEven<T>(9.4999f) ==  9);
  ok &= MathTestHelper("rnd_halfinttoeven(9.50)     ==  10", itk::Math::RoundHalfIntegerToEven<T>(9.50)    ==  10);
  ok &= MathTestHelper("rnd_halfinttoeven(9.50f)    ==  10", itk::Math::RoundHalfIntegerToEven<T>(9.50f)   ==  10);
  ok &= MathTestHelper("rnd_halfinttoeven(9.5001)   ==  10", itk::Math::RoundHalfIntegerToEven<T>(9.5001)  ==  10);
  ok &= MathTestHelper("rnd_halfinttoeven(9.5001f)  ==  10", itk::Math::RoundHalfIntegerToEven<T>(9.5001f) ==  10);

  ok &= MathTestHelper("rnd_halfintup(-8.4999)  == -8", itk::Math::RoundHalfIntegerUp<T>(-8.4999) == -8);
  ok &= MathTestHelper("rnd_halfintup(-8.4999f) == -8", itk::Math::RoundHalfIntegerUp<T>(-8.4999f)== -8);
  ok &= MathTestHelper("rnd_halfintup(-8.50)    == -8", itk::Math::RoundHalfIntegerUp<T>(-8.50)   == -8);
  ok &= MathTestHelper("rnd_halfintup(-8.50f)   == -8", itk::Math::RoundHalfIntegerUp<T>(-8.50f)  == -8);
  ok &= MathTestHelper("rnd_halfintup(-8.5001)  == -9", itk::Math::RoundHalfIntegerUp<T>(-8.5001) == -9);
  ok &= MathTestHelper("rnd_halfintup(-8.5001f) == -9", itk::Math::RoundHalfIntegerUp<T>(-8.5001f)== -9);
  ok &= MathTestHelper("rnd_halfintup(8.4999)   ==  8", itk::Math::RoundHalfIntegerUp<T>(8.4999)  ==  8);
  ok &= MathTestHelper("rnd_halfintup(8.4999f)  ==  8", itk::Math::RoundHalfIntegerUp<T>(8.4999f) ==  8);
  ok &= MathTestHelper("rnd_halfintup(8.50)     ==  9", itk::Math::RoundHalfIntegerUp<T>(8.50)    ==  9);
  ok &= MathTestHelper("rnd_halfintup(8.50f)    ==  9", itk::Math::RoundHalfIntegerUp<T>(8.50f)   ==  9);
  ok &= MathTestHelper("rnd_halfintup(8.5001)   ==  9", itk::Math::RoundHalfIntegerUp<T>(8.5001)  ==  9);
  ok &= MathTestHelper("rnd_halfintup(8.5001f)  ==  9", itk::Math::RoundHalfIntegerUp<T>(8.5001f) ==  9);

  ok &= MathTestHelper("rnd_halfintup(-9.4999)  == -9 ", itk::Math::RoundHalfIntegerUp<T>(-9.4999) == -9);
  ok &= MathTestHelper("rnd_halfintup(-9.4999f) == -9 ", itk::Math::RoundHalfIntegerUp<T>(-9.4999f)== -9);
  ok &= MathTestHelper("rnd_halfintup(-9.50)    == -9 ", itk::Math::RoundHalfIntegerUp<T>(-9.50)   == -9);
  ok &= MathTestHelper("rnd_halfintup(-9.50f)   == -9 ", itk::Math::RoundHalfIntegerUp<T>(-9.50f)  == -9);
  ok &= MathTestHelper("rnd_halfintup(-9.5001)  == -10", itk::Math::RoundHalfIntegerUp<T>(-9.5001) == -10);
  ok &= MathTestHelper("rnd_halfintup(-9.5001f) == -10", itk::Math::RoundHalfIntegerUp<T>(-9.5001f)== -10);
  ok &= MathTestHelper("rnd_halfintup(9.4999)   ==  9 ", itk::Math::RoundHalfIntegerUp<T>(9.4999)  ==  9);
  ok &= MathTestHelper("rnd_halfintup(9.4999f)  ==  9 ", itk::Math::RoundHalfIntegerUp<T>(9.4999f) ==  9);
  ok &= MathTestHelper("rnd_halfintup(9.50)     ==  10", itk::Math::RoundHalfIntegerUp<T>(9.50)    ==  10);
  ok &= MathTestHelper("rnd_halfintup(9.50f)    ==  10", itk::Math::RoundHalfIntegerUp<T>(9.50f)   ==  10);
  ok &= MathTestHelper("rnd_halfintup(9.5001)   ==  10", itk::Math::RoundHalfIntegerUp<T>(9.5001)  ==  10);
  ok &= MathTestHelper("rnd_halfintup(9.5001f)  ==  10", itk::Math::RoundHalfIntegerUp<T>(9.5001f) ==  10);

  ok &= MathTestHelper("floor(8.0)      ==  8", itk::Math::Floor<T>(8.0)      ==  8);
  ok &= MathTestHelper("floor(8.0f)     ==  8", itk::Math::Floor<T>(8.0f)     ==  8);
  ok &= MathTestHelper("floor(8.9999)   ==  8", itk::Math::Floor<T>(8.9999)   ==  8);
  ok &= MathTestHelper("floor(8.9999f)  ==  8", itk::Math::Floor<T>(8.9999f)  ==  8);
  ok &= MathTestHelper("floor(8.0001)   ==  8", itk::Math::Floor<T>(8.0001)   ==  8);
  ok &= MathTestHelper("floor(8.0001f)  ==  8", itk::Math::Floor<T>(8.0001f)  ==  8);
  ok &= MathTestHelper("floor(-8.0)     == -8", itk::Math::Floor<T>(-8.0)     == -8);
  ok &= MathTestHelper("floor(-8.0f)    == -8", itk::Math::Floor<T>(-8.0f)    == -8);
  ok &= MathTestHelper("floor(-8.9999)  == -9", itk::Math::Floor<T>(-8.9999)  == -9);
  ok &= MathTestHelper("floor(-8.9999f) == -9", itk::Math::Floor<T>(-8.9999f) == -9);
  ok &= MathTestHelper("floor(-8.0001)  == -9", itk::Math::Floor<T>(-8.0001)  == -9);
  ok &= MathTestHelper("floor(-8.0001f) == -9", itk::Math::Floor<T>(-8.0001f) == -9);

  ok &= MathTestHelper("floor(9.0)      ==  9 ", itk::Math::Floor<T>(9.0)      ==  9);
  ok &= MathTestHelper("floor(9.0f)     ==  9 ", itk::Math::Floor<T>(9.0f)     ==  9);
  ok &= MathTestHelper("floor(9.9999)   ==  9 ", itk::Math::Floor<T>(9.9999)   ==  9);
  ok &= MathTestHelper("floor(9.9999f)  ==  9 ", itk::Math::Floor<T>(9.9999f)  ==  9);
  ok &= MathTestHelper("floor(9.0001)   ==  9 ", itk::Math::Floor<T>(9.0001)   ==  9);
  ok &= MathTestHelper("floor(9.0001f)  ==  9 ", itk::Math::Floor<T>(9.0001f)  ==  9);
  ok &= MathTestHelper("floor(-9.0)     == -9 ", itk::Math::Floor<T>(-9.0)     == -9);
  ok &= MathTestHelper("floor(-9.0f)    == -9 ", itk::Math::Floor<T>(-9.0f)    == -9);
  ok &= MathTestHelper("floor(-9.9999)  == -10", itk::Math::Floor<T>(-9.9999)  == -10);
  ok &= MathTestHelper("floor(-9.9999f) == -10", itk::Math::Floor<T>(-9.9999f) == -10);
  ok &= MathTestHelper("floor(-9.0001)  == -10", itk::Math::Floor<T>(-9.0001)  == -10);
  ok &= MathTestHelper("floor(-9.0001f) == -10", itk::Math::Floor<T>(-9.0001f) == -10);

  ok &= MathTestHelper("ceil(8.0)      ==  8", itk::Math::Ceil<T>(8.0)      ==  8);
  ok &= MathTestHelper("ceil(8.0f)     ==  8", itk::Math::Ceil<T>(8.0f)     ==  8);
  ok &= MathTestHelper("ceil(8.9999)   ==  9", itk::Math::Ceil<T>(8.9999)   ==  9);
  ok &= MathTestHelper("ceil(8.9999f)  ==  9", itk::Math::Ceil<T>(8.9999f)  ==  9);
  ok &= MathTestHelper("ceil(8.0001)   ==  9", itk::Math::Ceil<T>(8.0001)   ==  9);
  ok &= MathTestHelper("ceil(8.0001f)  ==  9", itk::Math::Ceil<T>(8.0001f)  ==  9);
  ok &= MathTestHelper("ceil(-8.0)     == -8", itk::Math::Ceil<T>(-8.0)     == -8);
  ok &= MathTestHelper("ceil(-8.0f)    == -8", itk::Math::Ceil<T>(-8.0f)    == -8);
  ok &= MathTestHelper("ceil(-8.9999)  == -8", itk::Math::Ceil<T>(-8.9999)  == -8);
  ok &= MathTestHelper("ceil(-8.9999f) == -8", itk::Math::Ceil<T>(-8.9999f) == -8);
  ok &= MathTestHelper("ceil(-8.0001)  == -8", itk::Math::Ceil<T>(-8.0001)  == -8);
  ok &= MathTestHelper("ceil(-8.0001f) == -8", itk::Math::Ceil<T>(-8.0001f) == -8);

  ok &= MathTestHelper("ceil(9.0)      ==  9 ", itk::Math::Ceil<T>(9.0)      ==  9);
  ok &= MathTestHelper("ceil(9.0f)     ==  9 ", itk::Math::Ceil<T>(9.0f)     ==  9);
  ok &= MathTestHelper("ceil(9.9999)   ==  10", itk::Math::Ceil<T>(9.9999)   ==  10);
  ok &= MathTestHelper("ceil(9.9999f)  ==  10", itk::Math::Ceil<T>(9.9999f)  ==  10);
  ok &= MathTestHelper("ceil(9.0001)   ==  10", itk::Math::Ceil<T>(9.0001)   ==  10);
  ok &= MathTestHelper("ceil(9.0001f)  ==  10", itk::Math::Ceil<T>(9.0001f)  ==  10);
  ok &= MathTestHelper("ceil(-9.0)     == -9 ", itk::Math::Ceil<T>(-9.0)     == -9);
  ok &= MathTestHelper("ceil(-9.0f)    == -9 ", itk::Math::Ceil<T>(-9.0f)    == -9);
  ok &= MathTestHelper("ceil(-9.9999)  == -9 ", itk::Math::Ceil<T>(-9.9999)  == -9);
  ok &= MathTestHelper("ceil(-9.9999f) == -9 ", itk::Math::Ceil<T>(-9.9999f) == -9);
  ok &= MathTestHelper("ceil(-9.0001)  == -9 ", itk::Math::Ceil<T>(-9.0001)  == -9);
  ok &= MathTestHelper("ceil(-9.0001f) == -9 ", itk::Math::Ceil<T>(-9.0001f) == -9);

  return ok;
}

}
int itkMathRoundTest2( int, char *[] )
{
  bool ok = true;

  ok &= TemplatedRoundTest<char>();
  ok &= TemplatedRoundTest<short>();
  ok &= TemplatedRoundTest<int>();
  ok &= TemplatedRoundTest<long>();
#if VXL_HAS_INT_64
  ok &= TemplatedRoundTest<vxl_int_64>();
#endif
  

  if (!ok)
    {
    return EXIT_FAILURE;
    }
  else
    {
    std::cout<<"Test passed"<<std::endl;
    return EXIT_SUCCESS;
    }
}
