/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMathRoundTest.cxx
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
#include <math.h>
#include <string>
#include <iostream>
#include "itkIndex.h"

bool math_test_helper(std::string str, bool test)
{
  if (!test)
    {
    std::cout<<"test ("<<str<<") failed"<<std::endl;
    }
  return test;
}

int itkMathRoundTest( int, char *[] )
{
  bool ok = true;
  
  typedef itk::Index<3>::IndexValueType IndexValueType;

  ok &= math_test_helper("rnd(-8.4999)  == -8", itk::Math::Round<IndexValueType>(-8.4999)  == -8);
  ok &= math_test_helper("rnd(-8.4999f) == -8", itk::Math::Round<IndexValueType>(-8.4999f) == -8);
  ok &= math_test_helper("rnd(-8.50)    == -8", itk::Math::Round<IndexValueType>(-8.50)    == -8);
  ok &= math_test_helper("rnd(-8.50f)   == -8", itk::Math::Round<IndexValueType>(-8.50f)   == -8);
  ok &= math_test_helper("rnd(-8.5001)  == -9", itk::Math::Round<IndexValueType>(-8.5001)  == -9);
  ok &= math_test_helper("rnd(-8.5001f) == -9", itk::Math::Round<IndexValueType>(-8.5001f) == -9);
  ok &= math_test_helper("rnd(8.4999)   ==  8", itk::Math::Round<IndexValueType>(8.4999)   ==  8);
  ok &= math_test_helper("rnd(8.4999f)  ==  8", itk::Math::Round<IndexValueType>(8.4999f)  ==  8);
  ok &= math_test_helper("rnd(8.50)     ==  9", itk::Math::Round<IndexValueType>(8.50)     ==  9);
  ok &= math_test_helper("rnd(8.50f)    ==  9", itk::Math::Round<IndexValueType>(8.50f)    ==  9);
  ok &= math_test_helper("rnd(8.5001)   ==  9", itk::Math::Round<IndexValueType>(8.5001)   ==  9);
  ok &= math_test_helper("rnd(8.5001f)  ==  9", itk::Math::Round<IndexValueType>(8.5001f)  ==  9);

  ok &= math_test_helper("rnd(-9.4999)  == -9 ", itk::Math::Round<IndexValueType>(-9.4999)   == -9);
  ok &= math_test_helper("rnd(-9.4999f) == -9 ", itk::Math::Round<IndexValueType>(-9.4999f)  == -9);
  ok &= math_test_helper("rnd(-9.50)    == -9 ", itk::Math::Round<IndexValueType>(-9.50)     == -9);
  ok &= math_test_helper("rnd(-9.50f)   == -9 ", itk::Math::Round<IndexValueType>(-9.50f)    == -9);
  ok &= math_test_helper("rnd(-9.5001)  == -10", itk::Math::Round<IndexValueType>(-9.5001)   == -10);
  ok &= math_test_helper("rnd(-9.5001f) == -10", itk::Math::Round<IndexValueType>(-9.5001f)  == -10);
  ok &= math_test_helper("rnd(9.4999)   ==  9 ", itk::Math::Round<IndexValueType>(9.4999)    ==  9);
  ok &= math_test_helper("rnd(9.4999f)  ==  9 ", itk::Math::Round<IndexValueType>(9.4999f)   ==  9);
  ok &= math_test_helper("rnd(9.50)     ==  10", itk::Math::Round<IndexValueType>(9.50)      ==  10);
  ok &= math_test_helper("rnd(9.50f)    ==  10", itk::Math::Round<IndexValueType>(9.50f)     ==  10);
  ok &= math_test_helper("rnd(9.5001)   ==  10", itk::Math::Round<IndexValueType>(9.5001)    ==  10);
  ok &= math_test_helper("rnd(9.5001f)  ==  10", itk::Math::Round<IndexValueType>(9.5001f)   ==  10);

  ok &= math_test_helper("rnd_halfinttoeven(-8.4999)  == -8", itk::Math::RoundHalfIntegerToEven<IndexValueType>(-8.4999) == -8);
  ok &= math_test_helper("rnd_halfinttoeven(-8.4999f) == -8", itk::Math::RoundHalfIntegerToEven<IndexValueType>(-8.4999f)== -8);
  ok &= math_test_helper("rnd_halfinttoeven(-8.50)    == -8", itk::Math::RoundHalfIntegerToEven<IndexValueType>(-8.50)   == -8);
  ok &= math_test_helper("rnd_halfinttoeven(-8.50f)   == -8", itk::Math::RoundHalfIntegerToEven<IndexValueType>(-8.50f)  == -8);
  ok &= math_test_helper("rnd_halfinttoeven(-8.5001)  == -9", itk::Math::RoundHalfIntegerToEven<IndexValueType>(-8.5001) == -9);
  ok &= math_test_helper("rnd_halfinttoeven(-8.5001f) == -9", itk::Math::RoundHalfIntegerToEven<IndexValueType>(-8.5001f)== -9);
  ok &= math_test_helper("rnd_halfinttoeven(8.4999)   ==  8", itk::Math::RoundHalfIntegerToEven<IndexValueType>(8.4999)  ==  8);
  ok &= math_test_helper("rnd_halfinttoeven(8.4999f)  ==  8", itk::Math::RoundHalfIntegerToEven<IndexValueType>(8.4999f) ==  8);
  ok &= math_test_helper("rnd_halfinttoeven(8.50)     ==  8", itk::Math::RoundHalfIntegerToEven<IndexValueType>(8.50)    ==  8);
  ok &= math_test_helper("rnd_halfinttoeven(8.50f)    ==  8", itk::Math::RoundHalfIntegerToEven<IndexValueType>(8.50f)   ==  8);
  ok &= math_test_helper("rnd_halfinttoeven(8.5001)   ==  9", itk::Math::RoundHalfIntegerToEven<IndexValueType>(8.5001)  ==  9);
  ok &= math_test_helper("rnd_halfinttoeven(8.5001f)  ==  9", itk::Math::RoundHalfIntegerToEven<IndexValueType>(8.5001f) ==  9);

  ok &= math_test_helper("rnd_halfinttoeven(-9.4999)  == -9 ", itk::Math::RoundHalfIntegerToEven<IndexValueType>(-9.4999) == -9);
  ok &= math_test_helper("rnd_halfinttoeven(-9.4999f) == -9 ", itk::Math::RoundHalfIntegerToEven<IndexValueType>(-9.4999f)== -9);
  ok &= math_test_helper("rnd_halfinttoeven(-9.50)    == -10", itk::Math::RoundHalfIntegerToEven<IndexValueType>(-9.50)   == -10);
  ok &= math_test_helper("rnd_halfinttoeven(-9.50f)   == -10", itk::Math::RoundHalfIntegerToEven<IndexValueType>(-9.50f)  == -10);
  ok &= math_test_helper("rnd_halfinttoeven(-9.5001)  == -10", itk::Math::RoundHalfIntegerToEven<IndexValueType>(-9.5001) == -10);
  ok &= math_test_helper("rnd_halfinttoeven(-9.5001f) == -10", itk::Math::RoundHalfIntegerToEven<IndexValueType>(-9.5001f)== -10);
  ok &= math_test_helper("rnd_halfinttoeven(9.4999)   ==  9 ", itk::Math::RoundHalfIntegerToEven<IndexValueType>(9.4999)  ==  9);
  ok &= math_test_helper("rnd_halfinttoeven(9.4999f)  ==  9 ", itk::Math::RoundHalfIntegerToEven<IndexValueType>(9.4999f) ==  9);
  ok &= math_test_helper("rnd_halfinttoeven(9.50)     ==  10", itk::Math::RoundHalfIntegerToEven<IndexValueType>(9.50)    ==  10);
  ok &= math_test_helper("rnd_halfinttoeven(9.50f)    ==  10", itk::Math::RoundHalfIntegerToEven<IndexValueType>(9.50f)   ==  10);
  ok &= math_test_helper("rnd_halfinttoeven(9.5001)   ==  10", itk::Math::RoundHalfIntegerToEven<IndexValueType>(9.5001)  ==  10);
  ok &= math_test_helper("rnd_halfinttoeven(9.5001f)  ==  10", itk::Math::RoundHalfIntegerToEven<IndexValueType>(9.5001f) ==  10);

  ok &= math_test_helper("rnd_halfintup(-8.4999)  == -8", itk::Math::RoundHalfIntegerUp<IndexValueType>(-8.4999) == -8);
  ok &= math_test_helper("rnd_halfintup(-8.4999f) == -8", itk::Math::RoundHalfIntegerUp<IndexValueType>(-8.4999f)== -8);
  ok &= math_test_helper("rnd_halfintup(-8.50)    == -8", itk::Math::RoundHalfIntegerUp<IndexValueType>(-8.50)   == -8);
  ok &= math_test_helper("rnd_halfintup(-8.50f)   == -8", itk::Math::RoundHalfIntegerUp<IndexValueType>(-8.50f)  == -8);
  ok &= math_test_helper("rnd_halfintup(-8.5001)  == -9", itk::Math::RoundHalfIntegerUp<IndexValueType>(-8.5001) == -9);
  ok &= math_test_helper("rnd_halfintup(-8.5001f) == -9", itk::Math::RoundHalfIntegerUp<IndexValueType>(-8.5001f)== -9);
  ok &= math_test_helper("rnd_halfintup(8.4999)   ==  8", itk::Math::RoundHalfIntegerUp<IndexValueType>(8.4999)  ==  8);
  ok &= math_test_helper("rnd_halfintup(8.4999f)  ==  8", itk::Math::RoundHalfIntegerUp<IndexValueType>(8.4999f) ==  8);
  ok &= math_test_helper("rnd_halfintup(8.50)     ==  9", itk::Math::RoundHalfIntegerUp<IndexValueType>(8.50)    ==  9);
  ok &= math_test_helper("rnd_halfintup(8.50f)    ==  9", itk::Math::RoundHalfIntegerUp<IndexValueType>(8.50f)   ==  9);
  ok &= math_test_helper("rnd_halfintup(8.5001)   ==  9", itk::Math::RoundHalfIntegerUp<IndexValueType>(8.5001)  ==  9);
  ok &= math_test_helper("rnd_halfintup(8.5001f)  ==  9", itk::Math::RoundHalfIntegerUp<IndexValueType>(8.5001f) ==  9);

  ok &= math_test_helper("rnd_halfintup(-9.4999)  == -9 ", itk::Math::RoundHalfIntegerUp<IndexValueType>(-9.4999) == -9);
  ok &= math_test_helper("rnd_halfintup(-9.4999f) == -9 ", itk::Math::RoundHalfIntegerUp<IndexValueType>(-9.4999f)== -9);
  ok &= math_test_helper("rnd_halfintup(-9.50)    == -9 ", itk::Math::RoundHalfIntegerUp<IndexValueType>(-9.50)   == -9);
  ok &= math_test_helper("rnd_halfintup(-9.50f)   == -9 ", itk::Math::RoundHalfIntegerUp<IndexValueType>(-9.50f)  == -9);
  ok &= math_test_helper("rnd_halfintup(-9.5001)  == -10", itk::Math::RoundHalfIntegerUp<IndexValueType>(-9.5001) == -10);
  ok &= math_test_helper("rnd_halfintup(-9.5001f) == -10", itk::Math::RoundHalfIntegerUp<IndexValueType>(-9.5001f)== -10);
  ok &= math_test_helper("rnd_halfintup(9.4999)   ==  9 ", itk::Math::RoundHalfIntegerUp<IndexValueType>(9.4999)  ==  9);
  ok &= math_test_helper("rnd_halfintup(9.4999f)  ==  9 ", itk::Math::RoundHalfIntegerUp<IndexValueType>(9.4999f) ==  9);
  ok &= math_test_helper("rnd_halfintup(9.50)     ==  10", itk::Math::RoundHalfIntegerUp<IndexValueType>(9.50)    ==  10);
  ok &= math_test_helper("rnd_halfintup(9.50f)    ==  10", itk::Math::RoundHalfIntegerUp<IndexValueType>(9.50f)   ==  10);
  ok &= math_test_helper("rnd_halfintup(9.5001)   ==  10", itk::Math::RoundHalfIntegerUp<IndexValueType>(9.5001)  ==  10);
  ok &= math_test_helper("rnd_halfintup(9.5001f)  ==  10", itk::Math::RoundHalfIntegerUp<IndexValueType>(9.5001f) ==  10);

  ok &= math_test_helper("floor(8.0)      ==  8", itk::Math::Floor<IndexValueType>(8.0)      ==  8);
  ok &= math_test_helper("floor(8.0f)     ==  8", itk::Math::Floor<IndexValueType>(8.0f)     ==  8);
  ok &= math_test_helper("floor(8.9999)   ==  8", itk::Math::Floor<IndexValueType>(8.9999)   ==  8);
  ok &= math_test_helper("floor(8.9999f)  ==  8", itk::Math::Floor<IndexValueType>(8.9999f)  ==  8);
  ok &= math_test_helper("floor(8.0001)   ==  8", itk::Math::Floor<IndexValueType>(8.0001)   ==  8);
  ok &= math_test_helper("floor(8.0001f)  ==  8", itk::Math::Floor<IndexValueType>(8.0001f)  ==  8);
  ok &= math_test_helper("floor(-8.0)     == -8", itk::Math::Floor<IndexValueType>(-8.0)     == -8);
  ok &= math_test_helper("floor(-8.0f)    == -8", itk::Math::Floor<IndexValueType>(-8.0f)    == -8);
  ok &= math_test_helper("floor(-8.9999)  == -9", itk::Math::Floor<IndexValueType>(-8.9999)  == -9);
  ok &= math_test_helper("floor(-8.9999f) == -9", itk::Math::Floor<IndexValueType>(-8.9999f) == -9);
  ok &= math_test_helper("floor(-8.0001)  == -9", itk::Math::Floor<IndexValueType>(-8.0001)  == -9);
  ok &= math_test_helper("floor(-8.0001f) == -9", itk::Math::Floor<IndexValueType>(-8.0001f) == -9);

  ok &= math_test_helper("floor(9.0)      ==  9 ", itk::Math::Floor<IndexValueType>(9.0)      ==  9);
  ok &= math_test_helper("floor(9.0f)     ==  9 ", itk::Math::Floor<IndexValueType>(9.0f)     ==  9);
  ok &= math_test_helper("floor(9.9999)   ==  9 ", itk::Math::Floor<IndexValueType>(9.9999)   ==  9);
  ok &= math_test_helper("floor(9.9999f)  ==  9 ", itk::Math::Floor<IndexValueType>(9.9999f)  ==  9);
  ok &= math_test_helper("floor(9.0001)   ==  9 ", itk::Math::Floor<IndexValueType>(9.0001)   ==  9);
  ok &= math_test_helper("floor(9.0001f)  ==  9 ", itk::Math::Floor<IndexValueType>(9.0001f)  ==  9);
  ok &= math_test_helper("floor(-9.0)     == -9 ", itk::Math::Floor<IndexValueType>(-9.0)     == -9);
  ok &= math_test_helper("floor(-9.0f)    == -9 ", itk::Math::Floor<IndexValueType>(-9.0f)    == -9);
  ok &= math_test_helper("floor(-9.9999)  == -10", itk::Math::Floor<IndexValueType>(-9.9999)  == -10);
  ok &= math_test_helper("floor(-9.9999f) == -10", itk::Math::Floor<IndexValueType>(-9.9999f) == -10);
  ok &= math_test_helper("floor(-9.0001)  == -10", itk::Math::Floor<IndexValueType>(-9.0001)  == -10);
  ok &= math_test_helper("floor(-9.0001f) == -10", itk::Math::Floor<IndexValueType>(-9.0001f) == -10);

  ok &= math_test_helper("ceil(8.0)      ==  8", itk::Math::Ceil<IndexValueType>(8.0)      ==  8);
  ok &= math_test_helper("ceil(8.0f)     ==  8", itk::Math::Ceil<IndexValueType>(8.0f)     ==  8);
  ok &= math_test_helper("ceil(8.9999)   ==  9", itk::Math::Ceil<IndexValueType>(8.9999)   ==  9);
  ok &= math_test_helper("ceil(8.9999f)  ==  9", itk::Math::Ceil<IndexValueType>(8.9999f)  ==  9);
  ok &= math_test_helper("ceil(8.0001)   ==  9", itk::Math::Ceil<IndexValueType>(8.0001)   ==  9);
  ok &= math_test_helper("ceil(8.0001f)  ==  9", itk::Math::Ceil<IndexValueType>(8.0001f)  ==  9);
  ok &= math_test_helper("ceil(-8.0)     == -8", itk::Math::Ceil<IndexValueType>(-8.0)     == -8);
  ok &= math_test_helper("ceil(-8.0f)    == -8", itk::Math::Ceil<IndexValueType>(-8.0f)    == -8);
  ok &= math_test_helper("ceil(-8.9999)  == -8", itk::Math::Ceil<IndexValueType>(-8.9999)  == -8);
  ok &= math_test_helper("ceil(-8.9999f) == -8", itk::Math::Ceil<IndexValueType>(-8.9999f) == -8);
  ok &= math_test_helper("ceil(-8.0001)  == -8", itk::Math::Ceil<IndexValueType>(-8.0001)  == -8);
  ok &= math_test_helper("ceil(-8.0001f) == -8", itk::Math::Ceil<IndexValueType>(-8.0001f) == -8);

  ok &= math_test_helper("ceil(9.0)      ==  9 ", itk::Math::Ceil<IndexValueType>(9.0)      ==  9);
  ok &= math_test_helper("ceil(9.0f)     ==  9 ", itk::Math::Ceil<IndexValueType>(9.0f)     ==  9);
  ok &= math_test_helper("ceil(9.9999)   ==  10", itk::Math::Ceil<IndexValueType>(9.9999)   ==  10);
  ok &= math_test_helper("ceil(9.9999f)  ==  10", itk::Math::Ceil<IndexValueType>(9.9999f)  ==  10);
  ok &= math_test_helper("ceil(9.0001)   ==  10", itk::Math::Ceil<IndexValueType>(9.0001)   ==  10);
  ok &= math_test_helper("ceil(9.0001f)  ==  10", itk::Math::Ceil<IndexValueType>(9.0001f)  ==  10);
  ok &= math_test_helper("ceil(-9.0)     == -9 ", itk::Math::Ceil<IndexValueType>(-9.0)     == -9);
  ok &= math_test_helper("ceil(-9.0f)    == -9 ", itk::Math::Ceil<IndexValueType>(-9.0f)    == -9);
  ok &= math_test_helper("ceil(-9.9999)  == -9 ", itk::Math::Ceil<IndexValueType>(-9.9999)  == -9);
  ok &= math_test_helper("ceil(-9.9999f) == -9 ", itk::Math::Ceil<IndexValueType>(-9.9999f) == -9);
  ok &= math_test_helper("ceil(-9.0001)  == -9 ", itk::Math::Ceil<IndexValueType>(-9.0001)  == -9);
  ok &= math_test_helper("ceil(-9.0001f) == -9 ", itk::Math::Ceil<IndexValueType>(-9.0001f) == -9);

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
