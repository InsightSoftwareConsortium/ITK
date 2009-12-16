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
#include <cstdlib>

#define RoundTestHelperMacro( rndname, input, output )             \
  if ( rndname( (input) ) != (output))                              \
    {                                                               \
    std::cout << "Failure! " <<  #rndname << "(" << (int)(input) << ") expected "  \
              << (int)(output) << " but got " <<  (int)rndname( (input) )     \
                << std::endl;                                       \
      ok = false;                                                   \
    }

namespace 
{



template <class T>
bool TemplatedRoundTest( void )
{
   bool ok = true;

   const unsigned int numberOfElements = 15;
   
  // input data for rounding methods
  float input[] =  {-8.4999f, -8.50f, -8.5001f, 
                     8.4999f,  8.50f,  8.5001f, 
                    -9.4999f, -9.50f, -9.5001f, 
                    9.4999f,  9.50f,  9.5001f,
                    -0.4999f, -.50f, -.5001f};

  T roundOutput[] = { -8, -8, -9,
                       8,  9,  9,
                      -9, -9, -10,
                      9, 10,  10,
                      0, 0, -1};


  T halftoevenOutput[] = { -8, -8, -9,
                            8,  8,  9,
                           -9, -10, -10,
                           9, 10,  10,
                           0, 0, -1};


  T *halfupOutput = roundOutput;
  
  
  ////////
  // input data for floor and ceil methods
  float fcinput[] = { 8.0f,  8.9999f,  8.0001f,
                     -8.0f, -8.9999f, -8.0001f,
                      9.0f,  9.9999f,  9.0001f,
                      -9.0f, -9.9999f, -9.0001f,
                      -1.0f, -0.9999f, -1.0001f};

   T floorOutput[] =  {  8,  8,  8,
                        -8, -9, -9,
                         9,  9,  9,
                         -9,-10,-10, 
                         -1,-1, -2};

 
  T ceilOutput[] =  {  8,  9,  9,
                      -8, -8, -8,
                       9, 10, 10,
                       -9, -9, -9,
                       -1, 0, -1};

  
  // Round
  for (unsigned int i = 0; i < numberOfElements; ++i) 
    {
    
    RoundTestHelperMacro( itk::Math::Round<T>, (float)input[i], roundOutput[i] );

    RoundTestHelperMacro( itk::Math::Round<T>, (double)input[i], roundOutput[i] );
    
    }

  // RoundHalfIntegerToEven
  for (unsigned int i = 0; i < numberOfElements; ++i) 
    {

  
    RoundTestHelperMacro( itk::Math::RoundHalfIntegerToEven<T>, (float)input[i], halftoevenOutput[i] );

    RoundTestHelperMacro( itk::Math::RoundHalfIntegerToEven<T>, (double)input[i], halftoevenOutput[i] );

    }

  // RoundHalfIntegerUp
  for (unsigned int i = 0; i < numberOfElements; ++i) 
    {

    RoundTestHelperMacro( itk::Math::RoundHalfIntegerUp<T>, (float)input[i], halfupOutput[i] );

    RoundTestHelperMacro( itk::Math::RoundHalfIntegerUp<T>, (double)input[i], halfupOutput[i] );

    }



  // Floor
  for (unsigned int i = 0; i < numberOfElements; ++i) 
    {

    RoundTestHelperMacro( itk::Math::Floor<T>, (float)fcinput[i], floorOutput[i] );

    RoundTestHelperMacro( itk::Math::Floor<T>, (double)fcinput[i], floorOutput[i] );

    }
  
 

  // Ceil
  for (unsigned int i = 0; i < numberOfElements; ++i) 
    {

    RoundTestHelperMacro( itk::Math::Ceil<T>, (float)fcinput[i], ceilOutput[i] );

    RoundTestHelperMacro( itk::Math::Ceil<T>, (double)fcinput[i], ceilOutput[i] );

    }
  

  return ok;
}

}
int itkMathRoundTest2( int, char *[] )
{
  bool ok = true;

  std::cout << "Testing char type" << std::endl;
  ok &= TemplatedRoundTest<signed char>();
  std::cout << "Testing short type" << std::endl;
  ok &= TemplatedRoundTest<short>();
  std::cout << "Testing int type" << std::endl;
  ok &= TemplatedRoundTest<int>();
  std::cout << "Testing long type" << std::endl;
  ok &= TemplatedRoundTest<long>();
#ifdef ITK_HAS_INT_64
  std::cout << "Testing vxl_int_64 type" << std::endl;
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
