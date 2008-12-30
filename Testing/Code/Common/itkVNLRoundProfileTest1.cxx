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

int itkVNLRoundProfileTest1( int, char *[] )
{
  itk::TimeProbesCollectorBase  chronometer;

  typedef std::vector< double >   ArrayType;

  ArrayType input;
  ArrayType output1;
  ArrayType output2;

  const unsigned long numberOfValues = 100000L;

  const double initialValue = -5.0;

  const double valueIncrement = (-initialValue-initialValue) / numberOfValues;

  for( unsigned long i=0; i<numberOfValues; i++)
    {
    const double inputValue = initialValue + i * valueIncrement;
    double outputValue;

    if( inputValue >= 0.0 )
      {
      outputValue = static_cast< int >( inputValue + 0.5f );
      }
    else
      {
      outputValue = static_cast< int >( inputValue - 0.5f );
      } 

    input.push_back( inputValue );
    output1.push_back( outputValue );
    }

  output2.resize( output1.size() );

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

  chronometer.Report( std::cout );

  //
  // Now test the correctness of the output
  //
  inpItr   = input.begin();

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
