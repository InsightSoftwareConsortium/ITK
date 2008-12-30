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


  ArrayType::const_iterator  inpItr   = input.begin();
  ArrayType::const_iterator  inputEnd = input.end();

  ArrayType::iterator        outItr = output2.begin();

  itk::TimeProbesCollectorBase  chronometer;
  chronometer.Start("vnl_round");

  while( inpItr != inputEnd )
    {
    *outItr = vnl_math_rnd( *inpItr ); 
    ++outItr;
    ++inpItr;
    }

  chronometer.Stop("vnl_round");

  chronometer.Report( std::cout );

  //
  // Now test the correctness of the output
  //
  inpItr   = input.begin();

  ArrayType::const_iterator  outItr1 = output1.begin();
  ArrayType::const_iterator  outItr2 = output2.begin();

  const double tolerance = 1e-5;

  while( inpItr != inputEnd )
    {
    if( vnl_math_abs( *outItr1 - *outItr2 ) > tolerance )
      {
      std::cerr << "Error in : " << *inpItr << " : " << *outItr1 << " : " << *outItr2 << std::endl;
        return EXIT_FAILURE;
      }
    ++inpItr;
    ++outItr1;
    ++outItr2;
    }
  
  return EXIT_SUCCESS;
}
