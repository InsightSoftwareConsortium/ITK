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
  ArrayType output;
  ArrayType output2;

  const unsigned int numberOfValues = 1000;

  const double initialValue = -5.0;

  const double valueIncrement = (-initialValue-initialValue) / numberOfValues;

  for( unsigned int i=0; i<numberOfValues; i++)
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
    output.push_back( outputValue );
    }

  output2.resize( output.size() );


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
  
  return EXIT_SUCCESS;
}
