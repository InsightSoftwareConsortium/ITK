/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTimeProbesTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "itkTimeProbesCollectorBase.h"
#include <iostream>

//-------------------------
//
//   This file test the interface to the TimeProbe classes
//
//-------------------------
int itkTimeProbesTest(int, char* [] ) 
{
  
  std::cout << "clock() precision = " << CLOCKS_PER_SEC;
  std::cout << "  ticks per second " << std::endl;
  std::cout << std::endl;
  
  itk::TimeProbesCollectorBase   collector;

  const unsigned int N =  1000L;
  const unsigned int M = 10000L;

  collector.Start("Loop1"); // label that identify the range of the probe

  // Do slow stuff here...
  {
  for(unsigned int i=0; i<N; i++)
    {
    char * dummy = new char [ M ];
    for(unsigned int j=0; j<M; j++)
      {
      dummy[j] = j; 
      }
    delete [] dummy; 
    }
  }
  collector.Stop("Loop1");



  collector.Start("Loop2"); // label that identify the range of the probe

  // Do other slow stuff here...
  {
  for(unsigned int i=0; i<M; i++)
    {
    char * dummy = new char [ N ];
    for(unsigned int j=0; j<N; j++)
      {
      dummy[j] = j; 
      }
    delete [] dummy; 
    }
  }
  collector.Stop("Loop2");


  // Print the results of the time probes
  collector.Report();

  return EXIT_SUCCESS;

}

