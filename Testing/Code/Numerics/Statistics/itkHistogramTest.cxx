/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkHistogramTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "itkHistogram.h"



int itkHistogramTest(int, char**) 
{
  std::cout << "Histogram Test \n \n"; 
  
  typedef  float      MeasureType;
  const unsigned int  NumberOfBins = 100;

  typedef  itk::Statistics::Histogram< MeasureType,
                                       NumberOfBins   >   HistogramType;


  bool pass = true;

  if( !pass )
    {
    std::cout << "Test failed." << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;


}



