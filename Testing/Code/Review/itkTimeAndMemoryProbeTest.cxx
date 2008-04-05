/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkTimeAndMemoryProbeTest.cxx
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) Insight Software Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "itkTimeProbe.h"
#include "itkMemoryProbesCollectorBase.h"

#include <iostream>

int itkTimeAndMemoryProbeTest(int argc,char* argv[])
{
  (void)argc;
  (void)argv;

  itk::TimeProbe timeProbe;
  itk::MemoryProbesCollectorBase memoryProbes;

  timeProbe.Start();

  memoryProbes.Start("1KB Test");
  char* foo = new char[1*1024];
  memoryProbes.Stop("1KB Test");

  memoryProbes.Start("70KB Test");
  char* foo2 = new char[70*1024];
  memoryProbes.Stop("70KB Test");

  memoryProbes.Start("170KB Test");
  char* foo3 = new char[170*1024];
  memoryProbes.Stop("170KB Test");

  memoryProbes.Start("1024KB Test");
  char* foo4 = new char[1024*1024];
  memoryProbes.Stop("1024KB Test");

  memoryProbes.Report( std::cout );
  memoryProbes.Clear();

  memoryProbes.Start("Release Test");
  delete[] foo;
  delete[] foo2;
  delete[] foo3;
  delete[] foo4;
  memoryProbes.Stop("Release Test");

  memoryProbes.Report( std::cout );

  timeProbe.Stop();
  std::cout<<" The test has last "<<timeProbe.GetMean()<<timeProbe.GetUnit()<<std::endl;

  return EXIT_SUCCESS;
}
