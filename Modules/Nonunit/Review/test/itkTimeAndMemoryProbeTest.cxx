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
