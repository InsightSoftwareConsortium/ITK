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

#include "itkFileOutputWindow.h"

#include <iostream>


int itkFileOutputWindowTest(int, char* [] )
{

  // Declare the type for the morphology Filter
  typedef itk::FileOutputWindow myFileOutputWindow;

  // Create the filter
  myFileOutputWindow::Pointer window = myFileOutputWindow::New();

  // Test itkSetStringMacro
  const char * fileName = "";
  window->SetFileName(fileName);

  // Test itkGetStringMacro
  const char * fileName2 = window->GetFileName();
  std::cout << "window->GetFileName(): " << fileName2 << std::endl;

  // Test itkSetMacros
  const bool flush = false;
  window->SetFlush(flush);
  const bool append = false;
  window->SetAppend(append);

  // Test itkGetMacros
  bool flush2 = window->GetFlush();
  std::cout << "window->GetFlush(): " << flush2 << std::endl;
  bool append2 = window->GetAppend();
  std::cout << "window->GetAppend(): " << append2 << std::endl;

  // Test itkBooleanMacros
  window->FlushOn();
  window->AppendOn();

  // Test

  return EXIT_SUCCESS;
}
