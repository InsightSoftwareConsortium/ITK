
/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTestMain.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/


// This file is used to create TestDriver executables
// These executables are able to register a function pointer to a string name
// in a lookup table.   By including this file, it creates a main function
// that calls RegisterTests() then looks up the function pointer for the test
// specified on the command line.
#include "itkWin32Header.h"
#include <map>
#include <string>
#include <iostream>
#include "itkMultiThreader.h"

typedef int (*MainFuncPointer)(int , char**);
std::map<std::string, MainFuncPointer> StringToTestFunctionMap;

#define REGISTER_TEST(test) \
extern int test(int, char**); \
StringToTestFunctionMap[#test] = test

void RegisterTests();
void PrintAvailableTests()
{
  std::cout << "Available tests:\n";
  std::map<std::string, MainFuncPointer>::iterator j = StringToTestFunctionMap.begin();
  int i = 0;
  while(j != StringToTestFunctionMap.end())
    {
    std::cout << i << ". " << j->first << "\n";
    ++i;
    ++j;
    }
}

int main(int ac, char** av)
{
  RegisterTests();
  std::string testToRun;
  if(ac < 2)
    {
    PrintAvailableTests();
    std::cout << "To run a test, enter the test number: ";
    int testNum = 0;
    std::cin >> testNum;
    std::map<std::string, MainFuncPointer>::iterator j = StringToTestFunctionMap.begin();
    int i = 0;
    while(j != StringToTestFunctionMap.end() && i < testNum)
      {
      ++i;
      ++j;
      }
    if(j == StringToTestFunctionMap.end())
      {
      std::cerr << testNum << " is an invalid test number\n";
      return -1;
      }
    testToRun = j->first;
    }
  else
    {
    if (strcmp(av[1], "--with-threads") == 0)
      {
      int numThreads = atoi(av[2]);
      itk::MultiThreader::SetGlobalDefaultNumberOfThreads(numThreads);
      av += 2;
      ac -= 2;
      }
    else if (strcmp(av[1], "--without-threads") == 0)
      {
      itk::MultiThreader::SetGlobalDefaultNumberOfThreads(1);
      av += 1;
      ac -= 1;
      }
    testToRun = av[1];
    }
  std::map<std::string, MainFuncPointer>::iterator j = StringToTestFunctionMap.find(testToRun);
  if(j != StringToTestFunctionMap.end())
    {
    MainFuncPointer f = j->second;
    int result;
    try
      {
      // Invoke the test's "main" function.
      result = (*f)(ac-1, av+1);
      }
    catch(const itk::ExceptionObject& e)
      {
      std::cerr << "ITK test driver caught an ITK exception:\n";
      std::cerr << e.GetFile() << ":" << e.GetLine() << ":\n"
                << e.GetDescription() << "\n";
      result = -1;
      }
    catch(const std::exception& e)
      {
      std::cerr << "ITK test driver caught an exception:\n";
      std::cerr << e.what() << "\n";
      result = -1;
      }
    catch(...)
      {
      std::cerr << "ITK test driver caught an unknown exception!!!\n";
      result = -1;
      }
    return result;
    }
  PrintAvailableTests();
  std::cerr << "Failed: " << testToRun << ": No test registered with name " << testToRun << "\n";
  return -1;
}

