
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


typedef int (*MainFuncPointer)(int , char**);
std::map<std::string, MainFuncPointer> StringToTestFunctionMap;

#define REGISTER_TEST(test) \
extern int test(int, char**); \
StringToTestFunctionMap[#test] = test

void RegisterTests();
void PrintAvailableTests()
{
  std::cout << "Availiable tests:\n";
  std::map<std::string, MainFuncPointer>::iterator j = StringToTestFunctionMap.begin();
  while(j != StringToTestFunctionMap.end())
    {
    std::cout << j->first << "\n";
    ++j;
    }

}
int main(int ac, char** av)
{
  if(ac < 2)
    {
    PrintAvailableTests();
    std::cerr << "Usage: " << av[0] << " TestName\n";
    return -1;
    }
  RegisterTests();
  std::map<std::string, MainFuncPointer>::iterator j = StringToTestFunctionMap.find(av[1]);
  if(j != StringToTestFunctionMap.end())
    {
    MainFuncPointer f = j->second;
    return (*f)(ac-1, av+1);
    }
  PrintAvailableTests();
  std::cerr << "Failed: " << av[1] << ": No test registered with name " << av[1] << "\n";
  return -1;
}

