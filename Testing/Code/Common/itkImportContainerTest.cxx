/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImportContainerTest.cxx
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

#include <iostream>
#include "itkImportImageContainer.h"
#include "itkNumericTraits.h"
#include "itkOutputWindow.h"
#include "itkTextOutput.h"

int itkImportContainerTest(int , char * [] )
{
  typedef float PixelType;
  typedef itk::ImportImageContainer<unsigned long, PixelType> ContainerType;

  itk::OutputWindow::SetInstance(itk::TextOutput::New());

// First test with ContainerManagesMemory false
  PixelType *ptr1, *ptr2;
  {
  // Test 1: Create an empty container and print it
  ContainerType::Pointer container1 = ContainerType::New();
  container1->ContainerManageMemoryOff();
  container1->Print(std::cout);
  std::cout << "After New(), size is "
            << container1->Size()
            << ", capacity is "
            <<  container1->Capacity()
            << " and import pointer is "
            << container1->GetImportPointer()
            << std::endl;


  // Test 2: Reserve memory
  container1->Reserve(1000);
  std::cout << "After container1->Reserve(1000), size is "
            << container1->Size()
            << ", capacity is "
            <<  container1->Capacity()
            << " and import pointer is "
            << container1->GetImportPointer()
            << std::endl;

  // Test 3: Reserve a smaller amount of memory
  container1->Reserve(100);
  std::cout << "After container1->Reserve(100), size is "
            << container1->Size()
            << ", capacity is "
            <<  container1->Capacity()
            << " and import pointer is "
            << container1->GetImportPointer()
            << std::endl;

  ptr1 = container1->GetImportPointer();

  // Test 4: Squeeze the container
  container1->Squeeze();
  std::cout << "After container1->Squeeze() size is "
            << container1->Size()
            << ", capacity is "
            <<  container1->Capacity()
            << " and import pointer is "
            << container1->GetImportPointer()
            << std::endl;

  ptr2 = container1->GetImportPointer();

  // Test 5: Initialize the container
  container1->Initialize();
  std::cout << "After container1->Initialize() size is "
            << container1->Size()
            << ", capacity is "
            <<  container1->Capacity()
            << " and import pointer is "
            << container1->GetImportPointer()
            << std::endl;
  }

// now repeat the tests with a user provided pointer
  PixelType *myPtr = new float[2000];
  {
  // Test 1: Create an empty container and print it
  ContainerType::Pointer container1 = ContainerType::New();
  container1->Print(std::cout);
  std::cout << "After New(), size is "
            << container1->Size()
            << ", capacity is "
            <<  container1->Capacity()
            << " and import pointer is "
            << container1->GetImportPointer()
            << std::endl;

  container1->SetImportPointer(myPtr, 2000);
  std::cout << "After New(), size is "
            << container1->Size()
            << ", capacity is "
            <<  container1->Capacity()
            << " and import pointer is "
            << container1->GetImportPointer()
            << std::endl;


  // Test 2: Reserve memory
  container1->Reserve(1000);
  std::cout << "After container1->Reserve(1000), size is "
            << container1->Size()
            << ", capacity is "
            <<  container1->Capacity()
            << " and import pointer is "
            << container1->GetImportPointer()
            << std::endl;

  // Test 3: Reserve a smaller amount of memory
  container1->Reserve(100);
  std::cout << "After container1->Reserve(100), size is "
            << container1->Size()
            << ", capacity is "
            <<  container1->Capacity()
            << " and import pointer is "
            << container1->GetImportPointer()
            << std::endl;

  ptr1 = container1->GetImportPointer();

  // Test 4: Squeeze the container
  container1->Squeeze();
  std::cout << "After container1->Squeeze() size is "
            << container1->Size()
            << ", capacity is "
            <<  container1->Capacity()
            << " and import pointer is "
            << container1->GetImportPointer()
            << std::endl;

  ptr2 = container1->GetImportPointer();

  // Test 5: Initialize the container
  container1->Initialize();
  std::cout << "After container1->Initialize() size is "
            << container1->Size()
            << ", capacity is "
            <<  container1->Capacity()
            << " and import pointer is "
            << container1->GetImportPointer()
            << std::endl;
  }

  // Now repeat tests with ContainerManagesMemory true
  {
  // Test 1: Create an empty container and print it
  ContainerType::Pointer container1 = ContainerType::New();
  container1->Print(std::cout);
  std::cout << "After New(), size is "
            << container1->Size()
            << ", capacity is "
            <<  container1->Capacity()
            << " and import pointer is "
            << container1->GetImportPointer()
            << std::endl;


  // Test 2: Reserve memory
  container1->Reserve(1000);
  std::cout << "After container1->Reserve(1000), size is "
            << container1->Size()
            << ", capacity is "
            <<  container1->Capacity()
            << " and import pointer is "
            << container1->GetImportPointer()
            << std::endl;

  // Test 3: Reserve a smaller amount of memory
  container1->Reserve(100);
  std::cout << "After container1->Reserve(100), size is "
            << container1->Size()
            << ", capacity is "
            <<  container1->Capacity()
            << " and import pointer is "
            << container1->GetImportPointer()
            << std::endl;

  // Test 4: Squeeze the container
  container1->Squeeze();
  std::cout << "After container1->Squeeze() size is "
            << container1->Size()
            << ", capacity is "
            <<  container1->Capacity()
            << " and import pointer is "
            << container1->GetImportPointer()
            << std::endl;

  // Test 5: Initialize the container
  container1->Initialize();
  std::cout << "After container1->Initialize() size is "
            << container1->Size()
            << ", capacity is "
            <<  container1->Capacity()
            << " and import pointer is "
            << container1->GetImportPointer()
            << std::endl;
  }

  // Check the exception on the memory allocation
  bool caughtException = false;
  try
    {
    ContainerType::Pointer container1 = ContainerType::New();
    container1->Reserve( itk::NumericTraits<unsigned long>::max()/sizeof(PixelType));
    }
  catch (itk::ExceptionObject& err)
    {
    std::cout << "Caught expected exception: " << err << std::endl;
    caughtException = true;
    }
  // We must delete the memory
  delete ptr1;
  delete ptr2;
  delete myPtr;

  if (!caughtException)
    {
    std::cout << "Failed to catch expected exception" << std::endl;
    return EXIT_FAILURE;   
    }
  return EXIT_SUCCESS;   
}
