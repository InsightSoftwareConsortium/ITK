/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLightObjectTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "itkLightObject.h"

// Excersise the LightObject methods
int itkLightObjectTest(int, char* [] )
{
 

  typedef itk::LightObject    ObjectType;

  ObjectType::Pointer light = ObjectType::New();

  std::cout << "Light Object class name = ";
  std::cout << light->GetNameOfClass() << std::endl;

  std::cout << "sizeof(itk::LightObject) = " << sizeof(ObjectType) << std::endl;

  std::cout << "Printing LightObject: " << std::endl;
  light->Print( std::cout );
 
  std::cout << "Number of References counts: " << std::endl;
  const int counts1 = light->GetReferenceCount();
  std::cout << counts1 << std::endl;

  { // initialize scope for a SmartPointer
      ObjectType::Pointer secondreference = light;
      const int counts2 = light->GetReferenceCount();
      if( counts2 != counts1+1 )
        {
        std::cerr << "Problem in Reference counting increment" << std::endl;
        std::cout << "Test FAILED !" << std::endl;
        return EXIT_FAILURE;
        }
      else 
        {
        std::cout << "After assignment to another SmartPointer" << std::endl;
        std::cout << "reference count is:  " << counts2 << std::endl;
        }
  } // terminate the scope for the SmartPointer. Reference count should
    // decrement at this point.

  const int counts3 = light->GetReferenceCount();
  if( counts3 != counts1 )
    {
    std::cerr << "Problem in Reference counting decrement" << std::endl;
    std::cout << "Test FAILED !" << std::endl;
    return EXIT_FAILURE;
    }
  else 
    {
    std::cout << "After destroying one SmartPointer" << std::endl;
    std::cout << "reference count is:  " << counts3 << std::endl;
    }



  std::cout << "Test PASSED !" << std::endl;

  return EXIT_SUCCESS;
}



