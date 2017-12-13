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

  std::cout << "Printing LightObject via operator: " << *light << std::endl;

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
