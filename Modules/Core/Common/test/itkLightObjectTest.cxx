/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
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
int
itkLightObjectTest(int, char *[])
{


  using ObjectType = itk::LightObject;

  auto light = ObjectType::New();

  std::cout << "Light Object class name = ";
  std::cout << light->GetNameOfClass() << '\n';

  std::cout << "sizeof(itk::LightObject) = " << sizeof(ObjectType) << '\n';

  std::cout << "Printing LightObject: " << '\n';
  light->Print(std::cout);

  std::cout << "Printing LightObject via operator: " << *light << '\n';

  std::cout << "Number of References counts: " << '\n';
  const int counts1 = light->GetReferenceCount();
  std::cout << counts1 << '\n';

  { // initialize scope for a SmartPointer
    const ObjectType::Pointer secondreference = light;
    const int                 counts2 = light->GetReferenceCount();
    if (counts2 != counts1 + 1)
    {
      std::cerr << "Problem in Reference counting increment" << '\n';
      std::cout << "Test FAILED !" << '\n';
      return EXIT_FAILURE;
    }
    else
    {
      std::cout << "After assignment to another SmartPointer" << '\n';
      std::cout << "reference count is:  " << counts2 << '\n';
    }
  } // terminate the scope for the SmartPointer. Reference count should
    // decrement at this point.

  const int counts3 = light->GetReferenceCount();
  if (counts3 != counts1)
  {
    std::cerr << "Problem in Reference counting decrement" << '\n';
    std::cout << "Test FAILED !" << '\n';
    return EXIT_FAILURE;
  }
  else
  {
    std::cout << "After destroying one SmartPointer" << '\n';
    std::cout << "reference count is:  " << counts3 << '\n';
  }

  std::cout << "Test PASSED !" << '\n';

  return EXIT_SUCCESS;
}
