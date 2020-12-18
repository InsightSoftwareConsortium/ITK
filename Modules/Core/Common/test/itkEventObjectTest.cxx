/*=========================================================================
 *
 *  Copyright NumFOCUS
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

#include "itkEventObject.h"
#include <iostream>

namespace itk
{
itkEventMacroDeclaration(TestEvent, UserEvent);
itkEventMacroDefinition(TestEvent, UserEvent);
itkEventMacroDeclaration(TestDerivedEvent, TestEvent);
itkEventMacroDefinition(TestDerivedEvent, TestEvent);
itkEventMacroDeclaration(TestOtherEvent, AnyEvent);
itkEventMacroDefinition(TestOtherEvent, AnyEvent);
} // namespace itk


int
itkEventObjectTest(int, char *[])
{

  // test constructor
  itk::TestEvent event;

  itk::TestDerivedEvent derivedEvent;


  // test if the event derives
  if (!event.CheckEvent(&derivedEvent))
  {
    std::cerr << "Derivation test failed " << std::endl;
    return EXIT_FAILURE;
  }

  itk::TestEvent event2;
  // test if the event matches itself
  if (!event.CheckEvent(&event2))
  {
    std::cerr << "Same class test failed " << std::endl;
    return EXIT_FAILURE;
  }


  itk::TestOtherEvent otherEvent;
  // test that it doesn't match and unrelated event
  if (event.CheckEvent(&otherEvent))
  {
    std::cerr << "Error: matched unrelated event" << std::endl;
    return EXIT_FAILURE;
  }


  // exercise the PrintSelf() method by calling Print()
  event.Print(std::cout);

  // exercise the GetEventName() method
  std::cout << event.GetEventName() << std::endl;

  // exercise the shift operator
  std::cout << event << std::endl;

  return EXIT_SUCCESS;
}
