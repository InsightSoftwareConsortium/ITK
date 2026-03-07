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

#include "itkEventObject.h"
#include "itkGTest.h"

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

TEST(EventObject, EventDerivationCheck)
{
  // test constructor
  const itk::TestEvent  event;
  itk::TestDerivedEvent derivedEvent;

  // A base event should match a derived event
  EXPECT_TRUE(event.CheckEvent(&derivedEvent));

  // An event should match itself
  itk::TestEvent event2;
  EXPECT_TRUE(event.CheckEvent(&event2));

  // An event should not match an unrelated event
  itk::TestOtherEvent otherEvent;
  EXPECT_FALSE(event.CheckEvent(&otherEvent));
}

TEST(EventObject, PrintAndName)
{
  const itk::TestEvent event;

  // exercise the PrintSelf() method by calling Print()
  event.Print(std::cout);

  // exercise the GetEventName() method
  const char * name = event.GetEventName();
  std::cout << name << std::endl;
  EXPECT_STREQ(name, "TestEvent");

  // exercise the shift operator
  std::cout << event << std::endl;
}
