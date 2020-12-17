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

#include <iostream>
#include "itkObject.h"

#include "itkCommand.h"
#include "itkTestingMacros.h"


//
// This test is for testing the Command Observer functionality of the
// itk::Object class.
//


namespace
{

unsigned int onAnyCount = 0;

void
onAny(itk::Object *, const itk::EventObject &, void *)
{
  ++onAnyCount;
}

void
onAnyInvokeUser(itk::Object * o, const itk::EventObject &, void *)
{
  static bool invoke = true;
  if (invoke)
  {
    invoke = false;
    o->InvokeEvent(itk::UserEvent());
    invoke = true;
  }
}

void
onAnyConst(const itk::Object *, const itk::EventObject &, void *)
{
  ++onAnyCount;
}

void
onAnyThrow(itk::Object *, const itk::EventObject &, void *)
{
  throw;
}

void
onUserRemove(itk::Object * o, const itk::EventObject &, void * data)
{
  unsigned long idToRemove = *static_cast<unsigned long *>(data);
  o->RemoveObserver(idToRemove);
}

int
testDeleteObserverDuringEvent()
{
  itk::Object::Pointer o = itk::Object::New();


  unsigned long idToRemove;

  itk::CStyleCommand::Pointer removeCmd = itk::CStyleCommand::New();
  removeCmd->SetCallback(onUserRemove);
  removeCmd->SetObjectName("Remove Command");

  itk::CStyleCommand::Pointer cmd = itk::CStyleCommand::New();
  cmd->SetCallback(onAny);
  cmd->SetObjectName("Any Command 1");

  // Add Order 1
  o->AddObserver(itk::UserEvent(), removeCmd);
  idToRemove = o->AddObserver(itk::AnyEvent(), cmd);
  o->AddObserver(itk::AnyEvent(), cmd);
  removeCmd->SetClientData(&idToRemove);

  onAnyCount = 0;
  o->InvokeEvent(itk::AnyEvent());
  ITK_TEST_EXPECT_TRUE(onAnyCount == 2);

  onAnyCount = 0;
  o->InvokeEvent(itk::UserEvent());
  ITK_TEST_EXPECT_TRUE(onAnyCount == 1);


  onAnyCount = 0;
  o->InvokeEvent(itk::AnyEvent());
  ITK_TEST_EXPECT_TRUE(onAnyCount == 1);

  o->RemoveAllObservers();

  // Add Order 2
  idToRemove = o->AddObserver(itk::AnyEvent(), cmd);
  o->AddObserver(itk::UserEvent(), removeCmd);
  o->AddObserver(itk::AnyEvent(), cmd);
  removeCmd->SetClientData(&idToRemove);

  onAnyCount = 0;
  o->InvokeEvent(itk::AnyEvent());
  ITK_TEST_EXPECT_TRUE(onAnyCount == 2);

  onAnyCount = 0;
  o->InvokeEvent(itk::UserEvent());
  ITK_TEST_EXPECT_TRUE(onAnyCount == 2);


  onAnyCount = 0;
  o->InvokeEvent(itk::AnyEvent());
  ITK_TEST_EXPECT_TRUE(onAnyCount == 1);

  o->RemoveAllObservers();

  // Add Order 3
  idToRemove = o->AddObserver(itk::AnyEvent(), cmd);
  o->AddObserver(itk::AnyEvent(), cmd);
  o->AddObserver(itk::UserEvent(), removeCmd);
  removeCmd->SetClientData(&idToRemove);

  onAnyCount = 0;
  o->InvokeEvent(itk::AnyEvent());
  ITK_TEST_EXPECT_TRUE(onAnyCount == 2);

  onAnyCount = 0;
  o->InvokeEvent(itk::UserEvent());
  ITK_TEST_EXPECT_TRUE(onAnyCount == 2);


  onAnyCount = 0;
  o->InvokeEvent(itk::AnyEvent());
  ITK_TEST_EXPECT_TRUE(onAnyCount == 1);

  o->RemoveAllObservers();

  // Add Order 4
  o->AddObserver(itk::AnyEvent(), cmd);
  o->AddObserver(itk::UserEvent(), removeCmd);
  idToRemove = o->AddObserver(itk::AnyEvent(), cmd);
  removeCmd->SetClientData(&idToRemove);

  onAnyCount = 0;
  o->InvokeEvent(itk::AnyEvent());
  ITK_TEST_EXPECT_TRUE(onAnyCount == 2);

  onAnyCount = 0;
  o->InvokeEvent(itk::UserEvent());
  ITK_TEST_EXPECT_TRUE(onAnyCount == 1);


  onAnyCount = 0;
  o->InvokeEvent(itk::AnyEvent());
  ITK_TEST_EXPECT_TRUE(onAnyCount == 1);

  o->RemoveAllObservers();


  return EXIT_SUCCESS;
}


int
testCommandConstObject()
{

  itk::Object::Pointer      o = itk::Object::New();
  itk::Object::ConstPointer co = o;

  itk::CStyleCommand::Pointer cmd = itk::CStyleCommand::New();
  cmd->SetConstCallback(onAnyConst);
  cmd->SetObjectName("Any Command 1");

  itk::CStyleCommand::Pointer removeCmd = itk::CStyleCommand::New();
  removeCmd->SetCallback(onUserRemove);
  removeCmd->SetObjectName("Remove Command");

  co->AddObserver(itk::AnyEvent(), cmd);
  ITK_TEST_EXPECT_TRUE(co->HasObserver(itk::AnyEvent()));

  // the constant command doesn't get executed from the non-const
  // invocation
  onAnyCount = 0;
  o->InvokeEvent(itk::AnyEvent());
  ITK_TEST_EXPECT_TRUE(onAnyCount == 0);

  onAnyCount = 0;
  co->InvokeEvent(itk::AnyEvent());
  ITK_TEST_EXPECT_TRUE(onAnyCount == 1);

  return EXIT_SUCCESS;
}


int
testCommandRecursiveObject()
{
  // this test has an command invoking another event, while removing a
  // a Command.
  // This is a super-mean test that is not likely to really be used.

  itk::Object::Pointer      o = itk::Object::New();
  itk::Object::ConstPointer co = o;

  unsigned long idToRemove;

  itk::CStyleCommand::Pointer cmd = itk::CStyleCommand::New();
  cmd->SetCallback(onAny);
  cmd->SetObjectName("Any Command 1");

  itk::CStyleCommand::Pointer removeCmd = itk::CStyleCommand::New();
  removeCmd->SetCallback(onUserRemove);
  removeCmd->SetObjectName("Remove Command");

  itk::CStyleCommand::Pointer cmdInvoke = itk::CStyleCommand::New();
  cmdInvoke->SetCallback(onAnyInvokeUser);
  cmdInvoke->SetObjectName("Any Invoke User");

  // On 1 Remove 1
  idToRemove = o->AddObserver(itk::AnyEvent(), cmdInvoke);
  o->AddObserver(itk::UserEvent(), removeCmd);
  o->AddObserver(itk::AnyEvent(), cmd);
  removeCmd->SetClientData(&idToRemove);

  onAnyCount = 0;
  o->InvokeEvent(itk::AnyEvent());
  ITK_TEST_EXPECT_TRUE(onAnyCount == 2);

  o->RemoveAllObservers();

  // On 1 Remove 2
  o->AddObserver(itk::AnyEvent(), cmdInvoke);
  idToRemove = o->AddObserver(itk::UserEvent(), removeCmd);
  o->AddObserver(itk::AnyEvent(), cmd);
  removeCmd->SetClientData(&idToRemove);

  onAnyCount = 0;
  o->InvokeEvent(itk::AnyEvent());
  ITK_TEST_EXPECT_TRUE(onAnyCount == 2);

  o->RemoveAllObservers();

  // On 1 Remove 3
  o->AddObserver(itk::AnyEvent(), cmdInvoke);
  o->AddObserver(itk::UserEvent(), removeCmd);
  idToRemove = o->AddObserver(itk::AnyEvent(), cmd);
  removeCmd->SetClientData(&idToRemove);

  onAnyCount = 0;
  o->InvokeEvent(itk::AnyEvent());
  ITK_TEST_EXPECT_TRUE(onAnyCount == 0);

  return EXIT_SUCCESS;
}


bool
testDeleteEventThrow()
{
  // check the case where an exception in thrown in the DeleteEvent
  itk::Object::Pointer o = itk::Object::New();

  itk::CStyleCommand::Pointer cmd = itk::CStyleCommand::New();
  cmd->SetCallback(onAnyThrow);

  o->AddObserver(itk::DeleteEvent(), cmd);
  return EXIT_SUCCESS;
}

int
testLambdaCommand()
{
  // NOTE: cnt needs to be defined BEFORE "o" because it MUST exist when the "DeleteEvent()" is causes the
  //      FIRST OBSERVER LAMBDA to be called.  If 'cnt' is defined after 'o' then when the scope
  //      ends, 'cnt' is deleted first, followed by deleting 'o' which tries to increment 'cnt' when
  //      the 'DeleteEvent' tries to be processed.
  int cnt = 0;
  int name_of_class_cnt = 0;

  {
    // check the case where an exception in thrown in the DeleteEvent
    itk::Object::Pointer o = itk::Object::New();
    /*----- FIRST OBSERVER LAMBDA */
    o->AddObserver(itk::AnyEvent(), [&cnt](const itk::EventObject &) { ++cnt; });

    auto & objRef = *o.GetPointer();
    /*----- SECOND OBSERVER LAMBDA */
    o->AddObserver(itk::AnyEvent(), [&objRef, &name_of_class_cnt](const itk::EventObject & event) {
      ++name_of_class_cnt;
      std::cout << "Ivocation # " << name_of_class_cnt << "\nObject: " << objRef.GetNameOfClass() << " Event: " << event
                << std::endl;
    });

    o->InvokeEvent(itk::AnyEvent());
    ITK_TEST_EXPECT_EQUAL(1, cnt);
    ITK_TEST_EXPECT_EQUAL(1, name_of_class_cnt);

  }                              // A DeleteEvent is called here! as object "o" is deleted
  ITK_TEST_EXPECT_EQUAL(2, cnt); // Verify that cnt really was incremented during DeleteEvent!
  ITK_TEST_EXPECT_EQUAL(2, name_of_class_cnt);
  return EXIT_SUCCESS;
}


} // end namespace


int
itkCommandObserverObjectTest(int, char *[])
{
  bool ret = true;

  ret &= (testDeleteObserverDuringEvent() == EXIT_SUCCESS);
  ret &= (testCommandConstObject() == EXIT_SUCCESS);
  ret &= (testCommandRecursiveObject() == EXIT_SUCCESS);
  ret &= (testDeleteEventThrow() == EXIT_SUCCESS);
  ret &= (testLambdaCommand() == EXIT_SUCCESS);

  return ret ? EXIT_SUCCESS : EXIT_FAILURE;
}
