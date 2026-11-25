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

//
// This test is for testing the Command Observer functionality of the
// itk::Object class.
//

// First include the header file to be tested:
#include "itkCommand.h"
#include "itkObject.h"
#include "itkGTest.h"
#include <iostream>


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
  const unsigned long idToRemove = *static_cast<unsigned long *>(data);
  o->RemoveObserver(idToRemove);
}

} // namespace


TEST(CommandObserverObject, DeleteObserverDuringEvent)
{
  const itk::Object::Pointer o = itk::Object::New();

  const itk::CStyleCommand::Pointer removeCmd = itk::CStyleCommand::New();
  removeCmd->SetCallback(onUserRemove);
  removeCmd->SetObjectName("Remove Command");

  const itk::CStyleCommand::Pointer cmd = itk::CStyleCommand::New();
  cmd->SetCallback(onAny);
  cmd->SetObjectName("Any Command 1");

  // Add Order 1
  o->AddObserver(itk::UserEvent(), removeCmd);
  unsigned long idToRemove = o->AddObserver(itk::AnyEvent(), cmd);
  o->AddObserver(itk::AnyEvent(), cmd);
  removeCmd->SetClientData(&idToRemove);

  onAnyCount = 0;
  o->InvokeEvent(itk::AnyEvent());
  EXPECT_EQ(onAnyCount, 2u);

  onAnyCount = 0;
  o->InvokeEvent(itk::UserEvent());
  EXPECT_EQ(onAnyCount, 1u);


  onAnyCount = 0;
  o->InvokeEvent(itk::AnyEvent());
  EXPECT_EQ(onAnyCount, 1u);

  o->RemoveAllObservers();

  // Add Order 2
  idToRemove = o->AddObserver(itk::AnyEvent(), cmd);
  o->AddObserver(itk::UserEvent(), removeCmd);
  o->AddObserver(itk::AnyEvent(), cmd);
  removeCmd->SetClientData(&idToRemove);

  onAnyCount = 0;
  o->InvokeEvent(itk::AnyEvent());
  EXPECT_EQ(onAnyCount, 2u);

  onAnyCount = 0;
  o->InvokeEvent(itk::UserEvent());
  EXPECT_EQ(onAnyCount, 2u);


  onAnyCount = 0;
  o->InvokeEvent(itk::AnyEvent());
  EXPECT_EQ(onAnyCount, 1u);

  o->RemoveAllObservers();

  // Add Order 3
  idToRemove = o->AddObserver(itk::AnyEvent(), cmd);
  o->AddObserver(itk::AnyEvent(), cmd);
  o->AddObserver(itk::UserEvent(), removeCmd);
  removeCmd->SetClientData(&idToRemove);

  onAnyCount = 0;
  o->InvokeEvent(itk::AnyEvent());
  EXPECT_EQ(onAnyCount, 2u);

  onAnyCount = 0;
  o->InvokeEvent(itk::UserEvent());
  EXPECT_EQ(onAnyCount, 2u);


  onAnyCount = 0;
  o->InvokeEvent(itk::AnyEvent());
  EXPECT_EQ(onAnyCount, 1u);

  o->RemoveAllObservers();

  // Add Order 4
  o->AddObserver(itk::AnyEvent(), cmd);
  o->AddObserver(itk::UserEvent(), removeCmd);
  idToRemove = o->AddObserver(itk::AnyEvent(), cmd);
  removeCmd->SetClientData(&idToRemove);

  onAnyCount = 0;
  o->InvokeEvent(itk::AnyEvent());
  EXPECT_EQ(onAnyCount, 2u);

  onAnyCount = 0;
  o->InvokeEvent(itk::UserEvent());
  EXPECT_EQ(onAnyCount, 1u);


  onAnyCount = 0;
  o->InvokeEvent(itk::AnyEvent());
  EXPECT_EQ(onAnyCount, 1u);

  o->RemoveAllObservers();
}


TEST(CommandObserverObject, CommandConstObject)
{

  const itk::Object::Pointer      o = itk::Object::New();
  const itk::Object::ConstPointer co = o;

  const itk::CStyleCommand::Pointer cmd = itk::CStyleCommand::New();
  cmd->SetConstCallback(onAnyConst);
  cmd->SetObjectName("Any Command 1");

  const itk::CStyleCommand::Pointer removeCmd = itk::CStyleCommand::New();
  removeCmd->SetCallback(onUserRemove);
  removeCmd->SetObjectName("Remove Command");

  co->AddObserver(itk::AnyEvent(), cmd);
  EXPECT_TRUE(co->HasObserver(itk::AnyEvent()));

  // the constant command doesn't get executed from the non-const
  // invocation
  onAnyCount = 0;
  o->InvokeEvent(itk::AnyEvent());
  EXPECT_EQ(onAnyCount, 0u);

  onAnyCount = 0;
  co->InvokeEvent(itk::AnyEvent());
  EXPECT_EQ(onAnyCount, 1u);
}


TEST(CommandObserverObject, CommandRecursiveObject)
{
  // this test has an command invoking another event, while removing a
  // a Command.
  // This is a super-mean test that is not likely to really be used.

  const itk::Object::Pointer      o = itk::Object::New();
  const itk::Object::ConstPointer co = o;

  const itk::CStyleCommand::Pointer cmd = itk::CStyleCommand::New();
  cmd->SetCallback(onAny);
  cmd->SetObjectName("Any Command 1");

  const itk::CStyleCommand::Pointer removeCmd = itk::CStyleCommand::New();
  removeCmd->SetCallback(onUserRemove);
  removeCmd->SetObjectName("Remove Command");

  const itk::CStyleCommand::Pointer cmdInvoke = itk::CStyleCommand::New();
  cmdInvoke->SetCallback(onAnyInvokeUser);
  cmdInvoke->SetObjectName("Any Invoke User");

  // On 1 Remove 1
  unsigned long idToRemove = o->AddObserver(itk::AnyEvent(), cmdInvoke);
  o->AddObserver(itk::UserEvent(), removeCmd);
  o->AddObserver(itk::AnyEvent(), cmd);
  removeCmd->SetClientData(&idToRemove);

  onAnyCount = 0;
  o->InvokeEvent(itk::AnyEvent());
  EXPECT_EQ(onAnyCount, 2u);

  o->RemoveAllObservers();

  // On 1 Remove 2
  o->AddObserver(itk::AnyEvent(), cmdInvoke);
  idToRemove = o->AddObserver(itk::UserEvent(), removeCmd);
  o->AddObserver(itk::AnyEvent(), cmd);
  removeCmd->SetClientData(&idToRemove);

  onAnyCount = 0;
  o->InvokeEvent(itk::AnyEvent());
  EXPECT_EQ(onAnyCount, 2u);

  o->RemoveAllObservers();

  // On 1 Remove 3
  o->AddObserver(itk::AnyEvent(), cmdInvoke);
  o->AddObserver(itk::UserEvent(), removeCmd);
  idToRemove = o->AddObserver(itk::AnyEvent(), cmd);
  removeCmd->SetClientData(&idToRemove);

  onAnyCount = 0;
  o->InvokeEvent(itk::AnyEvent());
  EXPECT_EQ(onAnyCount, 0u);
}


TEST(CommandObserverObject, DeleteEventThrow)
{
  // check the case where an exception in thrown in the DeleteEvent
  const itk::Object::Pointer o = itk::Object::New();

  const itk::CStyleCommand::Pointer cmd = itk::CStyleCommand::New();
  cmd->SetCallback(onAnyThrow);

  o->AddObserver(itk::DeleteEvent(), cmd);
}


TEST(CommandObserverObject, LambdaCommand)
{
  // spell-check-disable
  // NOTE: cnt needs to be defined BEFORE "o" because it MUST exist when the "DeleteEvent()" is causes the
  //      FIRST OBSERVER LAMBDA to be called.  If 'cnt' is defined after 'o' then when the scope
  //      ends, 'cnt' is deleted first, followed by deleting 'o' which tries to increment 'cnt' when
  //      the 'DeleteEvent' tries to be processed.
  // spell-check-enable
  int cnt = 0;
  int name_of_class_cnt = 0;

  {
    // check the case where an exception in thrown in the DeleteEvent
    const itk::Object::Pointer o = itk::Object::New();
    /*----- FIRST OBSERVER LAMBDA */
    o->AddObserver(itk::AnyEvent(), [&cnt](const itk::EventObject &) { ++cnt; });

    auto & objRef = *o.GetPointer();
    /*----- SECOND OBSERVER LAMBDA */
    o->AddObserver(itk::AnyEvent(), [&objRef, &name_of_class_cnt](const itk::EventObject & event) {
      ++name_of_class_cnt;
      std::cout << "Invocation # " << name_of_class_cnt << "\nObject: " << objRef.GetNameOfClass()
                << " Event: " << event << std::endl;
    });

    o->InvokeEvent(itk::AnyEvent());
    EXPECT_EQ(1, cnt);
    EXPECT_EQ(1, name_of_class_cnt);

  } // A DeleteEvent is called here! as object "o" is deleted
  EXPECT_EQ(2, cnt); // Verify that cnt really was incremented during DeleteEvent!
  EXPECT_EQ(2, name_of_class_cnt);
}
