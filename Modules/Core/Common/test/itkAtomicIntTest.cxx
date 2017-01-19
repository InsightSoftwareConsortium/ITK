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
/*=========================================================================

  Program:   Visualization Toolkit
  Module:    TestAtomic.cxx

  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
  All rights reserved.
  See Copyright.txt or http://www.kitware.com/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
#include "itkAtomicInt.h"
#include "itkMultiThreader.h"
#include "itkObject.h"
#include <iostream>

namespace
{

int Total = 0;
itk::uint64_t Total64 = 0;
itk::AtomicInt<itk::uint32_t> TotalAtomic(0);
itk::AtomicInt<itk::uint64_t> TotalAtomic64(0);
const int Target = 1000000;
itk::int32_t Values32[Target+2];
itk::int64_t Values64[Target+2];
itk::AtomicInt<itk::int32_t*> AtomicPtr(Values32);
itk::AtomicInt<itk::int64_t*> Atomic64Ptr(Values64);

int NumThreads = 5;

itk::Object::Pointer AnObject;
}


ITK_THREAD_RETURN_TYPE MyFunction(void *)
{
  for (int i=0; i<Target/NumThreads; i++)
    {
    Total++;
    int idx = ++TotalAtomic;
    Values32[idx] = 1;

    Total64++;
    idx = ++TotalAtomic64;
    Values64[idx] = 1;

    AnObject->Modified();
    }

  return ITK_THREAD_RETURN_VALUE;
}

ITK_THREAD_RETURN_TYPE MyFunction2(void *)
{
  for (int i=0; i<Target/NumThreads; i++)
    {
    --TotalAtomic;

    --TotalAtomic64;
    }

  return ITK_THREAD_RETURN_VALUE;
}

ITK_THREAD_RETURN_TYPE MyFunction3(void *)
{
  for (int i=0; i<Target/NumThreads; i++)
    {
    int idx = TotalAtomic += 1;
    Values32[idx]++;

    idx = TotalAtomic64 += 1;
    Values64[idx]++;
    }

  return ITK_THREAD_RETURN_VALUE;
}

ITK_THREAD_RETURN_TYPE MyFunction4(void *)
{
  for (int i=0; i<Target/NumThreads; i++)
    {
    TotalAtomic++;
    TotalAtomic += 1;
    TotalAtomic--;
    TotalAtomic -= 1;

    TotalAtomic64++;
    TotalAtomic64 += 1;
    TotalAtomic64--;
    TotalAtomic64 -= 1;
    }

  return ITK_THREAD_RETURN_VALUE;
}

ITK_THREAD_RETURN_TYPE MyFunctionPtr(void *)
{
  for (int i=0; i<Target/NumThreads; i++)
    {
    itk::int32_t* ptr32 = ++AtomicPtr;
    (*ptr32) = 1;

    itk::int64_t* ptr64 = ++Atomic64Ptr;
    (*ptr64) = 1;
    }

  return ITK_THREAD_RETURN_VALUE;
}

template<class T>
int TestAtomicOperators(std::string name)
{
  itk::AtomicInt<T> testAtomic(1);
  if(testAtomic != 1)
  {
    std::cout << "Expecting testAtomic"<<name<<" value to be 1. Got "
              << testAtomic << std::endl;
    return 1;
  }
  testAtomic = 2;
  if(testAtomic != 2)
  {
    std::cout << "Expecting testAtomic"<<name<<" value to be 2. Got "
              << testAtomic << std::endl;
    return 1;
  }
  itk::AtomicInt<T> testAtomicCopy(3);
  testAtomic = testAtomicCopy;
  if(testAtomic != 3)
  {
    std::cout << "Expecting testAtomic"<<name<<" value to be 3. Got "
              << testAtomic << std::endl;
    return 1;
  }
  if(testAtomic.load() != 3)
  {
    std::cout << "Expecting testAtomic"<<name<<".load() value to be 3. Got "
              << testAtomic.load() << std::endl;
    return 1;
  }
  testAtomic.store(0);
  if(testAtomic != 0)
  {
    std::cout << "Expecting testAtomic"<<name<<" value to be 0. Got "
              << testAtomic << std::endl;
    return 1;
  }
  T preinc_val = ++testAtomic;
  if(preinc_val != 1)
  {
    std::cout << "Expecting PreIncrement ("<<name<<") value to be 1. Got "
              << preinc_val << std::endl;
    return 1;
  }
  T postinc_val = testAtomic++;
  if(postinc_val != 1)
  {
    std::cout << "Expecting PostIncrement ("<<name<<") value to be 1. Got "
              << postinc_val << std::endl;
    return 1;
  }
  T predec_val = --testAtomic;
  if(predec_val != 1)
  {
    std::cout << "Expecting PreDecrement ("<<name<<") value to be 1. Got "
              << predec_val << std::endl;
    return 1;
  }
  T postdec_val = testAtomic--;
  if(postdec_val != 1)
  {
    std::cout << "Expecting PostDecrement ("<<name<<") value to be 1. Got "
              << postdec_val << std::endl;
    return 1;
  }
  testAtomic += 2;
  T addAndFetch_val = testAtomic;
  if(addAndFetch_val != 2)
  {
    std::cout << "Expecting AddAndFetch ("<<name<<") value to be 2. Got "
              << addAndFetch_val << std::endl;
    return 1;
  }
  testAtomic -= 1;
  T subAndFetch_val = testAtomic;
  if(subAndFetch_val != 1)
  {
    std::cout << "Expecting SubAndFetch ("<<name<<") value to be 1. Got "
              << subAndFetch_val << std::endl;
    return 1;
  }
  return 0;
  }

int itkAtomicIntTest(int, char*[])
{
  // Verify all atomic operators
  if( TestAtomicOperators<itk::uint32_t>("32")
   || TestAtomicOperators<itk::uint64_t>("64")
    )
  {
    return 1;
  }
  // Verify atomic behavior
  Total = 0;
  TotalAtomic = 0;
  Total64 = 0;
  TotalAtomic64 = 0;

  AnObject = itk::Object::New();

  int beforeMTime = AnObject->GetMTime();

  for (int i=0; i<Target; i++)
    {
    Values32[i] = 0;
    Values64[i] = 0;
    }

  itk::MultiThreader::Pointer mt = itk::MultiThreader::New();
  mt->SetSingleMethod(MyFunction, NULL);
  mt->SetNumberOfThreads(NumThreads);
  mt->SingleMethodExecute();

  mt->SetSingleMethod(MyFunction2, NULL);
  mt->SingleMethodExecute();

  mt->SetSingleMethod(MyFunction3, NULL);
  mt->SingleMethodExecute();

  // Making sure that atomic incr returned unique
  // values each time. We expect all numbers from
  // 1 to Target-1 to be 2.
  if (Values32[0] != 0)
    {
    std::cout << "Expecting Values32[0] to be 0. Got "
              << Values32[0] << std::endl;
    return 1;
    }
  if (Values64[0] != 0)
    {
    std::cout << "Expecting Values64[0] to be 0. Got "
              << Values64[0] << std::endl;
    return 1;
    }
  for (int i=1; i<Target; i++)
    {
    if (Values32[i] != 2)
      {
      std::cout << "Expecting Values32[" << i << "] to be 2. Got "
                << Values32[i] << std::endl;
      return 1;
      }
    if (Values64[i] != 2)
      {
      std::cout << "Expecting Values64[" << i << "] to be 2. Got "
                << Values64[i] << std::endl;
      return 1;
      }
    }

  mt->SetSingleMethod(MyFunction4, NULL);
  mt->SingleMethodExecute();

  std::cout << Total << " " << TotalAtomic.load() << std::endl;
  std::cout << Total64 << " " << TotalAtomic64.load() << std::endl;

  std::cout << "MTime: " << AnObject->GetMTime() << std::endl;

  mt->SetSingleMethod(MyFunctionPtr, NULL);
  mt->SingleMethodExecute();

  // Making sure that pointer atomic incr returned unique
  // values each time incremented by the size of the pointer.
  // We expect all numbers from 1 to Target-1 to be 1.
  if (Values32[0] != 0)
    {
    std::cout << "Expecting Values32[0] to be 0. Got "
              << Values32[0] << std::endl;
    return 1;
    }
  if (Values64[0] != 0)
    {
    std::cout << "Expecting Values64[0] to be 0. Got "
              << Values64[0] << std::endl;
    return 1;
    }
  for (int i=1; i<Target; i++)
    {
    if (Values32[i] != 1)
      {
      std::cout << "Expecting Values32[" << i << "] to be 1. Got "
                << Values32[i] << std::endl;
      return 1;
      }
    if (Values64[i] != 1)
      {
      std::cout << "Expecting Values64[" << i << "] to be 1. Got "
                << Values64[i] << std::endl;
      return 1;
      }
    }

  if (TotalAtomic.load() != static_cast<itk::uint32_t>(Target) )
    {
    return 1;
    }

  if (TotalAtomic64.load() != static_cast<itk::uint64_t>(Target) )
    {
    return 1;
    }

  if (AnObject->GetReferenceCount() != 1)
    {
    return 1;
    }

  if ((int)AnObject->GetMTime() != Target + beforeMTime + 2 )
    {
    return 1;
    }

  return 0;
}
