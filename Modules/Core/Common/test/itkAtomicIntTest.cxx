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
int Values32[Target+2];
int Values64[Target+2];
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

int itkAtomicIntTest(int, char*[])
{
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

  if (TotalAtomic.load() != Target)
    {
    return 1;
    }

  if (TotalAtomic64.load() != Target)
    {
    return 1;
    }

  if (AnObject->GetReferenceCount() != 1)
    {
    return 1;
    }

  if ((int)AnObject->GetMTime() != Target + beforeMTime + 2)
    {
    return 1;
    }

  return 0;
}
