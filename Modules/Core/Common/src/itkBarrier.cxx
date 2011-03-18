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
#include "itkBarrier.h"
#include "itkMacro.h"

namespace itk
{
#ifdef ITK_USE_FETCHOP_BARRIERS
atomic_reservoir_t Barrier:: m_Reservoir = 0;
bool Barrier::               m_ReservoirInitialized = false;
int Barrier::                m_MaxBarriers = 1024;
#endif

Barrier::Barrier()
{
  m_NumberExpected = 0;
#ifndef ITK_USE_FETCHOP_BARRIERS
  m_NumberArrived  = 0;
  m_ConditionVariable = ConditionVariable::New();
#endif
}

Barrier::~Barrier()
{
#if defined ITK_USE_FETCHOP_BARRIERS
  if ( m_Pvar != 0 && Barrier::m_Reservoir != 0 )
    {
    atomic_free_variable(Barrier::m_Reservoir, m_Pvar);
    m_Pvar = 0;
    }

  if ( Barrier::m_Reservoir != 0 )
    {
    atomic_free_reservoir(Barrier::m_Reservoir);
    Barrier::m_Reservoir = 0;
    }
#endif
}

void Barrier::Initialize(unsigned int n)
{
  m_NumberExpected = n;

#if defined ITK_USE_FETCHOP_BARRIERS
  // Create the reservoir.
  if ( Barrier::m_ReservoirInitialized == false )
    {
    Barrier::m_Reservoir = atomic_alloc_reservoir(USE_DEFAULT_PM,
                                                  m_MaxBarriers, 0);
    if ( Barrier::m_Reservoir != 0 )
      {
      Barrier::m_ReservoirInitialized = true;
      }
    else
      {
      itkExceptionMacro(<< "atomic_alloc_reservoir call failed!");
      }
    }

  m_FetchopFlag = 0;
  m_Pvar = atomic_alloc_variable(Barrier::m_Reservoir, 0);
  storeop_store(m_Pvar, 0);
#endif
}

void Barrier::Wait()
{
#if defined ITK_USE_FETCHOP_BARRIERS
  int          gen = m_FetchopFlag;
  atomic_var_t val = atomic_fetch_and_increment(m_Pvar);
  if ( val == m_NumberExpected - 1 )
    {
    storeop_store(m_Pvar, 0);
    m_FetchopFlag++;
    }
  while ( m_FetchopFlag == gen )
    { // spin
    }
#else
  m_Mutex.Lock();
  m_NumberArrived++;
  if ( m_NumberArrived == m_NumberExpected )
    {
    // Clear all blocked threads
    m_NumberArrived = 0;
    m_ConditionVariable->Broadcast();
    }
  else
    {
    // Block this thread
    m_ConditionVariable->Wait(&m_Mutex);
    }
  m_Mutex.Unlock();
#endif
}
} // end namespace itk
