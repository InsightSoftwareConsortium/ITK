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

namespace itk
{

Barrier::Barrier() :
  m_NumberArrived(0),
  m_NumberExpected(0),
  m_ConditionVariable( ConditionVariable::New() ),
  m_Mutex()
{
}

Barrier::~Barrier()
{
}

void Barrier::Initialize(unsigned int n)
{
  m_NumberExpected = n;
}

void Barrier::Wait()
{
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
}
} // end namespace itk
