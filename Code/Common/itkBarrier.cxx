/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBarrier.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkBarrier_cxx_
#define __itkBarrier_cxx_

#include "itkBarrier.h"
#include "itkMacro.h"

#ifdef ITK_USE_SPROC
#include "itkMultiThreader.h"
#endif

namespace itk {

#ifdef ITK_USE_FETCHOP_BARRIERS
atomic_reservoir_t Barrier::m_Reservoir = 0;
bool Barrier::m_ReservoirInitialized = false;
int Barrier::m_MaxBarriers = 1024;
#endif

Barrier::Barrier()
{
  m_NumberExpected = 0;
#ifndef ITK_USE_SPROC
#ifndef ITK_USE_FETCHOP_BARRIERS
  m_NumberArrived  = 0;
  m_ConditionVariable = ConditionVariable::New();
#endif
#endif
}

Barrier::~Barrier()
{
#if defined ITK_USE_FETCHOP_BARRIERS
  if (m_Pvar !=0 && Barrier::m_Reservoir != 0 )
    {
    atomic_free_variable(Barrier::m_Reservoir, m_Pvar);
    m_Pvar = 0;
    }

  if (Barrier::m_Reservoir != 0)
    {
    atomic_free_reservoir(Barrier::m_Reservoir);
    Barrier::m_Reservoir = 0;
    }
#elif defined ITK_USE_SPROC
  if (m_Barrier != 0)
    {
    // Note: free_barrier should be called here
    // but is buggy and causes a seg fault. jc 7/29/03
    
    //    free_barrier( m_Barrier );
    }
#endif
}

void Barrier::Initialize( unsigned int n )
{
  m_NumberExpected = n;

#if defined ITK_USE_FETCHOP_BARRIERS
  // Create the reservoir.
  if (Barrier::m_ReservoirInitialized == false)
    {
    Barrier::m_Reservoir = atomic_alloc_reservoir(USE_DEFAULT_PM, m_MaxBarriers, 0);
    if (Barrier::m_Reservoir != 0)
      {
      Barrier::m_ReservoirInitialized = true;
      }
    else
      {
      itkExceptionMacro( << "atomic_alloc_reservoir call failed!" );
      }
    }

  m_FetchopFlag = 0;
  m_Pvar = atomic_alloc_variable(Barrier::m_Reservoir, 0);
  storeop_store(m_Pvar, 0);
#elif defined ITK_USE_SPROC
  if (MultiThreader::GetInitialized() == false)
    {
    MultiThreader::Initialize();
    }
  m_Barrier = new_barrier( MultiThreader::GetThreadArena() );
#endif  
}

void Barrier::Wait()
{
#if defined ITK_USE_FETCHOP_BARRIERS
  int gen = m_FetchopFlag;
  atomic_var_t val = atomic_fetch_and_increment(m_Pvar);
  if (val == m_NumberExpected - 1)
    {
    storeop_store(m_Pvar, 0);
    m_FetchopFlag++;
    }
  while (m_FetchopFlag == gen)
    { // spin
    }
#elif defined ITK_USE_SPROC
  barrier(m_Barrier, m_NumberExpected);
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
    m_ConditionVariable->Wait( &m_Mutex );
    }
  m_Mutex.Unlock();
#endif
}

}// end namespace itk

#endif
