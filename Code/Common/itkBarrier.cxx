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
  
#ifdef ITK_USE_SPROC
bool               Barrier::use_fetchop = true;
atomic_reservoir_t Barrier::reservoir= 0;
bool               Barrier::reservoir_initialized= false;
#endif
  
Barrier::Barrier()
{
  m_Barrier.m_CycleCount = 0;
  m_Barrier.m_NumberOfWaiters = 0;
}
  
void Barrier::Initialize (int count)
{
  if (count < 1) count= 1;
  m_NumberOfThreads = count;
  
#ifdef ITK_USE_SPROC
  m_NumberOfProcessors = static_cast<int>(sysmp(MP_NAPROCS));
  
  // create the reservoir only once (hoping that we wont be using more than 1 barriers)
  if (Barrier::reservoir_initialized == false)
    {
      Barrier::reservoir = atomic_alloc_reservoir(USE_DEFAULT_PM, 1, 0);
      if (Barrier::reservoir != 0) Barrier::reservoir_initialized= true;
    }
  
  if (Barrier::reservoir == 0)
    {
    Barrier::use_fetchop = false;
    }
  else
    {
    Barrier::use_fetchop = true;
    }
  
  if (m_NumberOfProcessors > 1)
    {
    if (Barrier::use_fetchop)
      {
    m_Barrier.flag = 0;
    
    // create the atomic variable in the reservoir
    m_Barrier.pvar = atomic_alloc_variable(Barrier::reservoir, 0);
    
    storeop_store(m_Barrier.pvar, 0); 
      }
    else
      {
    // this would be slower than using the fetchop library
    if (MultiThreader::GetInitialized() == false)
        {
        MultiThreader::Initialize();
        }
    
    m_Barrier.m_Barrier = new_barrier( MultiThreader::GetThreadArena() );
      }
    }
#endif
}

void Barrier::Remove()
{
#ifdef ITK_USE_SPROC
  if (m_NumberOfProcessors > 1)
    {
    if (Barrier::use_fetchop == false)
      {
      free_barrier (m_Barrier.m_Barrier);
      }
    else
      {
    // free the variable
    if ((m_Barrier.pvar != 0) && (Barrier::reservoir != 0))
      {
        atomic_free_variable (Barrier::reservoir, m_Barrier.pvar);
        m_Barrier.pvar= 0;
      }
    // free the reservoir
    if (Barrier::reservoir != 0)
      {
        atomic_free_reservoir (Barrier::reservoir);
        Barrier::reservoir= 0;
      }
      }
    }
#endif
}

Barrier::~Barrier()
{
#ifdef ITK_USE_SPROC  
  this->Remove();
#else
  this->Remove();
#endif
}

void Barrier::Wait()
{
#ifdef ITK_USE_SPROC
  if (m_NumberOfProcessors > 1)
    {
    if (Barrier::use_fetchop)
      {
    int gen = m_Barrier.flag;
    atomic_var_t val = atomic_fetch_and_increment (m_Barrier.pvar);
    if (val == m_NumberOfThreads - 1)
      {
        storeop_store (m_Barrier.pvar,0);
        m_Barrier.flag++;
      }
    
    while (m_Barrier.flag == gen) ; // spin
      }
    else
      {
      barrier (m_Barrier.m_Barrier, m_NumberOfThreads);
      }
    }
  else
    {
    m_Barrier.m_Mutex.Lock();
    
    ConditionVariable & cond = m_Barrier.m_CycleCount ? (*m_Barrier.m_Cond0) : (*m_Barrier.m_Cond1);
    
    int me = m_Barrier.m_NumberOfWaiters++;
    
    if (m_Barrier.m_NumberOfWaiters == m_NumberOfThreads)
      {
    m_Barrier.m_CycleCount = 1 - m_Barrier.m_CycleCount ;
    m_Barrier.m_NumberOfWaiters = 0;
    cond.Broadcast();
      }
    else
      {
      cond.Wait(&(m_Barrier.m_Mutex));
      }
    
    m_Barrier.m_Mutex.Unlock();
    }
  
#else
  
  m_Barrier.m_Mutex.Lock();
  
  ConditionVariable & cond = m_Barrier.m_CycleCount ? (*m_Barrier.m_Cond0) : (*m_Barrier.m_Cond1);
  
  int me = m_Barrier.m_NumberOfWaiters++;
  if (m_Barrier.m_NumberOfWaiters == m_NumberOfThreads)
    {
    m_Barrier.m_CycleCount = 1 - m_Barrier.m_CycleCount ;
    m_Barrier.m_NumberOfWaiters = 0;
    cond.Broadcast();
    }
  else
    {
    cond.Wait (&(m_Barrier.m_Mutex));
    }
  m_Barrier.m_Mutex.Unlock();
#endif
}

} //itk

#endif
