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

#endif

namespace itk {

Barrier::Barrier()
{
  m_NumberExpected = 0;
  m_NumberArrived  = 0;
  m_ConditionVariable = ConditionVariable::New(); 
}

Barrier::~Barrier()
{}

void Barrier::Initialize( unsigned int n )
{
  m_Mutex.Lock();
  m_NumberExpected = n;
  m_Mutex.Unlock();
}

void Barrier::Break()
{
  // Unblock all waiting threads.
  m_Mutex.Lock();
  m_ConditionVariable->Broadcast();
  m_NumberArrived = 0;
  m_Mutex.Unlock();
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
    m_ConditionVariable->Wait( &m_Mutex );
    }
  m_Mutex.Unlock();
}

}// end namespace itk

#endif
