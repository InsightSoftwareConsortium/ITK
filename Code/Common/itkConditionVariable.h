/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkConditionVariable.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef  _itkConditionVariable_h_
#define  _itkConditionVariable_h_

#include "itkMutexLock.h"
#include "itkSemaphore.h"
#include "itkLightObject.h"

namespace itk {

/** \class ConditionVariable
 * \brief
 *
*/
class ITK_EXPORT ConditionVariable : public LightObject
{
public:
  /** Standard class typedefs. */
  typedef ConditionVariable Self;
  typedef LightObject Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);  
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(ConditionVariable, Object);
  
  int m_NumberOfWaiters;
  MutexLock * m_Lock;
  MutexType m_Mutex;
  Semaphore::Pointer m_Sema;
#ifdef ITK_USE_PTHREADS
  pthread_cond_t m_Cond;
#endif

  void Wait(SimpleMutexLock * mutex);

  void Signal();
  
  void Broadcast();

private:
  ConditionVariable();
  ~ConditionVariable();
};

}//end of itk namespace

#endif
