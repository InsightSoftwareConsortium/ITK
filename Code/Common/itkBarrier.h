/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBarrier.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkBarrier_h_
#define __itkBarrier_h_

#include "itkLightObject.h"
#include "itkConditionVariable.h"
#include "itkMutexLock.h"

#ifdef ITK_USE_SPROC
#endif

namespace itk {

/**
 * \class Barrier
 *
 * 
 */
class ITK_EXPORT Barrier : public LightObject
{
public:
  /** Standard class typedefs. */
  typedef Barrier      Self;
  typedef LightObject  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(Barrier, Object);

  /** Creates a new system variable used to implement the barrier.  The
      argument to this method is the number of threads that must Wait() on the
      barrier before it is cleared. */
  void Initialize(unsigned int);
  
  /** A thread calling this method waits until m_NumberOfThreads have called
      Wait() on the barrier.  When the  */
  void Wait();

  /** Break the barrier.  Unblocks execution of all waiting threads. */
  void Break();
  
private:
  Barrier();
  ~Barrier();
  
  unsigned int m_NumberExpected;
  unsigned int m_NumberArrived;

  SimpleMutexLock m_Mutex;
  ConditionVariable::Pointer m_ConditionVariable;
};

} // end namespace itk

#endif
