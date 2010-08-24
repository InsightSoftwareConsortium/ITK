/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMutexLockHolder.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMutexLockHolder_h
#define __itkMutexLockHolder_h

#include "itkMacro.h"

namespace itk
{
/** \class MutexLockHolder
 *
 */
template< class TMutex >
class MutexLockHolder
{
public:
  typedef TMutex Mutex;
  MutexLockHolder(Mutex & mutex):m_Mutex(mutex)
  {
    m_Mutex.Lock();
  }

  ~MutexLockHolder()
  {
    m_Mutex.Unlock();
  }

protected:
  Mutex & m_Mutex;
};
} //end itk namespace

#endif
