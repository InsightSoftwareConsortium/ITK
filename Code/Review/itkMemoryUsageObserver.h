/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMemoryUsageObserver.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkMemoryUsageObserver_h
#define __itkMemoryUsageObserver_h

#include <itkMacro.h>
#include <itkObject.h>
#include <itkObjectFactory.h>

namespace itk
{

/** \class MemoryUsageObserver
* \brief The MemoryUsageObserver provides the memory usage of the process
*
* This class represents a memory load analyser object
* and provides a memory usage in platform-independent format.
*
*/

class ITKCommon_EXPORT MemoryUsageObserverBase
{
public:
  /** Define the type for the memory usage */
  typedef unsigned long        MemoryLoadType;

  /** Returns the memory load in kO */
  virtual MemoryLoadType GetMemoryUsage() = 0;

};

#if defined(WIN32) || defined(_WIN32)
class WindowsMemoryUsageObserver:public MemoryUsageObserverBase
{
public:
  virtual MemoryLoadType GetMemoryUsage();
};
#elif linux
class LinuxMemoryUsageObserver:public MemoryUsageObserverBase
{
public:
  virtual MemoryLoadType GetMemoryUsage();
};
#else // Unix and Mac Platforms
class SysResourceMemoryUsageObserver:public MemoryUsageObserverBase
{
public:
  virtual MemoryLoadType GetMemoryUsage();
};
#ifndef __APPLE__
class MallinfoMemoryUsageObserver:public MemoryUsageObserverBase
{
public:
  virtual MemoryLoadType GetMemoryUsage();
};
#endif
#endif


class ITKCommon_EXPORT MemoryUsageObserver:
#if defined(WIN32) || defined(_WIN32)
  public WindowsMemoryUsageObserver
#elif linux
  public LinuxMemoryUsageObserver
#elif __APPLE__
  public SysResourceMemoryUsageObserver
#else
  public MallinfoMemoryUsageObserver
#endif
{
};

} // end of namespace itk

#endif  // __itkMemoryUsageObserver_h
