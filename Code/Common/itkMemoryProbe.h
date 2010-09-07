/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMemoryProbe.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMemoryProbe_h
#define __itkMemoryProbe_h

#include "itkConfigure.h"
#include "itkResourceProbe.h"
#include "itkMemoryUsageObserver.h"

namespace itk
{
/** \class MemoryProbe
 *
 *  \brief Class for computing the memory allocated between two points in the code.
 *
 *   This class allows the user to trace the memory charge between the execution
 *   of two pieces of code. It can be started and stopped in order to evaluate
 *   the execution over multiple passes. The values of memory are taken from
 *   GetProcessMemoryInfo() for Windows, the SMAPS file for Linux
 *   and getrusage() otherwise.
 *
 */
class ITKCommon_EXPORT MemoryProbe:
  public ResourceProbe< long, double >
{
public:

  MemoryProbe();
  ~MemoryProbe();

  /** Type for measuring memory. */
  typedef long MemoryLoadType;

  /** Type for measuring the average memory. */
  typedef double MeanMemoryLoadType;
protected:
  virtual MemoryLoadType GetInstantValue(void) const;

private:
  mutable MemoryUsageObserver m_MemoryObserver;
};
} // end namespace itk

#endif //__itkMemoryProbe_h
