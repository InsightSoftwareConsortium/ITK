/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkResourceProbe.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkResourceProbe_h
#define __itkResourceProbe_h

#include "itkConfigure.h"
#include "itkWin32Header.h"
#include <string>

namespace itk
{
/** \class ResourceProbe
  *
  *  \brief Class for computing the change of a value between two points in the code.
  *
  *   This class is the base class of all the probes (time, memory, etc.) between
  *   the execution of two pieces of code. It can be started and stopped
  *   in order to evaluate the execution over multiple passes.
  *
  *   \sa TimeResourceProbe, MemoryResourceProbe
  *
  */
template< class ValueType, class MeanType >
class ITK_EXPORT ResourceProbe
{
public:

  /** Type for counting how many times the probe has been started and stopped.
    */
  typedef unsigned long CountType;
public:

  /** Constructor */
  ResourceProbe(const std::string & type, const std::string & unit);

  /** Destructor */
  virtual ~ResourceProbe();

  /** Returns the type probed value */
  std::string GetType(void) const;

  /** Returns the unit probed value */
  std::string GetUnit(void) const;

  /** Start counting the change of value */
  void        Start(void);

  /** Stop counting the change of value */
  void        Stop(void);

  /** Returns the number of times that the probe has been started */
  CountType   GetNumberOfStarts(void) const;

  /** Returns the number of times that the probe has been stopped */
  CountType   GetNumberOfStops(void) const;

  /** Returns the instant value of the probed system.
   */
  virtual ValueType   GetInstantValue(void) const = 0;

  /** Returns the accumulated value changes between the starts and stops
   *  of the probe */
  ValueType    GetTotal(void) const;

  /** Returns the average value changes between the starts and stops
   *  of the probe. Stop() has to be called at least once, returns 0 otherwise.
   */
  MeanType    GetMean(void) const;

private:

  ValueType m_StartValue;
  ValueType m_TotalValue;

  CountType m_NumberOfStarts;
  CountType m_NumberOfStops;

  std::string m_TypeString;
  std::string m_UnitString;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkResourceProbe.txx"
#endif

#endif //__itkResourceProbe_h
