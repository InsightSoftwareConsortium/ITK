/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTimeStamp.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkTimeStamp_h
#define __itkTimeStamp_h

#include "itkMacro.h"

namespace itk
{

/** \class TimeStamp
 * \brief Generate a unique, increasing time value.
 *
 * TimeStamp records a unique time when the method Modified() is 
 * executed. This time is guaranteed to be monotonically increasing.
 * Classes use this object to record modified and/or execution time.
 * There is built in support for the binary < and > comparison
 * operators between two TimeStamp objects. 
 *
 * \ingroup ITKSystemObjects
 */
class ITK_EXPORT TimeStamp 
{
public:
  /** Standard class typedefs. */
  typedef TimeStamp  Self;
  
  /** Create an instance of this class. We don't want to use reference
   * counting. */
  static Self* New();

  /** Constructor must remain public because classes instantiate
   * TimeStamps implicitly in their construction.  */
  TimeStamp() 
    {m_ModifiedTime = 0;}

  /** Destoy this instance. */
  void Delete() 
    {delete this;}

  /** The class name as a string.  */
  static const char *GetNameOfClass() 
    {return "TimeStamp";}

  /** Set this objects time to the current time. The current time is just a
   * monotonically increasing unsigned long integer. It is possible for this
   * number to wrap around back to zero.  This should only happen for
   * processes that have been running for a very long time, while constantly
   * changing objects within the program. When this does occur, the typical
   * consequence should be that some filters will update themselves when
   * really they don't need to.   */
  void Modified();

  /** Return this object's Modified time.  */
  unsigned long int GetMTime() const
    {return m_ModifiedTime;};

  /** Support comparisons of time stamp objects directly.  */
  int operator>(TimeStamp& ts) 
    {return (m_ModifiedTime > ts.m_ModifiedTime);}
  int operator<(TimeStamp& ts) 
    {return (m_ModifiedTime < ts.m_ModifiedTime);}

  /** Allow for typcasting to unsigned long.  */
  operator unsigned long() const
    {return m_ModifiedTime;}

private:
  unsigned long m_ModifiedTime;
};

  
} // end namespace itk
  
#endif
