/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTimeStamp.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
/** 
 * itkTimeStamp records a unique time when the method Modified() is 
 *  executed. This time is guaranteed to be monotonically increasing.
 *  Classes use this object to record modified and/or execution time.
 *  There is built in support for the binary < and > comparison
 *  operators between two itkTimeStamp objects. 
 */

#ifndef __itkTimeStamp_h
#define __itkTimeStamp_h

#include "itkWin32Header.h"

class ITK_EXPORT itkTimeStamp 
{
public:
  /** 
   * Create an instance of this class. We don't want to use reference
   * counting.
   */
  static itkTimeStamp *New();

  /** 
   * Constructor must remain public because classes instantiate
   * itkTimeStamps implicitly in their construction. 
   */
  itkTimeStamp() 
    {m_ModifiedTime = 0;}

  /** 
   * Destoy this instance.
   */
  void Delete() 
    {delete this;}

  /** 
   * The class name as a string. 
   */
  static const char *GetClassName() 
    {return "itkTimeStamp";}

  /** 
   * Set this objects time to the current time. The current time is just a
   * monotonically increasing unsigned long integer. It is possible for this
   * number to wrap around back to zero.  This should only happen for
   * rocesses that have been running for a very long time, while constantly
   * changing objects within the program. When this does occur, the typical
   * consequence should be that some filters will update themselves when
   * really they don't need to.  
   */
  void Modified();

  /** 
   * Return this object's Modified time. 
   */
  unsigned long int GetMTime() const
    {return m_ModifiedTime;};

  /** 
   * Support comparisons of time stamp objects directly. 
   */
  int operator>(itkTimeStamp& ts) 
    {return (m_ModifiedTime > ts.m_ModifiedTime);}
  int operator<(itkTimeStamp& ts) 
    {return (m_ModifiedTime < ts.m_ModifiedTime);}

  /** 
   * Allow for typcasting to unsigned long. 
   */
  operator unsigned long() 
    {return m_ModifiedTime;}

private:
  unsigned long m_ModifiedTime;
};

#endif
