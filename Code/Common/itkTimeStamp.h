/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTimeStamp.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

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
 */

class ITK_EXPORT TimeStamp 
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef TimeStamp  Self;
  
  /** 
   * Create an instance of this class. We don't want to use reference
   * counting.
   */
  static Self* New();

  /** 
   * Constructor must remain public because classes instantiate
   * TimeStamps implicitly in their construction. 
   */
  TimeStamp() 
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
    {return "TimeStamp";}

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
  int operator>(TimeStamp& ts) 
    {return (m_ModifiedTime > ts.m_ModifiedTime);}
  int operator<(TimeStamp& ts) 
    {return (m_ModifiedTime < ts.m_ModifiedTime);}


  /** 
   * Allow for typcasting to unsigned long. 
   */
  operator const unsigned long() const
    {return m_ModifiedTime;}


private:
  unsigned long m_ModifiedTime;
};

  
} // end namespace itk
  
#endif
