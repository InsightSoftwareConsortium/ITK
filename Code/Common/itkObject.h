/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkObject.h
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
#ifndef __itkObject_h
#define __itkObject_h

#include "itkLightObject.h"

namespace itk
{

/** \class Object
 * \brief Base class for most itk classes.
 *
 * Object is the second-highest level base class for most itk objects.
 * It extends the base object functionality of LightObject by
 * implementing debug flags/methods and modification time tracking.
 *
 * \ingroup ITKSystemObjects
 * \ingroup DataRepresentation
 */
class ITK_EXPORT Object: public LightObject
{
public:
  /** 
   * Smart pointer typedef support.
   */
  typedef Object              Self;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /**
   * Standard "Superclass" typedef.
   */
  typedef LightObject  Superclass;

  /**
   * Method for creation through the object factory.
   */
  static Pointer New();

  /** 
   * Standard part of all itk objects.
   */
  itkTypeMacro(Object, LightObject);

  /** 
   * Turn debugging output on. 
   */
  virtual void DebugOn();

  /** 
   * Turn debugging output off. 
   */
  virtual void DebugOff();
  
  /** 
   * Get the value of the debug flag. 
   */
  bool GetDebug() const;
  
  /** 
   * Set the value of the debug flag. A non-zero value turns debugging on.
   */
  void SetDebug(bool debugFlag);
  
  /** 
   * Return this objects modified time. 
   */
  virtual unsigned long GetMTime() const;

  /** 
   * Update the modification time for this object. Many filters rely on the
   * modification time to determine if they need to recompute their data. 
   */
  virtual void Modified() const;
  
  /** 
   * Increase the reference count (mark as used by another object). 
   */
  virtual void Register() const;

  /** 
   * Decrease the reference count (release by another object). 
   */
  virtual void UnRegister() const;

  /** 
   * Sets the reference count (use with care) 
   */
  virtual void SetReferenceCount(int);

  /** 
   * This is a global flag that controls whether any debug, warning
   *  or error messages are displayed. 
   */
  static void SetGlobalWarningDisplay(bool flag);
  static bool GetGlobalWarningDisplay();

  static void GlobalWarningDisplayOn()
    { Object::SetGlobalWarningDisplay(true); }
  static void GlobalWarningDisplayOff()
    { Object::SetGlobalWarningDisplay(false); }
  
protected:
  Object(); 
  virtual ~Object(); 
  Object(const Self&) {}
  void operator=(const Self&) {}

  /** 
   * Methods invoked by Print() to print information about the object
   * including superclasses. Typically not called by the user (use Print()
   * instead) but used in the hierarchical print process to combine the
   * output of several classes. 
   */
  virtual void PrintSelf(std::ostream& os, Indent indent) const;

private:
  /**
   * Enable/Disable debug messages.
   */
  bool m_Debug;
  
  /**
   * Keep track of modification time.
   */
  mutable TimeStamp m_MTime;
  
  /**
   * Global object debug flag.
   */
  static bool m_GlobalWarningDisplay;
};
  
} // end namespace itk

#endif

