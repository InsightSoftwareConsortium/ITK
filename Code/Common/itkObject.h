/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkObject.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
/**
 * itkObject is the highest level base class for most itk objects. It
 * implements reference counting, debug flags/methods, and defines an
 * API for object printing.
 */

#ifndef __itkObject_h
#define __itkObject_h

#include <iostream>
#include <typeinfo>

#include "itkSmartPointer.h"
#include "itkTimeStamp.h"
#include "itkIndent.h"
#include "itkSetGet.h"

class ITK_EXPORT itkObject 
{
public:
  /** 
   * Smart pointer typedef support.
   */
  typedef itkSmartPointer<itkObject> Pointer;

  /** 
   * Create an object with Debug turned off, modified time initialized to
   * zero, and reference count set to 1. 
   */
  static Pointer New();

  /** 
   * Delete an itk object.  This method should always be used to delete an
   * object when the new operator was used to create it. Using the C
   *  delete method will not work with reference counting. 
   */
  virtual void Delete();

  /** 
   * Return the name of this class as a string. Used by the object factory
   * (implemented in New()) to instantiate objects of a named type. Also
   * used for debugging and other output information. 
   */
  virtual const char *GetClassName() const
    {return "itkObject";}

#ifdef _WIN32
  /** 
   * Used to avoid dll boundary problems. 
   */
  void* operator new( size_t tSize, const char *, int);
  void* operator new( size_t tSize );
  void operator delete( void* p );
#endif 
  
  /** 
   * Cause the object to print itself out.
   */
  void Print(std::ostream& os);

  /** 
   * Overload operator<< to cause the object to print itself out 
   */
  std::ostream& operator<< (std::ostream& os) {this->Print(os); return os;}
    
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
  bool GetDebug();
  
  /** 
   * Set the value of the debug flag. A non-zero value turns debugging on.
   */
  void SetDebug(bool debugFlag);
  
  /** 
   * This method is called when itkErrorMacro executes. It allows 
   * the debugger to break on error. 
   */
  static void BreakOnError();
  
  /** 
   * Return this objects modified time. 
   */
  virtual unsigned long GetMTime();

  /** 
   * Update the modification time for this object. Many filters rely on the
   * modification time to determine if they need to recompute their data. 
   */
  virtual void Modified();
  
  /** 
   * This is a global flag that controls whether any debug, warning
   *  or error messages are displayed. 
   */
  static void SetGlobalWarningDisplay(bool flag);
  static void GlobalWarningDisplayOn()
    {itkObject::SetGlobalWarningDisplay(1);}
  static void GlobalWarningDisplayOff() 
    {itkObject::SetGlobalWarningDisplay(0);}
  static bool  GetGlobalWarningDisplay();
  
  /** 
   * Increase the reference count (mark as used by another object). 
   */
  void Register();

  /** 
   * Decrease the reference count (release by another object). 
   */
  virtual void UnRegister();

  /** 
   * Gets the reference count (use with care) 
   */
  int GetReferenceCount() const {return m_ReferenceCount;}

  /** 
   * Sets the reference count (use with care) 
   */
  void SetReferenceCount(int);

  /** 
   * A callback for when the destructor is called. Scripting
   * languages use this to know when a C++ object has been freed.
   * This is not intended for any use other than scripting. 
   */
  void SetDeleteMethod(void (*f)(void *));
  
protected:
  itkObject(); 
  virtual ~itkObject(); 
  itkObject(const itkObject&) {};
  void operator=(const itkObject&) {};

  /** 
   * Methods invoked by Print() to print information about the object
   * including superclasses. Typically not called by the user (use Print()
   * instead) but used in the hierarchical print process to combine the
   * output of several classes. 
   */
  virtual void PrintSelf(std::ostream& os, itkIndent indent);
  virtual void PrintHeader(std::ostream& os, itkIndent indent);
  virtual void PrintTrailer(std::ostream& os, itkIndent indent);

private:
  bool m_Debug;             /// Enable debug messages
  itkTimeStamp m_MTime;     /// Keep track of modification time
  int m_ReferenceCount;     /// Number of uses of this object by other objects
  void (*m_DeleteMethod)(void *);

  friend ITK_EXPORT std::ostream& operator<<(std::ostream& os, itkObject& o);
};

#endif

