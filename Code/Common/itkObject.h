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
#ifndef __itkObject_h
#define __itkObject_h

#include "itkLightObject.h"
#include "itkSmartPointer.h"
#include "itkTimeStamp.h"
#include "itkIndent.h"

#include <iostream>
#include <typeinfo>

namespace itk
{

/** \class Object
 * \brief Base class for most itk classes.
 *
 * Object is the second-highest level base class for most itk objects.
 * It extends the base object functionality of LightObject by
 * implementing debug flags/methods and modification time tracking.
 */
class ITK_EXPORT Object: public LightObject
{
public:
  /** 
   * Smart pointer typedef support.
   */
  typedef Object              Self;
  typedef SmartPointer<Self>  Pointer;

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
  virtual void Modified();
  
  /** 
   * Increase the reference count (mark as used by another object). 
   */
  virtual void Register();

  /** 
   * Decrease the reference count (release by another object). 
   */
  virtual void UnRegister();

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
  virtual void PrintSelf(std::ostream& os, Indent indent);
  virtual void PrintHeader(std::ostream& os, Indent indent);
  virtual void PrintTrailer(std::ostream& os, Indent indent);

private:
  /**
   * Enable/Disable debug messages.
   */
  bool m_Debug;
  
  /**
   * Keep track of modification time.
   */
  TimeStamp m_MTime;
  
  /**
   * Global object debug flag.
   */
  static bool m_GlobalWarningDisplay;
};
  
} // end namespace itk

#endif

