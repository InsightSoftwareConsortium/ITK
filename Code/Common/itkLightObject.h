/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLightObject.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
/**
 * itkLightObject is the highest level base class for most itk objects. It
 * implements reference counting, call-backs, and API for object printing.
 */

#ifndef __itkLightObject_h
#define __itkLightObject_h

#include <iostream>
#include <typeinfo>

#include "itkSmartPointer.h"
#include "itkTimeStamp.h"
#include "itkIndent.h"
#include "itkMacro.h"

class ITK_EXPORT itkLightObject 
{
public:
  /** 
   * Smart pointer typedef support.
   */
  typedef itkLightObject         Self;
  typedef itkSmartPointer<Self>  Pointer;

  /** 
   * Create an object with reference count set to 1. 
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
  virtual const char *GetClassName() const {return "itkLightObject";}

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
   * This method is called when itkErrorMacro executes. It allows 
   * the debugger to break on error. 
   */
  static void BreakOnError();
  
  /** 
   * Increase the reference count (mark as used by another object). 
   */
  virtual void Register();

  /** 
   * Decrease the reference count (release by another object). 
   */
  virtual void UnRegister();

  /** 
   * Gets the reference count (use with care) 
   */
  virtual int GetReferenceCount() const {return m_ReferenceCount;}

  /** 
   * Sets the reference count (use with care) 
   */
  virtual void SetReferenceCount(int);

  /** 
   * A callback for when the destructor is called. Scripting
   * languages use this to know when a C++ object has been freed.
   * This is not intended for any use other than scripting. 
   */
  virtual void SetDeleteMethod(void (*f)(void *));
  
protected:
  itkLightObject(); 
  virtual ~itkLightObject(); 
  itkLightObject(const Self&) {}
  void operator=(const Self&) {}

  /** 
   * Methods invoked by Print() to print information about the object
   * including superclasses. Typically not called by the user (use Print()
   * instead) but used in the hierarchical print process to combine the
   * output of several classes. 
   */
  virtual void PrintSelf(std::ostream& os, itkIndent indent);
  virtual void PrintHeader(std::ostream& os, itkIndent indent);
  virtual void PrintTrailer(std::ostream& os, itkIndent indent);

  /**
   * Number of uses of this object by other objects.
   */
  int m_ReferenceCount;
  
  /**
   * Call-back when object is deleted.  Used to interface with interpreted
   * languages.
   */
  void (*m_DeleteMethod)(void *);
};

#endif

