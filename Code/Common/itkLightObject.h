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
#ifndef __itkLightObject_h
#define __itkLightObject_h

#include <iostream>
#include <typeinfo>

#include "itkSmartPointer.h"
#include "itkTimeStamp.h"
#include "itkIndent.h"
#include "itkMacro.h"

/** \namespace itk
 * \brief The "itk" namespace contains all classes defined for Insight.
 */
namespace itk
{

/** \class LightObject
 * LightObject is the highest level base class for most itk objects. It
 * implements reference counting, call-backs, and API for object printing.
 */
class ITK_EXPORT LightObject 
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef LightObject         Self;

  /**
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;

  /**
   * Method for creation through the object factory.
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
  virtual const char *GetClassName() const {return "LightObject";}

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
  LightObject(); 
  virtual ~LightObject(); 
  LightObject(const Self&) {}
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

} // namespace itk
  
#endif
