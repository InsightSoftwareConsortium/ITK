/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLightObject.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkLightObject_h
#define __itkLightObject_h

#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include <iostream>
#include <typeinfo>

#include "itkSmartPointer.h"
#include "itkTimeStamp.h"
#include "itkIndent.h"
#include "itkSimpleFastMutexLock.h"

#include "itkMacro.h"

namespace itk
{
  
/** \class LightObject
 * \brief Light weight base class for most itk classes.
 * 
 * LightObject is the highest level base class for most itk objects. It
 * implements reference counting and the API for object printing.
 * It can be used as a lightweight base class in preference to Object.
 * (LightObject does not support callbacks or modified time as Object
 * does.) All ITK objects should be a subclass of LightObject or Object
 * with few exceptions (due to performance concerns).
 *
 * \sa Object
 * \ingroup ITKSystemObjects
 * \ingroup DataRepresentation
 */
class ITK_EXPORT LightObject 
{
public:
  /** Standard clas typedefs. */
  typedef LightObject         Self;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Method for creation through the object factory. */
  static Pointer New();

  /** Delete an itk object.  This method should always be used to delete an
   * object when the new operator was used to create it. Using the C
   *  delete method will not work with reference counting.  */
  virtual void Delete();

  /** Return the name of this class as a string. Used by the object factory
   * (implemented in New()) to instantiate objects of a named type. Also
   * used for debugging and other output information.  */
  virtual const char *GetNameOfClass() const 
    {return "LightObject";}

#ifdef _WIN32
  /** Used to avoid dll boundary problems.  */
  void* operator new(size_t);
  void* operator new[](size_t);
  void operator delete(void*);
  void operator delete[](void*, size_t);
#endif 
  
  /** Cause the object to print itself out. */
  void Print(std::ostream& os) const;

  /** This method is called when itkExceptionMacro executes. It allows 
   * the debugger to break on error.  */
  static void BreakOnError();
  
  /** Increase the reference count (mark as used by another object).  */
  virtual void Register() const;

  /** Decrease the reference count (release by another object).  */
  virtual void UnRegister() const;

  /** Gets the reference count on this object. */
  virtual int GetReferenceCount() const 
    {return m_ReferenceCount;}

  /** Sets the reference count on this object. This is a dangerous
   * method, use it with care. */
  virtual void SetReferenceCount(int);

protected:
  LightObject():m_ReferenceCount(1) {}
  virtual ~LightObject(); 

  /** Methods invoked by Print() to print information about the object
   * including superclasses. Typically not called by the user (use Print()
   * instead) but used in the hierarchical print process to combine the
   * output of several classes.  */
  virtual void PrintSelf(std::ostream& os, Indent indent) const;
  virtual void PrintHeader(std::ostream& os, Indent indent) const;
  virtual void PrintTrailer(std::ostream& os, Indent indent) const;
  
  /** Number of uses of this object by other objects. */
  mutable int m_ReferenceCount;

  /** Mutex lock to protect modification to the reference count */
  mutable SimpleFastMutexLock m_ReferenceCountLock;

private:
  LightObject(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  
};

} // end namespace itk
  
#endif
