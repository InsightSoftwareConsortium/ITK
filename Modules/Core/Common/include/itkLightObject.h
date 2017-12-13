/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkLightObject_h
#define itkLightObject_h

#include "itkMacro.h"
#include "itkSmartPointer.h"
#include "itkTimeStamp.h"
#include "itkIndent.h"
#include "itkAtomicInt.h"

#include <iostream>
#include <typeinfo>

#if defined( _WIN32 )
// To get LONG defined
  #include "itkWindows.h"
#elif defined( __APPLE__ )
// To get MAC_OS_X_VERSION_MIN_REQUIRED defined
  #include <AvailabilityMacros.h>
#endif

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
 * \ingroup ITKCommon
 */
class ITKCommon_EXPORT LightObject
{
public:
  /** Standard class typedefs. */
  typedef LightObject                Self;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  static Pointer New();

  /** Create an object from an instance, potentially deferring to a
   * factory.  This method allows you to create an instance of an
   * object that is exactly the same type as the referring object.
   * This is useful in cases where an object has been cast back to a
   * base class. */
  virtual Pointer CreateAnother() const;

  itkCloneMacro(Self);

  /** Delete an itk object.  This method should always be used to delete an
   * object when the new operator was used to create it. Using the C
   *  delete method will not work with reference counting.  */
  virtual void Delete();

  /** Return the name of this class as a string. Used by the object factory
   * (implemented in New()) to instantiate objects of a named type. Also
   * used for debugging and other output information.  */
  virtual const char * GetNameOfClass() const;

#ifdef _WIN32
  /** Used to avoid dll boundary problems.  */
  void * operator new(size_t);

  void * operator new[](size_t);

  void operator delete(void *);

  void operator delete[](void *, size_t);

#endif

  /** Cause the object to print itself out. */
  void Print(std::ostream & os, Indent indent = 0) const;

  /** This method is called when itkExceptionMacro executes. It allows
   * the debugger to break on error.  */
  static void BreakOnError();

  /** Increase the reference count (mark as used by another object).  */
  virtual void Register() const;

  /** Decrease the reference count (release by another object).  */
  virtual void UnRegister() const ITK_NOEXCEPT;

  /** Gets the reference count on this object. */
  virtual int GetReferenceCount() const
  { return m_ReferenceCount; }

  /** Sets the reference count on this object. This is a dangerous
   * method, use it with care. */
  virtual void SetReferenceCount(int);

protected:
  LightObject();
  virtual ~LightObject();

  /** Methods invoked by Print() to print information about the object
   * including superclasses. Typically not called by the user (use Print()
   * instead) but used in the hierarchical print process to combine the
   * output of several classes.  */
  virtual void PrintSelf(std::ostream & os, Indent indent) const;

  virtual void PrintHeader(std::ostream & os, Indent indent) const;

  virtual void PrintTrailer(std::ostream & os, Indent indent) const;

  /**
   * Actual implementation of the clone method. This method should be reimplemeted
   * in subclasses to clone the extra required parameters.
   */
  virtual LightObject::Pointer InternalClone() const;

  /** Number of uses of this object by other objects. */
  mutable AtomicInt<int> m_ReferenceCount;


private:
  ITK_DISALLOW_COPY_AND_ASSIGN(LightObject);
};

/**
 * This operator allows all subclasses of LightObject to be printed via <<.
 * It in turn invokes the Print method, which in turn will invoke the
 * PrintSelf method that all objects should define, if they have anything
 * interesting to print out.
 */
ITKCommon_EXPORT std::ostream &
operator<<(std::ostream & os, const LightObject & o);

} // end namespace itk

#endif
