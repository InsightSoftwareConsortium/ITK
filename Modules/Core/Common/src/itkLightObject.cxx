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
#include "itkFastMutexLock.h"

// Better name demanging for gcc
#if __GNUC__ > 3 || ( __GNUC__ == 3 && __GNUC_MINOR__ > 0 )
#ifndef __EMSCRIPTEN__
#define GCC_USEDEMANGLE
#endif
#endif

#ifdef GCC_USEDEMANGLE
#include <cstdlib>
#include <cxxabi.h>
#endif


namespace itk
{

LightObject::LightObject():m_ReferenceCount(1)
{
}

const char * LightObject::GetNameOfClass() const
{
 return "LightObject";
}

LightObject::Pointer
LightObject::New()
{
  Pointer      smartPtr;
  LightObject *rawPtr = ::itk::ObjectFactory< LightObject >::Create();

  if ( rawPtr == ITK_NULLPTR )
    {
    rawPtr = new LightObject;
    }
  smartPtr = rawPtr;
  rawPtr->UnRegister();
  return smartPtr;
}

LightObject::Pointer
LightObject::CreateAnother() const
{
  return LightObject::New();
}

LightObject::Pointer
LightObject::InternalClone() const
{
  // nothing to clone in the most basic class of the toolkit.
  // just return a new instance
  return CreateAnother();
}

/**
 * Delete a itk object. This method should always be used to delete an object
 * when the new operator was used to create it. Using the C++ delete method
 * will not work with reference counting.
 */
void
LightObject
::Delete()
{
  this->UnRegister();
}

/**
 * Avoid DLL boundary problems.
 */
#ifdef _WIN32
void *
LightObject
::operator new(size_t n)
{
  return new char[n];
}

void *
LightObject
::operator new[](size_t n)
{
  return new char[n];
}

void
LightObject
::operator delete(void *m)
{
  delete[] (char *)m;
}

void
LightObject
::operator delete[](void *m, size_t)
{
  delete[] (char *)m;
}

#endif

/**
 * This function will be common to all itk objects.  It just calls the
 * header/self/trailer virtual print methods, which can be overriden by
 * subclasses (any itk object).
 */
void
LightObject
::Print(std::ostream & os, Indent indent) const
{
  this->PrintHeader(os, indent);
  this->PrintSelf( os, indent.GetNextIndent() );
  this->PrintTrailer(os, indent);
}

/**
 * This method is called when itkExceptionMacro executes. It allows
 * the debugger to break on error.
 */
void
LightObject
::BreakOnError()
{}

/**
 * Increase the reference count (mark as used by another object).
 */
void
LightObject
::Register() const
{
  ++m_ReferenceCount;
}

/**
 * Decrease the reference count (release by another object).
 */
void
LightObject
::UnRegister() const ITK_NOEXCEPT
{
  // As ReferenceCount gets unlocked, we may have a race condition
  // to delete the object.

  if (  --m_ReferenceCount <= 0 )
    {
    delete this;
    }
}

/**
 * Sets the reference count (use with care)
 */
void
LightObject
::SetReferenceCount(int ref)
{
  m_ReferenceCount = ref;

  if ( ref <= 0 )
    {
    delete this;
    }
}

LightObject
::~LightObject()
{
  /**
   * warn user if reference counting is on and the object is being referenced
   * by another object.
   * a call to uncaught_exception is necessary here to avoid throwing an
   * exception if one has been thrown already. This is likely to
   * happen when a subclass constructor (say B) is throwing an exception: at
   * that point, the stack unwinds by calling all superclass destructors back
   * to this method (~LightObject): since the ref count is still 1, an
   * exception would be thrown again, causing the system to abort()!
   */
  if ( m_ReferenceCount > 0 && !std::uncaught_exception() )
    {
    // A general exception safety rule is that destructors should
    // never throw.  Something is wrong with a program that reaches
    // this point anyway.  Also this is the least-derived class so the
    // whole object has been destroyed by this point anyway.  Just
    // issue a warning.
    // itkExceptionMacro(<< "Trying to delete object with non-zero reference
    // count.");
    itkWarningMacro("Trying to delete object with non-zero reference count.");
    }
}

/**
 * Chaining method to print an object's instance variables, as well as
 * its superclasses.
 */
void
LightObject
::PrintSelf(std::ostream & os, Indent indent) const
{
#ifdef GCC_USEDEMANGLE
  char const *mangledName = typeid( *this ).name();
  int         status;
  char *      unmangled = abi::__cxa_demangle(mangledName, ITK_NULLPTR, ITK_NULLPTR, &status);

  os << indent << "RTTI typeinfo:   ";

  if ( status == 0 )
    {
    os << unmangled;
    free(unmangled);
    }
  else
    {
    os << mangledName;
    }

  os << std::endl;
#else
  os << indent << "RTTI typeinfo:   " << typeid( *this ).name() << std::endl;
#endif
  os << indent << "Reference Count: " << m_ReferenceCount << std::endl;
}

/**
 * Define a default print header for all objects.
 */
void
LightObject
::PrintHeader(std::ostream & os, Indent indent) const
{
  os << indent << this->GetNameOfClass() << " (" << this << ")\n";
}

/**
 * Define a default print trailer for all objects.
 */
void
LightObject
::PrintTrailer( std::ostream & itkNotUsed(os), Indent itkNotUsed(indent) ) const
{}

std::ostream &
operator<<(std::ostream & os, const LightObject & o)
{
  o.Print(os);
  return os;
}
} // end namespace itk
