/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkObject.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "itkObject.h"
#include "itkObjectFactory.h"

// Initialize static member that controls warning display
static bool itkObjectGlobalWarningDisplay = 1;

void 
itkObject
::SetGlobalWarningDisplay(bool val)
{
  itkObjectGlobalWarningDisplay = val;
}

bool 
itkObject
::GetGlobalWarningDisplay()
{
  return itkObjectGlobalWarningDisplay;
}

// avoid dll boundary problems
#ifdef _WIN32
void* 
itkObject
::operator new(size_t nSize, const char *, int)
{
  void* p=malloc(nSize);
  return p;
}

void* 
itkObject
::operator new(size_t nSize)
{
  void* p=malloc(nSize);
  return p;
}

void 
itkObject
::operator delete( void *p )
{
  free(p);
}
#endif 

itkObject::Pointer 
itkObject
::New()
{
  itkObject *ret = itkObjectFactory<itkObject>::Create();
  if ( ret )
    {
    return ret;
    }
  return new itkObject;
}

// Create an object with Debug turned off and modified time initialized 
// to zero.
itkObject
::itkObject()
{
  m_Debug = 0;
  this->Modified(); // Insures modified time > than any other time
  // initial reference count = 0 because smart pointer immediately increments it
  m_ReferenceCount = 0;
  m_DeleteMethod = NULL;
}

// Delete a itk object. This method should always be used to delete an object 
// when the new operator was used to create it. Using the C++ delete method
// will not work with reference counting.
void 
itkObject
::Delete() 
{
  this->UnRegister();
}

itkObject
::~itkObject() 
{
  itkDebugMacro(<< "Destructing!");

  // warn user if reference counting is on and the object is being referenced
  // by another object
  if ( m_ReferenceCount > 0)
    {
    itkErrorMacro(<< "Trying to delete object with non-zero reference count.");
    }
}

// This operator allows all subclasses of itkObject to be printed via <<.
// It in turn invokes the Print method, which in turn will invoke the
// PrintSelf method that all objects should define, if they have anything
// interesting to print out.
std::ostream& 
operator<<(std::ostream& os, itkObject& o)
{
  o.Print(os);
  return os;
}

void 
itkObject
::Print(std::ostream& os)
{
  itkIndent indent;

  this->PrintHeader(os,0); 
  this->PrintSelf(os, indent.GetNextIndent());
  this->PrintTrailer(os,0);
}

void 
itkObject
::PrintHeader(std::ostream& os, itkIndent indent)
{
  os << indent << this->GetClassName() << " (" << this << ")\n";
}

void 
itkObject
::PrintTrailer(std::ostream& os, itkIndent indent)
{
  os << indent << std::endl;
}

void 
itkObject
::SetDeleteMethod(void (*f)(void *))
{
  if (f != m_DeleteMethod)
    {
    m_DeleteMethod = f;
    this->Modified();
    }
}

// Chaining method to print an object's instance variables, as well as
// its superclasses.
void 
itkObject
::PrintSelf(std::ostream& os, itkIndent indent)
{
  os << indent << "m_Debug: " << (m_Debug ? "On\n" : "Off\n");

  if ( m_DeleteMethod )
    {
    os << indent << "Delete Method defined" << std::endl;
    }
  else
    {
    os << indent <<"No Delete Method" << std::endl;
    }
  os << indent << "Modified Time: " << this->GetMTime() << std::endl;
  os << indent << "Reference Count: " << m_ReferenceCount << std::endl;
}

// Return the modification for this object.
unsigned long 
itkObject
::GetMTime() 
{
  return m_MTime.GetMTime();
}

// Turn debugging output on.
void 
itkObject
::DebugOn()
{
  m_Debug = 1;
}

// Turn debugging output off.
void 
itkObject
::DebugOff()
{
  m_Debug = 0;
}

// Get the value of the debug flag.
bool 
itkObject
::GetDebug() const
{
  return m_Debug;
}

// Set the value of the debug flag. A non-zero value turns debugging on.
void 
itkObject
::SetDebug(bool debugFlag)
{
  m_Debug = debugFlag;
}


// This method is called when itkErrorMacro executes. It allows 
// the debugger to break on error.
void 
itkObject
::BreakOnError()
{
  ;
}

// Sets the reference count (use with care)
void 
itkObject
::SetReferenceCount(int ref)
{
  m_ReferenceCount = ref;
  itkDebugMacro(<< "Reference Count set to " << m_ReferenceCount);
}

// Increase the reference count (mark as used by another object).
void 
itkObject
::Register()
{
  m_ReferenceCount++;
  itkDebugMacro(<< "Registered, "
                << "ReferenceCount = " << m_ReferenceCount);

  if (m_ReferenceCount <= 0)
    {
    delete this;
    }
}

// Decrease the reference count (release by another object).
void 
itkObject
::UnRegister()
{
  itkDebugMacro(<< this << "UnRegistered, "
  << "ReferenceCount = " << (m_ReferenceCount-1));

  if (--m_ReferenceCount <= 0)
    {
    // invoke the delete method
    if ( m_DeleteMethod )
      {
      (*m_DeleteMethod)(this);
      }
    delete this;
    }
}

void 
itkObject
::Modified()
{
  m_MTime.Modified();
}

