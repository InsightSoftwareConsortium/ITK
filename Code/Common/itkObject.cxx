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


/**
 * Define the object factory construction method.
 */
itkObject::Pointer 
itkObject
::New()
{
  Self *ret = itkObjectFactory<Self>::Create();
  if(ret != NULL)
    {
    return ret;
    }
  return new Self;
}


/**
 * Turn debugging output on.
 */
void 
itkObject
::DebugOn()
{
  m_Debug = true;
}


/**
 * Turn debugging output off.
 */
void 
itkObject
::DebugOff()
{
  m_Debug = false;
}


/**
 * Get the value of the debug flag.
 */
bool 
itkObject
::GetDebug() const
{
  return m_Debug;
}


/**
 * Set the value of the debug flag. A non-zero value turns debugging on.
 */
void 
itkObject
::SetDebug(bool debugFlag)
{
  m_Debug = debugFlag;
}


/**
 * Return the modification for this object.
 */
unsigned long 
itkObject
::GetMTime() 
{
  return m_MTime.GetMTime();
}


/**
 * Make sure this object's modified time is greater than all others.
 */
void 
itkObject
::Modified()
{
  m_MTime.Modified();
}


/**
 * Increase the reference count (mark as used by another object).
 */
void 
itkObject
::Register()
{
  m_ReferenceCount++;
  itkDebugMacro(<< "Registered, "
                << "ReferenceCount = " << m_ReferenceCount);

  if(m_ReferenceCount <= 0)
    {
    delete this;
    }
}


/**
 * Decrease the reference count (release by another object).
 */
void 
itkObject
::UnRegister()
{
  itkDebugMacro(<< this << "UnRegistered, "
  << "ReferenceCount = " << (m_ReferenceCount-1));

  if(--m_ReferenceCount <= 0)
    {
    /**
     * If there is a delete method, invoke it.
     */
    if(m_DeleteMethod != NULL)
      {
      (*m_DeleteMethod)(this);
      }
    delete this;
    }
}


/**
 * Sets the reference count (use with care)
 */
void 
itkObject
::SetReferenceCount(int ref)
{
  m_ReferenceCount = ref;
  itkDebugMacro(<< "Reference Count set to " << m_ReferenceCount);
}


/**
 * Set the delete method, and update modification time if needed.
 */
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


/**
 * Set the value of the global debug output control flag.
 */
void 
itkObject
::SetGlobalWarningDisplay(bool val)
{
  m_GlobalWarningDisplay = val;
}


/**
 * Get the value of the global debug output control flag.
 */
bool 
itkObject
::GetGlobalWarningDisplay()
{
  return m_GlobalWarningDisplay;
}


/**
 * Create an object with Debug turned off and modified time initialized 
 * to the most recently modified object.
 */
itkObject
::itkObject():
  itkLightObject(),
  m_Debug(false)
{
  this->Modified();
}


itkObject
::~itkObject() 
{
  itkDebugMacro(<< "Destructing!");
}


/**
 * Chaining method to print an object's instance variables, as well as
 * its superclasses.
 */
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


/**
 * Define a default print header for all objects.
 */
void 
itkObject
::PrintHeader(std::ostream& os, itkIndent indent)
{
  os << indent << this->GetClassName() << " (" << this << ")\n";
}


/**
 * Define a default print trailer for all objects.
 */
void 
itkObject
::PrintTrailer(std::ostream& os, itkIndent indent)
{
  os << indent << std::endl;
}


/**
 * Initialize static member that controls warning display.
 */
bool itkObject::m_GlobalWarningDisplay = true;

