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

namespace itk
{

/**
 * Instance creation.
 */
Object::Pointer
Object
::New()
{
  Self *ret = ObjectFactory<Self>::Create();
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
Object
::DebugOn()
{
  m_Debug = true;
}


/**
 * Turn debugging output off.
 */
void 
Object
::DebugOff()
{
  m_Debug = false;
}


/**
 * Get the value of the debug flag.
 */
bool 
Object
::GetDebug() const
{
  return m_Debug;
}


/**
 * Set the value of the debug flag. A non-zero value turns debugging on.
 */
void 
Object
::SetDebug(bool debugFlag)
{
  m_Debug = debugFlag;
}


/**
 * Return the modification for this object.
 */
unsigned long 
Object
::GetMTime() 
{
  return m_MTime.GetMTime();
}


/**
 * Make sure this object's modified time is greater than all others.
 */
void 
Object
::Modified()
{
  m_MTime.Modified();
}


/**
 * Increase the reference count (mark as used by another object).
 */
void 
Object
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
Object
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
Object
::SetReferenceCount(int ref)
{
  m_ReferenceCount = ref;
  itkDebugMacro(<< "Reference Count set to " << m_ReferenceCount);
}


/**
 * Set the delete method, and update modification time if needed.
 */
void 
Object
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
Object
::SetGlobalWarningDisplay(bool val)
{
  m_GlobalWarningDisplay = val;
}


/**
 * Get the value of the global debug output control flag.
 */
bool 
Object
::GetGlobalWarningDisplay()
{
  return m_GlobalWarningDisplay;
}


/**
 * Create an object with Debug turned off and modified time initialized 
 * to the most recently modified object.
 */
Object
::Object():
  LightObject(),
  m_Debug(false)
{
  this->Modified();
}


Object
::~Object() 
{
  itkDebugMacro(<< "Destructing!");
}


/**
 * Chaining method to print an object's instance variables, as well as
 * its superclasses.
 */
void 
Object
::PrintSelf(std::ostream& os, Indent indent)
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
Object
::PrintHeader(std::ostream& os, Indent indent)
{
  os << indent << this->GetClassName() << " (" << this << ")\n";
}


/**
 * Define a default print trailer for all objects.
 */
void 
Object
::PrintTrailer(std::ostream& os, Indent indent)
{
  os << indent << std::endl;
}


/**
 * Initialize static member that controls warning display.
 */
bool Object::m_GlobalWarningDisplay = true;

} // end namespace itk
