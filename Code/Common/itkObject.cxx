/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkObject.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#include "itkObject.h"
#include "itkObjectFactory.h"
#include "itkCommand.h"

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
  if(ret)
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
::GetMTime() const
{
  return m_MTime.GetMTime();
}


/**
 * Make sure this object's modified time is greater than all others.
 */
void 
Object
::Modified() const
{
  m_MTime.Modified();
  InvokeEvent( Command::ModifiedEvent );
}


/**
 * Increase the reference count (mark as used by another object).
 */
void 
Object
::Register() const
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
::UnRegister() const
{
  itkDebugMacro(<< this << "UnRegistered, "
  << "ReferenceCount = " << (m_ReferenceCount-1));
// call the parent
  Superclass::UnRegister();
}


/**
 * Sets the reference count (use with care)
 */
void 
Object
::SetReferenceCount(int ref)
{
  itkDebugMacro(<< "Reference Count set to " << ref);
  // call the parent
  Superclass::SetReferenceCount(ref);
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
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Modified Time: " << this->GetMTime() << std::endl;
  os << indent << "m_Debug: " << (m_Debug ? "On\n" : "Off\n");
}

/**
 * Define a default print header for all objects.
 */
void 
Object
::PrintHeader(std::ostream& os, Indent indent) const
{
  os << indent << this->GetClassName() << " (" << this << ")\n";
}


/**
 * Define a default print trailer for all objects.
 */
void 
Object
::PrintTrailer(std::ostream& os, Indent indent) const
{
  os << indent << std::endl;
}


/**
 * Initialize static member that controls warning display.
 */
bool Object::m_GlobalWarningDisplay = true;

} // end namespace itk
