/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLightObject.cxx
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
#include "itkLightObject.h"
#include "itkObjectFactory.h"
#include "itkCommand.h"
#include <list>
#include <memory>

namespace itk
{
class Observer
{
public:
  Observer(Command* c, 
           unsigned long event,
           unsigned long tag) :m_Command(c),
                               m_Event(event),
                               m_Tag(tag)
    { }
  Command::Pointer m_Command;
  unsigned long m_Event;
  unsigned long m_Tag;
};
    
  
class SubjectImplementation
{
public:
  SubjectImplementation() {m_Count = 0;}
  ~SubjectImplementation();
  unsigned long AddObserver(unsigned long event, Command* cmd);
  void RemoveObserver(unsigned long tag);
  void InvokeEvent(unsigned long event, LightObject* self);
  void InvokeEvent(unsigned long event, const LightObject* self);
  Command *GetCommand(unsigned long tag);
  bool HasObserver(unsigned long event);
private:
  std::list<Observer* > m_Observers;
  unsigned long m_Count;
};

SubjectImplementation::
~SubjectImplementation()
{
  for(std::list<Observer* >::iterator i = m_Observers.begin();
      i != m_Observers.end(); ++i)
    {
    delete (*i);
    }
}


unsigned long 
SubjectImplementation::
AddObserver(unsigned long event,
	    Command* cmd)
{
  Observer* ptr = new Observer(cmd, event, m_Count);
  m_Observers.push_back(ptr);
  m_Count++;
  return ptr->m_Tag;
}


void
SubjectImplementation::
RemoveObserver(unsigned long tag)
{
  for(std::list<Observer* >::iterator i = m_Observers.begin();
      i != m_Observers.end(); ++i)
    {
    if((*i)->m_Tag == tag)
      {
      delete (*i);
      m_Observers.erase(i);
      return;
      }
    }
}


void 
SubjectImplementation::
InvokeEvent(unsigned long event,
	    LightObject* self)
{
  for(std::list<Observer* >::iterator i = m_Observers.begin();
      i != m_Observers.end(); ++i)
    {
    unsigned long e =  (*i)->m_Event;
    if( e == Command::AnyEvent || e == event)
      {
      (*i)->m_Command->Execute(self, event);
      }
    }
}

void 
SubjectImplementation::
InvokeEvent(unsigned long event,
	    const LightObject* self)
{
  for(std::list<Observer* >::iterator i = m_Observers.begin();
      i != m_Observers.end(); ++i)
    {
    unsigned long e =  (*i)->m_Event;
    if( e == Command::AnyEvent || e == event)
      {
      (*i)->m_Command->Execute(self, event);
      }
    }
}


Command*
SubjectImplementation::
GetCommand(unsigned long tag)
{
  for(std::list<Observer* >::iterator i = m_Observers.begin();
      i != m_Observers.end(); ++i)
    {
    if ( (*i)->m_Tag == tag)
      {
      return (*i)->m_Command;
      }
    }
  return 0;
}

bool
SubjectImplementation::
HasObserver(unsigned long event)
{
  for(std::list<Observer* >::iterator i = m_Observers.begin();
      i != m_Observers.end(); ++i)
    {
    unsigned long e =  (*i)->m_Event;
    if( e == Command::AnyEvent || e == event)
      {
      return true;
      }
    }
  return false;
}




  
    
/**
 * Instance creation.
 */
LightObject::Pointer
LightObject
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
void*
LightObject
::operator new(size_t n)
{
  return new char[n];
}

void*
LightObject
::operator new[](size_t n)
{
  return new char[n];
}

void
LightObject
::operator delete(void* m)
{
  delete [] (char*)m;
}

void
LightObject
::operator delete[](void* m, size_t)
{
  delete [] (char*)m;
}
#endif 


/**
 * This function will be common to all itk objects.  It just calls the
 * header/self/trailer virtual print methods, which can be overriden by
 * subclasses (any itk object).
 */
void 
LightObject
::Print(std::ostream& os) const
{
  Indent indent;

  this->PrintHeader(os,0); 
  this->PrintSelf(os, indent.GetNextIndent());
  this->PrintTrailer(os,0);
}


/**
 * This method is called when itkErrorMacro executes. It allows 
 * the debugger to break on error.
 */
void 
LightObject
::BreakOnError()
{
  ;  
}


/**
 * Increase the reference count (mark as used by another object).
 */
void 
LightObject
::Register() const
{
  m_ReferenceCount++;
}


/**
 * Decrease the reference count (release by another object).
 */
void 
LightObject
::UnRegister() const
{
  if(--m_ReferenceCount <= 0)
    {
    /**
     * If there is a delete method, invoke it.
     */
    this->InvokeEvent(Command::DeleteEvent);
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
  if(m_ReferenceCount <= 0)
    {
    /**
     * If there is a delete method, invoke it.
     */
    this->InvokeEvent(Command::DeleteEvent);
    delete this;
    }
}




/**
 * Create an object with a reference count of 1.
 */
LightObject
::LightObject():
  /**
   * Initial ref count = 1 during construction.  After the object is assigned
   * to a smart pointer, this extra initial reference count is removed.
   * See itkNewMacro.
   */
  m_ReferenceCount(1),
  m_SubjectImplementation(0)
{
}


LightObject
::~LightObject() 
{
  /**
   * warn user if reference counting is on and the object is being referenced
   * by another object
   */
  if ( m_ReferenceCount > 0)
    {
    itkErrorMacro(<< "Trying to delete object with non-zero reference count.");
    }
  delete m_SubjectImplementation;
}


/**
 * Chaining method to print an object's instance variables, as well as
 * its superclasses.
 */
void 
LightObject
::PrintSelf(std::ostream& os, Indent indent) const
{
  os << indent << "Reference Count: " << m_ReferenceCount << std::endl;
}


/**
 * Define a default print header for all objects.
 */
void 
LightObject
::PrintHeader(std::ostream& os, Indent indent) const
{
  os << indent << this->GetNameOfClass() << " (" << this << ")\n";
}


/**
 * Define a default print trailer for all objects.
 */
void 
LightObject
::PrintTrailer(std::ostream& os, Indent indent) const
{
  os << indent << std::endl;
}


/**
 * This operator allows all subclasses of LightObject to be printed via <<.
 * It in turn invokes the Print method, which in turn will invoke the
 * PrintSelf method that all objects should define, if they have anything
 * interesting to print out.
 */
std::ostream& 
operator<<(std::ostream& os, const LightObject& o)
{
  o.Print(os);
  return os;
}

unsigned long 
LightObject
::AddObserver(unsigned long event, Command *cmd)
{
  if (!this->m_SubjectImplementation)
    {
    this->m_SubjectImplementation = new SubjectImplementation;
    }
  return this->m_SubjectImplementation->AddObserver(event,cmd);
}

unsigned long
LightObject
::AddObserver(const char *event,Command *cmd)
{
  return this->AddObserver(Command::GetEventIdFromString(event), cmd);
}

Command*
LightObject
::GetCommand(unsigned long tag)
{
  if (this->m_SubjectImplementation)
    {
    return this->m_SubjectImplementation->GetCommand(tag);
    }
  return NULL;
}

void 
LightObject
::RemoveObserver(unsigned long tag)
{
  if (this->m_SubjectImplementation)
    {
    this->m_SubjectImplementation->RemoveObserver(tag);
    }
}

void 
LightObject
::InvokeEvent(unsigned long event)
{
  if (this->m_SubjectImplementation)
    {
    this->m_SubjectImplementation->InvokeEvent(event,this);
    }
}


void 
LightObject
::InvokeEvent(unsigned long event) const
{
  if (this->m_SubjectImplementation)
    {
    this->m_SubjectImplementation->InvokeEvent(event,this);
    }
}



void 
LightObject
::InvokeEvent(const char *event)
{
  this->InvokeEvent(Command::GetEventIdFromString(event));
}


void 
LightObject
::InvokeEvent(const char *event) const
{
  this->InvokeEvent(Command::GetEventIdFromString(event));
}



bool
LightObject
::HasObserver(unsigned long event)
{
  if (this->m_SubjectImplementation)
    {
    return this->m_SubjectImplementation->HasObserver(event);
    }
  return 0;
}

bool
LightObject
::HasObserver(const char *event)
{
  return this->HasObserver(Command::GetEventIdFromString(event));
}


} // end namespace itk
