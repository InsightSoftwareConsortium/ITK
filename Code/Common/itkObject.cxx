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
class Observer
{
public:
  Observer(Command* c, 
           const EventObject * event,
           unsigned long tag) :m_Command(c),
                               m_Event(event),
                               m_Tag(tag)
    { }
  virtual ~Observer() 
  { delete m_Event; }
  Command::Pointer m_Command;
  const EventObject * m_Event;
  unsigned long m_Tag;
};
    
  
class SubjectImplementation
{
public:
  SubjectImplementation() {m_Count = 0;}
  ~SubjectImplementation();
  unsigned long AddObserver(const EventObject & event, Command* cmd);
  void RemoveObserver(unsigned long tag);
  void InvokeEvent( const EventObject & event, Object* self);
  void InvokeEvent( const EventObject & event, const Object* self);
  Command *GetCommand(unsigned long tag);
  bool HasObserver(const EventObject & event) const;
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
AddObserver(const EventObject & event,
	    Command* cmd)
{
  Observer* ptr = new Observer(cmd, event.New(), m_Count);
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
InvokeEvent( const EventObject & event,
	    Object* self)
{
  for(std::list<Observer* >::iterator i = m_Observers.begin();
      i != m_Observers.end(); ++i)
    {
    const EventObject * e =  (*i)->m_Event;
    if(  typeid( *e ) == typeid( AnyEvent ) || 
         typeid( *e ) == typeid(   event  )    )
      {
      (*i)->m_Command->Execute(self, event);
      }
    }
}

void 
SubjectImplementation::
InvokeEvent( const EventObject & event,
	    const Object* self)
{
  for(std::list<Observer* >::iterator i = m_Observers.begin();
      i != m_Observers.end(); ++i)
    {
    const EventObject * e =  (*i)->m_Event;
    if( typeid( *e ) == typeid( AnyEvent ) || 
        typeid( *e ) == typeid(   event  )     )
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
HasObserver(const EventObject & event) const
{
  for(std::list<Observer* >::const_iterator i = m_Observers.begin();
      i != m_Observers.end(); ++i)
    {
    const EventObject * e =  (*i)->m_Event;
    if( typeid( *e ) == typeid( AnyEvent ) || 
        typeid( *e ) == typeid(   event  )     )
      {
      return true;
      }
    }
  return false;
}

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
::DebugOn() const
{
  m_Debug = true;
}


/**
 * Turn debugging output off.
 */
void 
Object
::DebugOff() const
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
::SetDebug(bool debugFlag) const
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
  InvokeEvent( ModifiedEvent() );
}


/**
 * Increase the reference count (mark as used by another object).
 */
void 
Object
::Register() const
{
  itkDebugMacro(<< "Registered, "
                << "ReferenceCount = " << (m_ReferenceCount+1));

  // call the parent
  Superclass::Register();
}


/**
 * Decrease the reference count (release by another object).
 */
void 
Object
::UnRegister() const
{
  // call the parent 
  itkDebugMacro(<< "UnRegistered, "
                << "ReferenceCount = " << (m_ReferenceCount-1));

  if ( (m_ReferenceCount-1) <= 0)
    {
    /**
     * If there is a delete method, invoke it.
     */
    this->InvokeEvent(DeleteEvent());
    }

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

  // ReferenceCount in now unlocked.  We may have a race condition to
  // to delete the object.
  if( ref <= 0 )
    {
    /**
     * If there is a delete method, invoke it.
     */
    this->InvokeEvent(DeleteEvent());
    }

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


unsigned long 
Object
::AddObserver(const EventObject & event, Command *cmd)
{
  if (!this->m_SubjectImplementation)
    {
    this->m_SubjectImplementation = new SubjectImplementation;
    }
  return this->m_SubjectImplementation->AddObserver(event,cmd);
}

unsigned long
Object
::AddObserver(const char *eventname,Command *cmd)
{
  const EventObject * event = 
         EventObject::CreateEventFromString( eventname );
  return this->AddObserver(*event, cmd);
  delete event;
}

Command*
Object
::GetCommand(unsigned long tag)
{
  if (this->m_SubjectImplementation)
    {
    return this->m_SubjectImplementation->GetCommand(tag);
    }
  return NULL;
}

void 
Object
::RemoveObserver(unsigned long tag)
{
  if (this->m_SubjectImplementation)
    {
    this->m_SubjectImplementation->RemoveObserver(tag);
    }
}

void 
Object
::InvokeEvent( const EventObject & event )
{
  if (this->m_SubjectImplementation)
    {
    this->m_SubjectImplementation->InvokeEvent(event,this);
    }
}


void 
Object
::InvokeEvent( const EventObject & event ) const
{
  if (this->m_SubjectImplementation)
    {
    this->m_SubjectImplementation->InvokeEvent(event,this);
    }
}

void 
Object
::InvokeEvent(const char *eventname )
{
  const EventObject * event = 
            EventObject::CreateEventFromString( eventname );
  this->InvokeEvent( *event );
  delete event; 
}

void 
Object
::InvokeEvent(const char *eventname) const
{
  const EventObject * event = 
            EventObject::CreateEventFromString( eventname );
  this->InvokeEvent( *event );
  delete event; 
}

bool
Object
::HasObserver( const EventObject & event ) const
{
  if (this->m_SubjectImplementation)
    {
    return this->m_SubjectImplementation->HasObserver(event);
    }
  return false;
}

bool
Object
::HasObserver( const char *eventname ) const
{
  const EventObject * event = 
            EventObject::CreateEventFromString( eventname );

  const bool result = this->HasObserver( *event );
  
  delete event; 
}

/**
 * Create an object with Debug turned off and modified time initialized 
 * to the most recently modified object.
 */
Object
::Object():
  LightObject(),
  m_Debug(false),
  m_SubjectImplementation(0)
{
  this->Modified();
}


Object
::~Object() 
{
  itkDebugMacro(<< "Destructing!");
  delete m_SubjectImplementation;
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
  os << indent << "Debug: " << (m_Debug ? "On\n" : "Off\n");
  os << indent << "Instance has ";
  if ( this->HasObserver( AnyEvent() ) )
    {
    os << "one or more observers\n";
    }
  else
    {
    os << "no observers\n";
    }
}

/**
 * Initialize static member that controls warning display.
 */
bool Object::m_GlobalWarningDisplay = true;

} // end namespace itk
