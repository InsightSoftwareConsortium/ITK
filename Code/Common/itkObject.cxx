/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkObject.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

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
  bool PrintObservers(std::ostream& os, Indent indent) const;
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
  Observer* ptr = new Observer(cmd, event.MakeObject(), m_Count);
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
    if(e->CheckEvent(&event))
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
    if(e->CheckEvent(&event))
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
    if(e->CheckEvent(&event))
      {
      return true;
      }
    }
  return false;
}

bool
SubjectImplementation::
PrintObservers(std::ostream& os, Indent indent) const
{
  if(m_Observers.empty())
    {
    return false;
    }
    
  for(std::list<Observer* >::const_iterator i = m_Observers.begin();
      i != m_Observers.end(); ++i)
    {
    const EventObject * e =  (*i)->m_Event;
    const Command* c = (*i)->m_Command;
    os << indent << e->GetEventName() << "(" << c->GetNameOfClass() << ")\n";
    }
  return true;
}

Object::Pointer
Object::New()
{
  Pointer smartPtr;
  Object *rawPtr = ::itk::ObjectFactory<Object>::Create();
  if(rawPtr == NULL)
    {
    rawPtr = new Object;
    }
  smartPtr = rawPtr;
  rawPtr->UnRegister();
  return smartPtr;
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
::PrintObservers(std::ostream& os, Indent indent) const
{
  if (this->m_SubjectImplementation)
    {
    return this->m_SubjectImplementation->PrintObservers(os, indent);
    }
  return false;
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
  os << indent << "Observers: \n";
  if(!this->PrintObservers(os, indent.GetNextIndent()))
    {
    os << indent.GetNextIndent() << "none\n";
    }
}

/**
 * Initialize static member that controls warning display.
 */
bool Object::m_GlobalWarningDisplay = true;

//Added By Hans J. Johnson
itk::MetaDataDictionary &
Object
::GetMetaDataDictionary(void)
{
  return m_MetaDataDictionary;
}

//Added By Hans J. Johnson
const itk::MetaDataDictionary &
Object
::GetMetaDataDictionary(void) const
{
  return m_MetaDataDictionary;
}

} // end namespace itk
