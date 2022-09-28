/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#include "itkCommand.h"
#include <algorithm>
#include <memory> // For unique_ptr.

#include "itkSingleton.h"

namespace itk
{
/**
 * Initialize static member that controls warning display.
 */
itkGetGlobalValueMacro(Object, bool, GlobalWarningDisplay, true);

bool * Object::m_GlobalWarningDisplay;

namespace
{
class ITKCommon_HIDDEN Observer
{
public:
  Observer(Command * c, const EventObject * event, unsigned long tag)
    : m_Command(c)
    , m_Event(event)
    , m_Tag(tag)
  {}
  Command::Pointer                   m_Command;
  std::unique_ptr<const EventObject> m_Event;
  unsigned long                      m_Tag;
};
} // namespace

class ITKCommon_HIDDEN Object::SubjectImplementation
{
public:
  SubjectImplementation() { m_Count = 0; }
  ~SubjectImplementation() = default;

  unsigned long
  AddObserver(const EventObject & event, Command * cmd);

  void
  RemoveObserver(unsigned long tag);

  void
  RemoveAllObservers();

  void
  InvokeEvent(const EventObject & event, Object * self);

  void
  InvokeEvent(const EventObject & event, const Object * self);

  Command *
  GetCommand(unsigned long tag);

  bool
  HasObserver(const EventObject & event) const;

  bool
  PrintObservers(std::ostream & os, Indent indent) const;

  bool m_ListModified{ false };

private:
  // RAII of ListModified state to ensure exception safety
  struct SaveRestoreListModified
  {
    // save the list modified flag, and reset to false
    SaveRestoreListModified(SubjectImplementation * s)
      : m_Subject(s)
      , m_Save(s->m_ListModified)
    {
      m_Subject->m_ListModified = false;
    }

    // restore modify flag, and propagate if modified
    ~SaveRestoreListModified() { m_Subject->m_ListModified = m_Save || m_Subject->m_ListModified; }

    SubjectImplementation * m_Subject;
    bool                    m_Save;
  };

  template <typename TObject>
  void
  InvokeEventRecursion(const EventObject & event, TObject * self, std::list<Observer>::const_reverse_iterator & i);

  std::list<Observer> m_Observers;
  unsigned long       m_Count;
};

unsigned long
Object::SubjectImplementation::AddObserver(const EventObject & event, Command * cmd)
{
  const unsigned long tag{ m_Count };
  m_Observers.emplace_back(cmd, event.MakeObject(), tag);
  ++m_Count;
  return tag;
}

void
Object::SubjectImplementation::RemoveObserver(unsigned long tag)
{
  for (auto i = m_Observers.begin(); i != m_Observers.end(); ++i)
  {
    if (i->m_Tag == tag)
    {
      m_Observers.erase(i);
      m_ListModified = true;
      return;
    }
  }
}

void
Object::SubjectImplementation::RemoveAllObservers()
{
  m_Observers.clear();
  m_ListModified = true;
}

void
Object::SubjectImplementation::InvokeEvent(const EventObject & event, Object * self)
{
  // While an event is being invoked, it's possible to remove
  // observers, or another event to be invoked. All methods which
  // remove observers mark the list as modified with the
  // m_ListModified flag. The modified flag is save to the stack and
  // marked false before recursively saving the current list.

  SaveRestoreListModified save(this);

  auto i = m_Observers.crbegin();
  InvokeEventRecursion(event, self, i);
}

void
Object::SubjectImplementation::InvokeEvent(const EventObject & event, const Object * self)
{
  SaveRestoreListModified save(this);

  auto i = m_Observers.crbegin();
  InvokeEventRecursion(event, self, i);
}

template <typename TObject>
void
Object::SubjectImplementation::InvokeEventRecursion(const EventObject &                           event,
                                                    TObject *                                     self,
                                                    std::list<Observer>::const_reverse_iterator & i)
{
  // This method recursively visits the list of observers in reverse
  // order so that on the last recursion the first observer can be
  // executed. Each iteration saves the list element on the
  // stack. Each observer's execution could potentially modify the
  // observer list, by placing the entire list on the stack we save the
  // list when the event is first invoked. If observers are removed
  // during execution, then the current list is search for the current
  // observer save on the stack.

  while (i != m_Observers.rend())
  {

    // save observer
    const Observer & o = *i;

    // Save its tag, before the observer could /possibly/ be removed.
    const unsigned long tag{ o.m_Tag };

    if (o.m_Event->CheckEvent(&event))
    {
      InvokeEventRecursion(event, self, ++i);

      const auto hasSameTag = [tag](const Observer & observer) { return observer.m_Tag == tag; };

      if (!m_ListModified || std::any_of(m_Observers.begin(), m_Observers.end(), hasSameTag))
      {
        o.m_Command->Execute(self, event);
      }

      return;
    }

    ++i;
  }

  return;
}

Command *
Object::SubjectImplementation::GetCommand(unsigned long tag)
{
  for (const auto & observer : m_Observers)
  {
    if (observer.m_Tag == tag)
    {
      return observer.m_Command;
    }
  }
  return nullptr;
}

bool
Object::SubjectImplementation::HasObserver(const EventObject & event) const
{
  for (const auto & observer : m_Observers)
  {
    const EventObject * e = observer.m_Event.get();
    if (e->CheckEvent(&event))
    {
      return true;
    }
  }
  return false;
}

bool
Object::SubjectImplementation::PrintObservers(std::ostream & os, Indent indent) const
{
  if (m_Observers.empty())
  {
    return false;
  }

  for (const auto & observer : m_Observers)
  {
    const EventObject * e = observer.m_Event.get();
    const Command *     c = observer.m_Command;
    os << indent << e->GetEventName() << "(" << c->GetNameOfClass();
    if (!c->GetObjectName().empty())
    {
      os << " \"" << c->GetObjectName() << "\"";
    }
    os << ")\n";
  }
  return true;
}

Object::Pointer
Object::New()
{
  Pointer  smartPtr;
  Object * rawPtr = itk::ObjectFactory<Object>::Create();

  if (rawPtr == nullptr)
  {
    rawPtr = new Object;
  }
  smartPtr = rawPtr;
  rawPtr->UnRegister();
  return smartPtr;
}

LightObject::Pointer
Object::CreateAnother() const
{
  return Object::New().GetPointer();
}

/**
 * Turn debugging output on.
 */
void
Object::DebugOn() const
{
  m_Debug = true;
}

/**
 * Turn debugging output off.
 */
void
Object::DebugOff() const
{
  m_Debug = false;
}

/**
 * Get the value of the debug flag.
 */
bool
Object::GetDebug() const
{
  return m_Debug;
}

/**
 * Set the value of the debug flag. A non-zero value turns debugging on.
 */
void
Object::SetDebug(bool debugFlag) const
{
  m_Debug = debugFlag;
}

/**
 * Return the modification for this object.
 */
ModifiedTimeType
Object::GetMTime() const
{
  return m_MTime.GetMTime();
}

/**
 * Return the modification for this object.
 */
const TimeStamp &
Object::GetTimeStamp() const
{
  return m_MTime;
}

/** Set the time stamp of this object. To be used very carefully !!!.
 * Most mortals will never need to call this method. */
void
Object::SetTimeStamp(const TimeStamp & timeStamp)
{
  this->m_MTime = timeStamp;
}

/**
 * Make sure this object's modified time is greater than all others.
 */
void
Object::Modified() const
{
  m_MTime.Modified();
  InvokeEvent(ModifiedEvent());
}

/**
 * Increase the reference count (mark as used by another object).
 */
void
Object::Register() const
{
  itkDebugMacro(<< "Registered, "
                << "ReferenceCount = " << (m_ReferenceCount + 1));

  // call the parent
  Superclass::Register();
}

/**
 * Decrease the reference count (release by another object).
 */
void
Object::UnRegister() const noexcept
{
  // call the parent
  itkDebugMacro(<< "UnRegistered, "
                << "ReferenceCount = " << (m_ReferenceCount - 1));

  if ((m_ReferenceCount - 1) <= 0)
  {
    /**
     * If there is a delete method, invoke it.
     */
    try
    {
      this->InvokeEvent(DeleteEvent());
    }
    catch (...)
    {
      // The macro is not use to avoid a memory allocation, and reduce
      // potential exceptions.
      // itkWarningMacro("Exception occurred in DeleteEvent Observer!");
      try
      {
        if (GetGlobalWarningDisplay())
        {
          itk::OutputWindowDisplayWarningText("WARNING: Exception occurred in DeleteEvent Observer!");
        }
      }
      catch (...)
      {
        // ignore exception in warning display
      }
    }
  }

  Superclass::UnRegister();
}

/**
 * Sets the reference count (use with care)
 */
void
Object::SetReferenceCount(int ref)
{
  itkDebugMacro(<< "Reference Count set to " << ref);

  // ReferenceCount in now unlocked.  We may have a race condition to
  // to delete the object.
  if (ref <= 0)
  {
    /**
     * If there is a delete method, invoke it.
     */
    try
    {
      this->InvokeEvent(DeleteEvent());
    }
    catch (...)
    {
      itkWarningMacro("Exception occurred in DeleteEvent Observer!");
    }
  }

  Superclass::SetReferenceCount(ref);
}

/**
 * Set the value of the global debug output control flag.
 */
void
Object::SetGlobalWarningDisplay(bool val)
{
  itkInitGlobalsMacro(GlobalWarningDisplay);
  *m_GlobalWarningDisplay = val;
}

/**
 * Get the value of the global debug output control flag.
 */
bool
Object::GetGlobalWarningDisplay()
{
  return *Object::GetGlobalWarningDisplayPointer();
}

unsigned long
Object::AddObserver(const EventObject & event, Command * cmd)
{
  const auto & thisAsConst = *this;
  return thisAsConst.AddObserver(event, cmd);
}

unsigned long
Object::AddObserver(const EventObject & event, Command * cmd) const
{
  if (!this->m_SubjectImplementation)
  {
    this->m_SubjectImplementation = std::make_unique<SubjectImplementation>();
  }
  return this->m_SubjectImplementation->AddObserver(event, cmd);
}

unsigned long
Object::AddObserver(const EventObject & event, std::function<void(const EventObject &)> function) const
{
  auto cmd = FunctionCommand::New();
  cmd->SetCallback(std::move(function));
  return this->AddObserver(event, cmd);
}


Command *
Object::GetCommand(unsigned long tag)
{
  if (this->m_SubjectImplementation)
  {
    return this->m_SubjectImplementation->GetCommand(tag);
  }
  return nullptr;
}

void
Object::RemoveObserver(unsigned long tag)
{
  if (this->m_SubjectImplementation)
  {
    this->m_SubjectImplementation->RemoveObserver(tag);
  }
}

void
Object::RemoveAllObservers()
{
  if (this->m_SubjectImplementation)
  {
    this->m_SubjectImplementation->RemoveAllObservers();
  }
}

void
Object::InvokeEvent(const EventObject & event)
{
  if (this->m_SubjectImplementation)
  {
    this->m_SubjectImplementation->InvokeEvent(event, this);
  }
}

void
Object::InvokeEvent(const EventObject & event) const
{
  if (this->m_SubjectImplementation)
  {
    this->m_SubjectImplementation->InvokeEvent(event, this);
  }
}

bool
Object::HasObserver(const EventObject & event) const
{
  if (this->m_SubjectImplementation)
  {
    return this->m_SubjectImplementation->HasObserver(event);
  }
  return false;
}

bool
Object::PrintObservers(std::ostream & os, Indent indent) const
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
Object::Object()
  : LightObject()
  , m_ObjectName()
{
  this->Modified();
}

Object::~Object()
{
  itkDebugMacro(<< "Destructing!");
}

/**
 * Chaining method to print an object's instance variables, as well as
 * its superclasses.
 */
void
Object::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Modified Time: " << this->GetMTime() << std::endl;
  os << indent << "Debug: " << (m_Debug ? "On\n" : "Off\n");
  os << indent << "Object Name: " << this->GetObjectName() << std::endl;
  os << indent << "Observers: \n";
  if (!this->PrintObservers(os, indent.GetNextIndent()))
  {
    os << indent.GetNextIndent() << "none\n";
  }
}

MetaDataDictionary &
Object::GetMetaDataDictionary()
{
  if (m_MetaDataDictionary == nullptr)
  {
    m_MetaDataDictionary = std::make_unique<MetaDataDictionary>();
  }
  return *m_MetaDataDictionary;
}

const MetaDataDictionary &
Object::GetMetaDataDictionary() const
{
  if (m_MetaDataDictionary == nullptr)
  {
    m_MetaDataDictionary = std::make_unique<MetaDataDictionary>();
  }
  return *m_MetaDataDictionary;
}

void
Object::SetMetaDataDictionary(const MetaDataDictionary & rhs)
{
  if (m_MetaDataDictionary == nullptr)
  {
    m_MetaDataDictionary = std::make_unique<MetaDataDictionary>(rhs);
    return;
  }
  *m_MetaDataDictionary = rhs;
}

void
Object::SetMetaDataDictionary(MetaDataDictionary && rrhs)
{
  if (m_MetaDataDictionary == nullptr)
  {
    m_MetaDataDictionary = std::make_unique<MetaDataDictionary>(std::move(rrhs));
    return;
  }
  *m_MetaDataDictionary = std::move(rrhs);
}

} // end namespace itk
