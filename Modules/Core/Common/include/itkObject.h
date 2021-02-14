/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkObject_h
#define itkObject_h

#include "itkLightObject.h"
#include "itkEventObject.h"
#include "itkMetaDataDictionary.h"
#include "itkSingletonMacro.h"

#include <functional>

namespace itk
{
// Forward reference because of private implementation
class SubjectImplementation;
// Forward reference because of circular dependencies
class ITK_FORWARD_EXPORT Command;

/** \class Object
 * \brief Base class for most ITK classes.
 *
 * Object is the second-highest level base class for most itk objects.
 * It extends the base object functionality of LightObject by
 * implementing callbacks (via object/observer), debug flags/methods,
 * and modification time tracking. Most ITK classes should be a subclass
 * of Object due to the need to keep track of modified time.
 *
 * \ingroup ITKSystemObjects
 * \ingroup DataRepresentation
 * \ingroup ITKCommon
 *
 * \sphinx
 * \sphinxexample{Core/Common/CreateAnother,Copy Another}
 * \endsphinx
 */
class ITKCommon_EXPORT Object : public LightObject
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(Object);

  /** Smart pointer type alias support */
  using Self = Object;
  using Superclass = LightObject;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  static Pointer
  New();

  /** Create an object from an instance, potentially deferring to a
   * factory.  This method allows you to create an instance of an
   * object that is exactly the same type as the referring object.
   * This is useful in cases where an object has been cast back to a
   * base class. */
  LightObject::Pointer
  CreateAnother() const override;

  /** Standard part of all itk objects. */
  itkTypeMacro(Object, LightObject);

  /** Turn debugging output on.  */
  virtual void
  DebugOn() const;

  /** Turn debugging output off.  */
  virtual void
  DebugOff() const;

  /** Get the value of the debug flag.  */
  bool
  GetDebug() const;

  /** Set the value of the debug flag. A non-zero value turns debugging on. */
  void
  SetDebug(bool debugFlag) const;

  /** Return this object's modified time.  */
  virtual ModifiedTimeType
  GetMTime() const;

  /** Return this object's time stamp.  */
  virtual const TimeStamp &
  GetTimeStamp() const;

  /** Update the modification time for this object. Many filters rely on the
   * modification time to determine if they need to recompute their data.  */
  virtual void
  Modified() const;

  /** Increase the reference count (mark as used by another object).  */
  void
  Register() const override;

  /** Decrease the reference count (release by another object).  */
  void
  UnRegister() const noexcept override;

  /** Sets the reference count (use with care)  */
  void
  SetReferenceCount(int) override;

  /** This is a global flag that controls whether any debug, warning
   *  or error messages are displayed.  */
  static void
  SetGlobalWarningDisplay(bool val);

  static bool
  GetGlobalWarningDisplay();

  static void
  GlobalWarningDisplayOn()
  {
    SetGlobalWarningDisplay(true);
  }
  static void
  GlobalWarningDisplayOff()
  {
    SetGlobalWarningDisplay(false);
  }

  /** \brief Add an observer/command to this object invoked for event
   *
   * This is an implementation of the subject/observer design
   * pattern. An observer is added by specifying an event to respond to
   * and an itk::Command to execute. It returns an unsigned long tag
   * which can be used later to remove the event or retrieve the
   * command.
   */
  unsigned long
  AddObserver(const EventObject & event, Command *);
  unsigned long
  AddObserver(const EventObject & event, Command *) const;

  /** \brief A convenient method to add an C++ lambda function as an observer.
   *
   * A FunctionCommand object in implicitly create to hold function, and
   * passed to this object as a standard observer. The command will be invoked
   * for the specified event.
   *
   * The function or lambda \b cannot captured this object as SmartPoint because
   * a circular dependency is generated which causing memory leaks.
   * Sample usage:
   *  \code
   *    auto &objRef = *o.GetPointer();
   *    o->AddObserver(itk::AnyEvent(), [&objRef](const itk::EventObject &event)
   *    { std::cout << "Object: " << objRef.GetNameOfClass() << " Event: " << event << std::endl; });
   *  \endcode
   *
   *  \see FunctionCommand
   */
  unsigned long
  AddObserver(const EventObject & event, std::function<void(const EventObject &)> function) const;

  /** Get the command associated with the given tag.  NOTE: This returns
   * a pointer to a Command, but it is safe to assign this to a
   * Command::Pointer.  Since Command inherits from LightObject, at this
   * point in the code, only a pointer or a reference to the Command can
   * be used.   */
  Command *
  GetCommand(unsigned long tag);

  /** Call Execute on all the Commands observing this event id. */
  void
  InvokeEvent(const EventObject &);

  /** Call Execute on all the Commands observing this event id.
   * The actions triggered by this call doesn't modify this object. */
  void
  InvokeEvent(const EventObject &) const;

  /** Remove the observer with this tag value. */
  void
  RemoveObserver(unsigned long tag);

  /** Remove all observers . */
  void
  RemoveAllObservers();

  /** Return true if an observer is registered for this event. */
  bool
  HasObserver(const EventObject & event) const;

  /**
   * \return A reference to this objects MetaDataDictionary.
   * \warning This reference may be changed.
   */
  MetaDataDictionary &
  GetMetaDataDictionary();

  /**
   * \return A constant reference to this objects MetaDataDictionary.
   */
  const MetaDataDictionary &
  GetMetaDataDictionary() const;

  /**
   * Set the MetaDataDictionary
   */
  void
  SetMetaDataDictionary(const MetaDataDictionary & rhs);
  void
  SetMetaDataDictionary(MetaDataDictionary && rrhs);

  /**
   * A facility to help application programmers set a
   * human identifiable name for a given object.
   * This has no inherent use in ITK, but is a
   * convenience to allow developers to provide a
   * name for this object.
   */
  itkSetMacro(ObjectName, std::string);
  itkGetConstReferenceMacro(ObjectName, std::string);

protected:
  Object();
  ~Object() override;

  /** Methods invoked by Print() to print information about the object
   * including superclasses. Typically not called by the user (use Print()
   * instead) but used in the hierarchical print process to combine the
   * output of several classes.  */
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  bool
  PrintObservers(std::ostream & os, Indent indent) const;

  /** Set the time stamp of this object.
   * This method must be used very carefully !!!.
   * Most mortals will never need to call this method. */
  virtual void
  SetTimeStamp(const TimeStamp & timeStamp);

private:
  /** Only used to synchronize the global variable across static libraries.*/
  itkGetGlobalDeclarationMacro(bool, GlobalWarningDisplay);

  /** Enable/Disable debug messages. */
  mutable bool m_Debug{ false };

  /** Keep track of modification time. */
  mutable TimeStamp m_MTime;

  /** Global object debug flag. */
  static bool * m_GlobalWarningDisplay;

  /** Implementation class for Subject/Observer Pattern.
   * This is only allocated if used. */
  SubjectImplementation * m_SubjectImplementation{ nullptr };
  /**
   * Implementation for holding Object MetaData
   * @see itk::MetaDataDictionary
   * @see itk::MetaDataObjectBase
   * @see itk::MetaDataObject
   * This is only allocated if used.
   */
  mutable MetaDataDictionary * m_MetaDataDictionary{ nullptr };

  std::string m_ObjectName;
};
} // end namespace itk

#endif
