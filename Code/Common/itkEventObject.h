/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkEventObject.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkEventObject_h
#define __itkEventObject_h

#include "itkIndent.h"


namespace itk
{

/** \class EventObject
 * \brief Abstraction of the Events used to communicating among filters
    and with GUIs.
 *
 * EventObject provides a standard coding for sending and receiving messages
 * indicating things like the initiation of processes, end of processes,
 * modification of filters.
 *
 * EventObjects form a hierarchy similar to the itk::ExceptionObject allowing
 * to factorize common events in a tree-like structure. Higher detail can
 * be assigned by users by subclassing existing itk::EventObjects.
 *
 * EventObjects are used by itk::Command and itk::Object for implementing the
 * Observer/Subject design pattern. Observers register their interest in 
 * particular kinds of events produced by a specific itk::Object. This 
 * mechanism decouples classes among them.
 *
 * As oppossed to itk::Exception, itk::EventObject does not represent error 
 * states, but simply flow of information allowing to trigger actions 
 * as a consecuence of changes occurring in state on some itk::Objects.
 *
 * itk::EventObject carries information in its own type, it relies on the 
 * appropiate use of the RTTI (Run Time Type Information).
 *
 * A set of standard EventObjects is defined near the end of itkIndent.h.
 *
 * \sa itk::Command
 * \sa itk::ExceptionObject
 *
 * \ingroup ITKSystemObjects 
 */
class ITK_EXPORT EventObject
{
public:
  /** Constructor and copy constructor.  Note that these functions will be
   * called when children are instantiated. */
  EventObject() {}
  
  /** Virtual destructor needed  */
  virtual ~EventObject() {}
  
  /**  Create an Event of this type This method work as a Factory for
   *  creating events of each particular type. */
  virtual EventObject* MakeObject() const=0;  
  
  /** Print Event information.  This method can be overridden by
   * specific Event subtypes.  The default is to print out the type of
   * the event. */
  virtual void Print(std::ostream& os) const;
  
  /** Return the StringName associated with the event. */
  virtual const char * GetEventName(void) const=0;
  
  /** Check if given event matches or derives from this event. */
  virtual bool CheckEvent(const EventObject*) const=0;
  
protected:
  /** Methods invoked by Print() to print information about the object
   * including superclasses. Typically not called by the user (use Print()
   * instead) but used in the hierarchical print process to combine the
   * output of several classes.  */
  virtual void PrintSelf(std::ostream& os, Indent indent) const;
  virtual void PrintHeader(std::ostream& os, Indent indent) const;
  virtual void PrintTrailer(std::ostream& os, Indent indent) const;
   
private:
  typedef  EventObject * EventFactoryFunction();
  EventObject(const EventObject&);
  void operator=(const EventObject&);
};


/** Generic inserter operator for EventObject and its subclasses. */
inline std::ostream& operator<<(std::ostream& os, EventObject &e)
{
  (&e)->Print(os);
  return os;
}




/*
 *  Macro for creating new Events
 */
#define itkEventMacro( classname , super ) \
 class classname : public super { \
   public: \
     typedef classname Self; \
     typedef super Superclass; \
     classname() {} \
     virtual ~classname() {} \
     virtual const char * GetEventName() const { return #classname; } \
     virtual bool CheckEvent(const ::itk::EventObject* e) const \
       { return dynamic_cast<const Self*>(e); } \
     virtual ::itk::EventObject* MakeObject() const \
       { return new Self; } \
   private: \
     classname(const Self&); \
     void operator=(const Self&); \
 }



/**
 *      Define some common ITK events
 */
itkEventMacro( NoEvent            , EventObject );
itkEventMacro( AnyEvent           , EventObject );
itkEventMacro( DeleteEvent        , AnyEvent );
itkEventMacro( StartEvent         , AnyEvent );
itkEventMacro( EndEvent           , AnyEvent );
itkEventMacro( ProgressEvent      , AnyEvent );
itkEventMacro( ExitEvent          , AnyEvent );
itkEventMacro( ModifiedEvent      , AnyEvent );
itkEventMacro( IterationEvent     , AnyEvent );
itkEventMacro( PickEvent          , AnyEvent );
itkEventMacro( StartPickEvent     , PickEvent   );
itkEventMacro( EndPickEvent       , PickEvent   );
itkEventMacro( AbortCheckEvent    , PickEvent   );

itkEventMacro( UserEvent          , AnyEvent );

   
} // end namespace itk

#endif

