/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkEventObject.h
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
 * \sa itk::Command
 * \sa itk::ExceptionObject
 *
 * \ingroup ITKSystemObjects 
 */
class ITK_EXPORT EventObject
{
public:
  /**
   * Constructor and copy constructor.  Note that these functions will be
   * called when children are instantiated.
   */
  EventObject()
  {
  };

  /**
   * Virtual destructor needed 
   */
  virtual ~EventObject() 
  {
  };

  /**
   * Copy Constructor
   */
  EventObject( const EventObject &orig )
  {
  }
  
  /**
   * Assignment and equivalence operators.
   */
  EventObject &operator= ( const EventObject &orig )
  {
     return *this;
  }
  
  /**
   *  Create an Event of this type
   *  This method work as a Factory for creating
   *  events of each particular type.
   */
  virtual  EventObject * New(void) const
  {
    return new EventObject;
  }
  
  /** 
   * Compare if two Events are of the same type.
   * Given that events do not have ivars, two of
   * them are considered equal when the actual type
   * is the same.
   */ 
  virtual bool operator==( const EventObject &orig ) const;
  virtual bool IsA( const EventObject &orig ) const;
         
 
  /**
   * Return the name of the class.
   */
  itkTypeMacro(EventObject, None);

   /**
   * Print Event information.  This method can be overridden by
   * specific Event subtypes.  The default is to print out the
   * type of the event.
   */
  virtual void Print(std::ostream& os) const;

  /** 
   * Return the StringName associated with the event
   */
  virtual const char * GetEventName(void) const;

  /** 
   * Create an event of the specific type associated with
   * the string
   */
  static EventObject * CreateEventFromString(const char *);

protected:
  /** 
   * Methods invoked by Print() to print information about the object
   * including superclasses. Typically not called by the user (use Print()
   * instead) but used in the hierarchical print process to combine the
   * output of several classes. 
   */
  virtual void PrintSelf(std::ostream& os, Indent indent) const;
  virtual void PrintHeader(std::ostream& os, Indent indent) const;
  virtual void PrintTrailer(std::ostream& os, Indent indent) const;
 
private:

  typedef  EventObject * EventFactoryFunction();
 
};




/**
 * Generic inserter operator for EventObject and its subclasses.
 *
 */
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
     typedef super Superclass; \
     classname() {} \
     virtual ~classname() {} \
     virtual const char * GetName(void) { \
      return "classname"; } \
     virtual EventObject * New(void) const { \
        return new classname; } \
 }



/**
 *      Define some common ITK events
 */
itkEventMacro( NoEvent            , EventObject );
itkEventMacro( DeleteEvent        , EventObject );
itkEventMacro( StartEvent         , EventObject );
itkEventMacro( EndEvent           , EventObject );
itkEventMacro( ProgressEvent      , EventObject );
itkEventMacro( PickEvent          , EventObject );
itkEventMacro( StartPickEvent     , PickEvent   );
itkEventMacro( EndPickEvent       , PickEvent   );
itkEventMacro( AbortCheckEvent    , PickEvent   );
itkEventMacro( ExitEvent          , EventObject );
itkEventMacro( ModifiedEvent      , EventObject );
itkEventMacro( IterationEvent     , EventObject );
itkEventMacro( AnyEvent           , EventObject );
itkEventMacro( UserEvent          , EventObject );


   
} // end namespace itk

#endif

