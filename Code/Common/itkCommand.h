/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCommand.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkCommand_h
#define __itkCommand_h

#include "itkLightObject.h"

namespace itk
{

/** \class Command
 * \brief superclass for callback/observer methods
 *
 * Command is an implementation of the command design pattern that is used
 * in callbacks (such as StartMethod(), ProgressMethod(), and EndMethod()) in
 * ITK. itkLightObject implements a Subject/Observer pattern. When a subject 
 * needs to notify a observer, it does so using a itkCommand.  The Execute 
 * method is called to run the command.
 */
  
// The superclass that all commands should be subclasses of
class ITK_EXPORT Command : public LightObject
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef Command         Self;

  /**
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;

  virtual void Execute(LightObject *caller, unsigned long event) = 0;

  /**
   * All the currently defined events are enumerated here.  developers
   * can use: "Command::UserEvent + int" to specify their own events.  
   */
  enum EventIds {
    NoEvent = 0,
    DeleteEvent,
    StartEvent,
    EndEvent,
    ProgressEvent,
    PickEvent,
    StartPickEvent,
    EndPickEvent,
    AbortCheckEvent,
    ExitEvent,
    AnyEvent,
    UserEvent = 1000
  };
    
  static unsigned long GetEventIdFromString(const char *event)
    {  
    if (!strcmp("DeleteEvent",event))
      {
      return Command::DeleteEvent;
      }
    if (!strcmp("StartEvent",event))
      {
      return Command::StartEvent;
      }
    if (!strcmp("EndEvent",event))
      {
      return Command::EndEvent;
      }
    if (!strcmp("ProgressEvent",event))
      {
      return Command::ProgressEvent;
      }
    if (!strcmp("PickEvent",event))
      {
      return Command::PickEvent;
      }
    if (!strcmp("ExitEvent",event))
      {
      return Command::PickEvent;
      }
    if (!strcmp("StartPickEvent",event))
      {
      return Command::StartPickEvent;
      }
    if (!strcmp("EndPickEvent",event))
      {
      return Command::EndPickEvent;
      }
    if (!strcmp("AbortCheckEvent",event))
      {
      return Command::AbortCheckEvent;
      }
    if (!strcmp("UserEvent",event))
      {
      return Command::UserEvent;
      }
    return Command::NoEvent;
    };
};
  
// some implementations for several callback types

/** \Class MemberCommand
 *  \brief Command subclass that calls a pointer to a member function
 *
 *  MemberCommand calls a pointer to a member function with the same
 *  arguments as Execute on Command.   
 */
template <class T>
class MemberCommand : public Command
{
public:
  /* 
   * pointer to a member function that takes a LightObject* and the event
   */
  typedef  void (T::*TMemberFunctionPointer)(LightObject*, unsigned long);
  
  /**
   * Standard "Self" typedef.
   */
  typedef MemberCommand         Self;

  /**
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;

  /**
   * Method for creation through the object factory.
   */
  static Pointer New()
    {
      Self *ret = ObjectFactory<Self>::Create();
      if(ret)
        {
        return ret;
        }
      return new Self;
    }

  /**
   *  Set the callback function along with the object that it will
   *  be invoked on.
   */
  void SetCallbackFunction(T* object,  
                           TMemberFunctionPointer memberFunction)
    {
      m_This = object;
      m_MemberFunction = memberFunction;
    }

  /**
   *  Invoke the member function.
   */
  virtual void Execute(LightObject *caller, unsigned long event)
    { 
      ((*m_This).*(m_MemberFunction))(caller, event);
    }

protected:
  T* m_This;
  TMemberFunctionPointer m_MemberFunction;
  MemberCommand(){}; 
  virtual ~MemberCommand(){}; 
  MemberCommand(const Self&) {}
  void operator=(const Self&) {}
};

/** \Class SimpleMemberCommand
 *  \brief Command subclass that calls a pointer to a member function
 *
 *  SimpleMemberCommand calls a pointer to a member function with no 
 *  arguments.   
 */
template <class T>
class SimpleMemberCommand : public Command
{ 
public:
  typedef  void (T::*TMemberFunctionPointer)(); 
   /**
   * Standard "Self" typedef.
   */
  typedef SimpleMemberCommand         Self;

  /**
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;

  /**
   * Method for creation through the object factory.
   */
  static Pointer New()
    {
      Self *ret = ObjectFactory<Self>::Create();
      if(ret)
        {
        return ret;
        }
      return new Self;
    }

  void SetCallbackFunction(T* object,  
                           TMemberFunctionPointer memberFunction)
    {
      m_This = object;
      m_MemberFunction = memberFunction;
    }
  
  virtual void Execute(LightObject *, unsigned long)
    { 
      ((*m_This).*(m_MemberFunction))();
    }

protected:
  T* m_This;
  TMemberFunctionPointer m_MemberFunction;
  SimpleMemberCommand(){}; 
  virtual ~SimpleMemberCommand(){}; 
  SimpleMemberCommand(const Self&) {}
  void operator=(const Self&) {}
};


/** \Class CStyleCommand
 *  \brief Command subclass that calls a pointer to a C function
 *
 *  CStyleCommand calls a pointer to a C function with the following
 *  arguments void func(LightObject *,void *clientdata)
 *  The clientdata is data that the command wants passed to itself
 *  each time.
 */

class CStyleCommand : public Command
{
public:
  typedef  void (*FunctionPointer)(LightObject*, unsigned long, void*);
  typedef  void (*DeleteDataFunctionPointer)(void*);
   /**
   * Standard "Self" typedef.
   */
  typedef CStyleCommand         Self;

  /**
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;

  /**
   * Method for creation through the object factory.
   */
  static Pointer New()
    {
      Self *ret = ObjectFactory<Self>::Create();
      if(ret)
        {
        return ret;
        }
      return new Self;
    }

  /**
   * Set the client data that will be passed into the C function when 
   * it is called.
   */
  void SetClientData(void *cd) {m_ClientData = cd;}

  /**
   * Set the C callback function pointer to be called at Execute time.
   */
  void SetCallback(FunctionPointer f)
    {m_Callback = f;}

  /**
   * Set the callback to delete the client data.
   */
  void SetClientDataDeleteCallback(DeleteDataFunctionPointer f)
    {m_ClientDataDeleteCallback = f;}
  
  /**
   * Execute the callback function.
   */
  void Execute(LightObject *caller, unsigned long event)
    {
    if (m_Callback)
      {
      m_Callback(caller, event, m_ClientData );
      }
    };

protected:
  CStyleCommand()
    { 
      m_ClientData = 0;
      m_Callback = 0; 
      m_ClientDataDeleteCallback = 0;
    }
  ~CStyleCommand() 
    { 
    if (m_ClientDataDeleteCallback)
      {
      m_ClientDataDeleteCallback(m_ClientData);
      }
    };
  void *m_ClientData;
  FunctionPointer m_Callback;
  DeleteDataFunctionPointer m_ClientDataDeleteCallback;
};


} // end namespace itk

#endif
