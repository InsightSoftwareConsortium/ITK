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

namespace itk
{

class LightObject;
/** \class Command
 * \brief superclass for callback/observer methods
 *
 * Command is an implementation of the command design pattern that is used
 * in callbacks (such as StartMethod(), ProgressMethod(), and EndMethod()) in
 * ITK. itkLightObject implements a Subject/Observer pattern. When a subject needs
 * to notify a observer, it does so using a itkCommand.  The Execute method
 * is called to run the command.
 */
  
// The superclass that all commands should be subclasses of
class ITK_EXPORT Command
{
public:
  virtual ~Command() {};
  void Delete() {delete this;};
  virtual void Execute(LightObject *caller, void *callData) = 0;

  // all the currently defined events
  // developers can use -- Command::UserEvent + int to
  // specify their own events. 
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

// a good command to use for generic function callbacks
// the function should have the format 
// void func(LightObject *,void *clientdata, void *calldata)
class CallbackCommand : public Command
{
public:
  CallbackCommand() { this->ClientData = NULL;
  this->Callback = NULL; this->ClientDataDeleteCallback = NULL;};
  ~CallbackCommand() 
    { 
    if (this->ClientDataDeleteCallback)
      {
      this->ClientDataDeleteCallback(this->ClientData);
      }
    };
  void SetClientData(void *cd) {this->ClientData = cd;};
  void SetCallback(void (*f)(LightObject *, void *, void *)) 
    {this->Callback = f;};
  void SetClientDataDeleteCallback(void (*f)(void *))
    {this->ClientDataDeleteCallback = f;};
  
  void Execute(LightObject *caller, void *callData)
    {
    if (this->Callback)
      {
      this->Callback(caller, this->ClientData, callData);
      }
    };
  
  void *ClientData;
  void (*Callback)(LightObject *, void *, void *);
  void (*ClientDataDeleteCallback)(void *);
};

} // end namespace itk

#endif



