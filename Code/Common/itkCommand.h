/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCommand.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkCommand_h
#define __itkCommand_h

#include "itkObject.h"
#include "itkObjectFactory.h"


namespace itk
{

/** \class Command
 * \brief superclass for callback/observer methods
 *
 * Command is an implementation of the command design pattern that is used
 * in callbacks (such as StartMethod(), ProgressMethod(), and EndMethod()) in
 * ITK. itkObject implements a Subject/Observer pattern. When a subject 
 * needs to notify a observer, it does so using a itkCommand.  The Execute 
 * method is called to run the command.
 *
 * \ingroup ITKSystemObjects
 */
  
// The superclass that all commands should be subclasses of
class ITKCommon_EXPORT Command : public Object
{
public:
  /** Standard class typedefs. */
  typedef Command         Self;
  typedef Object Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(Command,Object);
  
  /** Abstract method that defines the action to be taken by the command. */
  virtual void Execute(Object *caller, const EventObject & event ) = 0;

  /** Abstract method that defines the action to be taken by the command.
   * This variant is expected to be used when requests comes from a 
   * const Object */
  virtual void Execute(const Object *caller, const EventObject & event ) = 0;

};

 
// some implementations for several callback types

/** \Class MemberCommand
 *  \brief Command subclass that calls a pointer to a member function
 *
 *  MemberCommand calls a pointer to a member function with the same
 *  arguments as Execute on Command.   
 * 
 * \ingroup ITKSystemObjects
 */
template <class T>
class MemberCommand : public Command
{
public:
  /** pointer to a member function that takes a Object* and the event */
  typedef  void (T::*TMemberFunctionPointer)(Object*, const EventObject &);
  typedef  void (T::*TConstMemberFunctionPointer)(const Object*, const EventObject &);
    
  /** Standard class typedefs. */
  typedef MemberCommand         Self;
  typedef SmartPointer<Self>  Pointer;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(MemberCommand,Command);

  /**  Set the callback function along with the object that it will
   *  be invoked on. */
  void SetCallbackFunction(T* object,  
                           TMemberFunctionPointer memberFunction)
    {
      m_This = object;
      m_MemberFunction = memberFunction;
    }
  void SetCallbackFunction(T* object,  
                           TConstMemberFunctionPointer memberFunction)
    {
      m_This = object;
      m_ConstMemberFunction = memberFunction;
    }
  
  /**  Invoke the member function. */
  virtual void Execute(Object *caller, const EventObject & event )
    { 
      if( m_MemberFunction ) 
      {
        ((*m_This).*(m_MemberFunction))(caller, event);
      }
    }

  /**  Invoke the member function with a const object. */
  virtual void Execute( const Object *caller, const EventObject & event )
    { 
      if( m_ConstMemberFunction ) 
      {
        ((*m_This).*(m_ConstMemberFunction))(caller, event);
      }
    }

protected:
  T* m_This;
  TMemberFunctionPointer m_MemberFunction;
  TConstMemberFunctionPointer m_ConstMemberFunction;
  MemberCommand():m_MemberFunction(0),m_ConstMemberFunction(0) {}
  virtual ~MemberCommand(){}

private:
  MemberCommand(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

};


/** \Class ReceptorMemberCommand
 *  \brief Command subclass that calls a pointer to a member function
 *
 *  ReceptorMemberCommand calls a pointer to a member function with 
 *  only and itk::EventObject as argument
 * 
 * \ingroup ITKSystemObjects
 */
template <class T>
class ReceptorMemberCommand : public Command
{
public:
  /** pointer to a member function that takes a Object* and the event */
  typedef  void (T::*TMemberFunctionPointer)(const EventObject &);
  
  /** Standard class typedefs. */
  typedef ReceptorMemberCommand         Self;
  typedef SmartPointer<Self>  Pointer;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(ReceptorMemberCommand,Command);

  /**  Set the callback function along with the object that it will
   *  be invoked on. */
  void SetCallbackFunction(T* object,  
                           TMemberFunctionPointer memberFunction)
    {
      m_This = object;
      m_MemberFunction = memberFunction;
    }

  /**  Invoke the member function. */
  virtual void Execute(Object *caller, const EventObject & event )
    { 
      if( m_MemberFunction ) 
      {
        ((*m_This).*(m_MemberFunction))(event);
      }
    }

  /**  Invoke the member function with a const object */
  virtual void Execute( const Object *caller, const EventObject & event )
    { 
      if( m_MemberFunction ) 
      {
        ((*m_This).*(m_MemberFunction))(event);
      }
    }

protected:
  T* m_This;
  TMemberFunctionPointer m_MemberFunction;
  ReceptorMemberCommand():m_MemberFunction(0) {}
  virtual ~ReceptorMemberCommand() {}

private:
  ReceptorMemberCommand(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
};


/** \Class SimpleMemberCommand
 *  \brief Command subclass that calls a pointer to a member function
 *
 *  SimpleMemberCommand calls a pointer to a member function with no 
 *  arguments.   
 *
 * \ingroup ITKSystemObjects
 */
template <class T>
class SimpleMemberCommand : public Command
{ 
public:
  /** A method callback. */
  typedef  void (T::*TMemberFunctionPointer)(); 
  
  /** Standard class typedefs. */
  typedef SimpleMemberCommand         Self;
  typedef SmartPointer<Self>  Pointer;
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(SimpleMemberCommand,Command);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Specify the callback function. */
  void SetCallbackFunction(T* object,  
                           TMemberFunctionPointer memberFunction)
    {
      m_This = object;
      m_MemberFunction = memberFunction;
    }
  
  /** Invoke the callback function. */
  virtual void Execute(Object *,const EventObject & ) 
    { 
      if( m_MemberFunction ) 
      {
        ((*m_This).*(m_MemberFunction))();
      }
    }
  virtual void Execute(const Object *,const EventObject & ) 
    { 
      if( m_MemberFunction ) 
      {
        ((*m_This).*(m_MemberFunction))();
      }
    }
  
protected:
  T* m_This;
  TMemberFunctionPointer m_MemberFunction;
  SimpleMemberCommand():m_MemberFunction(0) {}
  virtual ~SimpleMemberCommand() {}

private:
  SimpleMemberCommand(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

};


/** \Class SimpleConstMemberCommand
 *  \brief Command subclass that calls a pointer to a member function
 *
 *  SimpleConstMemberCommand calls a pointer to a member function with no 
 *  arguments.   
 *
 * \ingroup ITKSystemObjects
 */
template <class T>
class SimpleConstMemberCommand : public Command
{ 
public:
  /** A const member method callback. */
  typedef  void (T::*TMemberFunctionPointer)() const; 

  /** Standard class typedefs. */
  typedef SimpleConstMemberCommand         Self;
  typedef SmartPointer<Self>  Pointer;
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(SimpleConstMemberCommand,Command);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Specify the const member method callback. */
  void SetCallbackFunction(const T* object,  
                           TMemberFunctionPointer memberFunction)
    {
      m_This = object;
      m_MemberFunction = memberFunction;
    }
  
  /** Invoke the const member method callback. */
  virtual void Execute(Object *,const EventObject & event ) 
    { 
      if( m_MemberFunction ) 
      {
        ((*m_This).*(m_MemberFunction))();
      }
    }
  virtual void Execute(const Object *,const EventObject & event ) 
    { 
      if( m_MemberFunction ) 
      {
        ((*m_This).*(m_MemberFunction))();
      }
    }
  
protected:
  const T* m_This;
  TMemberFunctionPointer m_MemberFunction;
  SimpleConstMemberCommand():m_MemberFunction(0) {}
  virtual ~SimpleConstMemberCommand() {} 

private:
  SimpleConstMemberCommand(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
};


/** \Class CStyleCommand
 *  \brief Command subclass that calls a pointer to a C function
 *
 *  CStyleCommand calls a pointer to a C function with the following
 *  arguments void func(Object *,void *clientdata)
 *  The clientdata is data that the command wants passed to itself
 *  each time.
 * 
 * \ingroup ITKSystemObjects
 */

class CStyleCommand : public Command
{
public:
  /** Typedefs for C-style callbacks. */
  typedef  void (*FunctionPointer)(Object*, const EventObject &, void*);
  typedef  void (*ConstFunctionPointer)(const Object*, const EventObject &, void*);
  typedef  void (*DeleteDataFunctionPointer)(void*);
  
  /** Standard class typedefs. */
  typedef CStyleCommand         Self;
  typedef SmartPointer<Self>  Pointer;
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(CStyleCommand,Command);
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Set the client data that will be passed into the C function when 
   * it is called. */
  void SetClientData(void *cd) {m_ClientData = cd;}

  /** Set the C callback function pointer to be called at Execute time. */
  void SetCallback(FunctionPointer f)
    {m_Callback = f;}
  void SetConstCallback(ConstFunctionPointer f)
    {m_ConstCallback = f;}
  
  /** Set the callback to delete the client data. */
  void SetClientDataDeleteCallback(DeleteDataFunctionPointer f)
    {m_ClientDataDeleteCallback = f;}
  
  /** Execute the callback function. */
  void Execute(Object *caller, const EventObject & event )
    {
    if (m_Callback)
      {
      m_Callback(caller, event, m_ClientData );
      }
    };

  /** Execute the callback function with a const Object */
  void Execute(const Object *caller, const EventObject & event )
    {
    if (m_ConstCallback)
      {
      m_ConstCallback(caller, event, m_ClientData );
      }
    };

protected:
  CStyleCommand(): m_ClientData(0),
                   m_Callback(0),
                   m_ConstCallback(0),
                   m_ClientDataDeleteCallback(0) {}
  ~CStyleCommand() 
    { 
    if (m_ClientDataDeleteCallback)
      {
      m_ClientDataDeleteCallback(m_ClientData);
      }
    };
  void *m_ClientData;
  FunctionPointer m_Callback;
  ConstFunctionPointer m_ConstCallback;
  DeleteDataFunctionPointer m_ClientDataDeleteCallback;
};


} // end namespace itk

#endif
