/*=========================================================================
 *
 *  Copyright Insight Software Consortium
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
#ifndef itkCommand_h
#define itkCommand_h

#include "itkObject.h"
#include "itkObjectFactory.h"

namespace itk
{
/** \class Command
 *  \brief Superclass for callback/observer methods.
 *
 * Command is an implementation of the command design pattern that is used
 * in callbacks (such as StartMethod(), ProgressMethod(), and EndMethod()) in
 * ITK. itk::Object implements a Subject/Observer pattern. When a subject
 * needs to notify a observer, it does so using a itk::Command. The Execute
 * method is called to run the command.
 *
 * \ingroup ITKSystemObjects
 * \ingroup ITKCommon
 *
 * \wiki
 * \wikiexample{Utilities/ObserveEvent,Observe an event}
 * \endwiki
 */

// The superclass that all commands should be subclasses of
class ITKCommon_EXPORT Command:public Object
{
public:
  /** Standard class typedefs. */
  typedef Command                    Self;
  typedef Object                     Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(Command, Object);

  /** Abstract method that defines the action to be taken by the command. */
  virtual void Execute(Object *caller, const EventObject & event) = 0;

  /** Abstract method that defines the action to be taken by the command.
   * This variant is expected to be used when requests comes from a
   * const Object */
  virtual void Execute(const Object *caller, const EventObject & event) = 0;

protected:
  Command();
  ~Command() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(Command);
};

// some implementations for several callback types

/** \class MemberCommand
 *  \brief A Command subclass that calls a pointer to a member function.
 *
 *  MemberCommand calls a pointer to a member function with the same
 *  arguments as Execute on Command.
 *
 * \ingroup ITKSystemObjects
 * \ingroup ITKCommon
 */
template< typename T >
class ITK_TEMPLATE_EXPORT MemberCommand:public Command
{
public:
  /** pointer to a member function that takes a Object* and the event */
  typedef  void ( T::*TMemberFunctionPointer )(Object *, const EventObject &);
  typedef  void ( T::*TConstMemberFunctionPointer )(const Object *,
                                                    const EventObject &);

  /** Standard class typedefs. */
  typedef MemberCommand        Self;
  typedef SmartPointer< Self > Pointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MemberCommand, Command);

  /**  Set the callback function along with the object that it will
   *  be invoked on. */
  void SetCallbackFunction(T *object,
                           TMemberFunctionPointer memberFunction)
  {
    m_This = object;
    m_MemberFunction = memberFunction;
  }

  void SetCallbackFunction(T *object,
                           TConstMemberFunctionPointer memberFunction)
  {
    m_This = object;
    m_ConstMemberFunction = memberFunction;
  }

  /**  Invoke the member function. */
  virtual void Execute(Object *caller, const EventObject & event) ITK_OVERRIDE
  {
    if ( m_MemberFunction )
      {
      ( ( *m_This ).*( m_MemberFunction ) )( caller, event );
      }
  }

  /**  Invoke the member function with a const object. */
  virtual void Execute(const Object *caller, const EventObject & event) ITK_OVERRIDE
  {
    if ( m_ConstMemberFunction )
      {
      ( ( *m_This ).*( m_ConstMemberFunction ) )( caller, event );
      }
  }

protected:
  T *                         m_This;
  TMemberFunctionPointer      m_MemberFunction;
  TConstMemberFunctionPointer m_ConstMemberFunction;

  MemberCommand() :
    m_This( ITK_NULLPTR ),
    m_MemberFunction( ITK_NULLPTR ),
    m_ConstMemberFunction( ITK_NULLPTR )
  {}

  virtual ~MemberCommand() ITK_OVERRIDE {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(MemberCommand);
};

/** \class ReceptorMemberCommand
 *  \brief A Command subclass that calls a pointer to a member function.
 *
 *  ReceptorMemberCommand calls a pointer to a member function with
 *  only and itk::EventObject as argument
 *
 * \ingroup ITKSystemObjects
 * \ingroup ITKCommon
 */
template< typename T >
class ITK_TEMPLATE_EXPORT ReceptorMemberCommand:public Command
{
public:
  /** pointer to a member function that takes a Object* and the event */
  typedef  void ( T::*TMemberFunctionPointer )(const EventObject &);

  /** Standard class typedefs. */
  typedef ReceptorMemberCommand Self;
  typedef SmartPointer< Self >  Pointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ReceptorMemberCommand, Command);

  /**  Set the callback function along with the object that it will
   *  be invoked on. */
  void SetCallbackFunction(T *object,
                           TMemberFunctionPointer memberFunction)
  {
    m_This = object;
    m_MemberFunction = memberFunction;
  }

  /**  Invoke the member function. */
  virtual void Execute(Object *, const EventObject & event) ITK_OVERRIDE
  {
    if ( m_MemberFunction )
      {
      ( ( *m_This ).*( m_MemberFunction ) )( event );
      }
  }

  /**  Invoke the member function with a const object */
  virtual void Execute(const Object *, const EventObject & event) ITK_OVERRIDE
  {
    if ( m_MemberFunction )
      {
      ( ( *m_This ).*( m_MemberFunction ) )( event );
      }
  }

protected:
  T *                    m_This;
  TMemberFunctionPointer m_MemberFunction;

  ReceptorMemberCommand() :
    m_This( ITK_NULLPTR ),
    m_MemberFunction( ITK_NULLPTR )
  {}

  virtual ~ReceptorMemberCommand() ITK_OVERRIDE {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ReceptorMemberCommand);
};

/** \class SimpleMemberCommand
 *  \brief A Command subclass that calls a pointer to a member function.
 *
 *  SimpleMemberCommand calls a pointer to a member function with no
 *  arguments.
 *
 * \ingroup ITKSystemObjects
 * \ingroup ITKCommon
 */
template< typename T >
class ITK_TEMPLATE_EXPORT SimpleMemberCommand:public Command
{
public:
  /** A method callback. */
  typedef  void ( T::*TMemberFunctionPointer )();

  /** Standard class typedefs. */
  typedef SimpleMemberCommand  Self;
  typedef SmartPointer< Self > Pointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(SimpleMemberCommand, Command);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Specify the callback function. */
  void SetCallbackFunction(T *object,
                           TMemberFunctionPointer memberFunction)
  {
    m_This = object;
    m_MemberFunction = memberFunction;
  }

  /** Invoke the callback function. */
  virtual void Execute(Object *, const EventObject &) ITK_OVERRIDE
  {
    if ( m_MemberFunction )
      {
      ( ( *m_This ).*( m_MemberFunction ) )( );
      }
  }

  virtual void Execute(const Object *, const EventObject &) ITK_OVERRIDE
  {
    if ( m_MemberFunction )
      {
      ( ( *m_This ).*( m_MemberFunction ) )( );
      }
  }

protected:
  T *                    m_This;
  TMemberFunctionPointer m_MemberFunction;

  SimpleMemberCommand() :
    m_This( ITK_NULLPTR ),
    m_MemberFunction( ITK_NULLPTR )
  {}

  virtual ~SimpleMemberCommand() ITK_OVERRIDE {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(SimpleMemberCommand);
};

/** \class SimpleConstMemberCommand
 *  \brief A Command subclass that calls a pointer to a member function.
 *
 *  SimpleConstMemberCommand calls a pointer to a member function with no
 *  arguments.
 *
 * \ingroup ITKSystemObjects
 * \ingroup ITKCommon
 */
template< typename T >
class ITK_TEMPLATE_EXPORT SimpleConstMemberCommand:public Command
{
public:
  /** A const member method callback. */
  typedef  void ( T::*TMemberFunctionPointer )() const;

  /** Standard class typedefs. */
  typedef SimpleConstMemberCommand Self;
  typedef SmartPointer< Self >     Pointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(SimpleConstMemberCommand, Command);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Specify the const member method callback. */
  void SetCallbackFunction(const T *object,
                           TMemberFunctionPointer memberFunction)
  {
    m_This = object;
    m_MemberFunction = memberFunction;
  }

  /** Invoke the const member method callback. */
  virtual void Execute(Object *, const EventObject &) ITK_OVERRIDE
  {
    if ( m_MemberFunction )
      {
      ( ( *m_This ).*( m_MemberFunction ) )( );
      }
  }

  virtual void Execute(const Object *, const EventObject &) ITK_OVERRIDE
  {
    if ( m_MemberFunction )
      {
      ( ( *m_This ).*( m_MemberFunction ) )( );
      }
  }

protected:
  const T *              m_This;
  TMemberFunctionPointer m_MemberFunction;

  SimpleConstMemberCommand() :
    m_This( ITK_NULLPTR ),
    m_MemberFunction( ITK_NULLPTR )
  {}

  virtual ~SimpleConstMemberCommand() {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(SimpleConstMemberCommand);
};

/** \class CStyleCommand
 *  \brief A Command subclass that calls a pointer to a C function.
 *
 *  CStyleCommand calls a pointer to a C function with the following
 *  arguments void func(Object *,void *clientdata)
 *  The clientdata is data that the command wants passed to itself
 *  each time.
 *
 * \ingroup ITKSystemObjects
 * \ingroup ITKCommon
 */

class ITKCommon_EXPORT CStyleCommand:public Command
{
public:
  /** Typedefs for C-style callbacks. */
  typedef  void ( *FunctionPointer )(Object *, const EventObject &, void *);
  typedef  void ( *ConstFunctionPointer )(const Object *,
                                          const EventObject &, void *);
  typedef  void ( *DeleteDataFunctionPointer )(void *);

  /** Standard class typedefs. */
  typedef CStyleCommand        Self;
  typedef SmartPointer< Self > Pointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(CStyleCommand, Command);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Set the client data that will be passed into the C function when
   * it is called. */
  void SetClientData(void *cd);

  /** Set the C callback function pointer to be called at Execute time. */
  void SetCallback(FunctionPointer f);
  void SetConstCallback(ConstFunctionPointer f);

  /** Set the callback to delete the client data. */
  void SetClientDataDeleteCallback(DeleteDataFunctionPointer f);

  /** Execute the callback function. */
  virtual void Execute(Object *caller, const EventObject & event) ITK_OVERRIDE;

  /** Execute the callback function with a const Object */
  virtual void Execute(const Object *caller, const EventObject & event) ITK_OVERRIDE;

protected:
  CStyleCommand();
  ~CStyleCommand() ITK_OVERRIDE;

  void *                    m_ClientData;
  FunctionPointer           m_Callback;
  ConstFunctionPointer      m_ConstCallback;
  DeleteDataFunctionPointer m_ClientDataDeleteCallback;
};
} // end namespace itk

#endif
