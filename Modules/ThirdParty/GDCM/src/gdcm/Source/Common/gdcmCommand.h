/*=========================================================================

  Program: GDCM (Grassroots DICOM). A DICOM library

  Copyright (c) 2006-2011 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
#ifndef GDCMCOMMAND_H
#define GDCMCOMMAND_H

#include "gdcmSubject.h"

namespace gdcm
{
class Event;

/**
 * \brief Command superclass for callback/observer methods
 * \see Subject
 */
class GDCM_EXPORT Command : public Subject
{
public :
  Command(const Command&) = delete;
  void operator=(const Command&) = delete;

  /// Abstract method that defines the action to be taken by the command.
  virtual void Execute(Subject *caller, const Event & event ) = 0;

  /** Abstract method that defines the action to be taken by the command.
   * This variant is expected to be used when requests comes from a
   * const Object
   */
  virtual void Execute(const Subject *caller, const Event & event ) = 0;

protected:
  Command();
  ~Command() override;
};

/** \class MemberCommand
 *  \brief Command subclass that calls a pointer to a member function
 *
 *  MemberCommand calls a pointer to a member function with the same
 *  arguments as Execute on Command.
 *
 */
template <class T>
class MemberCommand : public Command
{
public:

  /** pointer to a member function that takes a Subject* and the event */
  typedef  void (T::*TMemberFunctionPointer)(Subject*, const Event &);
  typedef  void (T::*TConstMemberFunctionPointer)(const Subject*,
                                                  const Event &);

  /** Standard class typedefs. */
  typedef MemberCommand       Self;
  //typedef SmartPointer<Self>  Pointer;

  MemberCommand(const Self&) = delete;
  void operator=(const Self&) = delete;

  /** Method for creation through the object factory. */
  static SmartPointer<MemberCommand> New()
    {
    return new MemberCommand;
    }

  /** Run-time type information (and related methods). */
  //gdcmTypeMacro(MemberCommand,Command);

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
  void Execute(Subject *caller, const Event & event ) override
    {
    if( m_MemberFunction )
      {
      ((*m_This).*(m_MemberFunction))(caller, event);
      }
    }

  /**  Invoke the member function with a const object. */
  void Execute( const Subject *caller, const Event & event ) override
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
  MemberCommand():m_MemberFunction(nullptr),m_ConstMemberFunction(nullptr) {}
  ~MemberCommand() override= default;

};

/** \class SimpleMemberCommand
 *  \brief Command subclass that calls a pointer to a member function
 *
 *  SimpleMemberCommand calls a pointer to a member function with no
 *  arguments.
 */
template <typename T>
class SimpleMemberCommand : public Command
{
public:

  /** A method callback. */
  typedef  void (T::*TMemberFunctionPointer)();

  /** Standard class typedefs. */
  typedef SimpleMemberCommand   Self;
  //typedef SmartPointer<Self>    Pointer;

  SimpleMemberCommand(const Self&) = delete;
  void operator=(const Self&) = delete;

  /** Run-time type information (and related methods). */
  //gdcmTypeMacro(SimpleMemberCommand,Command);

  /** Method for creation through the object factory. */
  static SmartPointer<SimpleMemberCommand> New()
    {
    return new SimpleMemberCommand;
    }

  /** Specify the callback function. */
  void SetCallbackFunction(T* object,
                           TMemberFunctionPointer memberFunction)
    {
    m_This = object;
    m_MemberFunction = memberFunction;
    }

  /** Invoke the callback function. */
  void Execute(Subject *,const Event & ) override
    {
    if( m_MemberFunction )
      {
      ((*m_This).*(m_MemberFunction))();
      }
    }
  void Execute(const Subject *,const Event & ) override
    {
    if( m_MemberFunction )
      {
      ((*m_This).*(m_MemberFunction))();
      }
    }

protected:
  T* m_This;
  TMemberFunctionPointer m_MemberFunction;
  SimpleMemberCommand():m_This(nullptr),m_MemberFunction(nullptr) {}
  ~SimpleMemberCommand() override = default;
};

} // end namespace gdcm
//-----------------------------------------------------------------------------
#endif //GDCMCOMMAND_H
