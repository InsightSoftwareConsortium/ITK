/*=========================================================================

  Program: GDCM (Grassroots DICOM). A DICOM library
  Module:  $URL$

  Copyright (c) 2006-2010 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
#ifndef GDCMSIMPLESUBJECTWATCHER_H
#define GDCMSIMPLESUBJECTWATCHER_H

#include "gdcmSubject.h"
#include "gdcmCommand.h"
#include "gdcmSmartPointer.h"
#include "gdcmAnonymizeEvent.h"

namespace gdcm
{
//-----------------------------------------------------------------------------
class Event;
/**
 * \brief SimpleSubjectWatcher
 * This is a typical Subject Watcher class. It will observe all events.
 */
class GDCM_EXPORT SimpleSubjectWatcher
{
public:
  SimpleSubjectWatcher(Subject * s, const char *comment = "");
  virtual ~SimpleSubjectWatcher();

protected:
  virtual void StartFilter();
  virtual void EndFilter();
  virtual void ShowProgress(Subject *caller, const Event &evt);
  virtual void ShowIteration();
  virtual void ShowAnonymization(Subject *caller, const Event &evt);
  virtual void ShowAbort();

protected:
  // Custom API used for internal Testing do not use !
  void TestAbortOn();
  void TestAbortOff();

private:
  SmartPointer<gdcm::Subject> m_Subject;
  std::string m_Comment;

  typedef SimpleMemberCommand<SimpleSubjectWatcher> SimpleCommandType;
  typedef MemberCommand<SimpleSubjectWatcher> CommandType;

  SmartPointer<SimpleCommandType> m_StartFilterCommand;
  SmartPointer<SimpleCommandType> m_EndFilterCommand;
  SmartPointer<CommandType> m_ProgressFilterCommand;
  SmartPointer<SimpleCommandType> m_IterationFilterCommand;
  SmartPointer<SimpleCommandType> m_AbortFilterCommand;
  SmartPointer<CommandType> m_AnonymizeFilterCommand;

  unsigned long m_StartTag;
  unsigned long m_EndTag;
  unsigned long m_ProgressTag;
  unsigned long m_IterationTag;
  unsigned long m_AbortTag;
  unsigned long m_AnonymizeTag;

  bool m_TestAbort;

  SimpleSubjectWatcher(const SimpleSubjectWatcher&);  // Not implemented.
  void operator=(const SimpleSubjectWatcher&);  // Not implemented.
};
} // end namespace gdcm
//-----------------------------------------------------------------------------
#endif //GDCMSIMPLESUBJECTWATCHER_H
