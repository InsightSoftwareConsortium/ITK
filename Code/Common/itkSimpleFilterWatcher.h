/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSimpleFilterWatcher.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkSimpleFilterWatcher_h
#define _itkSimpleFilterWatcher_h

#include "itkCommand.h"
#include "itkProcessObject.h"
#include <time.h>


namespace itk
{

/** \class SimpleFilterWatcher
 * \brief Simple mechanism for monitoring the pipeline events of a filter and reporting these events to std::cout
 *
 * SimpleFilterWatcher provides a simple mechanism for monitoring the
 * execution of filter.  SimpleFilterWatcher is a stack-based object
 * which takes a pointer to a ProcessObject at constructor
 * time. SimpleFilterWatcher creates a series of commands that are
 * registered as observers to the specified ProcessObject. The events
 * monitored are:
 *
 *      StartEvent
 *      EndEvent
 *      ProgressEvent
 *      IterationEvent
 *      AbortEvent
 *
 * The callbacks routines registered for these events emit a simple
 * message to std::cout.
 *
 * Example of use:
 *
 * typedef itk::BinaryThresholdImageFilter<ImageType> FilterType;
 * FilterType::Pointer thresholdFilter = FilterType::New();
 *
 * SimpleFilterWatcher watcher(thresholdFilter, "Threshold");
 *
 * The second argument to the constructor to SimpleFilterWatcher is an
 * optional string that is prepended to the event messages. This
 * allows the user to associate the emitted messages to a particular
 * filter/variable.
 *
 *
 * \todo Allow any stream object to be used for the output (not just std::cout)
 * 
 */
class ITKCommon_EXPORT SimpleFilterWatcher
{
public:
  /** Constructor. Takes a ProcessObject to monitor and an optional
   * comment string that is prepended to each event message. */
  SimpleFilterWatcher(itk::ProcessObject* o, char *comment="");

  /** Destructor. */
  virtual ~SimpleFilterWatcher() {}

  /** Method to get the name of the class be monitored by this
   *  SimpleFilterWatcher */
  const char *GetNameOfClass () {return m_Process->GetNameOfClass();}

  /** Methods to control the verbosity of the messages. Quiet
   * reporting limits the information emitted at a ProgressEvent. */
  void QuietOn() {m_Quiet = true;};
  void QuietOff() {m_Quiet = false;};

  /** Methods to use to test the AbortEvent of the a filter. If
   * TestAbort is on, the filter being watched will be aborted when
   * the progress reaches 30%.*/
  void TestAbortOn() {m_TestAbort = true;};
  void TestAbortOff() {m_TestAbort = true;};
  
protected:

  /** Callback method to show the ProgressEvent */
  virtual void ShowProgress()
  {
    m_Steps++;
    if (!m_Quiet)
      {
      std::cout << " | " << m_Process->GetProgress() << std::flush;
      if ((m_Steps % 10) == 0)
        {
        std::cout << std::endl;
        }
      }
    if (m_TestAbort)
      {
      if (m_Process->GetProgress() > .03)
        {
        m_Process->AbortGenerateDataOn();
        }
      }
  }

  /** Callback method to show the AbortEvent */
  virtual void ShowAbort()
  {
    std::cout << std::endl << "      ABORT" << std::endl << std::flush;
  }

  /** Callback method to show the IterationEvent */
  virtual void ShowIteration()
  {
    std::cout << " # " << std::flush;
    m_Iterations++;
  }

  /** Callback method to show the StartEvent */
  virtual void StartFilter()
  {
    m_Steps = 0;
    m_Iterations = 0;
    m_Start = ::clock();
    std::cout << "-------- Start " << m_Process->GetNameOfClass()
              << " \"" << m_Comment << "\" "
              << m_Process
              << (m_Quiet ? "Progress Quiet " : "Progress ")
              << std::flush;
  }

  /** Callback method to show the EndEvent */
  virtual void EndFilter()
  {
    m_End = ::clock();
    std::cout << std::endl << "Filter took "
              << static_cast<double>(m_End - m_Start) / CLOCKS_PER_SEC
              << " seconds.";
    std::cout << std::endl << std::endl
              << "-------- End " << m_Process->GetNameOfClass()
              << " \"" << m_Comment << "\" "
              << m_Process << std::flush;
    if (m_Steps < 1)
      {
      itkExceptionMacro ("Filter does not have progress.");
      }
    }

private:
  /** Default constructor, purposely not implemented */
  SimpleFilterWatcher(); 

  clock_t m_Start;
  clock_t m_End;
  int m_Steps;
  int m_Iterations;
  bool m_Quiet;
  bool m_TestAbort;
  std::string m_Comment;
  itk::ProcessObject::Pointer m_Process;
};

} // end namespace itk

#endif
