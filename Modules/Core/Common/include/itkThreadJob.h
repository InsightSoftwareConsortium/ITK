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

#ifndef itkThreadJob_h
#define itkThreadJob_h

#include "itkMacro.h"

namespace itk
{
/**
 * \class ThreadJob
 *
 * \brief This class is used to submit jobs to the thread pool.
 * The thread job maintains important information of the submitted job
 * such as Job Id, information to identify if the job has finished executing.
 * It holds the function pointer that the user sets to the function the
 * user wants to be executed in parallel by the thread pool.
 * Also holds the args pointer - it is passed to the executing function by
 * the thread pool.
 * \ingroup OSSystemObjects
 * \ingroup ITKCommon
 */
struct ThreadJob
{
public:
  typedef int JobIdType;

  ThreadJob() :
    m_ThreadFunction(0),
    m_Id(-1),
    m_Assigned(false),
    m_Executed(false),
    m_UserData(ITK_NULLPTR)
  {
  }

  ~ThreadJob()
  {
  }


/** Declaring function thatwill be called */
#if defined(_WIN32) || defined(_WIN64)
    DWORD ( __stdcall *m_ThreadFunction )( void * ptr );
#else
    void * (*m_ThreadFunction)(void *ptr);
#endif

  /** This is the Job's id. If it is -1 it means the job hasn't been
    initialized*/
  JobIdType m_Id;

  /** Set if the job is assigned to a thread */
  bool m_Assigned;

  /**  set if job is finished */
  bool m_Executed;

 /** Stores the user's data that needs to be passed into the function */
  void *m_UserData;

};

} // end namespace itk

#endif // itkThreadJob_h
