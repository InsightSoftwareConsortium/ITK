/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSemaphoreTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include <iostream>
#include "itkSemaphore.h"
#include "itkMultiThreader.h"

struct SemaphoreTestUserData
{
  itk::Semaphore::Pointer m_Sem;
  bool m_Flag;
};


ITK_THREAD_RETURN_TYPE print_message_function( void *ptr )
{
 int threadID = ( (itk::MultiThreader::ThreadInfoStruct *)(ptr) )->ThreadID;
 SemaphoreTestUserData *data = static_cast<SemaphoreTestUserData *>(
                      ( (itk::MultiThreader::ThreadInfoStruct *)(ptr) )->UserData );

 if (threadID == 0)
   {
   data->m_Flag = true;
   data->m_Sem->Up();  // unblock if thread 1 is waiting
   }
 else
   {
   data->m_Sem->Down();// block if thread 1 reaches here first
   data->m_Flag = false;
   }
 
  return ITK_THREAD_RETURN_VALUE;
}

int itkSemaphoreTest(int, char*[])
{
  
  SemaphoreTestUserData sem;
  sem.m_Sem = itk::Semaphore::New();
  sem.m_Flag = false;
  
  try
    {  
    sem.m_Sem->Initialize(0);
    itk::MultiThreader::Pointer multithreader = itk::MultiThreader::New();
    multithreader->SetNumberOfThreads(2);
    multithreader->SetSingleMethod( print_message_function, &sem);
    
    for (unsigned int i = 0; i < 2000; i++)
      {
      multithreader->SingleMethodExecute();
      if ( sem.m_Flag == true ) // flag should always be false
        {
        std::cerr << "[TEST FAILED]" << std::endl;
        return 1;
        }
      }
    
    sem.m_Sem->Remove();
    }
  catch (itk::ExceptionObject &e)
    {
    std::cerr << e << std::endl;
    return 2;
    }

  std::cout << "[TEST PASSED]" << std::endl;
  return EXIT_SUCCESS;
}
