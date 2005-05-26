/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkConsoleLogOutput.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include<iostream>
#include"itkConsoleLogOutput.h"


namespace itk
{

SimpleFastMutexLock ConsoleLogOutput::m_Mutex;


ConsoleLogOutput::ConsoleLogOutput()
{
  m_Mutex.Lock();
  std::cout.precision(15);
  m_Mutex.Unlock();
}


ConsoleLogOutput::~ConsoleLogOutput()
{
  m_Mutex.Lock();
  std::cout.flush();
  m_Mutex.Unlock();
}


/** flush a buffer */
void ConsoleLogOutput::Flush()
{
  ConsoleLogOutput::m_Mutex.Lock();
  std::cout.flush();
  ConsoleLogOutput::m_Mutex.Unlock();
}


/** Write to a buffer */
void ConsoleLogOutput::Write(double timestamp)
{
  ConsoleLogOutput::m_Mutex.Lock();
  std::cout << timestamp;
  ConsoleLogOutput::m_Mutex.Unlock();
}


/** Write to a buffer */
void ConsoleLogOutput::Write(std::string const &content)
{
  ConsoleLogOutput::m_Mutex.Lock();
  std::cout << content;
  ConsoleLogOutput::m_Mutex.Unlock();
}


/** Write to a buffer */
void ConsoleLogOutput::Write(std::string const &content, double timestamp)
{
  ConsoleLogOutput::m_Mutex.Lock();
  std::cout << timestamp << "  :  " << content;
  ConsoleLogOutput::m_Mutex.Unlock();
}


}

