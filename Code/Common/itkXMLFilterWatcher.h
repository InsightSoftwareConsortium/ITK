/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkXMLFilterWatcher.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkXMLFilterWatcher_h
#define __itkXMLFilterWatcher_h

#include "itkSimpleFilterWatcher.h"

namespace itk
{

/** \class XMLFilterWatcher
 * \brief Simple mechanism for monitoring the pipeline events of a
 * filter and reporting these events to std::cout. Formats reports
 * with xml.
 */
class ITKCommon_EXPORT XMLFilterWatcher: public SimpleFilterWatcher
{
public:
  XMLFilterWatcher(itk::ProcessObject* o, const char *comment="")
    : SimpleFilterWatcher(o, comment) {};

protected:

/** Callback method to show the ProgressEvent */
virtual void ShowProgress()
{
  if (m_Process)
    {
    m_Steps++;
    if (!m_Quiet)
      {
      std::cout << "<filter-progress>"
                << m_Process->GetProgress()
                << "</filter-progress>"
                << std::endl;
      std::cout << std::flush;
      }
    }
}

/** Callback method to show the StartEvent */
virtual void StartFilter()
{
  m_Steps = 0;
  m_Iterations = 0;
  m_TimeProbe.Start();
  if (!m_Quiet)
    {
    std::cout << "<filter-start>"
              << std::endl;
    std::cout << "<filter-name>"
              << (m_Process.GetPointer()
                  ? m_Process->GetNameOfClass() : "None")
              << "</filter-name>"
              << std::endl;
    std::cout << "<filter-comment>"
              << " \"" << m_Comment << "\" "
              << "</filter-comment>"
              << std::endl;
    std::cout << "</filter-start>"
              << std::endl;
    std::cout << std::flush;
    }
}

/** Callback method to show the EndEvent */
virtual void EndFilter()
{
  m_TimeProbe.Stop();
  if (!m_Quiet)
    {
    std::cout << "<filter-end>"
              << std::endl;
    std::cout << "<filter-name>"
              << (m_Process.GetPointer()
                  ? m_Process->GetNameOfClass() : "None")
              << "</filter-name>"
              << std::endl;
    std::cout << "<filter-time>"
              << m_TimeProbe.GetMeanTime()
              << "</filter-time>"
              << std::endl;
    std::cout << "</filter-end>";
    std::cout << std::flush;
    }
}
  
};

} // end namespace itk

#endif
