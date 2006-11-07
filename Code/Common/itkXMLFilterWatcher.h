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
class XMLFilterWatcher: public SimpleFilterWatcher
{
public:
  XMLFilterWatcher(itk::ProcessObject* o, const char *comment="")
    : SimpleFilterWatcher(o, comment) {};

protected:

/** Callback method to show the ProgressEvent */
virtual void ShowProgress()
{
  if (this->GetProcess())
    {
    int steps = this->GetSteps();
    steps++;
    this->SetSteps(steps);
    if (!this->GetQuiet())
      {
      std::cout << "<filter-progress>"
                << this->GetProcess()->GetProgress()
                << "</filter-progress>"
                << std::endl;
      std::cout << std::flush;
      }
    }
}

/** Callback method to show the StartEvent */
virtual void StartFilter()
{
  this->SetSteps(0);
  this->SetIterations(0);
  this->GetTimeProbe().Start();
  if (!this->GetQuiet())
    {
    std::cout << "<filter-start>"
              << std::endl;
    std::cout << "<filter-name>"
              << (this->GetProcess()
                  ? this->GetProcess()->GetNameOfClass() : "None")
              << "</filter-name>"
              << std::endl;
    std::cout << "<filter-comment>"
              << " \"" << this->GetComment() << "\" "
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
#if 0
  this-GetTimeProbe().Stop();
  if (!this->GetQuiet())
    {
    std::cout << "<filter-end>"
              << std::endl;
    std::cout << "<filter-name>"
              << (this->GetProcess()
                  ? this->GetProcess()->GetNameOfClass() : "None")
              << "</filter-name>"
              << std::endl;
    std::cout << "<filter-time>"
              << this->GetTimeProbe().GetMeanTime()
              << "</filter-time>"
              << std::endl;
    std::cout << "</filter-end>";
    std::cout << std::flush;
    }
#endif
}
  
};

} // end namespace itk

#endif
