/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFileOutputWindow.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef __itkFileOutputWindow_h
#define __itkFileOutputWindow_h

#include "itkOutputWindow.h"
#include "itkObjectFactory.h"

namespace itk
{
/** \class FileOutputWindow
 * \brief Messages sent from the system are sent to a file.
 *
 * Text messages that the system should display to the user are sent to 
 * this object (or subclasses of this object) and are logged to a file.
 *
 * \ingroup OSSystemObjects
 * 
 */

class ITK_EXPORT FileOutputWindow : public OutputWindow
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef FileOutputWindow        Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef OutputWindow  Superclass;

  /** 
   * Smart pointer typedef support. 
   */
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(FileOutputWindow, OutputWindow);

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
   * Send a string to display.
   */
  virtual void DisplayText(const char*);

  /**
   * Set the filename for the log file
   */
  itkSetStringMacro(FileName);
  
  /**
   * Get the filename for the log file
   */
  itkGetStringMacro(FileName);
  
  /**
   * Set/Get the buffer flushing mode
   */
  itkSetMacro(Flush,bool);
  itkGetMacro(Flush,bool);
  itkBooleanMacro(Flush);

  /**
   * Setting append will cause the log file to be 
   * opened in append mode.  Otherwise, if the log file exists,
   * it will be overwritten each time the vtkFileOutputWindow 
   * is created.
   */
  itkSetMacro(Append, bool);
  itkGetMacro(Append, bool);
  itkBooleanMacro(Append);
  

protected:
  FileOutputWindow();
  virtual ~FileOutputWindow();
  FileOutputWindow(const Self&) {}
  void operator=(const Self&) {}
  virtual void PrintSelf(std::ostream& os, Indent indent) const;
  void Initialize();

  std::ofstream *m_Stream;
  std::string m_FileName;
  bool m_Flush;
  bool m_Append;
};
  
} // end namespace itk

#endif
