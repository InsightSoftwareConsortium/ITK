/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFileOutputWindow.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkFileOutputWindow_h
#define __itkFileOutputWindow_h

#include "itkOutputWindow.h"
#include "itkObjectFactory.h"
#include <fstream>

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

class ITKCommon_EXPORT FileOutputWindow : public OutputWindow
{
public:
  /** Standard class typedefs. */
  typedef FileOutputWindow        Self;
  typedef OutputWindow  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(FileOutputWindow, OutputWindow);

  /** Send a string to display. */
  virtual void DisplayText(const char*);

  /** Set the filename for the log file */
  itkSetStringMacro(FileName);
  
  /** Get the filename for the log file */
  itkGetStringMacro(FileName);
  
  /** Set/Get the buffer flushing mode */
  itkSetMacro(Flush,bool);
  itkGetMacro(Flush,bool);
  itkBooleanMacro(Flush);
  
  /** Setting append will cause the log file to be 
   * opened in append mode.  Otherwise, if the log file exists,
   * it will be overwritten each time the FileOutputWindow 
   * is created. */
  itkSetMacro(Append, bool);
  itkGetMacro(Append, bool);
  itkBooleanMacro(Append);
  
protected:
  FileOutputWindow();
  virtual ~FileOutputWindow();
  virtual void PrintSelf(std::ostream& os, Indent indent) const;
  void Initialize();
  
  std::ofstream *m_Stream;
  std::string m_FileName;
  bool m_Flush;
  bool m_Append;

private:
  FileOutputWindow(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
};
  
} // end namespace itk

#endif
