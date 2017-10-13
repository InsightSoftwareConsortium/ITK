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
#ifndef itkFileOutputWindow_h
#define itkFileOutputWindow_h

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
 * \ingroup ITKCommon
 */

class ITKCommon_EXPORT FileOutputWindow:public OutputWindow
{
public:
  /** Standard class typedefs. */
  typedef FileOutputWindow           Self;
  typedef OutputWindow               Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(FileOutputWindow, OutputWindow);

  /** Send a string to display. */
  virtual void DisplayText(const char *) ITK_OVERRIDE;

  /** Set the filename for the log file */
  itkSetStringMacro(FileName);

  /** Get the filename for the log file */
  itkGetStringMacro(FileName);

  /** Set/Get the buffer flushing mode */
  itkSetMacro(Flush, bool);
  itkGetConstMacro(Flush, bool);
  itkBooleanMacro(Flush);

  /** Setting append will cause the log file to be
   * opened in append mode.  Otherwise, if the log file exists,
   * it will be overwritten each time the FileOutputWindow
   * is created. */
  itkSetMacro(Append, bool);
  itkGetConstMacro(Append, bool);
  itkBooleanMacro(Append);

protected:
  FileOutputWindow();
  virtual ~FileOutputWindow() ITK_OVERRIDE;
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  void Initialize();

  std::ofstream *m_Stream;

  std::string m_FileName;

  bool m_Flush;
  bool m_Append;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(FileOutputWindow);
};
} // end namespace itk

#endif
