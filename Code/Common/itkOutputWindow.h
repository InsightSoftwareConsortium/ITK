/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkOutputWindow.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkOutputWindow_h
#define __itkOutputWindow_h

#include "itkObject.h"

namespace itk
{
/** \class OutputWindow
 * \brief Messages sent from the system are collected by this object.
 *
 * Text messages that the system should display to the user are sent to 
 * this object (or subclasses of this object).
 *
 * \ingroup OSSystemObjects
 */
class ITKCommon_EXPORT OutputWindow : public Object
{
public:
  /** Standard class typedefs. */
  typedef OutputWindow        Self;
  typedef Object  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(OutputWindow, Object);

  /** This is a singleton pattern New.  There will only be ONE
   * reference to a OutputWindow object per process.  Clients that
   * call this must call Delete on the object so that the reference
   * counting will work.   The single instance will be unreferenced when
   * the program exits. */
  static Pointer New();

  /** Return the singleton instance with no reference counting. */
  static Pointer GetInstance();

  /** Supply a user defined output window. Call ->Delete() on the supplied
   * instance after setting it. */
  static void SetInstance(OutputWindow *instance);

  /** Send a string to display. */
  virtual void DisplayText(const char*);

  /** Send a string as an error message to display.
   * The default implementation calls DisplayText() but subclasses
   * could present this message differently. */
  virtual void DisplayErrorText(const char *t) { this->DisplayText(t); };

  /** Send a string as a warningmessage to display.
   * The default implementation calls DisplayText() but subclasses
   * could present this message differently. */
  virtual void DisplayWarningText(const char *t) { this->DisplayText(t); };

  /** Send a string as a message to display.
   * The default implementation calls DisplayText() but subclasses
   * could present this message differently. */
  virtual void DisplayGenericOutputText(const char *t) {this->DisplayText(t);}

  /** Send a string as a debug message to display.
   * The default implementation calls DisplayText() but subclasses
   * could present this message differently. */
  virtual void DisplayDebugText(const char *t) { this->DisplayText(t); };
  
  /** If PromptUser is set to true then each time a line of text
   * is displayed, the user is asked if they want to keep getting
   * messages. */
  itkSetMacro(PromptUser,bool);
  itkGetMacro(PromptUser,bool);
  itkBooleanMacro(PromptUser);
  
protected:
  OutputWindow();
  virtual ~OutputWindow();
  virtual void PrintSelf(std::ostream& os, Indent indent) const;

private:
  OutputWindow(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  bool m_PromptUser;
  static Pointer m_Instance;
};
  
} // end namespace itk

#endif
