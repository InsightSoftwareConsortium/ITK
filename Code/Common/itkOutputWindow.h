/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkOutputWindow.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

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
 */

class ITK_EXPORT OutputWindow : public Object
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef OutputWindow        Self;

  /** 
   * Smart pointer typedef support. 
   */
  typedef SmartPointer<Self>  Pointer;

  /** 
   * This is a singleton pattern New.  There will only be ONE
   * reference to a OutputWindow object per process.  Clients that
   * call this must call Delete on the object so that the reference
   * counting will work.   The single instance will be unreferenced when
   * the program exits.
   */
  static Pointer New();

  /**
   * Return the singleton instance with no reference counting.
   */
  static Pointer GetInstance();

  /**
   * Supply a user defined output window. Call ->Delete() on the supplied
   * instance after setting it.
   */
  static void SetInstance(OutputWindow *instance);

  /**
   * Send a string to display.
   */
  virtual void DisplayText(const char*);

  /**
   * If PromptUser is set to true then each time a line of text
   * is displayed, the user is asked if they want to keep getting
   * messages.
   */
  itkSetMacro(PromptUser,bool);
  itkGetMacro(PromptUser,bool);
  itkBooleanMacro(PromptUser);

protected:
  OutputWindow();
  virtual ~OutputWindow();
  OutputWindow(const Self&) {}
  void operator=(const Self&) {}
  virtual void PrintSelf(std::ostream& os, Indent indent);

private:
  bool m_PromptUser;
  static Pointer m_Instance;
};
  
} // end namespace itk

#endif
