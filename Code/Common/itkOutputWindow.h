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
/**
 * Text messages that the system should display to the user are sent to 
 * this object (or subclasses of this object).
 */
#ifndef __itkOutputWindow_h
#define __itkOutputWindow_h

#include "itkObject.h"

class ITK_EXPORT itkOutputWindow : public itkObject
{
public:
  /** 
   * Smart pointer typedef support. 
   */
  typedef itkSmartPointer<itkOutputWindow> Pointer;

  /** 
   * This is a singleton pattern New.  There will only be ONE
   * reference to a itkOutputWindow object per process.  Clients that
   * call this must call Delete on the object so that the reference
   * counting will work.   The single instance will be unreferenced when
   * the program exits.
   */
  static itkOutputWindow* New();

  /**
   * Return the singleton instance with no reference counting.
   */
  static itkOutputWindow* GetInstance();

  /**
   * Supply a user defined output window. Call ->Delete() on the supplied
   * instance after setting it.
   */
  static void SetInstance(itkOutputWindow *instance);

  /**
   * Send a string to display.
   */
  virtual void DisplayText(const char*);

  /**
   * If PromptUser is set to true then each time a line of text
   * is displayed, the user is asked if they want to keep getting
   * messages.
   */
  void SetPromptUser(bool flag) {itkSetMacro(m_PromptUser,flag);}
  bool GetPromptUser() const {itkGetMacro(m_PromptUser);}
  void PromptUserOn() {this->SetPromptUser(true);}
  void PromptUserOff() {this->SetPromptUser(false);}

protected:
  itkOutputWindow();
  virtual ~itkOutputWindow();
  itkOutputWindow(const itkOutputWindow&) {};
  void operator=(const itkOutputWindow&) {};
  virtual void PrintSelf(std::ostream& os, itkIndent indent);

private:
  bool m_PromptUser;
  static itkOutputWindow* m_Instance;
};

#endif
