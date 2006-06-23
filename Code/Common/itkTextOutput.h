/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTextOutput.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkTextOutput_h
#define __itkTextOutput_h

#include "itkOutputWindow.h"
#include "itkObjectFactory.h"

namespace itk
{

// this class is used to send output to stdout and not the itk window
class ITKCommon_EXPORT TextOutput : public OutputWindow
{
public:
  typedef TextOutput                Self;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  itkNewMacro(TextOutput);
  virtual void DisplayText(const char* s)
    { std::cout << s << std::endl; }

protected:
  TextOutput();
  virtual ~TextOutput();

private:
  TextOutput(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
};


}


#endif
