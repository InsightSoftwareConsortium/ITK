/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTextOutput.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
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
};


}


