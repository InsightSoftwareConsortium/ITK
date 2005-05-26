/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLogMacro.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itk_LogMacro_h
#define __itk_LogMacro_h


#define itkLogMacro( x, y)  \
{         \
  if (this->GetLogger() ) \
    {  \
    this->GetLogger()->Write(::itk::Logger::x, y); \
    }  \
}


#define itkLogMacroStatic( obj, x, y)  \
{         \
  if (obj->GetLogger() ) \
    {  \
    obj->GetLogger()->Write(::itk::Logger::x, y); \
    }  \
}



#endif  // __itk_LogMacro_h
