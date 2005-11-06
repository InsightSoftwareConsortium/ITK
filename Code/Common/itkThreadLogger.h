/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkThreadLogger.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkThreadLogger_h
#define __itkThreadLogger_h

#include "itkLogger.h"
#include "itkLoggerThreadWrapper.h"

namespace itk
{

/** Needed for backwards compatibility with the original code
  */
#if defined(USE_MSVS6_HACKS)
typedef LoggerThreadWrapper ThreadLogger;
#else
typedef LoggerThreadWrapper<Logger> ThreadLogger;
#endif

} // namespace itk


#endif  // __itkThreadLogger_h
