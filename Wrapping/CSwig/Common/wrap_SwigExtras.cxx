/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_SwigExtras.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include <vector>
#include <string>
#include <list>
#ifdef CABLE_CONFIGURATION
#include "itkCSwigMacros.h"

namespace _cable_
{
  const char* const group = "SwigExtras";
  namespace renames
  {
    typedef std::vector<std::string>::vector StringVector;
    typedef std::list<std::string>::list StringList;
  }
}


#endif
