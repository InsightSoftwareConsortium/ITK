/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itk_hashtable.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef itk_emulation_hashtable_h
#define itk_emulation_hashtable_h

#include <itksys/hash_fun.hxx>
#include <itksys/hashtable.hxx>

namespace itk
{
using itksys::hash;
using itksys::hashtable;
}

#endif // itk_emulation_hashtable_h
