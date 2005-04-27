/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itk_hash_set.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef itk_emulation_hash_set_h
#define itk_emulation_hash_set_h

#include <itksys/hash_set.hxx>

namespace itk
{
using itksys::hash;
using itksys::hash_set;
using itksys::hash_multiset;
}

#endif // itk_emulation_hash_set_h
