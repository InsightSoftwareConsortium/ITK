/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itk_hash_map.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef itk_emulation_hash_map_h
#define itk_emulation_hash_map_h

#include <itksys/hash_map.hxx>
namespace itk
{
using itksys::hash;
using itksys::hash_map;
using itksys::hash_multimap;
}

#endif // itk_emulation_hash_map_h
