/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTestCompareTypes.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifdef HAVE_SYS_TYPES_H
#  include <sys/types.h>
#endif /* HAVE_SYS_TYPES_H */

#ifdef HAVE_STDINT_H
#  include <stdint.h>
#endif /* HAVE_STDINT_H */

#ifdef HAVE_STDDEF_H
#  include <stddef.h>
#endif /* HAVE_STDDEF_H */

#define TYPE_LONG_LONG long long

typedef ITK_TEST_COMPARE_TYPE_1 Type1;
typedef ITK_TEST_COMPARE_TYPE_2 Type2;

void function(Type1**) {}

int main()
{
  Type2** p = 0;
  function(p);
  return 0;
}
