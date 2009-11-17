/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTestConvertTypes.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

/*=========================================================================

  Program:   Visualization Toolkit
  Module:    itkTestConvertTypes.cxx

  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
  All rights reserved.
  See Copyright.txt or http://www.kitware.com/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/

#define TYPE_UNSIGNED___INT64 unsigned __int64

typedef ITK_TEST_CONVERT_TYPE_TO TypeTo;
typedef ITK_TEST_CONVERT_TYPE_FROM TypeFrom;

void function(TypeTo& l, TypeFrom const& r)
{
  l = static_cast<TypeTo>(r);
}

int main()
{
  TypeTo tTo = TypeTo();
  TypeFrom tFrom = TypeFrom();
  function(tTo, tFrom);
  return 0;
}
