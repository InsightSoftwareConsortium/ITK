/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTestIOStreamsWCharFilenameConstructors.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

/*
   This file tests whether fstream's have a wchar_t * constructor
*/

#include <fstream>

int main() 
{
  std::ofstream ostr( L"tmptest.txt" );
  std::ifstream istr( L"tmptest.txt" );
  return 0;
}

