/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTestCStyleIOWCharFilename.cxx
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
   This file tests whether we have _wopen and the like
*/

#include <io.h> // for _wopen
#include <fcntl.h> // for _O_RDONLY
#include <stdio.h> // for _wfopen

int main() 
{
  _wopen( L"tmptest.txt", _O_RDONLY );
  _wfopen( L"tmptest.txt", L"r" );
  _wunlink( L"tmptest.txt" );
  return 0;
}

