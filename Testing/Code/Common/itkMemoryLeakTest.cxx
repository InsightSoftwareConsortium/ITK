/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMemoryLeakTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

// I suspect that Purify is not working correctly.  It should report a memory
// leak for this program.

int itkMemoryLeakTest(int, char* [] )
{
  // Leak 10000 bytes of memory, a little at a time.
  for(unsigned int i=0; i < 100; ++i)
    {
    char* leaker = new char[100];
    *leaker = 0; // Prevent unused variable warning.
    }
  
  return 0;
}
