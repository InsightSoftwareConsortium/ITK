/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkMemoryLeakTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


Copyright (c) 2000 National Library of Medicine
All rights reserved.

See COPYRIGHT.txt for copyright details.

=========================================================================*/

// I suspect that Purify is not working correctly.  It should report a memory
// leak for this program.

int main()
{
  // Leak 10000 bytes of memory, a little at a time.
  for(unsigned int i=0; i < 100; ++i)
    {
    char* leaker = new char[100];
    leaker = 0; // Prevent unused variable warning.
    }
  
  return 0;
}
