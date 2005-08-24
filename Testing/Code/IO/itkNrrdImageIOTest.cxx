/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNrrdImageIOTest.cxx
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

// some compilers have trouble with the size of this test
#define ITK_LEAN_AND_MEAN

#include "itkNrrdImageIOTest.h"


// This test is for the NRRD image IO.  The strategy is to generate random
// images of various data types, dimensionalities and sizes, write these images
// as NRRDs, read them back, and compare the read images with the originals.
int itkNrrdImageIOTest(int ac, char* av[])
{
  std::string inputFile;
  if(ac < 2)
    {
    std::cerr << "Usage: " << av[0] << " Output\n";
    return EXIT_FAILURE;
    }

  if (ac > 2)
    {
    inputFile = std::string(av[2]);
    }
  else
    {
    inputFile = std::string("null");
    }
  const int sz = 10;
  int ret = EXIT_SUCCESS;
  
  ret += itkNrrdImageIOTestReadWriteTest<char, 2>(std::string(av[1]), sz, inputFile);
  ret += itkNrrdImageIOTestReadWriteTest<unsigned short, 2>(std::string(av[1]), sz, inputFile);
  ret += itkNrrdImageIOTestReadWriteTest<signed short, 2>(std::string(av[1]), sz, inputFile);
  ret += itkNrrdImageIOTestReadWriteTest<float, 2>(std::string(av[1]), sz, inputFile);  
  ret += itkNrrdImageIOTestReadWriteTest<double, 2>(std::string(av[1]), sz, inputFile);
  ret += itkNrrdImageIOTestReadWriteTest<unsigned char, 2>(std::string(av[1]), sz, inputFile);

  ret += itkNrrdImageIOTestReadWriteTest<char, 3>(std::string(av[1]), sz, inputFile);
  ret += itkNrrdImageIOTestReadWriteTest<unsigned char, 3>(std::string(av[1]), sz, inputFile);
  ret += itkNrrdImageIOTestReadWriteTest<signed short, 3>(std::string(av[1]), sz, inputFile);
  ret += itkNrrdImageIOTestReadWriteTest<unsigned short, 3>(std::string(av[1]), sz, inputFile);
  ret += itkNrrdImageIOTestReadWriteTest<float, 3>(std::string(av[1]), sz, inputFile);
  ret += itkNrrdImageIOTestReadWriteTest<double, 3>(std::string(av[1]), sz, inputFile);

  ret += itkNrrdImageIOTestReadWriteTest<char, 4>(std::string(av[1]), sz, inputFile);
  ret += itkNrrdImageIOTestReadWriteTest<unsigned char, 4>(std::string(av[1]), sz, inputFile);
  ret += itkNrrdImageIOTestReadWriteTest<signed short, 4>(std::string(av[1]), sz, inputFile);
  ret += itkNrrdImageIOTestReadWriteTest<unsigned short, 4>(std::string(av[1]), sz, inputFile);
  ret += itkNrrdImageIOTestReadWriteTest<float, 4>(std::string(av[1]), sz, inputFile);
  ret += itkNrrdImageIOTestReadWriteTest<double, 4>(std::string(av[1]), sz, inputFile);

  // Test with compression on
  ret += itkNrrdImageIOTestReadWriteTest<char, 4>(std::string(av[1]), sz, inputFile, true);
  ret += itkNrrdImageIOTestReadWriteTest<unsigned char, 4>(std::string(av[1]), sz, inputFile, true);
  ret += itkNrrdImageIOTestReadWriteTest<signed short, 4>(std::string(av[1]), sz, inputFile, true);
  ret += itkNrrdImageIOTestReadWriteTest<unsigned short, 4>(std::string(av[1]), sz, inputFile, true);
  ret += itkNrrdImageIOTestReadWriteTest<float, 4>(std::string(av[1]), sz, inputFile, true);
  ret += itkNrrdImageIOTestReadWriteTest<double, 4>(std::string(av[1]), sz, inputFile, true);


  if (ret == EXIT_SUCCESS)
    {
    std::cout << "TEST PASSED!" << std::endl;
    }
  else
    {
    std::cout << "TEST FAILED WITH RETURN VALUE " << ret << std::endl;
    }
  return ret;
}
