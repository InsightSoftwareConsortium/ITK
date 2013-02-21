/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#define SPECIFIC_IMAGEIO_MODULE_TEST

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
  ret += itkNrrdImageIOTestReadWriteTest<uint16_t, 2>(std::string(av[1]), sz, inputFile);
  ret += itkNrrdImageIOTestReadWriteTest<int16_t, 2>(std::string(av[1]), sz, inputFile);
  ret += itkNrrdImageIOTestReadWriteTest<float, 2>(std::string(av[1]), sz, inputFile);
  ret += itkNrrdImageIOTestReadWriteTest<double, 2>(std::string(av[1]), sz, inputFile);
  ret += itkNrrdImageIOTestReadWriteTest<uint8_t, 2>(std::string(av[1]), sz, inputFile);

  ret += itkNrrdImageIOTestReadWriteTest<char, 3>(std::string(av[1]), sz, inputFile);
  ret += itkNrrdImageIOTestReadWriteTest<uint8_t, 3>(std::string(av[1]), sz, inputFile);
  ret += itkNrrdImageIOTestReadWriteTest<int16_t, 3>(std::string(av[1]), sz, inputFile);
  ret += itkNrrdImageIOTestReadWriteTest<uint16_t, 3>(std::string(av[1]), sz, inputFile);
  ret += itkNrrdImageIOTestReadWriteTest<float, 3>(std::string(av[1]), sz, inputFile);
  ret += itkNrrdImageIOTestReadWriteTest<double, 3>(std::string(av[1]), sz, inputFile);

  ret += itkNrrdImageIOTestReadWriteTest<char, 4>(std::string(av[1]), sz, inputFile);
  ret += itkNrrdImageIOTestReadWriteTest<uint8_t, 4>(std::string(av[1]), sz, inputFile);
  ret += itkNrrdImageIOTestReadWriteTest<int16_t, 4>(std::string(av[1]), sz, inputFile);
  ret += itkNrrdImageIOTestReadWriteTest<uint16_t, 4>(std::string(av[1]), sz, inputFile);
  ret += itkNrrdImageIOTestReadWriteTest<float, 4>(std::string(av[1]), sz, inputFile);
  ret += itkNrrdImageIOTestReadWriteTest<double, 4>(std::string(av[1]), sz, inputFile);

  // Test with compression on
  ret += itkNrrdImageIOTestReadWriteTest<char, 4>(std::string(av[1]), sz, inputFile, true);
  ret += itkNrrdImageIOTestReadWriteTest<uint8_t, 4>(std::string(av[1]), sz, inputFile, true);
  ret += itkNrrdImageIOTestReadWriteTest<int16_t, 4>(std::string(av[1]), sz, inputFile, true);
  ret += itkNrrdImageIOTestReadWriteTest<uint16_t, 4>(std::string(av[1]), sz, inputFile, true);
  ret += itkNrrdImageIOTestReadWriteTest<float, 4>(std::string(av[1]), sz, inputFile, true);
  ret += itkNrrdImageIOTestReadWriteTest<double, 4>(std::string(av[1]), sz, inputFile, true);

  // Now we try to read a file which doen't exist
  itkNrrdImageIOTestReadWriteTest<double, 4>(std::string(av[1]), sz, "IDontExist.nrrd" );

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
