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

#include "itkAnalyzeImageIOTest.h"

int
TestDegenerateHeaderFiles()
{
  std::string AugmentName("DegenerateHeaderTest");
  if(WriteAnalyzeTestFiles(AugmentName) == -1)
    {
    return EXIT_FAILURE;
    }
  std::string fname(AugmentName);
  fname += "LittleEndian.hdr";
  typedef itk::Image<unsigned short,3> ImageType;
  ImageType::Pointer img;
  std::fstream header(fname.c_str(),std::ios::binary | std::ios::in | std::ios::out);
  if(!header.is_open())
    {
    return EXIT_FAILURE;
    }
  header.seekg(40,std::ios::beg); // go to location of first element of dim array.
  short int zero[8];
  zero[0] = zero[1] = zero[2] = zero[3] = zero[4] = zero[5] = zero[6] = zero[7] = 0;
  header.write(reinterpret_cast<const char *>(zero),sizeof(zero));
  header.close();
  int error(0);
  try
    {
    img = itk::IOTestHelper::ReadImage<ImageType>(fname);
    }
  catch( itk::ExceptionObject & err )
    {
    std::cout << "Caught an exception: " << std::endl;
    std::cout << err << " " << __FILE__ << " " << __LINE__ << std::endl;
    error++;
    }
  return error ? 0 : 1;
}


int itkAnalyzeImageIOBadHeader(int ac, char* av[])
{
  //
  // first argument is passing in the writable directory to do all testing
  if(ac > 1)
    {
    char *testdir = *++av;
    --ac;
    itksys::SystemTools::ChangeDirectory(testdir);
    }
  itk::ObjectFactoryBase::UnRegisterAllFactories();
  itk::AnalyzeImageIOFactory::RegisterOneFactory();
  int result1 = TestDegenerateHeaderFiles();
  int result2(0);
  return !(result1 == 0 && result2 == 0);
}
