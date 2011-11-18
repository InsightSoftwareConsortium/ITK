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

#include <fstream>
#include "itkImageFileReader.h"
#include "itkMetaImageIO.h"


#define SPECIFIC_IMAGEIO_MODULE_TEST

int itkMetaImageIOGzTest(int ac, char* av[])
{
  if(ac < 2)
    {
    std::cerr << "Usage: itkMetaImageIOGzTest testDataDirectory"
              <<    std::endl;
    }
  int result(0);
  std::cout << "Test whether MetaIO will search for a compressed data file"
            << std::endl
            << "if it can't find the uncompressed data file"
            << std::endl;
  std::string headerName(av[1]);
  headerName += "/GzTest.mhd";
  std::ofstream hdr(headerName.c_str());
  hdr << "ObjectType = Image" << std::endl
      << "NDims = 2" << std::endl
      << "DimSize = 32 32" << std::endl
      << "ElementType = MET_USHORT" << std::endl
      << "ElementDataFile = GzTest.raw" << std::endl;
  hdr.close();
  std::string dataName(av[1]);
  dataName += "/GzTest.raw.gz";
  gzFile compressed = gzopen(dataName.c_str(),"wb");
  for(unsigned short i = 0; i < (32 * 32); i++)
    {
    unsigned short pixel = i & 0xff;
    if( gzwrite(compressed,&pixel,sizeof(pixel)) != sizeof(pixel) )
      {
      std::cerr << "Write error for " << dataName << std::endl;
      break;
      }
    }
  gzclose(compressed);

  typedef unsigned short           PixelType;
  typedef itk::Image<PixelType, 3> myImage;

  itk::ImageFileReader<myImage>::Pointer reader
    = itk::ImageFileReader<myImage>::New();
  reader->SetFileName(headerName.c_str());

  itk::MetaImageIO::Pointer io = itk::MetaImageIO::New();
  reader->SetImageIO(io);

  try
    {
    reader->Update();
    }
  catch (itk::ExceptionObject & e)
    {
    std::cerr << "exception in file reader " << std::endl;
    std::cerr << e << std::endl;
    result++;
    }
  std::cout << "Test whether absolute path in MetaIO header works" << std::endl;
  // re-write header
  headerName = av[1];
  headerName += "/AbsPathTest.mhd";
  std::ofstream hdr2(headerName.c_str());
  hdr2 << "ObjectType = Image" << std::endl
       << "NDims = 2" << std::endl
       << "DimSize = 32 32" << std::endl
       << "ElementType = MET_USHORT" << std::endl
       << "CompressedData = True" << std::endl
       << "ElementDataFile = " << dataName
       << std::endl;
  hdr2.close();
  reader->SetFileName(headerName.c_str());
  try
    {
    reader->Update();
    }
  catch (itk::ExceptionObject & e)
    {
    std::cerr << "Test failed " << std::endl;
    std::cerr << e << std::endl;
    result++;
    }

  return result == 0 ? EXIT_SUCCESS : EXIT_FAILURE;
}
