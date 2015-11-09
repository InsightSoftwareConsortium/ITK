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

#include "itkNiftiImageIOTest.h"

//The WriteNiftiTestFiles function writes binary data to disk to ensure that both big and little endian files are available.
//This allows all the data necessary to create the images to be stored in source files rather than have separate reference images.
int WriteNiftiTestFiles(const std::string & prefix)
{
#include "LittleEndian_hdr.h"
    struct nifti_1_header NiftiLittleEndian;
    memcpy(&NiftiLittleEndian,LittleEndian_hdr,sizeof(NiftiLittleEndian));
    NiftiLittleEndian.qform_code=NIFTI_XFORM_UNKNOWN;
    NiftiLittleEndian.sform_code=NIFTI_XFORM_UNKNOWN;
    strncpy(NiftiLittleEndian.magic,"ni1\0",4);
#include "LittleEndian_img.h"
#include "BigEndian_hdr.h"
    struct nifti_1_header NiftiBigEndian;
    memcpy(&NiftiBigEndian,BigEndian_hdr,sizeof(NiftiBigEndian));
    NiftiBigEndian.qform_code=NIFTI_XFORM_UNKNOWN;
    NiftiBigEndian.sform_code=NIFTI_XFORM_UNKNOWN;
    strncpy(NiftiBigEndian.magic,"ni1\0",4);
#include "BigEndian_img.h"
#include "itkMath.h"
    //Force to be Nifti-compliant
  std::ofstream little_hdr((prefix+"NiftiLittleEndian.hdr").c_str(), std::ios::binary | std::ios::out);
  if(!little_hdr.is_open())
    return EXIT_FAILURE;
  std::cout << "NiftiLittleEndian written" << std::endl;
  little_hdr.write(reinterpret_cast<const char *>(LittleEndian_hdr),sizeof(LittleEndian_hdr));
  little_hdr.close();
  std::ofstream little_img((prefix+"NiftiLittleEndian.img").c_str(), std::ios::binary | std::ios::out);
  if(!little_img.is_open())
    return EXIT_FAILURE;
  little_img.write(reinterpret_cast<const char *>(LittleEndian_img),sizeof(LittleEndian_img));
  little_img.close();
  std::ofstream big_hdr((prefix+"NiftiBigEndian.hdr").c_str(), std::ios::binary | std::ios::out);
  if(!big_hdr.is_open())
    return EXIT_FAILURE;
  big_hdr.write(reinterpret_cast<const char *>(BigEndian_hdr),sizeof(BigEndian_hdr));
  big_hdr.close();
  std::ofstream big_img((prefix+"NiftiBigEndian.img").c_str(), std::ios::binary | std::ios::out);
  if(!big_img.is_open())
    return EXIT_FAILURE;
  big_img.write(reinterpret_cast<const char *>(BigEndian_img),sizeof(BigEndian_img));
  big_img.close();
  return EXIT_SUCCESS;
}

int TestNiftiByteSwap(const std::string & prefix)
{
  int rval;
  typedef itk::Image<double, 3> ImageType;
  if(WriteNiftiTestFiles(prefix) == -1)
    {
      return EXIT_FAILURE;
    }

  ImageType::Pointer little;
  ImageType::Pointer big;

  try
    {
    little = itk::IOTestHelper::ReadImage<ImageType>(prefix+"NiftiLittleEndian.hdr", false);
    const std::string fname(prefix+"NiftiBigEndian.hdr");
    big = itk::IOTestHelper::ReadImage<ImageType>(fname, false);
    std::cout << "Printing Dictionary" << std::endl;
    big->GetMetaDataDictionary().Print(std::cout);
    }
  catch (itk::ExceptionObject &e)
    {
    e.Print(std::cerr);
    RemoveNiftiByteSwapTestFiles(prefix);
    return EXIT_FAILURE;
    }
  rval = 0;
  try
    {
      itk::ImageRegionConstIterator<ImageType> littleIter(little,
                                                          little->GetLargestPossibleRegion());
      itk::ImageRegionConstIterator<ImageType> bigIter(big,
                                                       big->GetLargestPossibleRegion());
      while(!littleIter.IsAtEnd())
        {
          if(itk::Math::NotExactlyEquals(littleIter.Get(), bigIter.Get()))
            break;
          ++littleIter;
          ++bigIter;
        }
      if(!littleIter.IsAtEnd() || !bigIter.IsAtEnd())
        rval = -1;
    }
  catch ( itk::ExceptionObject & ex )
    {
      std::cerr << "Error filling array" << ex << std::endl;
      rval= -1;
    }

  RemoveNiftiByteSwapTestFiles(prefix);
  return rval;
}

void RemoveNiftiByteSwapTestFiles(const std::string & prefix)
{
  itk::IOTestHelper::Remove((prefix+"NiftiLittleEndian.hdr").c_str());
  itk::IOTestHelper::Remove((prefix+"NiftiLittleEndian.img").c_str());
  itk::IOTestHelper::Remove((prefix+"NiftiBigEndian.hdr").c_str());
  itk::IOTestHelper::Remove((prefix+"NiftiBigEndian.img").c_str());
}
