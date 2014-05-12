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

int WriteAnalyzeTestFiles(const std::string & AugmentName)
{
#include "LittleEndian_hdr.h"
#include "LittleEndian_img.h"
#include "BigEndian_hdr.h"
#include "BigEndian_img.h"
  std::string LittleEndianHdrName=AugmentName+"LittleEndian.hdr";
  std::ofstream little_hdr(LittleEndianHdrName.c_str(), std::ios::binary | std::ios::out);
  if(!little_hdr.is_open())
    {
    return EXIT_FAILURE;
    }
  //std::cout << LittleEndianHdrName << " written" << std::endl;
  little_hdr.write(reinterpret_cast<const char *>(LittleEndian_hdr),sizeof(LittleEndian_hdr));
  little_hdr.close();

  std::string LittleEndianZName(AugmentName);
  LittleEndianZName += "LittleEndianZ.hdr";
  std::ofstream  littlez_hdr(LittleEndianZName.c_str(), std::ios::binary | std::ios::out);
  if(!littlez_hdr.is_open())
    {
    return EXIT_FAILURE;
    }
  littlez_hdr.write(reinterpret_cast<const char *>(LittleEndian_hdr),sizeof(LittleEndian_hdr));

  std::string LittleEndianImgName=AugmentName+"LittleEndian.img";
  std::ofstream little_img(LittleEndianImgName.c_str(), std::ios::binary | std::ios::out);
  if(!little_img.is_open())
    {
    return EXIT_FAILURE;
    }
  // write out compressed.
  little_img.write(reinterpret_cast<const char *>(LittleEndian_img),sizeof(LittleEndian_img));
  little_img.close();

  // write out compressed image
  std::string ImageZFilename(AugmentName);
  ImageZFilename += "LittleEndianZ.img.gz";
  gzFile  file_p = ::gzopen( ImageZFilename.c_str(), "wb" );
  if( file_p==ITK_NULLPTR )
    {
    return EXIT_FAILURE;
    }
  ::gzwrite(file_p,reinterpret_cast<const char *>(LittleEndian_img),
            sizeof(LittleEndian_img));
  ::gzclose(file_p);

  std::string BigEndianHdrName=AugmentName+"BigEndian.hdr";
  std::ofstream big_hdr(BigEndianHdrName.c_str(), std::ios::binary | std::ios::out);
  if(!big_hdr.is_open())
    {
    return EXIT_FAILURE;
    }
  big_hdr.write(reinterpret_cast<const char *>(BigEndian_hdr),sizeof(BigEndian_hdr));
  big_hdr.close();
  std::string BigEndianImgName=AugmentName+"BigEndian.img";
  std::ofstream big_img(BigEndianImgName.c_str(), std::ios::binary | std::ios::out);
  if(!big_img.is_open())
    {
    return EXIT_FAILURE;
    }
  big_img.write(reinterpret_cast<const char *>(BigEndian_img),sizeof(BigEndian_img));
  big_img.close();
  return EXIT_SUCCESS;
}
