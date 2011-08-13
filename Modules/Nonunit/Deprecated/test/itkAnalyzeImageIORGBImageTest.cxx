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

int itkAnalyzeImageIORGBImageTest(int ac, char* av[])
{
  //
  // first argument is passing in the writable directory to do all testing
  if(ac > 1)
    {
    char *testdir = *++av;
    --ac;
    itksys::SystemTools::ChangeDirectory(testdir);
    }
  const unsigned int Dimension = 3;
  typedef itk::RGBPixel<unsigned char>        RGBPixelType;
  typedef itk::Image<RGBPixelType, Dimension> RGBImageType;

  RGBImageType::Pointer im(NewRGBImage<RGBImageType>());
  itk::ImageRegionIterator<RGBImageType> it(im,im->GetLargestPossibleRegion());
  RGBImageType::DirectionType dir(CORDirCosines<RGBImageType>());
  im->SetDirection(dir);
  vnl_random randgen(8775070);
  for(it.GoToBegin(); !it.IsAtEnd(); ++it)
    {
    RGBPixelType pixel;
    pixel[0] = static_cast<RGBPixelType::ValueType>(randgen.lrand32(0,255));
    pixel[1] = static_cast<RGBPixelType::ValueType>(randgen.lrand32(0,255));
    pixel[2] = static_cast<RGBPixelType::ValueType>(randgen.lrand32(0,255));
    it.Set(pixel);
    }
  int status(EXIT_SUCCESS);
  const std::string filename("RGBImageTest.hdr");
  try
    {
    itk::IOTestHelper::WriteImage<RGBImageType,itk::AnalyzeImageIO>(im,filename);
    }
  catch ( itk::ExceptionObject & ex )
    {
      std::string message;
      message = "Problem found while writing ";
      message += filename;
      message += "\n";
      message += ex.GetLocation();
      message += "\n";
      message += ex.GetDescription();
      std::cerr << message << std::endl;
      status = EXIT_FAILURE;
    }
  if(status == EXIT_SUCCESS)
    {
    RGBImageType::Pointer im2;
    try
      {
      im2 = itk::IOTestHelper::ReadImage<RGBImageType>(filename);
      }
    catch ( itk::ExceptionObject & ex )
      {
      std::string message;
      message = "Problem found while reading ";
      message += filename;
      message += "\n";
      message += ex.GetLocation();
      message += "\n";
      message += ex.GetDescription();
      std::cerr << message << std::endl;
      status = EXIT_FAILURE;
      }
    if(status == EXIT_SUCCESS)
      {
      itk::ImageRegionIterator<RGBImageType> it2(im2,im2->GetLargestPossibleRegion());
      for(it.GoToBegin(),it2.GoToBegin();
          !it.IsAtEnd() && !it2.IsAtEnd();
          ++it,++it2)
        {
        if(it.Value() != it2.Value())
          {
          std::cout << "Pixel "
                    << it2.Value() << " (from disk) != "
                    << it.Value() << " (original image)"
                    << std::endl;
          status = EXIT_FAILURE;
          }
        }
      }
    }
  itk::IOTestHelper::Remove(filename.c_str());
  return status;
}
