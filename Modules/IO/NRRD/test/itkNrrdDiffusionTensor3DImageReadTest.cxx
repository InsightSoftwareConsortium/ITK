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
#include "itkNrrdImageIO.h"

#define SPECIFIC_IMAGEIO_MODULE_TEST

int itkNrrdDiffusionTensor3DImageReadTest( int ac, char* av[] )
{
  if(ac < 1)
    {
    std::cerr << "Usage: " << av[0] << " Input\n";
    return EXIT_FAILURE;
    }

  typedef itk::DiffusionTensor3D<float> PixelType;
  typedef itk::Image<PixelType, 3>      myImage;

  typedef itk::ImageFileReader<myImage>  ReaderType;

  ReaderType::Pointer reader = ReaderType::New();

  reader->SetImageIO( itk::NrrdImageIO::New() );

  reader->SetFileName(av[1]);

  try
    {
    reader->Update();
    }
  catch (itk::ExceptionObject & e)
    {
    std::cerr << "exception in file reader " << std::endl;
    std::cerr << e << std::endl;
    return EXIT_FAILURE;
    }

  myImage::Pointer image = reader->GetOutput();

  // Pick off some pixels in the test image (the first, the last, and
  // a few in between) and make sure that the values are very close to
  // what we know to be correct, given that we know exactly what
  // volume is being read.  I/O errors will generate more than tiny
  // differences near representational precision.

  float err = 0;
  myImage::IndexType coord;
  PixelType sample;
  coord[0] = 0;
  coord[1] = 0;
  coord[2] = 0;
  sample = image->GetPixel(coord);
  err += std::fabs(sample(0,0) - 4.0248222);
  err += std::fabs(sample(0,1) - -0.2367909);
  err += std::fabs(sample(0,2) - 0.23370844);
  err += std::fabs(sample(1,1) - 1.2593846);
  err += std::fabs(sample(1,2) - -0.042955428);
  err += std::fabs(sample(2,2) - -0.15239859);

  coord[0] = 4;
  coord[1] = 5;
  coord[2] = 6;
  sample = image->GetPixel(coord);
  err += std::fabs(sample(0,0) - 6.3746634);
  err += std::fabs(sample(0,1) - -0.10792637);
  err += std::fabs(sample(0,2) - 0.078167915);
  err += std::fabs(sample(1,1) - 6.2988205);
  err += std::fabs(sample(1,2) - 0.40197921);
  err += std::fabs(sample(2,2) - 4.8993769);

  coord[0] = 3;
  coord[1] = 3;
  coord[2] = 3;
  sample = image->GetPixel(coord);
  err += std::fabs(sample(0,0) - 4.9852095);
  err += std::fabs(sample(0,1) - 0.51356757);
  err += std::fabs(sample(0,2) - -1.1457335);
  err += std::fabs(sample(1,1) - 0.6538533);
  err += std::fabs(sample(1,2) - -0.19546235);
  err += std::fabs(sample(2,2) - 1.3520061);

  coord[0] = 3;
  coord[1] = 0;
  coord[2] = 5;
  sample = image->GetPixel(coord);
  err += std::fabs(sample(0,0) - 6.1530757);
  err += std::fabs(sample(0,1) - 0.40738893);
  err += std::fabs(sample(0,2) - 0.11354899);
  err += std::fabs(sample(1,1) - 5.968379);
  err += std::fabs(sample(1,2) - -0.22650599);
  err += std::fabs(sample(2,2) - 5.0935121);

  coord[0] = 0;
  coord[1] = 3;
  coord[2] = 6;
  sample = image->GetPixel(coord);
  err += std::fabs(sample(0,0) - 7.3227096);
  err += std::fabs(sample(0,1) - -0.34219909);
  err += std::fabs(sample(0,2) - -0.2011447);
  err += std::fabs(sample(1,1) - 5.6443777);
  err += std::fabs(sample(1,2) - 0.43205333);
  err += std::fabs(sample(2,2) - 5.3755102);

  double thresh = 0.00000041;
  if (err > thresh)
    {
    std::cout << "failure because err == " << err
              << "> " << thresh << std::endl;
    return EXIT_FAILURE;
    }
  else
    {
    return EXIT_SUCCESS;
    }

}
