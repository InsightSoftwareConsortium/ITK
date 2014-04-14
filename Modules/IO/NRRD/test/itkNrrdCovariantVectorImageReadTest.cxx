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

int itkNrrdCovariantVectorImageReadTest( int ac, char* av[] )
{
  if(ac < 1)
    {
    std::cerr << "Usage: " << av[0] << " Input\n";
    return EXIT_FAILURE;
    }

  typedef itk::CovariantVector<float, 4> PixelType;
  typedef itk::Image<PixelType, 3>       myImage;

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
  myImage::IndexType coord;
  PixelType sample;

  // The test image has been constructed so that the vector coefficients
  // coincide with sample coordinates
  double err = 0;
  unsigned int idx = 0;
  for (unsigned int zi=0; zi<5; zi++)
    {
    coord[2] = zi;
    for (unsigned int yi=0; yi<5; yi++)
      {
      coord[1] = yi;
      for (unsigned int xi=0; xi<5; xi++)
        {
        coord[0] = xi;
        sample = image->GetPixel(coord);
        err += std::fabs(sample[0] - coord[0]);
        err += std::fabs(sample[1] - coord[1]);
        err += std::fabs(sample[2] - coord[2]);
        err += std::fabs(sample[3] - idx);
        idx++;
        }
      }
    }

  if (err)
    {
    std::cout << "test FAILED because values not as expected\n";
    return EXIT_FAILURE;
    }
  else
    {
    return EXIT_SUCCESS;
    }

}
