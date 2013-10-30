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

#include "itkImageFileReader.h"
#include "itkGDCMImageIO.h"
#include "itkVersor.h"

#define SPECIFIC_IMAGEIO_MODULE_TEST

/** This test verifies that the direction cosines
 *  computed in itkGDCMImageIO are orthogonal
 */
int itkGDCMImageIOOrthoDirTest(int ac, char* av[])
{

  if(ac < 2)
    {
    std::cerr << "Usage: " << av[0] << " DicomImage\n";
    return EXIT_FAILURE;
    }

  typedef short                                  InputPixelType;
  typedef itk::Image< InputPixelType, 3 >        InputImageType;
  typedef itk::ImageFileReader< InputImageType > ReaderType;
  typedef itk::GDCMImageIO                       ImageIOType;

  ImageIOType::Pointer dcmImageIO = ImageIOType::New();

  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( av[1] );
  reader->SetImageIO( dcmImageIO );

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

  InputImageType::DirectionType directionCosines;
  directionCosines = reader->GetOutput()->GetDirection();

  std::cout << "Dir Cosines " << directionCosines;

  itk::Versor<itk::SpacePrecisionType> rotation;

  try
    {
    rotation.Set(directionCosines);
    }
  catch (itk::ExceptionObject & e)
    {
    std::cerr << "exception setting matrix" << std::endl;
    std::cerr << e << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;

}
