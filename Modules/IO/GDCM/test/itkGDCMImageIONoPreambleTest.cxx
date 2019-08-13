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

#include "itkGDCMImageIO.h"
#include "itkImageFileReader.h"

// Specific ImageIO test

/** This test verifies itkGDCMImageIO can read
 *  DICOM files that contain no preamble
 */
int itkGDCMImageIONoPreambleTest(int argc, char *argv[] )
{
  if(argc < 2)
    {
    std::cerr << "Usage: " << argv[0] << " DicomImage\n";
    return EXIT_FAILURE;
    }

  using InputPixelType = short;
  using InputImageType = itk::Image< InputPixelType, 3 >;
  using ReaderType = itk::ImageFileReader< InputImageType >;
  using ImageIOType = itk::GDCMImageIO;

  ImageIOType::Pointer dcmImageIO = ImageIOType::New();
  bool canRead = dcmImageIO->CanReadFile( argv[1] );
  if (!canRead)
    {
    std::cerr << "Cannot read file " << std::endl;
    return EXIT_FAILURE;
    }

  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );
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

  InputImageType::SizeType extentSize;
  extentSize = reader->GetOutput()->GetLargestPossibleRegion().GetSize();
  std::cout << "Read image dimensions: (" << extentSize[0] << ", " << extentSize[1] << ", "  << extentSize[2] << ")" << std::endl;
  if (extentSize[0] == 0 || extentSize[1] == 0 || extentSize[2] == 0)
    {
    std::cerr << "File read but empty " << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
