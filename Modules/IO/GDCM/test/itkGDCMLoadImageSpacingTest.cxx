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
#include <iostream>
#include "itkGDCMSeriesFileNames.h"
#include "itkGDCMImageIO.h"
#include "itkImageFileReader.h"
#include "gdcmImageHelper.h"

//
// This test is specifically for a problem detected in GDCMImageIO
// by Andriy Fedorov, who noticed that the DCMTK reader
// reported the correct spacing for this DICOM dataset, but GDCM
// report spacing of 1,1,1 ... looking through the gdcm library
// code in Modules/ThirdParty/GDCM, I noticed that in
// gdcmImageHelper.cxx there was a bunch of elaborate code that in the
// case of Andriy's dataset, used the wrong tag to find the spacing,
// ignored the error when the tag wasn't found, and silenetly returned
// 1,1,1 for the spacing.
// The patch that this test is part of has a fallback for the case of
// the MediaStorageTypes for which gdcm has trouble with the spacing tag.

int itkGDCMLoadImageSpacingTest(int argc, char *argv[])
{
  if(argc < 4)
    {
    std::cerr << "Usage: " << argv[0] << " Image Spacing0 Spacing1" << std::endl;
    return EXIT_FAILURE;
    }

  const char * imageFilename = argv[1];
  const double spacing0 = atof( argv[2] );
  const double spacing1 = atof( argv[3] );

  typedef itk::Image<unsigned short, 2>   ImageType;
  typedef itk::ImageFileReader<ImageType> ReaderType;

  itk::GDCMImageIO::Pointer imageIO = itk::GDCMImageIO::New();
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetImageIO( imageIO );
  reader->SetFileName( imageFilename );
  try
    {
    reader->Update();
    }
  catch( itk::ExceptionObject & error )
    {
    std::cerr << "Error when reading input: " << error << std::endl;
    }

  ImageType::Pointer image = reader->GetOutput();
  std::cout << image << std::endl;
  ImageType::SpacingType spacing = image->GetSpacing();
  if(std::abs( spacing[0]- spacing0 ) >= 0.000001 ||
     std::abs( spacing[1]- spacing1 ) >= 0.000001 )
    {
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
