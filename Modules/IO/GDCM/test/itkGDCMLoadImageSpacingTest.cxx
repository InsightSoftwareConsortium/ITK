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

typedef itk::Image<unsigned short, 2>   ImageType;
typedef itk::ImageFileReader<ImageType> ReaderType;

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

template <typename TIO, typename TSeriesNames>
int
ReadFile(const char *fname)
{
  typename TIO::Pointer imageIO = TIO::New();
  typename ReaderType::Pointer reader = ReaderType::New();

  reader->SetImageIO(imageIO);
  reader->SetFileName(fname);

  reader->Update();
  typename ImageType::Pointer image = reader->GetOutput();
  std::cout << image << std::endl;
  typename ImageType::SpacingType spacing = image->GetSpacing();
  if(vcl_abs(spacing[0]-0.178038) >= 0.000001 ||
     vcl_abs(spacing[1]-0.174924) >= 0.000001)
    {
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}

int itkGDCMLoadImageSpacingTest(int argc, char *argv[])
{
  if(argc < 2)
    {
    std::cerr << "Missing image filename argument" << std::endl;
    return EXIT_FAILURE;
    }

  const char *fname = argv[1];
  return ReadFile<itk::GDCMImageIO,itk::GDCMSeriesFileNames>(fname);
}
