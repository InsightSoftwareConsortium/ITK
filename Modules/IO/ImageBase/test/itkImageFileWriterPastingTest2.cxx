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
#include "itkImageFileWriter.h"
#include "itkTestingComparisonImageFilter.h"
#include "itkExtractImageFilter.h"
#include "itkPipelineMonitorImageFilter.h"

typedef unsigned char            PixelType;
typedef itk::Image<PixelType,3>  ImageType;
typedef ImageType::Pointer       ImagePointer;

bool SameImage(ImagePointer testImage, ImagePointer baselineImage)
{
  PixelType intensityTolerance = 0;
  int radiusTolerance = 0;
  unsigned long numberOfPixelTolerance = 0;

  typedef itk::Testing::ComparisonImageFilter<ImageType,ImageType> DiffType;
  DiffType::Pointer diff = DiffType::New();
  diff->SetValidInput(baselineImage);
  diff->SetTestInput(testImage);
  diff->SetDifferenceThreshold( intensityTolerance );
  diff->SetToleranceRadius( radiusTolerance );
  diff->UpdateLargestPossibleRegion();

  unsigned long status = diff->GetNumberOfPixelsWithDifferences();

  if (status > numberOfPixelTolerance)
    {
    return false;
    }

  return true;
}

int itkImageFileWriterPastingTest2(int argc, char* argv[])
{
  if( argc < 3 )
    {
    std::cerr << "Usage: " << argv[0] << " input output [existingFile]" << std::endl;
    return EXIT_FAILURE;
    }

  // We remove the output file
  if (argc == 3)
    {
    itksys::SystemTools::RemoveFile(argv[2]);
    }
  else
    {
    // copy this file to over write
    itksys::SystemTools::CopyAFile(argv[3], argv[2]);
    }


  typedef itk::ImageFileReader<ImageType>    ReaderType;
  typedef itk::ImageFileWriter< ImageType >  WriterType;

  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );
  reader->SetUseStreaming( true );

  // We decide how we want to read the image and we split accordingly
  // The image is read slice by slice
  reader->UpdateOutputInformation();

  ImageType::RegionType largestRegion;
  largestRegion = reader->GetOutput()->GetLargestPossibleRegion().GetSize();

  ImageType::IndexType pasteIndex;
  pasteIndex[0] = largestRegion.GetIndex()[0]+largestRegion.GetSize()[0]/3;
  pasteIndex[1] = largestRegion.GetIndex()[1]+largestRegion.GetSize()[1]/3;
  pasteIndex[2] = largestRegion.GetIndex()[2]+largestRegion.GetSize()[2]/3;
  ImageType::SizeType pasteSize;
  pasteSize[0] = largestRegion.GetSize()[0]/3;
  pasteSize[1] = largestRegion.GetSize()[1]/3;
  pasteSize[2] = largestRegion.GetSize()[2]/3;
  ImageType::RegionType pasteRegion(pasteIndex, pasteSize);

  typedef itk::PipelineMonitorImageFilter<ImageType> MonitorFilter;
  MonitorFilter::Pointer monitor = MonitorFilter::New();
  monitor->SetInput(reader->GetOutput());

  // Setup the writer
  WriterType::Pointer writer = WriterType::New();
  writer->SetFileName(argv[2]);
  writer->SetInput(monitor->GetOutput());

  // create a vaild region from the largest
  itk::ImageIORegion  ioregion(3);
  itk::ImageIORegion::IndexType index;

  index.push_back(pasteIndex[0]);
  index.push_back(pasteIndex[1]);
  index.push_back(pasteIndex[2]);
  ioregion.SetIndex(index);
  itk::ImageIORegion::SizeType size;
  size.push_back(pasteSize[0]);
  size.push_back(pasteSize[1]);
  size.push_back(pasteSize[2]);
  ioregion.SetSize(size);
  writer->SetIORegion(ioregion);

  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & err )
    {

    std::cerr << "ExceptionObject caught !" << std::endl;
    std::cerr << err << std::endl;
    if (argc > 3)
      {
      return EXIT_SUCCESS;
      }
    return EXIT_FAILURE;
    }

  //check that the pipeline executed as expected
  if (monitor->GetNumberOfUpdates() != 1)
    {
    std::cerr << "pipeline did not execute as expected" << std::endl;

    std::cout << monitor;
    return EXIT_FAILURE;
    }

  typedef itk::ExtractImageFilter<ImageType, ImageType> ExtractImageFilterType;
  ExtractImageFilterType::Pointer extractBaselineImage = ExtractImageFilterType::New();
  extractBaselineImage->SetDirectionCollapseToSubmatrix();
  extractBaselineImage->SetInput(reader->GetOutput());
  extractBaselineImage->SetExtractionRegion(pasteRegion);

  ReaderType::Pointer readerTestImage = ReaderType::New();
  readerTestImage->SetFileName( argv[2] );
  ExtractImageFilterType::Pointer extractTestImage = ExtractImageFilterType::New();
  extractTestImage->SetDirectionCollapseToSubmatrix();
  extractTestImage->SetInput(readerTestImage->GetOutput());
  extractTestImage->SetExtractionRegion(pasteRegion);

  if (!SameImage(extractTestImage->GetOutput(), extractBaselineImage->GetOutput()))
    {
    std::cerr << "input paste and output paste regions don't match!\n";
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
