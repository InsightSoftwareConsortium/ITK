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

typedef unsigned char            PixelType;
typedef itk::Image<PixelType,3>  ImageType;
typedef ImageType::Pointer       ImagePointer;

namespace {

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
}

int itkImageFileWriterPastingTest3(int argc, char* argv[])
{
  if( argc < 3 )
    {
    std::cerr << "Usage: " << argv[0] << " input output" << std::endl;
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


  ImageType::Pointer image;

  // update just a specific region
  // Then let the reader go out of scope to orphan the image to have
  // no source.
  {
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );
  reader->SetUseStreaming( true );
  reader->UpdateOutputInformation();

  ImageType::RegionType largestRegion = reader->GetOutput()->GetLargestPossibleRegion();

  ImageType::RegionType ioRegion;
  ImageType::IndexType ioIndex;
  ioIndex[0] = largestRegion.GetIndex()[0]+largestRegion.GetSize()[0]/3;
  ioIndex[1] = largestRegion.GetIndex()[1]+largestRegion.GetSize()[1]/3;
  ioIndex[2] = largestRegion.GetIndex()[2]+largestRegion.GetSize()[2]/3;
  ImageType::SizeType ioSize;
  ioSize[0] = largestRegion.GetSize()[0]/3;
  ioSize[1] = largestRegion.GetSize()[1]/3;
  ioSize[2] = largestRegion.GetSize()[2]/3;
  ioRegion = ImageType::RegionType(ioIndex, ioSize);

  image = reader->GetOutput();

  image->SetRequestedRegion( ioRegion );
  image->Update();
  }

  // Setup the writer with an image which doesn't have a source
  //
  // We expect that the writer should respect the LargestPossibleRegion
  WriterType::Pointer writer = WriterType::New();
  writer->SetFileName(argv[2]);
  writer->SetInput( image );

  // convert the buffered region of the image into an io region
  itk::ImageIORegion  ioRegion(3);
  itk::ImageIORegionAdaptor< ImageType::ImageDimension >::Convert( image->GetBufferedRegion(),
                                                                   ioRegion,
                                                                   image->GetLargestPossibleRegion().GetIndex() );
  writer->SetIORegion(ioRegion);

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


  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );
  reader->UpdateOutputInformation();

  ReaderType::Pointer readerTestImage = ReaderType::New();
  readerTestImage->SetFileName( argv[2] );
  readerTestImage->UpdateOutputInformation();

  // Test that the size of the two images are the same
  if ( reader->GetOutput()->GetLargestPossibleRegion() !=
       readerTestImage->GetOutput()->GetLargestPossibleRegion() )
    {
    std::cerr << "Image size don't match!" << std::endl;
    std::cerr << "Input size: " << reader->GetOutput()->GetLargestPossibleRegion()
              << "Output size: " << readerTestImage->GetOutput()->GetLargestPossibleRegion()
              << std::endl;
    return EXIT_FAILURE;
    }

  // compare the two subregions
  typedef itk::ExtractImageFilter<ImageType, ImageType> ExtractImageFilterType;
  ExtractImageFilterType::Pointer extractTestImage = ExtractImageFilterType::New();
  extractTestImage->SetInput(readerTestImage->GetOutput());
  extractTestImage->SetDirectionCollapseToSubmatrix();
  extractTestImage->SetExtractionRegion( image->GetBufferedRegion() );
  extractTestImage->InPlaceOn();

  typedef itk::ExtractImageFilter<ImageType, ImageType> ExtractImageFilterType;
  ExtractImageFilterType::Pointer extractBaselineImage = ExtractImageFilterType::New();
  extractBaselineImage->SetInput(reader->GetOutput());
  extractBaselineImage->SetDirectionCollapseToSubmatrix();
  extractBaselineImage->SetExtractionRegion( image->GetBufferedRegion() );
  extractBaselineImage->InPlaceOn();


  if (!SameImage(extractTestImage->GetOutput(), extractBaselineImage->GetOutput()))
    {
    std::cerr << "input paste and output paste regions don't match!\n";
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
