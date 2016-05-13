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
#include "itkPipelineMonitorImageFilter.h"
#include "itkTestingComparisonImageFilter.h"


typedef unsigned char            PixelType;
typedef itk::Image<PixelType,3>  ImageType;

typedef itk::ImageFileReader<ImageType>         ReaderType;
typedef itk::ImageFileWriter< ImageType >       WriterType;


bool SameImage(std::string output, std::string baseline) {

  PixelType intensityTolerance = 0;
  unsigned int radiusTolerance = 0;
  unsigned int numberOfPixelTolerance = 0;

  ReaderType::Pointer testReader = ReaderType::New();
  ReaderType::Pointer baselineReader = ReaderType::New();
  testReader->SetFileName(output);
  baselineReader->SetFileName(baseline);

  typedef itk::Testing::ComparisonImageFilter<ImageType,ImageType> DiffType;
  DiffType::Pointer diff = DiffType::New();
  diff->SetValidInput(baselineReader->GetOutput());
  diff->SetTestInput(testReader->GetOutput());
  diff->SetDifferenceThreshold( intensityTolerance );
  diff->SetToleranceRadius( radiusTolerance );
  diff->UpdateLargestPossibleRegion();

   unsigned long status = diff->GetNumberOfPixelsWithDifferences();

   if (status > numberOfPixelTolerance)
     return false;
   return true;
}

// This test is designed to improve coverage and test boundary cases
int itkImageFileWriterStreamingTest2(int argc, char* argv[])
{


  if( argc < 3 )
    {
    std::cerr << "Usage: " << argv[0] << " input output " << std::endl;
    return EXIT_FAILURE;
    }

  // We remove the output file
  itksys::SystemTools::RemoveFile(argv[2]);

  //


  unsigned int numberOfDataPieces = 4;

  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );
  reader->SetUseStreaming( true );

  typedef itk::PipelineMonitorImageFilter<ImageType> MonitorFilter;
  MonitorFilter::Pointer monitor = MonitorFilter::New();
  monitor->SetInput(reader->GetOutput());

  WriterType::Pointer writer = WriterType::New();
  writer->SetFileName(argv[2]);
  writer->SetInput(monitor->GetOutput());
  writer->SetNumberOfStreamDivisions(numberOfDataPieces);

  if (std::string(argv[2]) != writer->GetFileName())
    return EXIT_FAILURE;

  if (numberOfDataPieces != writer->GetNumberOfStreamDivisions())
    return EXIT_FAILURE;

  // write the whole image
  try
    {
    std::cout << "=== Updating ==" << std::endl;
    writer->Update();
    }
  catch( itk::ExceptionObject & err )
    {
    std::cerr << "ExceptionObject caught !" << std::endl;
    std::cerr << err << std::endl;
    return EXIT_FAILURE;
    }

  if (!monitor->VerifyAllInputCanStream(4))
    {
    std::cout << monitor;
    return EXIT_FAILURE;
    }

  if (!SameImage( argv[1], argv[2]))
    {
    return EXIT_FAILURE;
    }

  reader->Modified();
  // get the size of the image
  reader->UpdateOutputInformation();
  ImageType::RegionType largestRegion;
  largestRegion = reader->GetOutput()->GetLargestPossibleRegion().GetSize();
  itk::ImageIORegion  ioregion(3);


  ////////////////////////////////////////////////
  // test 1x1 size
  ioregion.SetIndex(0, largestRegion.GetIndex()[0]+largestRegion.GetSize()[0]/2 );
  ioregion.SetIndex(1, largestRegion.GetIndex()[1]+largestRegion.GetSize()[1]/2 );
  ioregion.SetIndex(2, largestRegion.GetIndex()[2]+largestRegion.GetSize()[2]/2 );
  ioregion.SetSize(0, 1);
  ioregion.SetSize(1, 1);
  ioregion.SetSize(2, 1);

  writer->SetIORegion(ioregion);

  try
    {
    std::cout << "=== Updating 1x1x1 IORegion ==" << std::endl;
    writer->Update();
    }
  catch( itk::ExceptionObject & err )
    {
    std::cerr << "ExceptionObject caught !" << std::endl;
    std::cerr << err << std::endl;
    return EXIT_FAILURE;
    }


  if (!monitor->VerifyAllInputCanStream(1))
    {
    std::cout << monitor;
    return EXIT_FAILURE;
    }

  if (!SameImage( argv[1], argv[2]))
    return EXIT_FAILURE;

  reader->Modified();
  ////////////////////////////////////////////////
  // test 2x2 with odd offset
  ioregion.SetIndex(0, largestRegion.GetIndex()[0]+largestRegion.GetSize()[0]/2 + 1);
  ioregion.SetIndex(1, largestRegion.GetIndex()[1]+largestRegion.GetSize()[1]/2 + 1);
  ioregion.SetIndex(2, largestRegion.GetIndex()[2]+largestRegion.GetSize()[2]/2 + 1);
  ioregion.SetSize(0, 2);
  ioregion.SetSize(1, 2);
  ioregion.SetSize(2, 2);

  writer->SetIORegion(ioregion);

  try
    {
    std::cout << "=== Updating 2x2x2 IORegion with odd offset ==" << std::endl;
    writer->Update();
    }
  catch( itk::ExceptionObject & err )
    {
    std::cerr << "ExceptionObject caught !" << std::endl;
    std::cerr << err << std::endl;
    return EXIT_FAILURE;
    }


  if (!monitor->VerifyAllInputCanStream(-1))
    {
    std::cout << monitor;
    return EXIT_FAILURE;
    }

  if (!SameImage( argv[1], argv[2]))
    return EXIT_FAILURE;


  reader->Modified();
  ////////////////////////////////////////////////
  // test long skiny
  ioregion.SetIndex(0, largestRegion.GetIndex()[0]);
  ioregion.SetIndex(1, largestRegion.GetIndex()[1]);
  ioregion.SetIndex(2, largestRegion.GetIndex()[2]);
  ioregion.SetSize(0, 1);
  ioregion.SetSize(1, 1);
  ioregion.SetSize(2, largestRegion.GetSize()[2]);

  writer->SetIORegion(ioregion);

  try
    {
    std::cout << "=== Updating 1x1xlong IORegion ==" << std::endl;
    writer->Update();
    }
  catch( itk::ExceptionObject & err )
    {
    std::cerr << "ExceptionObject caught !" << std::endl;
    std::cerr << err << std::endl;
    return EXIT_FAILURE;
    }

  if (!monitor->VerifyAllInputCanStream(-1))
    {
    std::cout << monitor;
    return EXIT_FAILURE;
    }

  if (!SameImage( argv[1], argv[2]))
    return EXIT_FAILURE;


  reader->Modified();
  ////////////////////////////////////////////////
  // test long skiny
  ioregion.SetIndex(0, largestRegion.GetIndex()[0]);
  ioregion.SetIndex(1, largestRegion.GetIndex()[1]);
  ioregion.SetIndex(2, largestRegion.GetIndex()[2]);
  ioregion.SetSize(0, 1);
  ioregion.SetSize(1, largestRegion.GetSize()[1]);
  ioregion.SetSize(2, 1);

  writer->SetIORegion(ioregion);

  try
    {
    std::cout << "=== Updating 1xlongx1 IORegion ==" << std::endl;
    writer->Update();
    }
  catch( itk::ExceptionObject & err )
    {
    std::cerr << "ExceptionObject caught !" << std::endl;
    std::cerr << err << std::endl;
    return EXIT_FAILURE;
    }

  if (!monitor->VerifyAllInputCanStream(-1))
    {
    std::cout << monitor;
    return EXIT_FAILURE;
    }

  if (!SameImage( argv[1], argv[2]))
    {
    return EXIT_FAILURE;
    }

  reader->Modified();
  ////////////////////////////////////////////////
  // test full region
  ioregion.SetIndex(0, largestRegion.GetIndex()[0]);
  ioregion.SetIndex(1, largestRegion.GetIndex()[1]);
  ioregion.SetIndex(2, largestRegion.GetIndex()[2]);
  ioregion.SetSize(0, largestRegion.GetSize()[0]);
  ioregion.SetSize(1, largestRegion.GetSize()[1]);
  ioregion.SetSize(2, largestRegion.GetSize()[2]);

  writer->SetIORegion(ioregion);

  try
    {
    std::cout << "=== Updating Full IORegion ==" << std::endl;
    writer->Update();
    }
  catch( itk::ExceptionObject & err )
    {
    std::cerr << "ExceptionObject caught !" << std::endl;
    std::cerr << err << std::endl;
    return EXIT_FAILURE;
    }

  if (!monitor->VerifyAllInputCanStream(4))
    {
    std::cout << monitor;
    return EXIT_FAILURE;
    }

  if (!SameImage( argv[1], argv[2]))
    {
    return EXIT_FAILURE;
    }

  reader->Modified();
  bool thrownException = false;
  ////////////////////////////////////////////////
  // test out of bounds region
  ioregion.SetIndex(0, largestRegion.GetIndex()[0] - 1);
  ioregion.SetIndex(1, largestRegion.GetIndex()[1]);
  ioregion.SetIndex(2, largestRegion.GetIndex()[2]);
  ioregion.SetSize(0, largestRegion.GetSize()[0]);
  ioregion.SetSize(1, largestRegion.GetSize()[1]);
  ioregion.SetSize(2, largestRegion.GetSize()[2]);

  writer->SetIORegion(ioregion);

  try
    {
    std::cout << "=== Updating out of bounds IORegion ==" << std::endl;
    writer->Update();
    }
  catch( itk::ExceptionObject & err )
    {
    std::cout << "Caught expected exception" << std::endl;
    std::cout << err << std::endl;
    thrownException = true;
    }

  if (!thrownException)
    return EXIT_FAILURE;


  reader->Modified();
  thrownException = false;
  ////////////////////////////////////////////////
  // test out of bounds region
  ioregion.SetIndex(0, largestRegion.GetIndex()[0]+largestRegion.GetSize()[0]/2 + 1);
  ioregion.SetIndex(1, largestRegion.GetIndex()[1]+largestRegion.GetSize()[1]/2 + 1);
  ioregion.SetIndex(2, largestRegion.GetIndex()[2]+largestRegion.GetSize()[2]/2 + 1);
  ioregion.SetSize(0, largestRegion.GetSize()[0]);
  ioregion.SetSize(1, largestRegion.GetSize()[1]);
  ioregion.SetSize(2, largestRegion.GetSize()[2]+1);

  writer->SetIORegion(ioregion);

  try
    {
    std::cout << "=== Updating out of bounds IORegion ==" << std::endl;
    writer->Update();
    }
  catch( itk::ExceptionObject & err )
    {
    std::cout << "Caught expected exception" << std::endl;
    std::cout << err << std::endl;
    thrownException = true;
    }

  if (!thrownException)
    {
    return EXIT_FAILURE;
    }

  reader->Modified();
  ////////////////////////////////////////////////
  // test when regions aren't matching
  ImageType::RegionType halfLargestRegion;
  halfLargestRegion.SetIndex(largestRegion.GetIndex());
  halfLargestRegion.SetSize(0, largestRegion.GetSize()[0]/2);
  halfLargestRegion.SetSize(1, largestRegion.GetSize()[1]/2);
  halfLargestRegion.SetSize(2, largestRegion.GetSize()[2]/2);


  monitor->GetOutput()->SetRequestedRegion(halfLargestRegion);

  ioregion.SetIndex(0, largestRegion.GetIndex()[0]);
  ioregion.SetIndex(1, largestRegion.GetIndex()[1]);
  ioregion.SetIndex(2, largestRegion.GetIndex()[2]);
  ioregion.SetSize(0, largestRegion.GetSize()[0]/3);
  ioregion.SetSize(1, largestRegion.GetSize()[1]/3);
  ioregion.SetSize(2, largestRegion.GetSize()[2]/3);


  writer->SetIORegion(ioregion);

  try
    {
    std::cout << "=== Preparing mismatched IORegion ==" << std::endl;
    monitor->Update();
    monitor->VerifyAllInputCanStream(1);
    std::cout << "=== Updating mismatched IORegion ==" << std::endl;
    writer->Update();
    }
  catch( itk::ExceptionObject & err )
    {
    std::cerr << "ExceptionObject caught !" << std::endl;
    std::cerr << err << std::endl;
    return EXIT_FAILURE;
    }

   if (!monitor->VerifyAllNoUpdate())
     {
     std::cout << monitor;
     return EXIT_FAILURE;
     }


   if (!SameImage( argv[1], argv[2]))
     return EXIT_FAILURE;


  return EXIT_SUCCESS;
}
