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
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkImageDuplicator.h"
#include "itkAbsImageFilter.h"

int itkImageDuplicatorTest2( int argc, char* argv[] )
{
  if(argc < 3)
    {
    std::cerr << "Usage: " << argv[0] << " Input Output\n";
    return EXIT_FAILURE;
    }

  typedef float PixelType;
  const unsigned int Dimension = 3;
  typedef itk::Image<PixelType, Dimension> ImageType;

  typedef itk::ImageFileReader<ImageType> ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  typedef itk::ImageFileWriter<ImageType> WriterType;
  WriterType::Pointer writer = WriterType::New();
  typedef itk::ImageDuplicator<ImageType> DuplicatorType;
  DuplicatorType::Pointer dup = DuplicatorType::New();
  typedef itk::AbsImageFilter<ImageType, ImageType> AbsType;
  AbsType::Pointer absF = AbsType::New();

  reader->SetFileName(argv[1]);

  try
    {
    reader->Update();
    ImageType::Pointer inImage = reader->GetOutput();

    ImageType::RegionType lpr = inImage->GetLargestPossibleRegion();
    ImageType::RegionType region = lpr;
    for (unsigned d = 0; d < Dimension; d++)
      {
      itk::IndexValueType size = region.GetSize(d);
      region.SetIndex(d, size/4);
      region.SetSize(d, size / 2);
      }

    absF->SetInput(inImage);
    absF->GetOutput()->SetRequestedRegion(region);
    absF->Update();
    ImageType::Pointer absImage = absF->GetOutput(); //different buffered and largest regions

    dup->SetInputImage(absF->GetOutput());
    dup->Update();
    ImageType::ConstPointer dupImage = dup->GetOutput();

    writer->SetInput(dupImage);
    writer->SetFileName(argv[2]);
    writer->Update();
    std::cout << "Test SUCCESS" << std::endl;
    }
  catch (itk::ExceptionObject & e)
    {
    std::cerr << e << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
