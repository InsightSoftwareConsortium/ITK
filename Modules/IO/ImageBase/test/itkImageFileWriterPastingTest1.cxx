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

int itkImageFileWriterPastingTest1(int argc, char* argv[])
{
  if( argc < 3 )
    {
    std::cerr << "Usage: " << argv[0] << " input output" << std::endl;
    return EXIT_FAILURE;
    }

  // We remove the output file
  itksys::SystemTools::RemoveFile(argv[2]);

  typedef unsigned char            PixelType;
  typedef itk::Image<PixelType,3>  ImageType;

  typedef itk::ImageFileReader<ImageType>    ReaderType;
  typedef itk::ImageFileWriter< ImageType >  WriterType;

  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );
  reader->SetUseStreaming( true );

  ImageType::RegionType region;
  ImageType::SizeType size;
  ImageType::SizeType fullsize;
  ImageType::IndexType index;

  unsigned int m_NumberOfPieces = 10;

  // We decide how we want to read the image and we split accordingly
  // The image is read slice by slice
  reader->UpdateOutputInformation();
  fullsize = reader->GetOutput()->GetLargestPossibleRegion().GetSize();

  index.Fill(0);
  size[0] = fullsize[0];
  size[1] = fullsize[1];
  size[2] = 0;

  unsigned int zsize = fullsize[2]/m_NumberOfPieces;

  // Setup the writer
  WriterType::Pointer writer = WriterType::New();
  writer->SetFileName(argv[2]);

  for(unsigned int i=0;i<m_NumberOfPieces;i++)
    {
    std::cout << "Reading piece " << i+1 << " of " << m_NumberOfPieces << std::endl;

    index[2] += size[2];

    // At the end we need to adjust the size to make sure
    // we are reading everything
    if(i == m_NumberOfPieces-1)
      {
      size[2] = fullsize[2]-index[2];
      }
    else
      {
      size[2] = zsize;
      }

    region.SetIndex(index);
    region.SetSize(size);

    // Write the image
    itk::ImageIORegion  ioregion(3);
    itk::ImageIORegionAdaptor<ImageType::ImageDimension>::
      Convert( region, ioregion, reader->GetOutput()->GetLargestPossibleRegion().GetIndex());

    writer->SetIORegion(ioregion);
    writer->SetInput(reader->GetOutput());

    try
      {
      writer->Update();
      }
    catch( itk::ExceptionObject & err )
      {
      std::cerr << "ExceptionObject caught !" << std::endl;
      std::cerr << err << std::endl;
      return EXIT_FAILURE;
      }
    } // end for pieces


  return EXIT_SUCCESS;
}
