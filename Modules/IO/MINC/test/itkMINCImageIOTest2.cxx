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

#include "itkImage.h"
#include <iostream>

#include "itkMINCImageIO.h"
#include "itkMINCImageIOFactory.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

int itkMINCImageIOTest2( int argc, char * argv [] )
{

  if ( argc < 3 )
    {
    std::cerr << "Missing Arguments " << std::endl;
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << " inputfile outputfile " << std::endl;
    return EXIT_FAILURE;
    }

  typedef itk::Image< unsigned short, 3 > ImageType;

  itk::MINCImageIO::Pointer mincIO1 = itk::MINCImageIO::New();
  itk::MINCImageIO::Pointer mincIO2 = itk::MINCImageIO::New();

  typedef itk::ImageFileReader< ImageType > ReaderType;
  typedef itk::ImageFileWriter< ImageType > WriterType;

  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer = WriterType::New();

  reader->SetImageIO( mincIO1 );
  writer->SetImageIO( mincIO2 );

  reader->SetFileName( argv[1] );
  writer->SetFileName( argv[2] );

  writer->SetInput( reader->GetOutput() );

  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  ImageType::ConstPointer image = reader->GetOutput();

  image->Print( std::cout );

  return EXIT_SUCCESS;
}
