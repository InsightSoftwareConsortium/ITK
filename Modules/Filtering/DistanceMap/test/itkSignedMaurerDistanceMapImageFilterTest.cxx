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
#include "itkImageFileWriter.h"

#include "itkSignedMaurerDistanceMapImageFilter.h"
#include "itkSignedDanielssonDistanceMapImageFilter.h"

int itkSignedMaurerDistanceMapImageFilterTest( int argc, char * argv[] )
{
   if( ! ( argc == 3 || argc == 4 ) )
    {
    std::cerr << "Usage: " << argv[0]
      << " InputImage SignedMaurDistanceMapFilterOutput [SignedDanielssonDistanceMapImageFilterOutput]\n";
    return -1;
    }

 const unsigned int      ImageDimension = 3;
  typedef int             InputPixelType;
  typedef double          OutputPixelType;

  typedef itk::Image<InputPixelType,  ImageDimension>  InputImageType;
  typedef itk::Image<OutputPixelType, ImageDimension>  OutputImageType;

  typedef itk::ImageFileReader<InputImageType>    ReaderType;
  typedef itk::ImageFileWriter<OutputImageType>   WriterType;
  typedef InputImageType::SizeType                InputSizeType;

  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(argv[1]);
  reader->Update();

    {
    typedef itk::SignedMaurerDistanceMapImageFilter
      <InputImageType, OutputImageType>  FilterType;

    FilterType::Pointer filter = FilterType::New();
    filter->SetInput( reader->GetOutput() );
    filter->SetSquaredDistance( false );
    filter->SetUseImageSpacing( true  );
    filter->SetInsideIsPositive( true );
    filter->Update();
    filter->Print(std::cout);

    WriterType::Pointer writer = WriterType::New();
    writer->SetInput( filter->GetOutput() );
    writer->SetFileName( argv[2] );
    writer->UseCompressionOn();
    writer->Update();
    }

  if(argc == 4 )
    {
    typedef itk::SignedDanielssonDistanceMapImageFilter
      <InputImageType, OutputImageType>  FilterType;

    FilterType::Pointer filter = FilterType::New();
    filter->SetInput( reader->GetOutput() );
    filter->SetSquaredDistance( false );
    filter->SetUseImageSpacing( true  );
    filter->SetInsideIsPositive( true );
    filter->Update();
    filter->Print(std::cout);

    WriterType::Pointer writer = WriterType::New();
    writer->SetInput( filter->GetOutput() );
    writer->SetFileName( argv[3] );
    writer->UseCompressionOn();
    writer->Update();
    }

  return EXIT_SUCCESS;
}
