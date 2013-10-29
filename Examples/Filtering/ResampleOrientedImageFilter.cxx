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
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkMatrix.h"

#include "itkResampleImageFilter.h"

int main( int argc, char * argv[] )
{
  if( argc < 7 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  inputImageFile  outputImageFile";
    std::cerr << "  direction cosines" << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int                               Dimension = 2;
  typedef unsigned char                            InputPixelType;
  typedef unsigned char                            OutputPixelType;
  typedef itk::Image< InputPixelType,  Dimension > InputImageType;
  typedef itk::Image< OutputPixelType, Dimension > OutputImageType;

  typedef itk::ImageFileReader< InputImageType  >  ReaderType;
  typedef itk::ImageFileWriter< OutputImageType >  WriterType;

  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer = WriterType::New();

  reader->SetFileName( argv[1] );
  writer->SetFileName( argv[2] );


  typedef itk::ResampleImageFilter<InputImageType,OutputImageType> FilterType;
  FilterType::Pointer filter = FilterType::New();

  filter->SetDefaultPixelValue( 0 );

    {
    //pixel spacing in millimeters
    const double spacing[ Dimension ] = { 1.0, 1.0 };
    filter->SetOutputSpacing( spacing );
    }

    {
    // space coordinate of origin
    const double origin[ Dimension ] = { 0.0, 0.0 };
    filter->SetOutputOrigin( origin );
    }

  OutputImageType::DirectionType direction;
  direction(0,0) = atoi(argv[3]);
  direction(1,0) = atoi(argv[4]);
  direction(0,1) = atoi(argv[5]);
  direction(1,1) = atoi(argv[6]);
  filter->SetOutputDirection(direction);

  InputImageType::SizeType   size;
  size[0] = 300;  // number of pixels along X
  size[1] = 300;  // number of pixels along Y

  filter->SetSize( size );
  filter->SetInput( reader->GetOutput() );
  writer->SetInput( filter->GetOutput() );
  writer->Update();

  return EXIT_SUCCESS;
}
