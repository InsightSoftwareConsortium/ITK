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
#include "itkGaussianBlurImageFunction.h"
#include "itkImageRegionIterator.h"

int main( int argc, char * argv[] )
{
  if( argc < 5 ) {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  inputImageFile  outputImageFile sigma maxKernelWidth" << std::endl;
    return EXIT_FAILURE;
  }

  typedef itk::Image< float, 2 >                     ImageType;
  typedef itk::ImageFileReader< ImageType >          ReaderType;
  typedef itk::ImageRegionIterator< ImageType >      IteratorType;
  typedef itk::ImageRegionConstIterator< ImageType > ConstIteratorType;

  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );
  reader->Update();

  const ImageType * inputImage = reader->GetOutput();

  ImageType::RegionType region = inputImage->GetBufferedRegion();

  ConstIteratorType it( inputImage, region );

  ImageType::Pointer output = ImageType::New();

  output->SetRegions( region );
  output->SetOrigin(  inputImage->GetOrigin()  );
  output->SetSpacing( inputImage->GetSpacing() );
  output->Allocate();

  IteratorType out( output, region );

  typedef itk::GaussianBlurImageFunction< ImageType > GFunctionType;
  GFunctionType::Pointer gaussianFunction = GFunctionType::New();
  gaussianFunction->SetInputImage( inputImage );

  GFunctionType::ErrorArrayType setError;
  setError.Fill( 0.01 );
  gaussianFunction->SetMaximumError( setError );
  gaussianFunction->SetSigma( atof( argv[3] ) );
  gaussianFunction->SetMaximumKernelWidth( atoi( argv[4] ) );

  it.GoToBegin();
  out.GoToBegin();

  while( !it.IsAtEnd() )
    {
    out.Set( gaussianFunction->EvaluateAtIndex(it.GetIndex() ) );
    ++it;
    ++out;
    }

  typedef itk::ImageFileWriter < ImageType > WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetFileName(argv[2]);
  writer->SetInput(output);
  writer->Update();

  return EXIT_SUCCESS;
}
