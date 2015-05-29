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

//

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkFilterWatcher.h"
#include "itkFFTPadImageFilter.h"
#include "itkZeroFluxNeumannBoundaryCondition.h"
#include "itkPeriodicBoundaryCondition.h"
#include "itkConstantBoundaryCondition.h"

int itkFFTPadImageFilterTest( int argc, char * argv[] )
{
  if( argc < 5 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  inputImageFile outputImageFile greatestPrimeFactor padType" << std::endl;
    return EXIT_FAILURE;
    }

  //
  //  The following code defines the input and output pixel types and their
  //  associated image types.
  //
  const unsigned int Dimension = 2;
  typedef unsigned char                       PixelType;
  typedef itk::Image< PixelType,  Dimension > ImageType;


  // readers/writers
  typedef itk::ImageFileReader< ImageType  >  ReaderType;
  typedef itk::ImageFileWriter< ImageType >   WriterType;
  typedef itk::FFTPadImageFilter< ImageType > FFTPadType;


  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  itk::ZeroFluxNeumannBoundaryCondition< ImageType > zfnCond;
  itk::ConstantBoundaryCondition< ImageType > zeroCond;
  itk::PeriodicBoundaryCondition< ImageType > wrapCond;

  // Create the filters
  FFTPadType::Pointer  fftpad = FFTPadType::New();
  fftpad->SetInput(  reader->GetOutput() );
  fftpad->SetSizeGreatestPrimeFactor( atoi( argv[3] ) );
  std::string padMethod = argv[4];
  if( padMethod == "Mirror")
    {
//    fftpad->SetBoundaryCondition( &mirrorCond );
    }
  else if( padMethod == "Zero")
    {
    fftpad->SetBoundaryCondition( &zeroCond );
    }
  else if( padMethod == "ZeroFluxNeumann")
    {
    fftpad->SetBoundaryCondition( &zfnCond );
    }
  else if( padMethod == "Wrap")
    {
    fftpad->SetBoundaryCondition( &wrapCond );
    }
  FilterWatcher watchFFTPad(fftpad,"fftpad");

  WriterType::Pointer writer  = WriterType::New();
  writer->SetInput(fftpad->GetOutput());
  writer->SetFileName(  argv[2] );
  writer->Update();

  return EXIT_SUCCESS;

}
