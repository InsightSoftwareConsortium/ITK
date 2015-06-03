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

/** Example illustrating use of functions to convert between complex valued
 * voxels, magnitude and phase, and real and imaginary representations.
 *
 * \author Simon K. Warfield simon.warfield\@childrens.harvard.edu
 *
 * \note Attribution Notice. This research work was made possible by Grant
 * Number R01 RR021885 (PI Simon K. Warfield, Ph.D.) from
 * the National Center for Research Resources (NCRR), a component of the
 * National Institutes of Health (NIH).  Its contents are solely the
 * responsibility of the authors and do not necessarily represent the
 * official view of NCRR or NIH.
 *
 */

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkComposeImageFilter.h"

int itkImageReadRealAndImaginaryWriteComplexTest( int argc, char * argv[] )
{
  if( argc != 4 )
    {
    std::cerr << "Usage: " << argv[0] << " inputReal inputImaginary outputComplex" << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int Dimension = 2;

  typedef float                                                   InputPixelType;
  typedef float                                                   OutputPixelType;
  typedef itk::Image< InputPixelType,Dimension >                  InputImageType;
  typedef itk::Image< std::complex<OutputPixelType>,Dimension >   OutputImageType;
  typedef itk::ImageFileReader< InputImageType >                  ReaderType;
  typedef itk::ImageFileWriter< OutputImageType >                 WriterType;

  typedef itk::ComposeImageFilter <
    InputImageType, OutputImageType >  RealAndImaginary2ComplexFilterType;

  ReaderType::Pointer readerReal = ReaderType::New();
  ReaderType::Pointer readerImag = ReaderType::New();
  WriterType::Pointer writer = WriterType::New();

  RealAndImaginary2ComplexFilterType::Pointer RealAndImaginary2Complex = RealAndImaginary2ComplexFilterType::New();

  readerReal->SetFileName( argv[1] );
  readerImag->SetFileName( argv[2] );
  writer->SetFileName( argv[3] );

// Read the image and get its size
  readerReal->Update();
  readerImag->Update();

  RealAndImaginary2Complex->SetInput1(readerReal->GetOutput());
  RealAndImaginary2Complex->SetInput2(readerImag->GetOutput());

  writer->SetInput(RealAndImaginary2Complex->GetOutput());

  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Error writing the magnitude image: " << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  // check that the default template parameters work
  typedef itk::ComposeImageFilter < InputImageType > DefaultParametersFilterType;
  DefaultParametersFilterType::Pointer temp = DefaultParametersFilterType::New();
  if( temp.IsNull() )
    {
    return EXIT_FAILURE;
    }
  return EXIT_SUCCESS;
}
