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

#include "itkConstantBoundaryCondition.h"
#include "itkWienerDeconvolutionImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkTestingMacros.h"

int itkWienerDeconvolutionImageFilterTest(int argc, char * argv[])
{

  if ( argc < 4 )
    {
    std::cout << "Usage: " << argv[0]
      << " inputImage kernelImage outputImage [normalizeImage]" << std::endl;
    return EXIT_FAILURE;
    }

  const int ImageDimension = 2;

  typedef float                                    PixelType;
  typedef itk::Image< PixelType, ImageDimension >  ImageType;
  typedef itk::ImageFileReader< ImageType >        ReaderType;

  ReaderType::Pointer reader1 = ReaderType::New();
  reader1->SetFileName( argv[1] );
  reader1->Update();

  ReaderType::Pointer reader2 = ReaderType::New();
  reader2->SetFileName( argv[2] );
  reader2->Update();

  itk::ConstantBoundaryCondition< ImageType > cbc;
  cbc.SetConstant( 0.0 );

  typedef itk::FFTConvolutionImageFilter< ImageType > ConvolutionFilterType;
  ConvolutionFilterType::Pointer convolutionFilter
    = ConvolutionFilterType::New();
  convolutionFilter->SetInput( reader1->GetOutput() );
  convolutionFilter->SetKernelImage( reader2->GetOutput() );
  convolutionFilter->SetBoundaryCondition( &cbc );
  // Use the same SizeGreatestPrimeFactor across FFT backends to get
  // consistent results.
  convolutionFilter->SetSizeGreatestPrimeFactor( 5 );

  bool normalize = false;
  if( argc >= 5 )
    {
    normalize = static_cast< bool >( atoi( argv[4] ) );
    }

  convolutionFilter->SetNormalize( normalize );

  typedef itk::WienerDeconvolutionImageFilter< ImageType > DeconvolutionFilterType;
  DeconvolutionFilterType::Pointer deconvolutionFilter = DeconvolutionFilterType::New();

  deconvolutionFilter->SetInput( convolutionFilter->GetOutput() );
  deconvolutionFilter->SetKernelImage( reader2->GetOutput() );
  deconvolutionFilter->SetNormalize( normalize );
  deconvolutionFilter->SetBoundaryCondition( &cbc );
  deconvolutionFilter->SetSizeGreatestPrimeFactor( 5 );

  // Check default NoiseVariance value
  TEST_SET_GET_VALUE( 0.0, deconvolutionFilter->GetNoiseVariance() );
  double noiseVariance = 1.0;
  deconvolutionFilter->SetNoiseVariance( noiseVariance );
  TEST_SET_GET_VALUE( noiseVariance, deconvolutionFilter->GetNoiseVariance() );

  typedef itk::ImageFileWriter< ImageType > WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetFileName( argv[3] );
  writer->SetInput( deconvolutionFilter->GetOutput() );

  try
    {
    writer->Update();
    }
  catch ( itk::ExceptionObject & excp )
    {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  deconvolutionFilter->Print( std::cout );

  // Instantiate types with non-default template parameters
  typedef itk::Image< float, ImageDimension >  FloatImageType;
  typedef itk::Image< double, ImageDimension > DoubleImageType;
  typedef itk::Image< int, ImageDimension >    IntImageType;

  typedef itk::WienerDeconvolutionImageFilter< FloatImageType,
                                               DoubleImageType,
                                               IntImageType,
                                               float > FilterType;
  FilterType::Pointer filter = FilterType::New();
  filter->Print( std::cout );

  return EXIT_SUCCESS;
}
