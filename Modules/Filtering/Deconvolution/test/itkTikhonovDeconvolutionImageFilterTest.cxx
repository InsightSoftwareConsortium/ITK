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
#include "itkTikhonovDeconvolutionImageFilter.h"
#include "itkFFTConvolutionImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkTestingMacros.h"

int itkTikhonovDeconvolutionImageFilterTest(int argc, char * argv[])
{

  if ( argc < 4 )
    {
    std::cout << "Usage: " << itkNameOfTestExecutableMacro(argv)
      << " inputImage kernelImage outputImage [normalizeImage]" << std::endl;
    return EXIT_FAILURE;
    }

  constexpr int ImageDimension = 2;

  using PixelType = float;
  using ImageType = itk::Image<PixelType, ImageDimension>;
  using ReaderType = itk::ImageFileReader<ImageType>;

  ReaderType::Pointer reader1 = ReaderType::New();
  reader1->SetFileName( argv[1] );
  reader1->Update();

  ReaderType::Pointer reader2 = ReaderType::New();
  reader2->SetFileName( argv[2] );
  reader2->Update();

  itk::ConstantBoundaryCondition< ImageType > cbc;
  cbc.SetConstant( 0.0 );

  using ConvolutionFilterType = itk::FFTConvolutionImageFilter<ImageType>;
  ConvolutionFilterType::Pointer convolutionFilter
    = ConvolutionFilterType::New();
  convolutionFilter->SetInput( reader1->GetOutput() );
  convolutionFilter->SetKernelImage( reader2->GetOutput() );

  // Use the same SizeGreatestPrimeFactor across FFT backends to get
  // consistent results.
  convolutionFilter->SetSizeGreatestPrimeFactor( 5 );


  bool normalize = false;
  if( argc >= 5 )
    {
    normalize = static_cast< bool >( std::stoi( argv[4] ) );
    }

  convolutionFilter->SetNormalize( normalize );

  using DeconvolutionFilterType = itk::TikhonovDeconvolutionImageFilter<ImageType>;
  DeconvolutionFilterType::Pointer deconvolutionFilter = DeconvolutionFilterType::New();

  deconvolutionFilter->SetInput( convolutionFilter->GetOutput() );
  deconvolutionFilter->SetKernelImage( reader2->GetOutput() );
  deconvolutionFilter->SetNormalize( normalize );
  deconvolutionFilter->SetBoundaryCondition( &cbc );
  deconvolutionFilter->SetSizeGreatestPrimeFactor( 5 );

  // Check default RegularizationConstant value
  ITK_TEST_SET_GET_VALUE( 0.0, deconvolutionFilter->GetRegularizationConstant() );
  double regularizationConstant = 1.0e-4;
  deconvolutionFilter->SetRegularizationConstant( regularizationConstant );
  ITK_TEST_SET_GET_VALUE( regularizationConstant,
                      deconvolutionFilter->GetRegularizationConstant() );

  using WriterType = itk::ImageFileWriter<ImageType>;
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
  using FloatImageType = itk::Image< float, ImageDimension >;
  using DoubleImageType = itk::Image< double, ImageDimension >;
  using IntImageType = itk::Image< int, ImageDimension >;

  using FilterType = itk::TikhonovDeconvolutionImageFilter< FloatImageType,
                                                 DoubleImageType,
                                                 IntImageType,
                                                 float >;
  FilterType::Pointer filter = FilterType::New();
  filter->Print( std::cout );

  return EXIT_SUCCESS;
}
