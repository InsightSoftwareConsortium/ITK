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

#include "itkFFTConvolutionImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkRichardsonLucyDeconvolutionImageFilter.h"
#include "itkDeconvolutionIterationCommand.h"
#include "itkSimpleFilterWatcher.h"

int itkRichardsonLucyDeconvolutionImageFilterTest(int argc, char* argv[])
{
  if ( argc < 5 )
    {
    std::cerr << "Usage: " << argv[0]
              << " <input image> <kernel image> <output image> <iterations> [convolution image]"
              << std::endl;
    return EXIT_FAILURE;
    }

  typedef float                              PixelType;
  const unsigned int                         Dimension = 2;
  typedef itk::Image< PixelType, Dimension > ImageType;
  typedef itk::ImageFileReader< ImageType >  ReaderType;
  typedef itk::ImageFileWriter< ImageType >  WriterType;

  ReaderType::Pointer inputReader = ReaderType::New();
  inputReader->SetFileName( argv[1] );
  inputReader->Update();

  ReaderType::Pointer kernelReader = ReaderType::New();
  kernelReader->SetFileName( argv[2] );
  kernelReader->Update();

  // Generate a convolution of the input image with the kernel image
  typedef itk::FFTConvolutionImageFilter< ImageType > ConvolutionFilterType;
  ConvolutionFilterType::Pointer convolutionFilter = ConvolutionFilterType::New();
  convolutionFilter->SetInput( inputReader->GetOutput() );
  convolutionFilter->NormalizeOn();
  convolutionFilter->SetKernelImage( kernelReader->GetOutput() );

  // Test the deconvolution algorithm
  typedef itk::RichardsonLucyDeconvolutionImageFilter< ImageType > DeconvolutionFilterType;
  DeconvolutionFilterType::Pointer deconvolutionFilter = DeconvolutionFilterType::New();
  deconvolutionFilter->SetInput( convolutionFilter->GetOutput() );
  deconvolutionFilter->SetKernelImage( kernelReader->GetOutput() );
  deconvolutionFilter->NormalizeOn();
  unsigned int iterations = static_cast< unsigned int >( atoi( argv[4] ) );
  deconvolutionFilter->SetNumberOfIterations( iterations );

  // Add an observer to report on filter iteration progress
  typedef itk::DeconvolutionIterationCommand< DeconvolutionFilterType > IterationCommandType;
  IterationCommandType::Pointer observer = IterationCommandType::New();
  deconvolutionFilter->AddObserver( itk::IterationEvent(), observer );

  itk::SimpleFilterWatcher watcher(deconvolutionFilter);

  // Write the deconvolution result
  try
    {
    WriterType::Pointer writer = WriterType::New();
    writer->SetFileName( argv[3] );
    writer->SetInput( deconvolutionFilter->GetOutput() );
    writer->Update();
    }
  catch ( itk::ExceptionObject & e )
    {
    std::cerr << "Unexpected exception caught when writing deconvolution image: "
              << e << std::endl;
    return EXIT_FAILURE;
    }

  if ( !observer->GetInvoked() )
    {
    std::cerr << "Iteration command observer was never invoked, but should have been." << std::endl;
    return EXIT_FAILURE;
    }

  // Tests to increase coverage
  deconvolutionFilter->Print( std::cout );

  const DeconvolutionFilterType::InternalImageType * estimate =
    deconvolutionFilter->GetCurrentEstimate();
  if ( estimate != ITK_NULLPTR )
    {
    std::cerr << "Estimate should be ITK_NULLPTR after the last iteration." << std::endl;
    return EXIT_FAILURE;
    }

  unsigned int numIterations = 5;
  deconvolutionFilter->SetNumberOfIterations( numIterations );
  if ( deconvolutionFilter->GetNumberOfIterations() != numIterations )
    {
    std::cerr << "Set/GetNumberOfIterations() test failed." << std::endl;
    return EXIT_FAILURE;
    }

  deconvolutionFilter->SetStopIteration( false );
  deconvolutionFilter->SetStopIteration( true );
  if ( deconvolutionFilter->GetStopIteration() != true )
    {
    std::cerr << "Set/GetStopIteration() test failed." << std::endl;
    return EXIT_FAILURE;
    }

  unsigned int iteration = deconvolutionFilter->GetIteration();
  std::cout << "Iteration: " << iteration << std::endl;

  std::cout << deconvolutionFilter->
    DeconvolutionFilterType::Superclass::GetNameOfClass() << std::endl;

  // Instantiate types with non-default template parameters
  typedef itk::Image< float, Dimension >  FloatImageType;
  typedef itk::Image< double, Dimension > DoubleImageType;
  typedef itk::Image< int, Dimension >    IntImageType;

  typedef itk::RichardsonLucyDeconvolutionImageFilter< FloatImageType,
                                                       DoubleImageType,
                                                       IntImageType,
                                                       float > FilterType;
  FilterType::Pointer filter = FilterType::New();
  filter->Print( std::cout );

  return EXIT_SUCCESS;
}
