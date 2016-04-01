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

#include "itkHessianToObjectnessMeasureImageFilter.h"
#include "itkMultiScaleHessianBasedMeasureImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkSimpleFilterWatcher.h"

int itkMultiScaleHessianBasedMeasureImageFilterTest( int argc, char *argv[] )
{
  if ( argc < 4 )
    {
    std::cerr << "Missing Parameters: "
              << argv[0]
              << " InputImage"
              << " EnhancedOutputImage ScalesOutputImage "
              << " [SigmaMin SigmaMax NumberOfScales ObjectDimension Bright/Dark EnhancedOutputImage2 ScalesOutputImage3]" << std::endl;
    return EXIT_FAILURE;
    }

  // Define the dimension of the images
  const unsigned int Dimension = 2;

  typedef float                                 InputPixelType;
  typedef itk::Image<InputPixelType,Dimension>  InputImageType;


  typedef float                                 OutputPixelType;
  typedef itk::Image<OutputPixelType,Dimension> OutputImageType;

  typedef itk::ImageFileReader<InputImageType>  FileReaderType;

  typedef itk::ImageFileWriter<OutputImageType> FileWriterType;

  typedef itk::NumericTraits< InputPixelType >::RealType RealPixelType;

  typedef itk::SymmetricSecondRankTensor< RealPixelType, Dimension > HessianPixelType;
  typedef itk::Image< HessianPixelType, Dimension >                  HessianImageType;

  // Declare the type of enhancement filter
  typedef itk::HessianToObjectnessMeasureImageFilter< HessianImageType,OutputImageType > ObjectnessFilterType;

  // Declare the type of multiscale enhancement filter
  typedef itk::MultiScaleHessianBasedMeasureImageFilter< InputImageType,HessianImageType, OutputImageType > MultiScaleEnhancementFilterType;

  FileReaderType::Pointer imageReader = FileReaderType::New();
  imageReader->SetFileName(argv[1]);
  try
    {
    imageReader->Update();
    }
  catch (itk::ExceptionObject &ex)
    {
    std::cout << ex << std::endl;
    return EXIT_FAILURE;
    }

  ObjectnessFilterType::Pointer objectnessFilter = ObjectnessFilterType::New();
  objectnessFilter->SetScaleObjectnessMeasure(false);
  objectnessFilter->SetBrightObject(true);
  objectnessFilter->SetAlpha(0.5);
  objectnessFilter->SetBeta(0.5);
  objectnessFilter->SetGamma(5.0);


  MultiScaleEnhancementFilterType::Pointer multiScaleEnhancementFilter = MultiScaleEnhancementFilterType::New();
  multiScaleEnhancementFilter->SetInput(imageReader->GetOutput());
  multiScaleEnhancementFilter->SetHessianToMeasureFilter( objectnessFilter );
  multiScaleEnhancementFilter->SetSigmaStepMethodToLogarithmic();

  itk::SimpleFilterWatcher watcher(multiScaleEnhancementFilter);

  const double tolerance = 0.01;

   if ( argc > 4 )
    {
    double sigmaMinimum = atof( argv[4] );
    multiScaleEnhancementFilter->SetSigmaMinimum( sigmaMinimum );

    if( itk::Math::abs( multiScaleEnhancementFilter->GetSigmaMinimum() - sigmaMinimum ) > tolerance )
      {
      std::cerr << " Error in Set/GetSigmaMinimum() " << std::endl;
      return EXIT_FAILURE;
      }
    }

  if ( argc > 5 )
    {
    double sigmaMaximum = atof( argv[5] );
    multiScaleEnhancementFilter->SetSigmaMaximum( sigmaMaximum );

    if( itk::Math::abs( multiScaleEnhancementFilter->GetSigmaMaximum() - sigmaMaximum ) > tolerance )
      {
      std::cerr << " Error in Set/GetSigmaMaximum() " << std::endl;
      return EXIT_FAILURE;
      }
    }

  if ( argc > 6 )
    {
    unsigned int numberOfSigmaSteps = atoi( argv[6] );
    multiScaleEnhancementFilter->SetNumberOfSigmaSteps( numberOfSigmaSteps );

    if( multiScaleEnhancementFilter->GetNumberOfSigmaSteps() != numberOfSigmaSteps )
      {
      std::cerr << " Error in Set/GetNumberOfSigmaSteps() " << std::endl;
      return EXIT_FAILURE;
      }
    }

  if ( argc > 7 )
    {
    objectnessFilter->SetObjectDimension( atoi(argv[7]) );
    }

  if ( argc > 8 )
    {
    objectnessFilter->SetBrightObject( atoi(argv[8]) );
    }

  multiScaleEnhancementFilter->GenerateScalesOutputOn();
  if ( !multiScaleEnhancementFilter->GetGenerateScalesOutput() )
    {
    std::cerr << "Error in Set/GetGenerateScalesOutput()" << std::endl;
    return EXIT_FAILURE;
    }

  multiScaleEnhancementFilter->SetGenerateScalesOutput( false );
  if ( multiScaleEnhancementFilter->GetGenerateScalesOutput() )
    {
    std::cerr << "Error in Set/GetGenerateScalesOutput()" << std::endl;
    return EXIT_FAILURE;
    }
  multiScaleEnhancementFilter->SetGenerateScalesOutput( true );

  multiScaleEnhancementFilter->GenerateHessianOutputOn();
  if ( !multiScaleEnhancementFilter->GetGenerateHessianOutput() )
    {
    std::cerr << "Error in Set/GetGenerateHessianOutput()" << std::endl;
    return EXIT_FAILURE;
    }
  multiScaleEnhancementFilter->SetGenerateHessianOutput( false );
  if ( multiScaleEnhancementFilter->GetGenerateHessianOutput() )
    {
    std::cerr << "Error in Set/GetGenerateHessianOutput()" << std::endl;
    return EXIT_FAILURE;
    }
  multiScaleEnhancementFilter->SetGenerateHessianOutput( true );

  try
    {
    multiScaleEnhancementFilter->Update();
    }
  catch (itk::ExceptionObject &e)
    {
    std::cerr << e << std::endl;
    }

  FileWriterType::Pointer writer = FileWriterType::New();
  writer->SetFileName(argv[2]);
  writer->UseCompressionOn();
  writer->SetInput( multiScaleEnhancementFilter->GetOutput() );

  try
    {
    writer->Update();
    }
  catch (itk::ExceptionObject &e)
    {
    std::cerr << e << std::endl;
    }

  writer->SetFileName(argv[3]);
  writer->UseCompressionOn();
  writer->SetInput( multiScaleEnhancementFilter->GetScalesOutput() );

  try
    {
    writer->Update();
    }
  catch (itk::ExceptionObject &e)
    {
    std::cerr << e << std::endl;
    }

  const HessianImageType * hessianImage =
    multiScaleEnhancementFilter->GetHessianOutput();

  std::cout << "Hessian Image Buffered Region = " << std::endl;
  std::cout << hessianImage->GetBufferedRegion() << std::endl;

  //Print out
  multiScaleEnhancementFilter->Print( std::cout );

  if ( argc > 9 )
    {
    //Change sigma step to equispaced type and regnerate vesselness image
    multiScaleEnhancementFilter->SetSigmaStepMethod(
          MultiScaleEnhancementFilterType::EquispacedSigmaSteps);

    try
      {
      multiScaleEnhancementFilter->Update();
      }
    catch (itk::ExceptionObject &e)
      {
      std::cerr << e << std::endl;
      }

    FileWriterType::Pointer writer2 = FileWriterType::New();
    writer2->SetFileName(argv[9]);
    writer2->UseCompressionOn();
    writer2->SetInput( multiScaleEnhancementFilter->GetOutput() );

    try
      {
      writer2->Update();
      }
    catch (itk::ExceptionObject &e)
      {
      std::cerr << e << std::endl;
      }
    }

  if ( argc > 10 )
    {
    //Change NonNegativeHessianBasedMeasure to Off and regnerate vesselness image
    multiScaleEnhancementFilter->NonNegativeHessianBasedMeasureOff();

    multiScaleEnhancementFilter->Print( std::cout );

    try
      {
      multiScaleEnhancementFilter->Update();
      }
    catch (itk::ExceptionObject &e)
      {
      std::cerr << e << std::endl;
      }

    FileWriterType::Pointer writer3 = FileWriterType::New();
    writer3->SetFileName(argv[10]);
    writer3->UseCompressionOn();
    writer3->SetInput( multiScaleEnhancementFilter->GetScalesOutput() );

    try
      {
      writer3->Update();
      }
    catch (itk::ExceptionObject &e)
      {
      std::cerr << e << std::endl;
      }
    }

  //Test for NumberOfSigmaSteps = 0
  multiScaleEnhancementFilter->SetNumberOfSigmaSteps( 0 );
  try
    {
    multiScaleEnhancementFilter->Update();
    }
  catch (itk::ExceptionObject &e)
    {
    std::cerr << e << std::endl;
    }

  //Test for NumberOfSigmaSteps = 1
  multiScaleEnhancementFilter->SetNumberOfSigmaSteps( 1 );
  try
    {
    multiScaleEnhancementFilter->Update();
    }
  catch (itk::ExceptionObject &e)
    {
    std::cerr << e << std::endl;
    }

  return EXIT_SUCCESS;
}
