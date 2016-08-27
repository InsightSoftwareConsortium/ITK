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
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkHessianRecursiveGaussianImageFilter.h"

int itkHessianToObjectnessMeasureImageFilterTest( int argc, char *argv[] )
{
  if ( argc < 3 )
    {
    std::cerr << "Missing Parameters: "
              << argv[0]
              << " Input_Image"
              << " Enhanced_Output_Image [ObjectDimension] [Bright/Dark]" << std::endl;
    return EXIT_FAILURE;
    }

  // Define the dimension of the images
  const unsigned char Dim = 2;

  typedef float PixelType;

  // Declare the types of the images
  typedef itk::Image<PixelType,Dim> ImageType;

  typedef itk::ImageFileReader<ImageType> FileReaderType;
  typedef itk::ImageFileWriter<ImageType> FileWriterType;

  // Declare the type of the recursive Gaussian filter
  typedef itk::HessianRecursiveGaussianImageFilter<
                                            ImageType >  GaussianImageFilterType;

  typedef GaussianImageFilterType::OutputImageType        HessianImageType;

  // Delcare the type of objectness measure image filter

  typedef itk::HessianToObjectnessMeasureImageFilter<HessianImageType, ImageType > ObjectnessFilterType;

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

  // Create a Gaussian Filter
  GaussianImageFilterType::Pointer gaussianFilter = GaussianImageFilterType::New();

  // Create a vesselness Filter
  ObjectnessFilterType::Pointer objectnessFilter = ObjectnessFilterType::New();

  // Connect the input images
  gaussianFilter->SetInput( imageReader->GetOutput() );
  objectnessFilter->SetInput( gaussianFilter->GetOutput() );

  objectnessFilter->SetScaleObjectnessMeasure(false);
  if ( objectnessFilter->GetScaleObjectnessMeasure() )
    {
    std::cerr << "Error in Set/GetScaleObjectnessMeasure method" << std::endl;
    return EXIT_FAILURE;
    }

  //Exercise more methods
  objectnessFilter->ScaleObjectnessMeasureOn();
  if ( !objectnessFilter->GetScaleObjectnessMeasure() )
    {
    std::cerr << "Error in ScaleObjectnessMeasureOn" << std::endl;
    return EXIT_FAILURE;
    }

  objectnessFilter->ScaleObjectnessMeasureOff();
  if ( objectnessFilter->GetScaleObjectnessMeasure() )
    {
    std::cerr << "Error in ScaleObjectnessMeasureOff method" << std::endl;
    return EXIT_FAILURE;
    }

  objectnessFilter->SetBrightObject(true);
  if ( ! objectnessFilter->GetBrightObject() )
    {
    std::cerr << "Error in Set/GetBrightObject method" << std::endl;
    return EXIT_FAILURE;
    }

  const double tolerance = 0.001;

  double alphaValue = 0.5;
  objectnessFilter->SetAlpha(alphaValue);
  if( itk::Math::abs( objectnessFilter->GetAlpha() - alphaValue ) >= tolerance )
    {
    std::cerr << "Error in Set/GetAlpha() method" << std::endl;
    return EXIT_FAILURE;
    }

  double  betaValue = 0.5;
  objectnessFilter->SetBeta(betaValue);
  if( itk::Math::abs( objectnessFilter->GetBeta() - betaValue ) >= tolerance )
    {
    std::cerr << "Error in Set/GetBeta() method" << std::endl;
    return EXIT_FAILURE;
    }


  double  gammaValue = 0.5;
  objectnessFilter->SetGamma( gammaValue );
  if( itk::Math::abs( objectnessFilter->GetGamma() - gammaValue ) >= tolerance )
    {
    std::cerr << "Error in Set/GetGamma() method" << std::endl;
    return EXIT_FAILURE;
    }


  //Verify exceptions will be thrown if the object dimension is larger than
  //the image dimension
  objectnessFilter->SetObjectDimension( 3 );
  try
    {
    objectnessFilter->Update();
    std::cerr << "Exceptions should have been thrown ( objectDimension > imageDimension )"
              << std::endl;
    return EXIT_FAILURE;
    }
  catch (itk::ExceptionObject &e)
    {
    std::cerr << e << std::endl;
    }

  if ( argc >= 3 )
    {
    unsigned int objectDimension = atoi(argv[3]);
    objectnessFilter->SetObjectDimension( objectDimension );

    if ( objectnessFilter->GetObjectDimension() != objectDimension )
      {
      std::cerr << "Error in Set/GetObjectDimension() method" << std::endl;
      return EXIT_FAILURE;
      }
    }

  if ( argc >= 4 )
    {
    bool brightObject = atoi( argv[4] );
    objectnessFilter->SetBrightObject( brightObject );
    if ( objectnessFilter->GetBrightObject() != brightObject )
      {
      std::cerr << "Error in Set/GetBrightObject() method" << std::endl;
      return EXIT_FAILURE;
      }
    }

  try
    {
    objectnessFilter->Update();
    }
  catch (itk::ExceptionObject &e)
    {
    std::cerr << e << std::endl;
    }

  FileWriterType::Pointer writer = FileWriterType::New();
  writer->SetFileName(argv[2]);
  writer->UseCompressionOn();
  writer->SetInput(objectnessFilter->GetOutput());

  try
    {
    writer->Update();
    }
  catch (itk::ExceptionObject &e)
    {
    std::cerr << e << std::endl;
    }

  //Print out
  objectnessFilter->Print( std::cout );

  return EXIT_SUCCESS;
}
