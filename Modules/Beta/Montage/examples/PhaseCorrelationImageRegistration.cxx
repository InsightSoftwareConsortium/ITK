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

//    INPUTS: {BrainProtonDensitySliceBorder20.png}
//    INPUTS: {BrainProtonDensitySliceShifted13x17y.png}


//include headers neccessary for phase correlation image registration
#include "itkPhaseCorrelationImageRegistrationMethod.h"
#include "itkPhaseCorrelationOperator.h"
#include "itkMaxPhaseCorrelationOptimizer.h"

//include headers for image, IO, casting and resampling
#include "itkImageFileReader.h"
#include "itkResampleImageFilter.h"
#include "itkCastImageFilter.h"
#include "itkTransformFileWriter.h"


int main( int argc, char *argv[] )
{

  //inputs and output are passed as command line arguments
  if( argc < 4 )
    {
    std::cout << "Usage: " << std::endl;
    std::cout << argv[0] << "  FixedImageFile  MovingImageFile"
                         << "  OutputTransformFile" << std::endl;
    return EXIT_FAILURE;
    }
  const char * fixedImageFile = argv[1];
  const char * movingImageFile = argv[2];
  const char * transformFile = argv[3];


  //
  // read the images
  //
  const unsigned int                          Dimension = 2;
  typedef float                               PixelType;
  typedef itk::Image< PixelType, Dimension >  ImageType;

  typedef itk::ImageFileReader<ImageType>  ReaderType;

  ReaderType::Pointer fixedReader  = ReaderType::New();
  ReaderType::Pointer movingReader = ReaderType::New();
  fixedReader->SetFileName( fixedImageFile );
  movingReader->SetFileName( movingImageFile );

  try
    {
    fixedReader->Update();
    movingReader->Update();
    }
  catch (itk::ExceptionObject & e)
    {
    std::cerr << "Unable to read input images!" << std::endl;
    std::cerr << e << std::endl;

    return EXIT_FAILURE;
    }


  //
  // init the registration method
  //

  typedef itk::PhaseCorrelationImageRegistrationMethod< ImageType, ImageType > RegistrationType;
  RegistrationType::Pointer pcmRegistration = RegistrationType::New();
  pcmRegistration->SetFixedImage( fixedReader->GetOutput() );
  pcmRegistration->SetMovingImage( movingReader->GetOutput() );

  typedef itk::PhaseCorrelationOperator< itk::NumericTraits< PixelType >::RealType, Dimension >
    OperatorType;
  OperatorType::Pointer pcmOperator = OperatorType::New();
  pcmRegistration->SetOperator( pcmOperator );

  typedef itk::MaxPhaseCorrelationOptimizer<RegistrationType> OptimizerType;
  OptimizerType::Pointer pcmOptimizer = OptimizerType::New();
  pcmRegistration->SetOptimizer( pcmOptimizer );


  //
  // execute the registration
  //
  try
    {
    pcmRegistration->Update();
    }
  catch ( itk::ExceptionObject & e )
    {
    std::cout << "Some error during registration:" << std::endl;
    std::cout << e << std::endl;

    return EXIT_FAILURE;
    }

  //
  // get the results
  //
  RegistrationType::ParametersType parameters
      = pcmRegistration->GetTransformParameters();
  RegistrationType::TransformType::ConstPointer transform
      = pcmRegistration->GetOutput()->Get();

  std::cout << "Translation found: " << parameters << std::endl;


  //
  // write output transform to file
  //
  typedef itk::TransformFileWriterTemplate< double > WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( pcmRegistration->GetOutput()->Get() );
  writer->SetFileName( transformFile );

  try
    {
    writer->Update();
    }
  catch(itk::ExceptionObject & e)
    {
    std::cerr << "Unable to generate or write output image!" << std::endl;
    std::cerr << e << std::endl;

    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
