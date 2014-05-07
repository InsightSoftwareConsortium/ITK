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

#include "itkImageFileReader.h"
#include "itkBayesianClassifierImageFilter.h"
#include "itkBayesianClassifierInitializationImageFilter.h"
#include "itkImageFileWriter.h"
#include "itkGradientAnisotropicDiffusionImageFilter.h"
#include "itkRescaleIntensityImageFilter.h"

#include "itkPipelineMonitorImageFilter.h"

int itkBayesianClassifierImageFilterTest(int argc, char* argv[] )
{

  if( argc < 5 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << " inputImageFile outputImageFile numberOfClasses smoothingIterations" << std::endl;
    return EXIT_FAILURE;
    }


  // setup reader
  const unsigned int Dimension = 2;
  typedef unsigned char                           InputPixelType;
  typedef itk::Image< InputPixelType, Dimension > InputImageType;
  typedef itk::ImageFileReader< InputImageType >  ReaderType;

  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  typedef unsigned char  LabelType;
  typedef float          PriorType;
  typedef float          PosteriorType;

  typedef itk::BayesianClassifierInitializationImageFilter< InputImageType >
                                                BayesianInitializerType;

  BayesianInitializerType::Pointer bayesianInitializer = BayesianInitializerType::New();

  bayesianInitializer->SetInput( reader->GetOutput() );
  bayesianInitializer->SetNumberOfClasses( atoi( argv[3] ) );

  typedef BayesianInitializerType::OutputImageType  InitialLabelImageType;

  typedef itk::BayesianClassifierImageFilter<
    InitialLabelImageType, LabelType, PosteriorType, PriorType >   ClassifierFilterType;

  ClassifierFilterType::Pointer filter = ClassifierFilterType::New();

  filter->SetInput( bayesianInitializer->GetOutput() );

  //
  //  Exercise Set/GetNumberOfSmoothingIterations()
  //
  filter->SetNumberOfSmoothingIterations( 1 );
  if( filter->GetNumberOfSmoothingIterations() != 1 )
    {
    std::cerr << "Error in Set/GetNumberOfSmoothingIterations()" << std::endl;
    return EXIT_FAILURE;
    }

  filter->SetNumberOfSmoothingIterations( 19 );
  if( filter->GetNumberOfSmoothingIterations() != 19 )
    {
    std::cerr << "Error in Set/GetNumberOfSmoothingIterations()" << std::endl;
    return EXIT_FAILURE;
    }

  filter->SetNumberOfSmoothingIterations( 0 );

  filter->SetNumberOfSmoothingIterations( atoi( argv[4] ));
  typedef ClassifierFilterType::ExtractedComponentImageType ExtractedComponentImageType;
  typedef itk::GradientAnisotropicDiffusionImageFilter<
    ExtractedComponentImageType, ExtractedComponentImageType >  SmoothingFilterType;
  SmoothingFilterType::Pointer smoother = SmoothingFilterType::New();
  smoother->SetNumberOfIterations( 1 );
  smoother->SetTimeStep( 0.125 );
  smoother->SetConductanceParameter( 3 );
  filter->SetSmoothingFilter( smoother );

  //
  //  Exercise Set/GetSmoothingFilter()
  //
  if( filter->GetSmoothingFilter().GetPointer() != smoother.GetPointer() )
    {
    std::cerr << "Error in Set/GetSmoothingFilter()" << std::endl;
    return EXIT_FAILURE;
    }

  filter->SetSmoothingFilter( ITK_NULLPTR );
  if( filter->GetSmoothingFilter().GetPointer() != ITK_NULLPTR )
    {
    std::cerr << "Error in Set/GetSmoothingFilter()" << std::endl;
    return EXIT_FAILURE;
    }

  filter->SetSmoothingFilter( smoother );


  typedef itk::PipelineMonitorImageFilter<InputImageType> MonitorFilterType;
  MonitorFilterType::Pointer monitor =  MonitorFilterType::New();
  monitor->SetInput(filter->GetOutput());


  typedef ClassifierFilterType::OutputImageType      ClassifierOutputImageType;
  typedef itk::Image< unsigned char, Dimension >     OutputImageType;
  typedef itk::RescaleIntensityImageFilter<
    ClassifierOutputImageType, OutputImageType >   RescalerType;
  RescalerType::Pointer rescaler = RescalerType::New();
  rescaler->SetInput( monitor->GetOutput() );
  rescaler->SetOutputMinimum( 0 );
  rescaler->SetOutputMaximum( 255 );

  typedef itk::ImageFileWriter< OutputImageType >    WriterType;

  WriterType::Pointer writer = WriterType::New();
  writer->SetFileName( argv[2] );

  writer->SetInput( rescaler->GetOutput() );

  try
    {
    filter->Update();
    writer->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Exception caught: " << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }


  if (!monitor->VerifyAllInputCanNotStream())
    {
    std::cout << "pipeline did not execute as expected!" << std::endl;
    return EXIT_FAILURE;
    }

  filter->Print( std::cout );
  std::cout << "Test passed." << std::endl;

  typedef ClassifierFilterType::PriorsImageType   PriorsImageType;

  const InputImageType * inputImage = reader->GetOutput();

  PriorsImageType::Pointer priorsImage = PriorsImageType::New();
  priorsImage->CopyInformation( inputImage );
  priorsImage->SetRegions( inputImage->GetLargestPossibleRegion() );
  priorsImage->SetNumberOfComponentsPerPixel(5);
  priorsImage->Allocate();

  filter->SetPriors( priorsImage );

///TEST valid image type combinations.  I have a hypothesis that the vector element type for
//TestInitialLabelImageType must be the same as for TestPriorType
    {
    const unsigned int TestDimension = 2;
    typedef unsigned char  TestLabelType;
    typedef float          TestPosteriorType;

    typedef float                                            TestPriorType;
    typedef itk::VectorImage< TestPriorType ,TestDimension > TestInitialLabelImageType;

    typedef itk::BayesianClassifierImageFilter<
      TestInitialLabelImageType, TestLabelType, TestPosteriorType, TestPriorType >   TestClassifierFilterType;
    TestClassifierFilterType::Pointer test=TestClassifierFilterType::New();
    if(test.IsNull())
      {
      return EXIT_FAILURE;
      }
    }

    {
    const unsigned int TestDimension = 2;
    typedef unsigned char  TestLabelType;
    typedef float          TestPosteriorType;

    typedef float          TestPriorType;
    typedef itk::VectorImage< double ,TestDimension > TestInitialLabelImageType; //The element type MUST be the PriorType

    typedef itk::BayesianClassifierImageFilter<
      TestInitialLabelImageType, TestLabelType, TestPosteriorType, TestPriorType >   TestClassifierFilterType;
    TestClassifierFilterType::Pointer test=TestClassifierFilterType::New();
    if(test.IsNull())
      {
      return EXIT_FAILURE;
      }
    }

    {
    const unsigned int TestDimension = 2;
    typedef unsigned char  TestLabelType;
    typedef float          TestPosteriorType;

    typedef double          TestPriorType;
    typedef itk::VectorImage< TestPriorType ,TestDimension > TestInitialLabelImageType; //The element type MUST be the PriorType

    typedef itk::BayesianClassifierImageFilter<
      TestInitialLabelImageType, TestLabelType, TestPosteriorType, TestPriorType >   TestClassifierFilterType;
    TestClassifierFilterType::Pointer test=TestClassifierFilterType::New();
    if(test.IsNull())
      {
      return EXIT_FAILURE;
      }
    }
  return EXIT_SUCCESS;
}
