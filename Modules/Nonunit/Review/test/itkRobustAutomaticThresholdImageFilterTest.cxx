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
#include "itkImageFileWriter.h"

#include "itkSimpleFilterWatcher.h"
#include "itkRobustAutomaticThresholdImageFilter.h"
#include "itkGradientMagnitudeRecursiveGaussianImageFilter.h"

int itkRobustAutomaticThresholdImageFilterTest(int argc, char * argv[])
{

  if( argc != 4 )
    {
    std::cerr << "usage: " << argv[0] << " inputImage outputImage pow" << std::endl;
    return EXIT_FAILURE;
    }

  const int dim = 2;

  typedef unsigned short           PType;
  typedef itk::Image< PType, dim > IType;

  typedef float                     RPType;
  typedef itk::Image< RPType, dim > RIType;

  typedef itk::ImageFileReader< IType > ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  typedef itk::GradientMagnitudeRecursiveGaussianImageFilter< IType, RIType > GradientType;
  GradientType::Pointer gradient = GradientType::New();
  gradient->SetInput( reader->GetOutput() );
  gradient->SetSigma( 10 );
  gradient->Update();

  typedef itk::RobustAutomaticThresholdImageFilter< IType, RIType > FilterType;
  FilterType::Pointer filter = FilterType::New();
  filter->SetInput( reader->GetOutput() );
  filter->SetGradientImage( gradient->GetOutput() );
  filter->SetPow( atof(argv[3]) );

  itk::SimpleFilterWatcher watcher(filter, "filter");

  typedef itk::ImageFileWriter< IType > WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( filter->GetOutput() );
  writer->SetFileName( argv[2] );

  try
    {
    writer->Update();
    }
  catch ( itk::ExceptionObject & excp )
    {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;

}
