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

#include "itkScalarChanAndVeseSparseLevelSetImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkAtanRegularizedHeavisideStepFunction.h"

int itkScalarChanAndVeseSparseLevelSetImageFilterTest2( int argc, char * argv [] )
{

  if( argc < 4 )
    {
    std::cerr << "Missing arguments" << std::endl;
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "inputLevelSetImage inputFeatureImage ";
    std::cerr << " outputLevelSetImage" << std::endl;
    return EXIT_FAILURE;
    }

  unsigned int nb_iteration = 50;
  double rms = 0.;
  double epsilon = 1.;
  double mu = 0.;
  double nu = 0.;
  double l1 = 1.;
  double l2 = 3.;

  const unsigned int Dimension = 2;
  typedef float ScalarPixelType;

  typedef itk::Image< ScalarPixelType, Dimension > LevelSetImageType;
  typedef itk::Image< ScalarPixelType, Dimension > FeatureImageType;

  typedef itk::ScalarChanAndVeseLevelSetFunctionData< LevelSetImageType, FeatureImageType >
    DataHelperType;

  typedef itk::ConstrainedRegionBasedLevelSetFunctionSharedData< LevelSetImageType, FeatureImageType, DataHelperType >
    SharedDataHelperType;

  typedef itk::ScalarChanAndVeseLevelSetFunction< LevelSetImageType,
    FeatureImageType, SharedDataHelperType > LevelSetFunctionType;

  typedef itk::ScalarChanAndVeseSparseLevelSetImageFilter< LevelSetImageType,
    FeatureImageType, LevelSetImageType, LevelSetFunctionType, SharedDataHelperType > MultiLevelSetType;

  typedef itk::ImageFileReader< LevelSetImageType >     LevelSetReaderType;
  typedef itk::ImageFileReader< FeatureImageType >      FeatureReaderType;
  typedef itk::ImageFileWriter< LevelSetImageType >     WriterType;

  typedef itk::AtanRegularizedHeavisideStepFunction< ScalarPixelType, ScalarPixelType >  DomainFunctionType;

  DomainFunctionType::Pointer domainFunction = DomainFunctionType::New();

  domainFunction->SetEpsilon( epsilon );

  LevelSetReaderType::Pointer levelSetReader1 = LevelSetReaderType::New();
  levelSetReader1->SetFileName( argv[1] );
  levelSetReader1->Update();

  FeatureReaderType::Pointer featureReader = FeatureReaderType::New();
  featureReader->SetFileName( argv[2] );
  featureReader->Update();

  MultiLevelSetType::Pointer levelSetFilter = MultiLevelSetType::New();

  levelSetFilter->SetFunctionCount( 1 );   // Protected ?
  levelSetFilter->SetFeatureImage( featureReader->GetOutput() );
  levelSetFilter->SetLevelSet( 0, levelSetReader1->GetOutput() );
  levelSetFilter->SetNumberOfIterations( nb_iteration );
  levelSetFilter->SetMaximumRMSError( rms );
  levelSetFilter->SetUseImageSpacing( 0 );
  levelSetFilter->SetInPlace( false );

  levelSetFilter->GetDifferenceFunction(0)->SetDomainFunction( domainFunction );
  levelSetFilter->GetDifferenceFunction(0)->SetCurvatureWeight( mu );
  levelSetFilter->GetDifferenceFunction(0)->SetAreaWeight( nu );
  levelSetFilter->GetDifferenceFunction(0)->SetLambda1( l1 );
  levelSetFilter->GetDifferenceFunction(0)->SetLambda2( l2 );

  levelSetFilter->Update();

  WriterType::Pointer writer1 = WriterType::New();

  writer1->SetInput( levelSetFilter->GetOutput() );
  writer1->UseCompressionOn();

  writer1->SetFileName( argv[3] );

  try
    {
    writer1->Update();
    }
  catch( itk::ExceptionObject & excep )
    {
    std::cerr << "Exception caught !" << std::endl;
    std::cerr << excep << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
