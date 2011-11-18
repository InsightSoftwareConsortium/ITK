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

#include "itkScalarChanAndVeseDenseLevelSetImageFilter.h"
#include "itkUnconstrainedRegionBasedLevelSetFunctionSharedData.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkAtanRegularizedHeavisideStepFunction.h"

int itkScalarChanAndVeseDenseLevelSetImageFilterTest4( int argc, char * argv [] )
{

  if( argc < 4 )
    {
    std::cerr << "Missing arguments" << std::endl;
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "inputLevelSetImage inputFeatureImage ";
    std::cerr << " outputLevelSetImage CurvatureWeight AreaWeight";
    std::cerr << " ReinitializationWeight VolumeWeight Volume" << std::endl;
    return EXIT_FAILURE;
    }

  unsigned int nb_iteration = 500;
  double rms = 0.;
  double epsilon = 1.;
  double curvature_weight = atof( argv[4] );
  double area_weight = atof( argv[5] );
  double reinitialization_weight = atof( argv[6] );
  double volume_weight = atof( argv[7] );
  double volume = atof( argv[8] );
  double l1 = 1.;
  double l2 = 1.;

  const unsigned int Dimension = 2;
  typedef float ScalarPixelType;

  typedef itk::Image< ScalarPixelType, Dimension > LevelSetImageType;
  typedef itk::Image< ScalarPixelType, Dimension > FeatureImageType;

  typedef itk::ScalarChanAndVeseLevelSetFunctionData< LevelSetImageType, FeatureImageType >
    DataHelperType;

  typedef itk::UnconstrainedRegionBasedLevelSetFunctionSharedData< LevelSetImageType, FeatureImageType, DataHelperType >
    SharedDataHelperType;

  typedef itk::ScalarChanAndVeseLevelSetFunction< LevelSetImageType,
    FeatureImageType, SharedDataHelperType > LevelSetFunctionType;

  typedef itk::ScalarChanAndVeseDenseLevelSetImageFilter< LevelSetImageType,
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

  levelSetReader1->Update();
  levelSetFilter->SetFunctionCount( 1 );   // Protected ?
  levelSetFilter->SetFeatureImage( featureReader->GetOutput() );
  levelSetFilter->SetLevelSet( 0, levelSetReader1->GetOutput() );
  levelSetFilter->SetNumberOfIterations( nb_iteration );
  levelSetFilter->SetMaximumRMSError( rms );
  levelSetFilter->SetUseImageSpacing( 0 );
  levelSetFilter->SetInPlace( false );

  levelSetFilter->GetDifferenceFunction(0)->SetDomainFunction( domainFunction );
  levelSetFilter->GetDifferenceFunction(0)->SetCurvatureWeight( curvature_weight );
  levelSetFilter->GetDifferenceFunction(0)->SetAreaWeight( area_weight );
  levelSetFilter->GetDifferenceFunction(0)->SetReinitializationSmoothingWeight( reinitialization_weight );
  levelSetFilter->GetDifferenceFunction(0)->SetVolumeMatchingWeight( volume_weight );
  levelSetFilter->GetDifferenceFunction(0)->SetVolume( volume );
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
