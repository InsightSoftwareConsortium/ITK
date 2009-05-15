/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkScalarChanAndVeseDenseLevelSetImageFilterTest2.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include "itkScalarChanAndVeseDenseLevelSetImageFilter.h"
#include "itkScalarChanAndVeseLevelSetFunction.h"
#include "itkScalarChanAndVeseLevelSetFunctionData.h"
#include "itkConstrainedRegionBasedLevelSetFunctionSharedData.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkImage.h"
#include "itkTestingMacros.h"
#include "itkAtanRegularizedHeavisideStepFunction.h"

int itkScalarChanAndVeseDenseLevelSetImageFilterTest2( int argc, char * argv [] )
{

  if( argc < 4 )
    {
    std::cerr << "Missing arguments" << std::endl;
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "inputLevelSetImage inputFeatureImage ";
    std::cerr << " outputLevelSetImage CurvatureWeight AreaWeight";
    std::cerr << " LaplacianWeight VolumeWeight Volume" << std::endl;
    return EXIT_FAILURE;
    }

  unsigned int nb_iteration = 50;
  double rms = 0.;
  double epsilon = 1.;
  double curvature_weight = atof( argv[4] );
  double area_weight = atof( argv[5] );
  double laplacian_weight = atof( argv[6] );
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

  typedef itk::ConstrainedRegionBasedLevelSetFunctionSharedData< LevelSetImageType, FeatureImageType, DataHelperType >
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

  levelSetFilter->GetDifferenceFunction(0)->SetDomainFunction( domainFunction );
  levelSetFilter->GetDifferenceFunction(0)->SetCurvatureWeight( curvature_weight );
  levelSetFilter->GetDifferenceFunction(0)->SetAreaWeight( area_weight );
  levelSetFilter->GetDifferenceFunction(0)->SetLaplacianSmoothingWeight( laplacian_weight );
  levelSetFilter->GetDifferenceFunction(0)->SetVolumeMatchingWeight( volume_weight );
  levelSetFilter->GetDifferenceFunction(0)->SetVolume( volume );
  levelSetFilter->GetDifferenceFunction(0)->SetLambda1( l1 );
  levelSetFilter->GetDifferenceFunction(0)->SetLambda2( l2 );


  levelSetFilter->Update();

  WriterType::Pointer writer1 = WriterType::New();

  writer1->SetInput( levelSetFilter->GetOutput() );

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
