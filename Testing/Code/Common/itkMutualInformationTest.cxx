/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkMutualInformationTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


Copyright (c) 2000 National Library of Medicine
All rights reserved.

See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "itkImage.h"
#include "itkScalar.h"
#include "itkImageRegionIterator.h"
#include "itkMutualInformationRigidRegistrationVW.h"
#include "itkKernelFunction.h"

#include "vnl/vnl_sample.h"

int main()
{

  /* ---------------------------------------------------
   * create a simple image with simple derivatives
   */
  typedef itk::Image<float,2> ImageType;

  ImageType::SizeType size = {{64,64}};
  ImageType::IndexType index = {{0,0}};
  ImageType::RegionType region;
  region.SetSize( size );
  region.SetIndex( index );

  ImageType::Pointer imgReference = ImageType::New();
  imgReference->SetLargestPossibleRegion( region );
  imgReference->SetBufferedRegion( region );
  imgReference->Allocate();

  ImageType::Pointer imgTest = ImageType::New();
  imgTest->SetLargestPossibleRegion( region );
  imgTest->SetBufferedRegion( region );
  imgTest->Allocate();

  ImageType::Pointer imgDeriv[ImageType::ImageDimension];
  for( int j = 0; j < ImageType::ImageDimension; j++ )
    {
    imgDeriv[j] = ImageType::New();
    imgDeriv[j]->SetLargestPossibleRegion( region );
    imgDeriv[j]->SetBufferedRegion( region );
    imgDeriv[j]->Allocate();
    }

  typedef
    itk::ImageRegionIterator<ImageType::PixelType,ImageType::ImageDimension>
      Iterator;
  Iterator testIter( imgTest, region );
  Iterator refIter( imgReference, region );
  Iterator colIter( imgDeriv[0], region );
  Iterator rowIter( imgDeriv[1], region );

  while ( !testIter.IsAtEnd() )
    {

    index = testIter.GetIndex();

    double factor = 0.5;
    *testIter = vnl_math_sqrt( vnl_math_sqr( float(index[0]) - 32.5 ) +
      factor * vnl_math_sqr( float(index[1]) - 32.5 ) );
    *colIter = ( float(index[0]) - 32.5 ) / *testIter;
    *rowIter = factor * ( float(index[1]) - 32.5 ) / *testIter;

    double value = 33.0;
    *testIter = *testIter / value;
    *refIter = *testIter + vnl_sample_uniform( -0.005, 0.005);
    *colIter = *colIter / value;
    *rowIter = *rowIter / value;

    ++testIter;
    ++refIter;
    ++colIter;
    ++rowIter;
    }

  /*------------------------------------------------------------
   * Create mutual information registration object
   */
  typedef
    itk::MutualInformationRigidRegistrationVW<ImageType,ImageType,ImageType>
      RegistratorType;

  RegistratorType::Pointer registrator = RegistratorType::New();

  registrator->SetReferenceImage( imgReference );
  registrator->SetTestImage( imgTest );
  registrator->SetTestImageDerivative( imgDeriv[0], 0 );
  registrator->SetTestImageDerivative( imgDeriv[1], 1 );

  RegistratorType::MatrixType initMatrix;
  RegistratorType::VectorType initVector;

  initMatrix.set_identity();
  initMatrix.put( 0, 1, -0.16 );
  initMatrix.put( 1, 0, 0.16 );
  initVector.fill( 32.5 );
  initVector -= initMatrix * initVector;

  registrator->SetInitialAffineMatrix( initMatrix );
  registrator->SetInitialAffineVector( initVector );

  // setup the internal calculator
  typedef RegistratorType::DefaultCalculatorType CalculatorType;
  CalculatorType::Pointer calculator =
    dynamic_cast<CalculatorType*> (
      registrator->GetMutualInformationCalculator().GetPointer() );

  calculator->SetReferenceStdDev( 0.1 );
  calculator->SetTestStdDev( 0.1 );
  calculator->SetNumberOfSamples( 100 );

  typedef itk::KernelFunction KernelFunction;
  itk::KernelFunction::Pointer kernel = 
    dynamic_cast<KernelFunction*>(
      itk::GaussianKernelFunction::New().GetPointer() );

  calculator->SetKernelFunction( kernel );

  // printout the internal calculator parameters
  std::cout << "Calculator parameters" << std::endl;
  std::cout << "No.of Samples: " << calculator->GetNumberOfSamples();
  std::cout << std::endl;
  std::cout << "Reference Std. Dev: " << calculator->GetReferenceStdDev();
  std::cout << std::endl;
  std::cout << "Test Std. Dev: " << calculator->GetTestStdDev();
  std::cout << std::endl;

  registrator->SetLearningRate( 5.0 );
  registrator->SetNumberOfIterations( 200 );

  registrator->SetDebugOn( false );

  registrator->Maximize();

  std::cout << "last parameters" << std::endl;
  std::cout << registrator->GetLastAffineMatrix() << std::endl;
  std::cout << registrator->GetLastAffineVector() << std::endl;

  return EXIT_SUCCESS;

}
