/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkActiveShapeModelCalculatorTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
//#if defined(_MSC_VER)
//#pragma warning ( disable : 4786 )
//#endif

#include "itkImageFileReader.h"
#include "itkActiveShapeModelCalculator.h"
#include "itkActiveShapeModelGradientSearchMethod.h"


int itkActiveShapeModelCalculatorTest( int argc, char * argv[] )
{
    if( argc < 3 )
    {
        std::cerr << "Usage: " << std::endl;
        std::cerr << argv[0] << " Input3DImageBinaryFile Input2DImageFile" << std::endl;
        return -1;
    }

  const unsigned int Dimension             = 3;
  const float m_Tolerance                  = 2.0;


  typedef float      PixelType;
  typedef vnl_vector<double>                                              VectorOfDoubleType;
  typedef vnl_matrix<double>                                              MatrixOfDoubleType; 
  typedef itk::Image< PixelType, Dimension >    Image3DType;
  typedef itk::Image< unsigned char, 2 >    Image2DType;

  typedef itk::ImageFileReader< Image3DType  >  ReaderType;
  typedef itk::ActiveShapeModelCalculator< Image3DType > ActiveShapeModelCalculatorType;
  typedef itk::ActiveShapeModelGradientSearchMethod< Image2DType> ImageSearchType;
  ActiveShapeModelCalculatorType::Pointer 
    applyActiveShapeModelCalculator = ActiveShapeModelCalculatorType::New();
  ImageSearchType::Pointer 
    ImageSearch = ImageSearchType::New();
  ReaderType::Pointer reader = ReaderType::New();
  Image3DType::ConstPointer inputImage;

  const char * inputFilename  = argv[1];
  reader->SetFileName( inputFilename  );
  inputImage = reader->GetOutput();
  reader->Update( );
  applyActiveShapeModelCalculator->SetImage( inputImage );  
  //----------------------------------------------------------------------
  //Set the parameters for ActiveShapeModelCalculator
  //----------------------------------------------------------------------
  applyActiveShapeModelCalculator->SetTolerance( m_Tolerance );

  applyActiveShapeModelCalculator->GenerateData();


  //Test the printself function to increase coverage
  applyActiveShapeModelCalculator->Print(std::cout);

  //Exercise TypeMacro in superclass
  typedef ActiveShapeModelCalculatorType::Superclass GenericEstimatorType;
  std::cout << applyActiveShapeModelCalculator->GenericEstimatorType::GetNameOfClass() << std::endl;

  //Print out the number of training images and the number of principal 
  //components
  std::cout << "The number of training images are: " <<
    applyActiveShapeModelCalculator->GetNumberOfTrainingImages() << std::endl;

  std::cout << "The mean shape: " <<
    applyActiveShapeModelCalculator->GetMeanShape() << std::endl;

  vnl_vector<double> eigenValues  =  applyActiveShapeModelCalculator->GetEigenvalues();
  vnl_matrix<double> eigenVectors =  applyActiveShapeModelCalculator->GetEigenvector();


  //Print the eigen values and eigen vectors
  std::cout << "The " << eigenValues.size() << " first values of the eigen vectors are:" << std::endl;
   for(unsigned int i = 0; i < eigenVectors.rows(); i++) 
    {
    std::cout<< eigenVectors.get_row(i)<<" ";
    }  
  std::cout <<""<<std::endl;
  std::cout << "The " << eigenValues.size() << " largest eigen values are:" << std::endl;
  std::cout << eigenValues << std::endl;

  const unsigned int m_LenghtOfProfile            = 3;
  const unsigned int m_NumberOfIteration          = 2;

  typedef itk::ImageFileReader< Image2DType  >  Reader2DType;
  Reader2DType::Pointer reader1 = Reader2DType::New();
  const char * input2DFilename  = argv[2];
  reader1->SetFileName( input2DFilename  );
  reader1->Update( );

  ImageSearch->SetImage( reader1->GetOutput() );

 //----------------------------------------------------------------------
  //Set the parameters ActiveShapeModelSearchingImageFilter
  //----------------------------------------------------------------------
  ImageSearch->SetLenghtOfProfile( m_LenghtOfProfile );
  ImageSearch->SetNumberOfIteration( m_NumberOfIteration );
  ImageSearch->SetMeanShape( applyActiveShapeModelCalculator->GetMeanShape() + 0.5);
  ImageSearch->SetEigenValues( applyActiveShapeModelCalculator->GetEigenvalues() );
  ImageSearch->SetEigenVectors( applyActiveShapeModelCalculator->GetEigenvector() );

  ImageSearch->GenerateData();


  //Test the printself function to increase coverage
  ImageSearch->Print(std::cout);

  //Exercise TypeMacro in superclass
  typedef ImageSearchType::Superclass GenericEstimator2Type;
  std::cout << ImageSearch->GenericEstimator2Type::GetNameOfClass() << std::endl;

  std::cout << "The new shape: " <<
  ImageSearch->GetNewShape() << std::endl;


  // Software Guide : BeginCodeSnippet
  // Software Guide : EndCodeSnippet

  return 0;

}
