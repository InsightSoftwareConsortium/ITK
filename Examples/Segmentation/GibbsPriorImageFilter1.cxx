/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    GibbsPriorImageFilter1.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

// Software Guide : BeginLatex
//
// This example illustrates the use of the \doxygen{RGBGibbsPriorFilter},
// The filter outputs a binary segmentation that can be improved by the
// deformable model. It is the first part of our hybrid framework.
//
// First, we include the header file.
//
// Software Guide : EndLatex 

#include <iostream>
#include <string>
#include <math.h>

#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif
// Software Guide : BeginCodeSnippet
#include "itkRGBGibbsPriorFilter.h"
// Software Guide : EndCodeSnippet

// classes help the Gibbs filter to segment the image
#include "itkImageClassifierBase.h"
#include "itkImageGaussianModelEstimator.h"
#include "itkMahalanobisDistanceMembershipFunction.h"
#include "itkMinimumDecisionRule.h"

// image storage and I/O classes
#include "itkSize.h"
#include "itkImage.h"
#include "itkVector.h"
#include "itkImageRegionIterator.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#define   NUM_CLASSES         3
#define   MAX_NUM_ITER        1

int main( int argc, char *argv[] )
{
  if( argc != 4 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " inputImage trainimage outputImage" << std::endl;
    return 1;
    }

  std::cout<< "Gibbs Prior Test Begins: " << std::endl;

  //  Software Guide : BeginLatex
  //
  //  The input is a single channel 2D image, the channel number is  
  //  NUMBANDS = 1, NDIMENSION is set to 3.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  const unsigned short NUMBANDS = 1;
  const unsigned short NDIMENSION = 3;

  typedef itk::Image<itk::Vector<unsigned short,NUMBANDS>,NDIMENSION> VecImageType; 
  // Software Guide : EndCodeSnippet  

  //  Software Guide : BeginLatex
  //  
  //  The Gibbs Prior segmentation is performed first to generate a
  //  rough segmentation that yields a sample of tissue from a region 
  //  to be segmented, which will be combined to form the input for the
  //  marching cube method. We define the pixel type of the output of 
  //  Gibbs Prior filter to be unsigned short
  //
  //  Software Guide : EndLatex 
  
  // Software Guide : BeginCodeSnippet
  typedef itk::Image<unsigned short, NDIMENSION > ClassImageType;
  // Software Guide : EndCodeSnippet

  //
  // We instantiate reader and writer types
  //
  typedef  itk::ImageFileReader< ClassImageType > ReaderType;
  typedef  itk::ImageFileWriter<  ClassImageType  > WriterType;

  ReaderType::Pointer inputimagereader = ReaderType::New();
  ReaderType::Pointer trainingimagereader = ReaderType::New();
  WriterType::Pointer writer = WriterType::New();

  inputimagereader->SetFileName( argv[1] );
  trainingimagereader->SetFileName( argv[2] );
  writer->SetFileName( argv[3] );

  //
  // We convert the input into vector images
  //  
  VecImageType::Pointer vecImage = VecImageType::New();

  typedef VecImageType::PixelType VecImagePixelType;

  VecImageType::SizeType vecImgSize = { {181 , 217, 1} };

  VecImageType::IndexType index;
  index.Fill(0);

  VecImageType::RegionType region;

  region.SetSize( vecImgSize );
  region.SetIndex( index );

  vecImage->SetLargestPossibleRegion( region );
  vecImage->SetBufferedRegion( region );
  vecImage->Allocate();

  // setup the iterators
  typedef VecImageType::PixelType::VectorType VecPixelType;

  enum { VecImageDimension = VecImageType::ImageDimension };
  typedef itk::ImageRegionIterator< VecImageType > VecIterator;

  VecIterator vecIt( vecImage, vecImage->GetBufferedRegion() );
  vecIt.GoToBegin();

  inputimagereader->Update();
  trainingimagereader->Update();

  typedef itk::ImageRegionIterator< ClassImageType > ClassIterator;

  ClassIterator inputIt( inputimagereader->GetOutput(), inputimagereader->GetOutput()->GetBufferedRegion() );
  inputIt.GoToBegin();

  //Set up the vector to store the image  data
  typedef VecImageType::PixelType     DataVector;
  DataVector   dblVec; 

  while ( !vecIt.IsAtEnd() ) { 
    dblVec[0] = inputIt.Get();
    vecIt.Set(dblVec); 
    ++vecIt;
    ++inputIt;
  }

  //----------------------------------------------------------------------
  //Set membership function (Using the statistics objects)
  //----------------------------------------------------------------------

  namespace stat = itk::Statistics;

  typedef VecImageType::PixelType VecImagePixelType;
  typedef stat::MahalanobisDistanceMembershipFunction< VecImagePixelType > 
    MembershipFunctionType ;
  typedef MembershipFunctionType::Pointer MembershipFunctionPointer ;

  typedef std::vector< MembershipFunctionPointer > 
    MembershipFunctionPointerVector;  

  //----------------------------------------------------------------------
  // Set the image model estimator (train the class models)
  //----------------------------------------------------------------------

  typedef itk::ImageGaussianModelEstimator<VecImageType,
    MembershipFunctionType, ClassImageType> 
    ImageGaussianModelEstimatorType;
  
  ImageGaussianModelEstimatorType::Pointer 
    applyEstimateModel = ImageGaussianModelEstimatorType::New();  

  applyEstimateModel->SetNumberOfModels(NUM_CLASSES);
  applyEstimateModel->SetInputImage(vecImage);
  applyEstimateModel->SetTrainingImage(trainingimagereader->GetOutput());  



  //Run the gaussian classifier algorithm
  applyEstimateModel->Update();

  std::cout << " site 1 " << std::endl;
  
  applyEstimateModel->Print(std::cout); 

  MembershipFunctionPointerVector membershipFunctions = 
    applyEstimateModel->GetMembershipFunctions();

  std::cout << " site 2 " << std::endl;

  //----------------------------------------------------------------------
  //Set the decision rule 
  //----------------------------------------------------------------------  
  typedef itk::DecisionRuleBase::Pointer DecisionRuleBasePointer;

  typedef itk::MinimumDecisionRule DecisionRuleType;
  DecisionRuleType::Pointer  
    myDecisionRule = DecisionRuleType::New();




  std::cout << " site 3 " << std::endl;

  //----------------------------------------------------------------------
  // Set the classifier to be used and assigne the parameters for the 
  // supervised classifier algorithm except the input image which is 
  // grabbed from the Gibbs application pipeline.
  //----------------------------------------------------------------------
  //---------------------------------------------------------------------
  typedef VecImagePixelType MeasurementVectorType;

  //  Software Guide : BeginLatex
  //  
  //  Then we define the classifier that is needed
  //  for the Gibbs Prior model to make correct segmenting decision
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet 
  typedef itk::ImageClassifierBase< VecImageType,
    ClassImageType > ClassifierType;

  typedef itk::ClassifierBase<VecImageType>::Pointer 
    ClassifierBasePointer;

  typedef ClassifierType::Pointer ClassifierPointer;
  ClassifierPointer myClassifier = ClassifierType::New();
  // Software Guide : EndCodeSnippet 

  // Set the Classifier parameters
  myClassifier->SetNumberOfClasses(NUM_CLASSES);

  // Set the decison rule 
  myClassifier->
    SetDecisionRule((DecisionRuleBasePointer) myDecisionRule );

  //Add the membership functions
  for( unsigned int i=0; i<NUM_CLASSES; i++ )
    {
    myClassifier->AddMembershipFunction( membershipFunctions[i] );
    }

  //Set the Gibbs Prior labeller
  //  Software Guide : BeginLatex
  //  
  //  After that we can define the multi-channel Gibbs Prior 
  //  model. 
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet 
  typedef itk::RGBGibbsPriorFilter<VecImageType,ClassImageType> GibbsPriorFilterType;
  GibbsPriorFilterType::Pointer applyGibbsImageFilter = GibbsPriorFilterType::New();
  // Software Guide : EndCodeSnippet 

  // Set the MRF labeller parameters
  //  Software Guide : BeginLatex
  //  
  //  The parameters for Gibbs Prior filter are defined below. The NumberOfClass
  //  indicates how many different objects are in the image; the maximum number of 
  //  iteration is the number of minimization steps; the cluster size set the 
  //  lower limitation of the object's size; the boundary gradient is the estimation
  //  of the variance between objects and background at the boundary region.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  applyGibbsImageFilter->SetNumberOfClasses(NUM_CLASSES);
  applyGibbsImageFilter->SetMaximumNumberOfIterations(MAX_NUM_ITER);
  applyGibbsImageFilter->SetClusterSize(10);
  applyGibbsImageFilter->SetBoundaryGradient(6);
  applyGibbsImageFilter->SetObjectLabel(1);
  // Software Guide : EndCodeSnippet 
  
  //  Software Guide : BeginLatex
  //  
  //  Set the input, classifier for the Gibbs Prior filter
  //  and the input for the classifier. The classifer will calculate the
  //  mean and variance of the object using the classimage, the results 
  //  will be used as parameters for the Gibbs Prior model
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  applyGibbsImageFilter->SetInput(vecImage);
  applyGibbsImageFilter->SetClassifier( myClassifier ); 

  applyGibbsImageFilter->SetTrainingImage(trainingimagereader->GetOutput());  
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //  
  //  Execution of the Gibbs Prior filter using the Update() method.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  applyGibbsImageFilter->Update();
  // Software Guide : EndCodeSnippet

  std::cout << "applyGibbsImageFilter: " << applyGibbsImageFilter;

  writer->SetInput( applyGibbsImageFilter->GetOutput() );
  writer->Update();

  //  Software Guide : BeginLatex
  //
  //  We execute this program on the image \code{brainweb89.png}. The following parameters
  //  are passed to the command line:
  // 
  //  \begin{verbatim}
  //  GibbsGuide.exe brainweb89.png brainweb89_train.png brainweb_gp.png
  //  \end{verbatim}
  //
  //  brainweb89train is a training image that helps to estimate the object statistics.
  //
  //  Note that in order to segment successfully other images, one has to create suitable
  //  training image.
  // 
  //  We can segment color (RGB) and other multi-channel images. Examples illustrating
  //  it will be posted soon on the ITK site.
  //
  //  Software Guide : EndLatex 

  return 0;
}
