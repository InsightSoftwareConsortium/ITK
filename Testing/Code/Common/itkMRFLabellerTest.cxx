/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMRFLabellerTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/

// Insight classes
#include "itkImage.h"
#include "itkScalar.h"
#include "itkVector.h"
#include "vnl/vnl_matrix_fixed.h"
#include "itkImageRegionIterator.h"
#include "itkGaussianSupervisedClassifier.h"
#include "itkMRFLabeller.h"

//Data definitons 
#define   IMGWIDTH            2
#define   IMGHEIGHT           2
#define   NFRAMES             4
#define   NUMBANDS            2  
#define   NDIMENSION          3
#define   NUM_CLASSES         3
#define   MAX_NUM_ITER       50


int main()
{

  //------------------------------------------------------
  //Create a simple test image with width, height, and 
  //depth 4 vectors each with each vector having data for 
  //2 bands.
  //------------------------------------------------------
  typedef itk::Image<itk::Vector<double,NUMBANDS>,NDIMENSION> VecImageType; 

  VecImageType::Pointer vecImage = VecImageType::New();

  VecImageType::SizeType vecImgSize = { IMGWIDTH , IMGHEIGHT, NFRAMES };

  VecImageType::IndexType index = VecImageType::IndexType::ZeroIndex;
  VecImageType::RegionType region;

  region.SetSize( vecImgSize );
  region.SetIndex( index );

  vecImage->SetLargestPossibleRegion( region );
  vecImage->SetBufferedRegion( region );
  vecImage->Allocate();

  // setup the iterators
  typedef VecImageType::PixelType::VectorType VecPixelType;

  enum { VecImageDimension = VecImageType::ImageDimension };
  typedef
    itk::ImageRegionIterator< VecPixelType, VecImageDimension> VecIterator;

  VecIterator outIt( vecImage, vecImage->GetBufferedRegion() );
  outIt = outIt.Begin();

  //--------------------------------------------------------------------------
  //Manually create and store each vector
  //--------------------------------------------------------------------------
  //Slice 1
  //Vector no. 1
  *outIt = 21,19; ++outIt;
  //Vector no. 2
  *outIt = 20,20; ++outIt;
  //Vector no. 3
  *outIt = 8,11; ++outIt;
  //Vector no. 4
  *outIt = 10,12; ++outIt;

  //Slice 2
  //Vector no. 1
  *outIt = 22,21; ++outIt;
  //Vector no. 2
  *outIt = 21,22; ++outIt;
  //Vector no. 3
  *outIt = 11,12; ++outIt;
  //Vector no. 4
  *outIt = 9,10; ++outIt;

  //Slice 3
  //Vector no. 1 
  *outIt = 19,20; ++outIt;
  //Vector no. 2
  *outIt = 19,21; ++outIt;
  //Vector no. 3
  *outIt = 11,11; ++outIt;
  //Vector no. 4
  *outIt = 11,10; ++outIt;

  //Slice 4
  //Vector no. 1
  *outIt = 18,18; ++outIt;
  //Vector no. 2
  *outIt = 18,20; ++outIt;
  //Vector no. 3
  *outIt = 12,10; ++outIt;
  //Vector no. 4
  *outIt = 14,13; ++outIt;

  //---------------------------------------------------------------
  //Generate the training data
  //---------------------------------------------------------------
  typedef itk::Image<unsigned short,NDIMENSION> ClassImageType; 
  ClassImageType::Pointer classImage  = ClassImageType::New();

  ClassImageType::SizeType classImgSize = { IMGWIDTH , IMGHEIGHT, NFRAMES };

  ClassImageType::IndexType classindex = ClassImageType::IndexType::ZeroIndex;
  ClassImageType::RegionType classregion;

  classregion.SetSize( classImgSize );
  classregion.SetIndex( classindex );

  classImage->SetLargestPossibleRegion( classregion );
  classImage->SetBufferedRegion( classregion );
  classImage->Allocate();

  // setup the iterators
  typedef ClassImageType::PixelType ClassImagePixelType;

  unsigned int ClassImageDimension = NDIMENSION;

  typedef
    itk::ImageRegionIterator<ClassImagePixelType, NDIMENSION>  
      ClassImageIterator;

  ClassImageIterator 
	  classoutIt( classImage, classImage->GetBufferedRegion() );
  classoutIt = classoutIt.Begin();



  ClassImagePixelType outputPixel;
  //--------------------------------------------------------------------------
  //Manually create and store each vector
  //--------------------------------------------------------------------------
  //Slice 1
  //Pixel no. 1
  itk::ScalarTraits<ClassImagePixelType>::SetScalar(outputPixel, 2 );
  *classoutIt = outputPixel;
  ++classoutIt;

  //Pixel no. 2 
  itk::ScalarTraits<ClassImagePixelType>::SetScalar(outputPixel, 2 );
  *classoutIt = outputPixel;
  ++classoutIt;

  //Pixel no. 3
  itk::ScalarTraits<ClassImagePixelType>::SetScalar(outputPixel, 1 );
  *classoutIt = outputPixel;
  ++classoutIt;

  //Pixel no. 4
  itk::ScalarTraits<ClassImagePixelType>::SetScalar(outputPixel, 1 );
  *classoutIt = outputPixel;
  ++classoutIt;

  //Slice 2
  //Pixel no. 1
  itk::ScalarTraits<ClassImagePixelType>::SetScalar(outputPixel, 0 );
  *classoutIt = outputPixel;
  ++classoutIt;
  
  //Pixel no. 2
  itk::ScalarTraits<ClassImagePixelType>::SetScalar(outputPixel, 0 );
  *classoutIt = outputPixel;
  ++classoutIt;

  //Pixel no. 3
  itk::ScalarTraits<ClassImagePixelType>::SetScalar(outputPixel, 0 );
  *classoutIt = outputPixel;
  ++classoutIt;

  //Pixel no. 4
  itk::ScalarTraits<ClassImagePixelType>::SetScalar(outputPixel, 0 );
  *classoutIt = outputPixel;
  ++classoutIt;

  //Slice 3
  //Pixel no. 1 
  itk::ScalarTraits<ClassImagePixelType>::SetScalar(outputPixel, 2 );
  *classoutIt = outputPixel;
  ++classoutIt;

  //Pixel no. 2
  itk::ScalarTraits<ClassImagePixelType>::SetScalar(outputPixel, 2 );
  *classoutIt = outputPixel;
  ++classoutIt;
  
  //Pixel no. 3
  itk::ScalarTraits<ClassImagePixelType>::SetScalar(outputPixel, 1 );
  *classoutIt = outputPixel;
  ++classoutIt;

  //Pixel no. 4
  itk::ScalarTraits<ClassImagePixelType>::SetScalar(outputPixel, 1 );
  *classoutIt = outputPixel;
  ++classoutIt;

  //Slice 4
  //Pixel no. 1
  itk::ScalarTraits<ClassImagePixelType>::SetScalar(outputPixel, 0 );
  *classoutIt = outputPixel;
  ++classoutIt;
  
  //Pixel no. 2
  itk::ScalarTraits<ClassImagePixelType>::SetScalar(outputPixel, 0 );
  *classoutIt = outputPixel;
  ++classoutIt;

  //Pixel no. 3
  itk::ScalarTraits<ClassImagePixelType>::SetScalar(outputPixel, 0 );
  *classoutIt = outputPixel;
  ++classoutIt;

  //Pixel no. 4
  itk::ScalarTraits<ClassImagePixelType>::SetScalar(outputPixel, 0 );
  *classoutIt = outputPixel;
  ++classoutIt;

  //----------------------------------------------------------------------
  // Test code for the supervised classifier algorithm
  //----------------------------------------------------------------------

  //---------------------------------------------------------------------
  // Multiband data is now available in the right format
  //---------------------------------------------------------------------
  typedef 
	itk::Classifier<VecImageType,ClassImageType>::Pointer 
	  ClassifierType;

  //Instantiate the classifier to be used
  typedef itk::GaussianSupervisedClassifier<VecImageType,ClassImageType> 
    GaussianSupervisedClassifierType;

  GaussianSupervisedClassifierType::Pointer 
	  myGaussianClassifier = GaussianSupervisedClassifierType::New();

  //Set the MRF labeller
  typedef itk::MRFLabeller<VecImageType,ClassImageType> MRFLabellerType;
  MRFLabellerType::Pointer applyMRFLabeller = MRFLabellerType::New();

  // Set the MRF labeller parameters
  applyMRFLabeller->SetNumClasses(NUM_CLASSES);
  applyMRFLabeller->SetMaxNumIter(MAX_NUM_ITER);
  applyMRFLabeller->SetErrorTollerance(0.00);
 
  applyMRFLabeller->SetInput(vecImage);
  applyMRFLabeller
    ->SetClassifier((ClassifierType) myGaussianClassifier ); 

  //Since a suvervised classifier is used, it requires a training image
  applyMRFLabeller->SetTrainingImage(classImage);  
  
  //Kick off the MRF labeller function
  applyMRFLabeller->Update();
  
  ClassImageType::Pointer  outClassImage = applyMRFLabeller->GetOutput();

  //Print the mrf labelled image
  ClassImageIterator labeloutIt( outClassImage, outClassImage->GetBufferedRegion() );
  labeloutIt = labeloutIt.Begin();
  ClassImageIterator labeloutItEnd = labeloutIt.End();

  //Verify if the results were as per expectation
  labeloutIt = labeloutIt.Begin();
  labeloutItEnd = labeloutIt.End();
  bool passTest = true;

  //Loop through the data set
  while(labeloutIt != labeloutItEnd)
  {
    int classIndex = 
      (int) itk::ScalarTraits<ClassImagePixelType>::GetScalar( *labeloutIt );
    if (classIndex != 2)
    {
      passTest = false;
      break;
    }
    ++labeloutIt;

    classIndex = 
      (int) itk::ScalarTraits<ClassImagePixelType>::GetScalar( *labeloutIt );
    if (classIndex != 2)
    {
      passTest = false;
      break;
    }
    ++labeloutIt;

    classIndex = 
      (int) itk::ScalarTraits<ClassImagePixelType>::GetScalar( *labeloutIt );
    if (classIndex != 1)
    {
      passTest = false;
      break;
    }
    ++labeloutIt;

    classIndex = 
      (int) itk::ScalarTraits<ClassImagePixelType>::GetScalar( *labeloutIt );
    if (classIndex != 1)
    {
      passTest = false;
      break;
    }
    ++labeloutIt;

  }//end while

  if( passTest = true ) 
    std::cout<< "MRF labeller Test Passed" << std::endl;
  else 
    std::cout<< "MRF labeller Test failed" << std::endl;

  return 0;
}
