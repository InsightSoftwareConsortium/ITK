/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSupervisedClassifierTest.cxx
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

#include "itkPixelTraits.h"

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
  typedef VecImageType::PixelType VecPixelType;

  enum { VecImageDimension = VecImageType::ImageDimension };
  typedef
    itk::ImageRegionIterator< VecPixelType, VecImageDimension> VecIterator;

  VecIterator outIt( vecImage, vecImage->GetBufferedRegion() );
  outIt = outIt.Begin();

  //Set up the vector to store the image  data
  typedef VecImageType::PixelType::VectorType DataVector;
  DataVector   dblVec; 

  //--------------------------------------------------------------------------
  //Manually create and store each vector
  //--------------------------------------------------------------------------
  //Slice 1
  //Vector no. 1
  dblVec[0] = 21; 
  dblVec[1] = 19;
  (*outIt).SetVector(dblVec); ++outIt;
  //Vector no. 2
  dblVec[0] = 20; 
  dblVec[1] = 20;
  (*outIt).SetVector(dblVec); ++outIt;
  //Vector no. 3
  dblVec[0] = 8; 
  dblVec[1] = 11;
  (*outIt).SetVector(dblVec); ++outIt;
  //Vector no. 4
  dblVec[0] = 10; 
  dblVec[1] = 12;
  (*outIt).SetVector(dblVec); ++outIt;

  //Slice 2
  //Vector no. 1
  dblVec[0] = 22; 
  dblVec[1] = 21;
  (*outIt).SetVector(dblVec); ++outIt;
  //Vector no. 2
  dblVec[0] = 21; 
  dblVec[1] = 22;
  (*outIt).SetVector(dblVec); ++outIt;
  //Vector no. 3
  dblVec[0] = 11; 
  dblVec[1] = 12;
  (*outIt).SetVector(dblVec); ++outIt;
  //Vector no. 4
  dblVec[0] = 9; 
  dblVec[1] = 10;
  (*outIt).SetVector(dblVec); ++outIt;

  //Slice 3
  //Vector no. 1 
  dblVec[0] = 19; 
  dblVec[1] = 20;
  (*outIt).SetVector(dblVec); ++outIt;
  //Vector no. 2
  dblVec[0] = 19; 
  dblVec[1] = 21;
  (*outIt).SetVector(dblVec); ++outIt;
  //Vector no. 3
  dblVec[0] = 11; 
  dblVec[1] = 11;
  (*outIt).SetVector(dblVec); ++outIt;
  //Vector no. 4
  dblVec[0] = 11; 
  dblVec[1] = 10;
  (*outIt).SetVector(dblVec); ++outIt;

  //Slice 4
  //Vector no. 1
  dblVec[0] = 18; 
  dblVec[1] = 18;
  (*outIt).SetVector(dblVec); ++outIt;
  //Vector no. 2
  dblVec[0] = 18; 
  dblVec[1] = 20;
  (*outIt).SetVector(dblVec); ++outIt;
  //Vector no. 3
  dblVec[0] = 12; 
  dblVec[1] = 10;
  (*outIt).SetVector(dblVec); ++outIt;
  //Vector no. 4
  dblVec[0] = 14; 
  dblVec[1] = 13;
  (*outIt).SetVector(dblVec); ++outIt;

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
  typedef itk::GaussianSupervisedClassifier<VecImageType,ClassImageType> 
	  GaussianSupervisedClassifierType;
  GaussianSupervisedClassifierType::Pointer 
	  applyClassifier = GaussianSupervisedClassifierType::New();

  // Set the Classifier parameters
  applyClassifier->SetNumClasses(NUM_CLASSES);
  applyClassifier->SetInputImage(vecImage);
  applyClassifier->SetTrainingImage(classImage);  

  //Run the gaussian classifier algorithm
  applyClassifier->ClassifyImage();

  ClassImageType::Pointer outClassImage =  
	  applyClassifier->GetClassifiedImage();

  //Print the gaussian classified image
  ClassImageIterator labeloutIt( outClassImage, 
                                 outClassImage->GetBufferedRegion() );
  labeloutIt = labeloutIt.Begin();
  ClassImageIterator labeloutItEnd = labeloutIt.End();

  int i=0;
  while(labeloutIt != labeloutItEnd)
  {
    //Print the classified index
    int classIndex = 
      (int) itk::ScalarTraits<ClassImagePixelType>::
        GetScalar( *labeloutIt );
	  std::cout << " Pixel No" << i << " Value " << classIndex << std::endl;
	  ++i;
	  ++labeloutIt;
  }//end while

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
    std::cout<< "Supervised Classifier Test Passed" << std::endl;
  else 
    std::cout<< "Supervised Classifier Test failed" << std::endl;


  return 0;
}