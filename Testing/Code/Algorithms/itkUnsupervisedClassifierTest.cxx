/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkUnsupervisedClassifierTest.cxx
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
#include "vnl/vnl_math.h"
#include "itkImageRegionIterator.h"
#include "itkKMeansUnsupervisedClassifier.h"

//File name definitions 
#define   IMGWIDTH            16
#define   IMGHEIGHT           1
#define   NFRAMES             1
#define   NUMBANDS            2
#define   NDIMENSION          3


#define   CDBKWIDTH           4
#define   CDBKHEIGHT          1
#define   NFRAMES             1
#define   NCODEWORDS          CDBKWIDTH * CDBKHEIGHT * NFRAMES
#define   NUMBANDS            2
#define   NDIMENSION          3
#define   STARTFRAME          0
#define   NUM_BYTES_PER_PIXEL 1
#define   ONEBAND             1


int main()
{

  //------------------------------------------------------
  //Create a simple test vector with 16 entries and 2 bands
  //------------------------------------------------------
  typedef itk::Image<itk::Vector<double,NUMBANDS>,NDIMENSION> VecImageType; 

  VecImageType::Pointer vecImage = VecImageType::New();

  VecImageType::SizeType vecImgSize = {{ IMGWIDTH , IMGHEIGHT, NFRAMES }};

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

  //--------------------------------------------------------------------------
  //Manually create and store each vector
  //--------------------------------------------------------------------------

  //Vector no. 1
  (*outIt) = 21,9; ++outIt;
  //Vector no. 2
  (*outIt) = 10,20; ++outIt;
  //Vector no. 3
  (*outIt) = 8,21; ++outIt;
  //Vector no. 4
  (*outIt) = 10,23; ++outIt;
  //Vector no. 5
  (*outIt) = 12,21; ++outIt;
  //Vector no. 6
  (*outIt) = 11,12; ++outIt;
  //Vector no. 7
  (*outIt) = 15,22; ++outIt;
  //Vector no. 8
  (*outIt) = 9,10; ++outIt;
  //Vector no. 9
  (*outIt) = 19,10; ++outIt;
  //Vector no. 10
  (*outIt) = 19,10; ++outIt;
  //Vector no. 11
  (*outIt) = 21,21; ++outIt;
  //Vector no. 12
  (*outIt) = 11,20; ++outIt;
  //Vector no. 13
  (*outIt) = 8,18; ++outIt;
  //Vector no. 14
  (*outIt) = 18,10; ++outIt;
  //Vector no. 15
  (*outIt) = 22,10; ++outIt;
  //Vector no. 16
  (*outIt) = 24,23; ++outIt;

  //---------------------------------------------------------------
  //Input the codebook
  //---------------------------------------------------------------
  //------------------------------------------------------------------
  //Read the codebook into an vnl_matrix
  //------------------------------------------------------------------

  vnl_matrix<double> inCDBK(NCODEWORDS, NUMBANDS);
  //There are 4 entries to the code book
  int r,c;
  r=0; c=0; inCDBK.put(r,c,10);
  r=0; c=1; inCDBK.put(r,c,10);
  r=1; c=0; inCDBK.put(r,c,10);
  r=1; c=1; inCDBK.put(r,c,20);
  r=2; c=0; inCDBK.put(r,c,20);
  r=2; c=1; inCDBK.put(r,c,10);
  r=3; c=0; inCDBK.put(r,c,20);
  r=3; c=1; inCDBK.put(r,c,20);

  //----------------------------------------------------------------------
  // Test code for the Unsupervised classifier algorithm
  //----------------------------------------------------------------------

  //---------------------------------------------------------------------
  // Multiband data is now available in the right format
  //---------------------------------------------------------------------
  // Begin the application of the Kmeans algorithm
  //---------------------------------------------------------------------
  //Set the Kmeans classifier
  //---------------------------------------------------------------------
  typedef itk::KMeansUnsupervisedClassifier<VecImageType,VecImageType> 
    KMeansUnsupervisedClassifierType;

  KMeansUnsupervisedClassifierType::Pointer 
    applyKMeansClusterer = KMeansUnsupervisedClassifierType::New();


  //----------------------------------------------------------------------
  //Set the parameters of the clusterer
  //----------------------------------------------------------------------
  applyKMeansClusterer->SetInputImage(vecImage);
  applyKMeansClusterer->SetNumClasses(NCODEWORDS);
  applyKMeansClusterer->SetThreshold(0.01);
  applyKMeansClusterer->Cluster();
  //applyKMeansClusterer->PrintKmeansAlgorithmResults();

  //Validation with no codebook/initial Kmeans estimate
  vnl_matrix<double> kmeansResult = applyKMeansClusterer->GetKmeansResults();
  vnl_matrix<double> errorMat = (kmeansResult - inCDBK);
  double error =0;
  double meanCDBKvalue = 0;

  for(int r = 0; r < NCODEWORDS; r++)
    for(int c = 0; c < NUMBANDS; c++)
    {
      error += vnl_math_abs((kmeansResult[r][c]-inCDBK[r][c])/inCDBK[r][c]);
      meanCDBKvalue += inCDBK[r][c];
    }

  error /= NCODEWORDS*NUMBANDS;
  meanCDBKvalue /= NCODEWORDS*NUMBANDS; 

  if( error < 0.1 * meanCDBKvalue)
    std::cout << "Kmeans algorithm passed (without initial input)"<<std::endl;
  else
    std::cout << "Kmeans algorithm failed (without initial input)"<<std::endl;

  applyKMeansClusterer->SetCodebook(inCDBK);
  applyKMeansClusterer->Cluster();
  //applyKMeansClusterer->PrintKmeansAlgorithmResults();

  //Validation with initial Kmeans estimate provided as input by the user
  kmeansResult = applyKMeansClusterer->GetKmeansResults();
  errorMat = (kmeansResult - inCDBK);
  error =0;
  meanCDBKvalue = 0;

  for(int r = 0; r < NCODEWORDS; r++)
    for(int c = 0; c < NUMBANDS; c++)
    {
      error += vnl_math_abs((kmeansResult[r][c]-inCDBK[r][c])/inCDBK[r][c]);
      meanCDBKvalue += inCDBK[r][c];
    }

  error /= NCODEWORDS*NUMBANDS;
  meanCDBKvalue /= NCODEWORDS*NUMBANDS; 

  if( error < 0.1 * meanCDBKvalue)
    std::cout << "Kmeans algorithm passed (with initial input)"<<std::endl;
  else
    std::cout << "Kmeans algorithm failed (with initial input)"<<std::endl;

  return 0;


}
