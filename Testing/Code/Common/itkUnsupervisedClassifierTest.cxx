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

  VecImageType::SizeType vecImgSize = { IMGWIDTH , IMGHEIGHT, NFRAMES };
  int numberOfBands = NUMBANDS;

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
  //Vector no. 1
  dblVec[0] = 21; 
  dblVec[1] = 9;
  (*outIt).SetVector(dblVec); ++outIt;
  //Vector no. 2
  dblVec[0] = 10; 
  dblVec[1] = 20;
  (*outIt).SetVector(dblVec); ++outIt;
  //Vector no. 3
  dblVec[0] = 8; 
  dblVec[1] = 21;
  (*outIt).SetVector(dblVec); ++outIt;
  //Vector no. 4
  dblVec[0] = 10; 
  dblVec[1] = 23;
  (*outIt).SetVector(dblVec); ++outIt;
  //Vector no. 5
  dblVec[0] = 12; 
  dblVec[1] = 21;
  (*outIt).SetVector(dblVec); ++outIt;
  //Vector no. 6
  dblVec[0] = 11; 
  dblVec[1] = 12;
  (*outIt).SetVector(dblVec); ++outIt;
  //Vector no. 7
  dblVec[0] = 15; 
  dblVec[1] = 22;
  (*outIt).SetVector(dblVec); ++outIt;
  //Vector no. 8
  dblVec[0] = 9; 
  dblVec[1] = 10;
  (*outIt).SetVector(dblVec); ++outIt;
  //Vector no. 9
  dblVec[0] = 19; 
  dblVec[1] = 10;
  (*outIt).SetVector(dblVec); ++outIt;
  //Vector no. 10
  dblVec[0] = 19; 
  dblVec[1] = 10;
  (*outIt).SetVector(dblVec); ++outIt;
  //Vector no. 11
  dblVec[0] = 21; 
  dblVec[1] = 21;
  (*outIt).SetVector(dblVec); ++outIt;
  //Vector no. 12
  dblVec[0] = 11; 
  dblVec[1] = 20;
  (*outIt).SetVector(dblVec); ++outIt;
  //Vector no. 13
  dblVec[0] = 8; 
  dblVec[1] = 18;
  (*outIt).SetVector(dblVec); ++outIt;
  //Vector no. 14
  dblVec[0] = 18; 
  dblVec[1] = 10;
  (*outIt).SetVector(dblVec); ++outIt;
  //Vector no. 15
  dblVec[0] = 22; 
  dblVec[1] = 10;
  (*outIt).SetVector(dblVec); ++outIt;
  //Vector no. 16
  dblVec[0] = 24; 
  dblVec[1] = 23;
  (*outIt).SetVector(dblVec); ++outIt;

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