/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMRFLabellerTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

// Insight classes

#include "itkGaussianSupervisedClassifier.h"
#include "itkMRFImageFilter.h"

#include "itkSize.h"
#include "itkImage.h"
#include "itkVector.h"
#include "vnl/vnl_matrix_fixed.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkConstNeighborhoodIterator.h"
#include "itkNeighborhoodIterator.h"
#include "itkNeighborhoodAlgorithm.h"
#include "itkNeighborhood.h"

//Data definitons 
#define   IMGWIDTH            6
#define   IMGHEIGHT           6
#define   NFRAMES             3
#define   NUMBANDS            2  
#define   NDIMENSION          3
#define   NUM_CLASSES         3
#define   MAX_NUM_ITER        5
#define   NEIGHBORHOOD_RAD    1


int itkMRFLabellerTest(int, char**)
{

  //------------------------------------------------------
  //Create a simple test image with width, height, and 
  //depth 4 vectors each with each vector having data for 
  //2 bands.
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
  typedef VecImageType::PixelType::VectorType VecPixelType;

  enum { VecImageDimension = VecImageType::ImageDimension };
  typedef itk::ImageRegionIteratorWithIndex< VecImageType > VecIterator;

  VecIterator outIt( vecImage, vecImage->GetBufferedRegion() );

  //Set up the vector to store the image  data
  typedef VecImageType::PixelType     DataVector;
  DataVector   dblVec; 

  int i,k;
  int halfWidth = (int) (vecImgSize[0])/2;
  int halfHeight = (int) (vecImgSize[1])/2;
 
  //--------------------------------------------------------------------------
  //Manually create and store each vector
  //--------------------------------------------------------------------------
  //Slice 1
  //--------------------------------------------------------------------------
  //Row 1-3
  for( k=0; k< halfHeight; k++)
    {
    //Vector no. 1-3 
    dblVec[0] = 21; dblVec[1] = 19;
    for( i=0; i< halfWidth; ++i, ++outIt ) outIt.Set(dblVec); 
 
    //Vector no. 4-6 
    dblVec[0] = 18; dblVec[1] = 14;
    for( i=0; i< halfWidth; ++i, ++outIt ) outIt.Set(dblVec); 
    }

  //Row 4-6
  for( k=0; k< halfHeight; k++)
    {
    //Vector no. 1-3 
    dblVec[0] = 15; dblVec[1] = 11;
    for( i=0; i< halfWidth; ++i, ++outIt ) outIt.Set(dblVec); 
 
    //Vector no. 4-6 
    dblVec[0] = 10; dblVec[1] = 16;
    for( i=0; i< halfWidth; ++i, ++outIt ) outIt.Set(dblVec); 
    }

  //--------------------------------------------------------------------------
  //Slice 2
  //--------------------------------------------------------------------------
  //Row 1-3
  for( k=0; k< halfHeight; k++)
    {
    //Vector no. 1-3 Row k
    dblVec[0] = 14; dblVec[1] = 20;
    for( i=0; i< halfWidth; ++i, ++outIt ) outIt.Set(dblVec); 
 
    //Vector no. 4-6 Row k
    dblVec[0] = 18; dblVec[1] = 22;
    for( i=0; i< halfWidth; ++i, ++outIt ) outIt.Set(dblVec); 
    }

  //Row 4-6
  for( k=0; k< halfHeight; k++)
    {
    //Vector no. 1-3 Row k
    dblVec[0] = 15; dblVec[1] = 15;
    for( i=0; i< halfWidth; ++i, ++outIt ) outIt.Set(dblVec); 
 
    //Vector no. 4-6 Row k
    dblVec[0] = 12; dblVec[1] = 12;
    for( i=0; i< halfWidth; ++i, ++outIt ) outIt.Set(dblVec); 
    }

  //--------------------------------------------------------------------------
  //Slice 3
  //--------------------------------------------------------------------------
  //Row 1-3
  for( k=0; k< halfHeight; k++)
    {
    //Vector no. 1-3 Row k
    dblVec[0] = 19; dblVec[1] = 20;
    for( i=0; i< halfWidth; ++i, ++outIt ) outIt.Set(dblVec); 
 
    //Vector no. 4-6 Row k
    dblVec[0] = 19; dblVec[1] = 21;
    for( i=0; i< halfWidth; ++i, ++outIt ) outIt.Set(dblVec); 
    }

  //Row 4-6
  for( k=0; k< halfHeight; k++)
    {
    //Vector no. 1-3 Row k
    dblVec[0] = 12; dblVec[1] = 12;
    for( i=0; i< halfWidth; ++i, ++outIt ) outIt.Set(dblVec); 
 
    //Vector no. 4-6 Row k
    dblVec[0] = 11; dblVec[1] = 10;
    for( i=0; i< halfWidth; ++i, ++outIt ) outIt.Set(dblVec); 
    }
/*
  //--------------------------------------------------------------------------
  //Slice 4
  //--------------------------------------------------------------------------
  for( k=0; k< halfHeight; k++)
    {
    //Vector no. 1-3 Row k
    dblVec[0] = 16; dblVec[1] = 18;
    for( i=0; i< halfWidth; ++i, ++outIt ) outIt.Set(dblVec); 
 
    //Vector no. 4-6 Row k
    dblVec[0] = 18; dblVec[1] = 18;
    for( i=0; i< halfWidth; ++i, ++outIt ) outIt.Set(dblVec); 
    }

  //Row 4-6
  for( k=0; k< halfHeight; k++)
    {
    //Vector no. 1-3 Row k
    dblVec[0] = 15; dblVec[1] = 13;
    for( i=0; i< halfWidth; ++i, ++outIt ) outIt.Set(dblVec); 
 
    //Vector no. 4-6 Row k
    dblVec[0] = 14; dblVec[1] = 13;
    for( i=0; i< halfWidth; ++i, ++outIt ) outIt.Set(dblVec); 
    }
*/
  //---------------------------------------------------------------
  //Generate the training data
  //---------------------------------------------------------------
  typedef itk::Image<unsigned short,NDIMENSION> ClassImageType; 
  ClassImageType::Pointer classImage  = ClassImageType::New();

  ClassImageType::SizeType classImgSize = {{ IMGWIDTH , IMGHEIGHT, NFRAMES }};

  ClassImageType::IndexType classindex = ClassImageType::IndexType::ZeroIndex;
  ClassImageType::RegionType classregion;

  classregion.SetSize( classImgSize );
  classregion.SetIndex( classindex );

  classImage->SetLargestPossibleRegion( classregion );
  classImage->SetBufferedRegion( classregion );
  classImage->Allocate();

  // setup the iterators
  typedef ClassImageType::PixelType ClassImagePixelType;

  typedef  itk::ImageRegionIteratorWithIndex<ClassImageType>  ClassImageIterator;

  ClassImageIterator classoutIt( classImage, classImage->GetBufferedRegion() );
  //--------------------------------------------------------------------------
  //Manually create and store each vector
  //--------------------------------------------------------------------------
  //Slice 1
  //--------------------------------------------------------------------------
  //Row 1-3

  for( k=0; k< halfHeight; k++)
    {
    //Vector no. 1-3 Row k
    for( i=0; i< (halfWidth*2); ++i, ++classoutIt ) classoutIt.Set( 2 ); 
    }

  //Row 4-6
  for( k=0; k< halfHeight; k++)
    {
    for( i=0; i< (halfWidth*2); ++i, ++classoutIt ) classoutIt.Set( 1 );
    }
  //-------------------------------------------------------------------------- 
  //Slice 2
  //--------------------------------------------------------------------------
  //Row 1-6
  for( k=0; k< (halfHeight*2); k++)
    {
    //Vector no. 1-3 Row k
    for( i=0; i< (halfWidth*2); ++i, ++classoutIt ) classoutIt.Set( 0 ); 
    }

  //--------------------------------------------------------------------------
  //Slice 3
  //--------------------------------------------------------------------------
  for( k=0; k< halfHeight; k++)
    {
    //Vector no. 1-3 Row k
    for( i=0; i< (halfWidth*2); ++i, ++classoutIt ) classoutIt.Set( 2 ); 
    }

  //Row 4-6
  for( k=0; k< halfHeight; k++)
    {
    for( i=0; i< (halfWidth*2); ++i, ++classoutIt ) classoutIt.Set( 1 );
    }
/*
  //--------------------------------------------------------------------------
  //Slice 4
  //--------------------------------------------------------------------------
  //Row 1-6
  for( k=0; k< (halfHeight*2); k++)
    {
    //Vector no. 1-3 Row k
    for( i=0; i< (halfWidth*2); ++i, ++classoutIt ) classoutIt.Set( 0 ); 
    }
*/
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
  typedef itk::MRFImageFilter<VecImageType,ClassImageType> MRFImageFilterType;
  MRFImageFilterType::Pointer applyMRFImageFilter = MRFImageFilterType::New();

  // Set the MRF labeller parameters
  applyMRFImageFilter->SetNumberOfClasses(NUM_CLASSES);
  applyMRFImageFilter->SetMaximumNumberOfIterations(MAX_NUM_ITER);
  applyMRFImageFilter->SetErrorTolerance(0.10);

  //For setting up a square/cubic or hypercubic neighborhood
  applyMRFImageFilter->SetNeighborhoodRadius( NEIGHBORHOOD_RAD );

  //For setting up a rectangular/cuboidal or hypercuboidal neighborhood
  //itk::Size<NDIMENSION> radius = {{1, 10, 5}};
  //applyMRFImageFilter->SetNeighborhoodRadius( radius );
 
  applyMRFImageFilter->SetInput(vecImage);
  applyMRFImageFilter
    ->SetClassifier((ClassifierType) myGaussianClassifier ); 

  //Since a suvervised classifier is used, it requires a training image
  applyMRFImageFilter->SetTrainingImage(classImage);  
  
  //Kick off the MRF labeller function
  applyMRFImageFilter->Update();
  
  ClassImageType::Pointer  outClassImage = applyMRFImageFilter->GetOutput();

  //Print the mrf labelled image
  ClassImageIterator labeloutIt( outClassImage, outClassImage->GetBufferedRegion() );

  int sumtmp =0;
  while( !labeloutIt.IsAtEnd() )
    {
    sumtmp += (int)  labeloutIt.Get();
    ++labeloutIt;
    }  

  //---------------------------------------------------------------------
  // Set up the neighborhood iterators and the valid neighborhoods
  // for iteration
  //---------------------------------------------------------------------

  //Set up the nighborhood iterators
  /** Labelled image neighborhood interator typedef */

  typedef itk::NeighborhoodIterator< ClassImageType >
    OutImageNeighborhoodIterator;

  typedef OutImageNeighborhoodIterator::RadiusType 
    OutImageNeighborhoodRadiusType;

  typedef  
    itk::NeighborhoodAlgorithm::ImageBoundaryFacesCalculator< ClassImageType >
      OutImageFacesCalculator;
  
  typedef  OutImageFacesCalculator::FaceListType     OutImageFaceListType;

  typedef  OutImageFaceListType::iterator            OutImageFaceListIterator;

  OutImageNeighborhoodRadiusType outImageNeighborhoodRadius;
  outImageNeighborhoodRadius.Fill( 1 );

  //Define the face list for the input/labelled image
  OutImageFacesCalculator     outImageFacesCalculator;

  OutImageFaceListType        outImageFaceList;

  //Compute the faces for the neighborhoods in the input/labelled image 
  outImageFaceList =
    outImageFacesCalculator( outClassImage, 
                             outClassImage->GetBufferedRegion(),
                             outImageNeighborhoodRadius );  

  //Set up a face list iterator
  OutImageFaceListIterator outImageFaceListIter
    = outImageFaceList.begin();

  //Walk through the entire data set (not visiting the boundaries )
  OutImageNeighborhoodIterator
    nOutImageNeighborhoodIter( outImageNeighborhoodRadius,
                               outClassImage, 
                               *outImageFaceListIter );

  int sum = 0;
  typedef ClassImageType::PixelType ClassImagePixelType;
  ClassImagePixelType *outLabel;

  //Loop through the labelled region and add the pixel labels
  while( !nOutImageNeighborhoodIter.IsAtEnd() )
    {
    outLabel = nOutImageNeighborhoodIter.GetCenterValue();
    sum += ( int ) (*outLabel); 
    ++nOutImageNeighborhoodIter;
    }
  //Loop through the data set

  if( sum == 22 )
    std::cout<< "MRF labeller Test Passed" << std::endl;
  else 
    std::cout<< "MRF labeller Test failed" << std::endl;

  return 0;
}
