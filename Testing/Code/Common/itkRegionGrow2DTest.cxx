/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegionGrow2DTest.cxx
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
#include "itkPixelTraits.h"

#include "itkRegionGrowKLM.h"

#define   IMGWIDTH            6
#define   IMGHEIGHT           6
#define   NFRAMES             1
#define   NUMBANDS            1
#define   NDIMENSION          3
#define   STARTFRAME          0
#define   NUM_BYTES_PER_PIXEL 1

#define   REGIONGROW_NUMREGIONS    4
#define   REGIONGROW_LAMBDA      1000
#define   REGIONGROW_GRANULARITY    2

//
// This test mutual information registration
//

int main()
{

  //---------------------------------------------------------------
  //Generate the training data
  //---------------------------------------------------------------
  typedef itk::Image<itk::Vector<double,1>,NDIMENSION> ImageType; 
  ImageType::Pointer image  = ImageType::New();

  ImageType::SizeType ImageSize = { IMGWIDTH , IMGHEIGHT, NFRAMES };

  ImageType::IndexType index = ImageType::IndexType::ZeroIndex;
  ImageType::RegionType region;

  region.SetSize( ImageSize );
  region.SetIndex( index );

  image->SetLargestPossibleRegion( region );
  image->SetBufferedRegion( region );
  image->Allocate();

  // setup the iterators
  typedef ImageType::PixelType ImagePixelType;

  enum { imageDimension = ImageType::ImageDimension };
  typedef
    itk::ImageRegionIterator< ImagePixelType, imageDimension> 
	  ImageIterator;

  ImageIterator 
	inIt( image, image->GetBufferedRegion() );
  inIt = inIt.Begin();

  //Set up the vector to store the image  data
  typedef ImageType::PixelType::VectorType ImageData;
  ImageData   pixelData; 

  //--------------------------------------------------------------------------
  //Manually create and store each pixel.
  //The image is a 6 x 6 matrix with 9 regions.
  //-------------------------------------------------------------------------- 
  //  03 | 03 | 30 | 30 | 20 | 20 |
  //  03 | 03 | 30 | 30 | 20 | 20 | 
  //  04 | 04 | 40 | 40 | 40 | 40 |
  //  04 | 04 | 40 | 40 | 40 | 40 |
  //  03 | 03 | 02 | 02 | 04 | 04 |
  //  03 | 03 | 02 | 02 | 04 | 04 |
  //--------------------------------------------------------------------------
  // Fill the row no. 1
  //--------------------------------------------------------------------------
  *inIt = 03; ++inIt; 
  *inIt = 03; ++inIt;
  *inIt = 30; ++inIt; 
  *inIt = 30; ++inIt; 
  *inIt = 20; ++inIt; 
  *inIt = 20; ++inIt;
  //--------------------------------------------------------------------------
  // Fill the row no. 2
  //--------------------------------------------------------------------------
  *inIt = 03; ++inIt; 
  *inIt = 03; ++inIt;
  *inIt = 30; ++inIt; 
  *inIt = 30; ++inIt; 
  *inIt = 20; ++inIt; 
  *inIt = 20; ++inIt;
  //--------------------------------------------------------------------------
  // Fill the row no. 3
  //--------------------------------------------------------------------------
  *inIt = 04; ++inIt; 
  *inIt = 04; ++inIt;
  *inIt = 40; ++inIt; 
  *inIt = 40; ++inIt; 
  *inIt = 40; ++inIt; 
  *inIt = 40; ++inIt;
  //--------------------------------------------------------------------------
  // Fill the row no. 4
  //--------------------------------------------------------------------------
  *inIt = 04; ++inIt; 
  *inIt = 04; ++inIt;
  *inIt = 40; ++inIt; 
  *inIt = 40; ++inIt; 
  *inIt = 40; ++inIt; 
  *inIt = 40; ++inIt;
  //--------------------------------------------------------------------------
  // Fill the row no. 5
  //--------------------------------------------------------------------------
  *inIt = 03; ++inIt; 
  *inIt = 03; ++inIt;
  *inIt = 02; ++inIt; 
  *inIt = 02; ++inIt; 
  *inIt = 04; ++inIt; 
  *inIt = 04; ++inIt;
  //--------------------------------------------------------------------------
  // Fill the row no. 6
  //--------------------------------------------------------------------------
  *inIt = 03; ++inIt; 
  *inIt = 03; ++inIt;
  *inIt = 02; ++inIt; 
  *inIt = 02; ++inIt; 
  *inIt = 04; ++inIt; 
  *inIt = 04; ++inIt;
  //--------------------------------------------------------------------------
  // Test code for the Region Grow algorithm
  //----------------------------------------------------------------------
  // Begin the application of the Region Grow KLM algorithm
  //---------------------------------------------------------------------
  // Set the Region Grow Algorithm classifier/segmentor
  //---------------------------------------------------------------------

  typedef itk::RegionGrowKLM<ImageType,ImageType> RegionGrowKLMT;

  RegionGrowKLMT::Pointer applyRegionGrowKLM = RegionGrowKLMT::New();
  
  //----------------------------------------------------------------------
  //Set the parameters of the clusterer
  //----------------------------------------------------------------------
  applyRegionGrowKLM->SetInput(image);
  applyRegionGrowKLM->SetMaxNumRegions(REGIONGROW_NUMREGIONS);
  applyRegionGrowKLM->SetMaxLambda(REGIONGROW_LAMBDA);
  applyRegionGrowKLM->SetRowGridSize(REGIONGROW_GRANULARITY);
  applyRegionGrowKLM->SetColGridSize(REGIONGROW_GRANULARITY);


  //Kick off the Region grow function
  applyRegionGrowKLM->Update();

  typedef itk::Image<itk::Vector<double,NUMBANDS>,NDIMENSION> OutputImageType; 
  OutputImageType::Pointer outImage = applyRegionGrowKLM->GetOutput();

  //Make sure that the labelled image type is set to unsigned integer
  //as labels associated with different regions are always integers 
  typedef itk::Image<unsigned short, NDIMENSION> LabelledImageType;
  LabelledImageType::Pointer labelledImage
    = applyRegionGrowKLM->GetLabelledImage();

  //Flag the completion of the algorithm
  std::cout<<"Completed execution of KLM Region Growing Algorithm"<<std::endl;
  std::cout<<"Final segmented image"<<std::endl;
  std::cout<<" "<<std::endl;

  //Loop through the labels and check if they match with segmented label
  //if match is found then skip if no match found a new label is identified
  //The test passes if the number of identified uniques is exactly equal to 
  //the number of expected regions


  //Print out the results
  //This should return unique integer labels of the segmented regions.
  //The region labels may or may not be consecutive integers but they
  //should be unique.

  // setup the iterators
  typedef LabelledImageType::PixelType LabelledImagePixelType;

  enum { labelledImageDimension = LabelledImageType::ImageDimension };
  typedef
    itk::ImageRegionIterator< LabelledImagePixelType, labelledImageDimension> 
	  LabelImageIterator;

  LabelImageIterator 
	labelIt( labelledImage, labelledImage->GetBufferedRegion() );
  labelIt = labelIt.Begin();
  LabelImageIterator labelItEnd = labelIt.End();


  unsigned short pixelLabel;  
  unsigned short maxpixelLabel=0;

  while(labelIt != labelItEnd)
  {
    pixelLabel = 
	    (unsigned short)  
        itk::ScalarTraits<LabelledImagePixelType>::GetScalar(*labelIt);

    if(pixelLabel > maxpixelLabel)
	    maxpixelLabel = pixelLabel;
	  ++labelIt;

  }//end while iterator loop


  //Check the size of the vector of unique labels
  if( maxpixelLabel > applyRegionGrowKLM->GetMaxNumRegions() )
  {
    std::cout<<"Region Grow with KLM algorithm failed" << std::endl;
    std::cout<<"More than the max set num regions found "<<std::endl;
	  std::cout<<"Increase the lambda parameter is suggested" <<std::endl;
  }

  if( maxpixelLabel < applyRegionGrowKLM->GetMaxNumRegions() )
  {
	  std::cout<<"Region Grow with KLM algorithm passed" << std::endl;
    std::cout<<"Fewer regions than desired found"<<std::endl;
  }

  if( maxpixelLabel == applyRegionGrowKLM->GetMaxNumRegions() )
  {
	  std::cout<<"Region Grow with KLM algorithm passed" << std::endl;
    std::cout<<"Desired number of regions found"<<std::endl;
  }

  //Print out the resulting labelled image
  labelIt = labelIt.Begin();
  while(labelIt != labelItEnd)
  {
    for(int k=0; k<IMGWIDTH;k++)
	  {
	    pixelLabel = 
	      (unsigned short)  itk::ScalarTraits<LabelledImagePixelType>::GetScalar(*labelIt);
	    std::cout << pixelLabel << std::ends;
	    ++labelIt;
	  }//end row
	  std::cout<<""<<std::endl;
  }//end while


  return 0;


}
