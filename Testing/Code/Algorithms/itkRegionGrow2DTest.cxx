/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegionGrow2DTest.cxx
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
#include "itkImage.h"
#include "itkVector.h"
#include "vnl/vnl_matrix_fixed.h"
#include "itkImageRegionIterator.h"
#include "itkTextOutput.h"

#include "itkKLMRegionGrowImageFilter.h"

#define   IMGWIDTH            6
#define   IMGHEIGHT           6
#define   NFRAMES             1
#define   NFRAMES3D           2
#define   NUMBANDS            1
#define   NDIMENSION2D        2
#define   NDIMENSION3D        3
#define   STARTFRAME          0
#define   NUM_BYTES_PER_PIXEL 1

#define   REGIONGROW_NUMREGIONS      4
#define   REGIONGROW_NUMREGIONS3D    4
#define   REGIONGROW_LAMBDA       1000
#define   REGIONGROW_ROW_GRIDSIZE    2
#define   REGIONGROW_COL_GRIDSIZE    2
#define   REGIONGROW_ROW_GRIDSIZE_3D 3
#define   REGIONGROW_COL_GRIDSIZE_3D 3
#define   REGIONGROW_SLICE_GRIDSIZE  1

static unsigned int test_regiongrowKLM2D();
static unsigned int test_regiongrowKLM3D();

//
// This test mutual information registration
//


int itkRegionGrow2DTest(int, char* [] )
{
  //Test the KLM algorithm applied to 2D data
  test_regiongrowKLM2D();

  //Test the 3D extension of the KLM algorithm
  test_regiongrowKLM3D();
  return( 0 );
}


unsigned int test_regiongrowKLM2D()
{
  //---------------------------------------------------------------
  //Generate the training data
  //---------------------------------------------------------------
  typedef itk::Image<itk::Vector<double,NUMBANDS>,NDIMENSION2D> ImageType; 
  ImageType::Pointer image  = ImageType::New();

  ImageType::SizeType ImageSize = {{ IMGWIDTH , IMGHEIGHT }};

  ImageType::IndexType index;
  index.Fill(0);

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
    itk::ImageRegionIterator< ImageType > ImageIterator;

  ImageIterator 
  inIt( image, image->GetBufferedRegion() );

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
  inIt.Set( 03 ); ++inIt; 
  inIt.Set( 03 ); ++inIt;
  inIt.Set( 30 ); ++inIt; 
  inIt.Set( 30 ); ++inIt; 
  inIt.Set( 20 ); ++inIt; 
  inIt.Set( 20 ); ++inIt;
  //--------------------------------------------------------------------------
  // Fill the row no. 2
  //--------------------------------------------------------------------------
  inIt.Set( 03 ); ++inIt; 
  inIt.Set( 03 ); ++inIt;
  inIt.Set( 30 ); ++inIt; 
  inIt.Set( 30 ); ++inIt; 
  inIt.Set( 20 ); ++inIt; 
  inIt.Set( 20 ); ++inIt;
  //--------------------------------------------------------------------------
  // Fill the row no. 3
  //--------------------------------------------------------------------------
  inIt.Set( 04 ); ++inIt; 
  inIt.Set( 04 ); ++inIt;
  inIt.Set( 40 ); ++inIt; 
  inIt.Set( 40 ); ++inIt; 
  inIt.Set( 40 ); ++inIt; 
  inIt.Set( 40 ); ++inIt;
  //--------------------------------------------------------------------------
  // Fill the row no. 4
  //--------------------------------------------------------------------------
  inIt.Set( 04 ); ++inIt; 
  inIt.Set( 04 ); ++inIt;
  inIt.Set( 40 ); ++inIt; 
  inIt.Set( 40 ); ++inIt; 
  inIt.Set( 40 ); ++inIt; 
  inIt.Set( 40 ); ++inIt;
  //--------------------------------------------------------------------------
  // Fill the row no. 5
  //--------------------------------------------------------------------------
  inIt.Set( 03 ); ++inIt; 
  inIt.Set( 03 ); ++inIt;
  inIt.Set( 02 ); ++inIt; 
  inIt.Set( 02 ); ++inIt; 
  inIt.Set( 04 ); ++inIt; 
  inIt.Set( 04 ); ++inIt;
  //--------------------------------------------------------------------------
  // Fill the row no. 6
  //--------------------------------------------------------------------------
  inIt.Set( 03 ); ++inIt; 
  inIt.Set( 03 ); ++inIt;
  inIt.Set( 02 ); ++inIt; 
  inIt.Set( 02 ); ++inIt; 
  inIt.Set( 04 ); ++inIt; 
  inIt.Set( 04 ); ++inIt;
  //--------------------------------------------------------------------------
  // Test code for the Region Grow algorithm
  //----------------------------------------------------------------------
  // Begin the application of the Region Grow KLM algorithm
  //---------------------------------------------------------------------
  // Set the Region Grow Algorithm classifier/segmentor
  //---------------------------------------------------------------------

  typedef itk::KLMRegionGrowImageFilter<ImageType,ImageType> KLMRegionGrowImageFilterT;
  KLMRegionGrowImageFilterT::Pointer applyRegionGrowImageFilterKLM = KLMRegionGrowImageFilterT::New();
  
  //----------------------------------------------------------------------
  //Set the parameters of the clusterer
  //----------------------------------------------------------------------
  applyRegionGrowImageFilterKLM->SetInput(image);
  applyRegionGrowImageFilterKLM->SetMaximumNumberOfRegions(REGIONGROW_NUMREGIONS);
  applyRegionGrowImageFilterKLM->SetMaxLambda(REGIONGROW_LAMBDA);
  applyRegionGrowImageFilterKLM->SetRowGridSize(REGIONGROW_ROW_GRIDSIZE);
  applyRegionGrowImageFilterKLM->SetColGridSize(REGIONGROW_COL_GRIDSIZE);


  //Kick off the Region grow function
  applyRegionGrowImageFilterKLM->Update();
  applyRegionGrowImageFilterKLM->Print(std::cout);

  typedef itk::Image<itk::Vector<double,NUMBANDS>,NDIMENSION2D> OutputImageType; 
  OutputImageType::Pointer outImage = applyRegionGrowImageFilterKLM->GetOutput();

  //Make sure that the labelled image type is set to unsigned integer
  //as labels associated with different regions are always integers 
  typedef itk::Image<unsigned short, NDIMENSION2D> LabelledImageType;
  LabelledImageType::Pointer labelledImage
    = applyRegionGrowImageFilterKLM->GetLabelledImage();

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
    itk::ImageRegionIterator< LabelledImageType > LabelImageIterator;

  LabelImageIterator 
  labelIt( labelledImage, labelledImage->GetBufferedRegion() );

  unsigned short pixelLabel;  
  unsigned short maxpixelLabel=0;

  while(!labelIt.IsAtEnd())
  {
    pixelLabel = (unsigned short) labelIt.Get();

    if(pixelLabel > maxpixelLabel)
      maxpixelLabel = pixelLabel;
    ++labelIt;

  }//end while iterator loop


  //Check the size of the vector of unique labels
  if( maxpixelLabel > applyRegionGrowImageFilterKLM->GetMaximumNumberOfRegions() )
  {
    std::cout<<"Region Grow with KLM algorithm failed" << std::endl;
    std::cout<<"More than the max set num regions found "<<std::endl;
    std::cout<<"Increase the lambda parameter is suggested" <<std::endl;
  }

  if( maxpixelLabel < applyRegionGrowImageFilterKLM->GetMaximumNumberOfRegions() )
  {
    std::cout<<"Region Grow with KLM algorithm passed" << std::endl;
    std::cout<<"Fewer regions than desired found"<<std::endl;
  }

  if( maxpixelLabel == applyRegionGrowImageFilterKLM->GetMaximumNumberOfRegions() )
  {
    std::cout<<"Region Grow with KLM algorithm passed" << std::endl;
    std::cout<<"Desired number of regions found"<<std::endl;
  }

  //Print out the resulting labelled image
  labelIt.GoToBegin();
  while(!labelIt.IsAtEnd())
  {
    for(int k=0; k<IMGWIDTH;k++)
    {
      pixelLabel = (unsigned short) labelIt.Get();
      std::cout << pixelLabel;
      ++labelIt;
    }//end row
    std::cout<<""<<std::endl;
  }//end while

  
  return 0;
} // End test_regiongrow2D()


unsigned int test_regiongrowKLM3D()
{

  itk::OutputWindow::SetInstance(itk::TextOutput::New().GetPointer());
  //---------------------------------------------------------------
  //Generate the training data
  //---------------------------------------------------------------
  typedef itk::Image<itk::Vector<double,NUMBANDS>,NDIMENSION3D> ImageType; 
  ImageType::Pointer image  = ImageType::New();

  ImageType::SizeType ImageSize = {{ IMGWIDTH , IMGHEIGHT, NFRAMES3D }};

  ImageType::IndexType index;
  index.Fill(0);

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
    itk::ImageRegionIterator< ImageType > ImageIterator;

  ImageIterator 
  inIt( image, image->GetBufferedRegion() );

  //Set up the vector to store the image  data
  typedef ImageType::PixelType::VectorType ImageData;
  ImageData   pixelData; 

  //--------------------------------------------------------------------------
  //Manually create and store each pixel. ) (In image slice 1)
  //The slice is a 6 x 6 matrix with 9 regions.
  //-------------------------------------------------------------------------- 
  //  03 | 03 | 03 | 20 | 20 | 20 |
  //  03 | 03 | 03 | 20 | 20 | 20 | 
  //  03 | 03 | 03 | 20 | 20 | 20 |
  //  40 | 40 | 40 | 40 | 40 | 40 |
  //  40 | 40 | 40 | 40 | 40 | 40 |
  //  40 | 40 | 40 | 40 | 40 | 40 |
  //--------------------------------------------------------------------------
  // Fill the row no. 1
  //--------------------------------------------------------------------------
  inIt.Set( 03 ); ++inIt; 
  inIt.Set( 03 ); ++inIt;
  inIt.Set( 03 ); ++inIt; 
  inIt.Set( 20 ); ++inIt; 
  inIt.Set( 20 ); ++inIt; 
  inIt.Set( 20 ); ++inIt;
  //--------------------------------------------------------------------------
  // Fill the row no. 2
  //--------------------------------------------------------------------------
  inIt.Set( 03 ); ++inIt; 
  inIt.Set( 03 ); ++inIt;
  inIt.Set( 03 ); ++inIt; 
  inIt.Set( 20 ); ++inIt; 
  inIt.Set( 20 ); ++inIt; 
  inIt.Set( 20 ); ++inIt;
  //--------------------------------------------------------------------------
  // Fill the row no. 3
  //--------------------------------------------------------------------------
  inIt.Set( 03 ); ++inIt; 
  inIt.Set( 03 ); ++inIt;
  inIt.Set( 03 ); ++inIt; 
  inIt.Set( 20 ); ++inIt; 
  inIt.Set( 20 ); ++inIt; 
  inIt.Set( 20 ); ++inIt;
  //--------------------------------------------------------------------------
  // Fill the row no. 4
  //--------------------------------------------------------------------------
  inIt.Set( 40 ); ++inIt; 
  inIt.Set( 40 ); ++inIt;
  inIt.Set( 40 ); ++inIt; 
  inIt.Set( 40 ); ++inIt; 
  inIt.Set( 40 ); ++inIt; 
  inIt.Set( 40 ); ++inIt;
  //--------------------------------------------------------------------------
  // Fill the row no. 5
  //--------------------------------------------------------------------------
  inIt.Set( 40 ); ++inIt; 
  inIt.Set( 40 ); ++inIt;
  inIt.Set( 40 ); ++inIt; 
  inIt.Set( 40 ); ++inIt; 
  inIt.Set( 40 ); ++inIt; 
  inIt.Set( 40 ); ++inIt;
  //--------------------------------------------------------------------------
  // Fill the row no. 6
  //--------------------------------------------------------------------------
  inIt.Set( 40 ); ++inIt; 
  inIt.Set( 40 ); ++inIt;
  inIt.Set( 40 ); ++inIt; 
  inIt.Set( 40 ); ++inIt; 
  inIt.Set( 40 ); ++inIt; 
  inIt.Set( 40 ); ++inIt;
  //--------------------------------------------------------------------------
  //Manually create and store each pixel. ) (In image slice 2)
  //The slice is a 6 x 6 matrix with 9 regions.
  //-------------------------------------------------------------------------- 
  //  10 | 10 | 10 | 20 | 20 | 20 |
  //  10 | 10 | 10 | 20 | 20 | 20 | 
  //  10 | 10 | 10 | 20 | 20 | 20 |
  //  30 | 30 | 30 | 20 | 20 | 20 |
  //  30 | 30 | 30 | 20 | 20 | 20 |
  //  30 | 30 | 30 | 20 | 20 | 20 |
  //--------------------------------------------------------------------------
  // Fill the row no. 1
  //--------------------------------------------------------------------------
  inIt.Set( 10 ); ++inIt; 
  inIt.Set( 10 ); ++inIt;
  inIt.Set( 10 ); ++inIt; 
  inIt.Set( 20 ); ++inIt; 
  inIt.Set( 20 ); ++inIt; 
  inIt.Set( 20 ); ++inIt;
  //--------------------------------------------------------------------------
  // Fill the row no. 2
  //--------------------------------------------------------------------------
  inIt.Set( 10 ); ++inIt; 
  inIt.Set( 10 ); ++inIt;
  inIt.Set( 10 ); ++inIt; 
  inIt.Set( 20 ); ++inIt; 
  inIt.Set( 20 ); ++inIt; 
  inIt.Set( 20 ); ++inIt;
  //--------------------------------------------------------------------------
  // Fill the row no. 3
  //--------------------------------------------------------------------------
  inIt.Set( 10 ); ++inIt; 
  inIt.Set( 10 ); ++inIt;
  inIt.Set( 10 ); ++inIt; 
  inIt.Set( 20 ); ++inIt; 
  inIt.Set( 20 ); ++inIt; 
  inIt.Set( 20 ); ++inIt;
  //--------------------------------------------------------------------------
  // Fill the row no. 4
  //--------------------------------------------------------------------------
  inIt.Set( 30 ); ++inIt; 
  inIt.Set( 30 ); ++inIt;
  inIt.Set( 30 ); ++inIt; 
  inIt.Set( 20 ); ++inIt; 
  inIt.Set( 20 ); ++inIt; 
  inIt.Set( 20 ); ++inIt;
  //--------------------------------------------------------------------------
  // Fill the row no. 5
  //--------------------------------------------------------------------------
  inIt.Set( 30 ); ++inIt; 
  inIt.Set( 30 ); ++inIt;
  inIt.Set( 30 ); ++inIt; 
  inIt.Set( 20 ); ++inIt; 
  inIt.Set( 20 ); ++inIt; 
  inIt.Set( 20 ); ++inIt;
  //--------------------------------------------------------------------------
  // Fill the row no. 6
  //--------------------------------------------------------------------------
  inIt.Set( 30 ); ++inIt; 
  inIt.Set( 30 ); ++inIt;
  inIt.Set( 30 ); ++inIt; 
  inIt.Set( 20 ); ++inIt; 
  inIt.Set( 20 ); ++inIt; 
  inIt.Set( 20 ); ++inIt;

  //--------------------------------------------------------------------------
  // Test code for the Region Grow algorithm
  //----------------------------------------------------------------------
  // Begin the application of the Region Grow KLM algorithm
  //---------------------------------------------------------------------
  // Set the Region Grow Algorithm classifier/segmentor
  //---------------------------------------------------------------------

  typedef itk::KLMRegionGrowImageFilter<ImageType,ImageType> KLMRegionGrowImageFilterT;
  KLMRegionGrowImageFilterT::Pointer applyRegionGrowImageFilterKLM = KLMRegionGrowImageFilterT::New();
  
  //----------------------------------------------------------------------
  //Set the parameters of the clusterer
  //----------------------------------------------------------------------
  applyRegionGrowImageFilterKLM->SetInput(image);
  applyRegionGrowImageFilterKLM->SetMaximumNumberOfRegions( REGIONGROW_NUMREGIONS3D );
  applyRegionGrowImageFilterKLM->SetMaxLambda( REGIONGROW_LAMBDA );
  applyRegionGrowImageFilterKLM->SetRowGridSize( REGIONGROW_ROW_GRIDSIZE_3D );
  applyRegionGrowImageFilterKLM->SetColGridSize( REGIONGROW_COL_GRIDSIZE_3D );
  applyRegionGrowImageFilterKLM->SetSliceGridSize( REGIONGROW_SLICE_GRIDSIZE );


  // Turn on the debug operations
  applyRegionGrowImageFilterKLM->DebugOff();

  //Kick off the Region grow function
  applyRegionGrowImageFilterKLM->Update();

  typedef itk::Image<itk::Vector<double,NUMBANDS>,NDIMENSION3D> OutputImageType; 
  OutputImageType::Pointer outImage = applyRegionGrowImageFilterKLM->GetOutput();

  applyRegionGrowImageFilterKLM->Print(std::cout);

  //Make sure that the labelled image type is set to unsigned integer
  //as labels associated with different regions are always integers 
  typedef itk::Image<unsigned short, NDIMENSION3D> LabelledImageType;
  LabelledImageType::Pointer labelledImage
    = applyRegionGrowImageFilterKLM->GetLabelledImage();

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
    itk::ImageRegionIterator< LabelledImageType > LabelImageIterator;

  LabelImageIterator 
  labelIt( labelledImage, labelledImage->GetBufferedRegion() );

  unsigned short pixelLabel;  
  unsigned short maxpixelLabel=0;

  while(!labelIt.IsAtEnd())
  {
    pixelLabel = (unsigned short) labelIt.Get();

    if(pixelLabel > maxpixelLabel)
      maxpixelLabel = pixelLabel;
    ++labelIt;

  }//end while iterator loop


  //Check the size of the vector of unique labels
  if( maxpixelLabel > applyRegionGrowImageFilterKLM->GetMaximumNumberOfRegions() )
  {
    std::cout<<"Region Grow with KLM algorithm failed" << std::endl;
    std::cout<<"More than the max set num regions found "<<std::endl;
    std::cout<<"Increase the lambda parameter is suggested" <<std::endl;
  }

  if( maxpixelLabel < applyRegionGrowImageFilterKLM->GetMaximumNumberOfRegions() )
  {
    std::cout<<"Region Grow with KLM algorithm passed" << std::endl;
    std::cout<<"Fewer regions than desired found"<<std::endl;
  }

  if( maxpixelLabel == applyRegionGrowImageFilterKLM->GetMaximumNumberOfRegions() )
  {
    std::cout<<"Region Grow with KLM algorithm passed" << std::endl;
    std::cout<<"Desired number of regions found"<<std::endl;
  }

  //Print out the resulting labelled image
  labelIt.GoToBegin();
  while(!labelIt.IsAtEnd())
  {
    for(int k=0; k<IMGWIDTH;k++)
    {
      pixelLabel = (unsigned short) labelIt.Get();
      std::cout << pixelLabel;
      ++labelIt;
    }//end row
    std::cout<<""<<std::endl;
  }//end while

  return 0;
} // End test_regiongrow3D()

