/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMedialNodeCorrespondencesTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include <stdio.h>

// Native ITK stuff
#include "itkSize.h"
#include "itkIndex.h"
#include "itkImage.h"
#include "itkImageRegionIterator.h"
#include "itkPoint.h"

// Blox stuff
#include "itkBloxBoundaryPointImage.h"
#include "itkBloxCoreAtomImage.h"
#include "itkGradientImageToBloxBoundaryPointImageFilter.h"
#include "itkBloxBoundaryPointToCoreAtomImageFilter.h"

// Spatial function stuff
#include "itkSphereSpatialFunction.h"
#include "itkFloodFilledSpatialFunctionConditionalIterator.h"

// DOG gradient related stuff
#include "itkBinomialBlurImageFilter.h"
#include "itkDifferenceOfGaussiansGradientImageFilter.h"

// Medial node correspondence related stuff
#include "itkMatrixResizeableDataObject.h"
#include "itkCoreAtomImageToDistanceMatrixProcess.h"


// Main for testing various classes related to medial node (clustered and 
// statistically analyzed core atoms) correspondences. There are to be 4 tests 
// included. Specifically tests for, itkCoreAtomImageToDistanceMatrixProcess, 
// itkCoreAtomImageToUnaryCorrespondenceMatrixProcess,
// itkMedialNodePairCorrespondenceProcess, and itkMedialNodeTripletCorrespondenceProcess.
// These tests explicitely test higher level processing as well as lower level
// processing with auxillary classes. Each test requires two images, boundary points
// and core atoms. They were all included in this test to avoid redundant code
// in the tests. Each test returns EXIT_FAILURE upon failing. If all the tests
// pass and EXIT_SUCCESS is returned.

int itkMedialNodeCorrespondencesTest(int, char *[])
{
  const unsigned int dim = 3;

  //-----------------Create a new input image--------------------
  // Image size and spacing parameters
  unsigned long sourceImageSize[]  = { 20,20,20 };
  double sourceImageSpacing[] = { 1.0,1.0,1.0 };
  double sourceImageOrigin[] = { 0,0,0 };

  int intensity1 = 255;
  int intensity2 = 128;

  typedef itk::BloxCoreAtomImage<dim> CoreAtomType;
  CoreAtomType::Pointer bloxCoreAtomImage1 = CoreAtomType::New();
  CoreAtomType::Pointer bloxCoreAtomImage2 = CoreAtomType::New();

  typedef itk::Image< unsigned char, dim > ImageType;
  typedef itk::SphereSpatialFunction<dim> FunctionType;
  typedef FunctionType::InputType FunctionPositionType;
  typedef itk::FloodFilledSpatialFunctionConditionalIterator <ImageType, FunctionType> ItType;
  typedef ImageType OutputType;
  typedef itk::DifferenceOfGaussiansGradientImageFilter<OutputType, double> DOGFilterType;
  typedef itk::GradientImageToBloxBoundaryPointImageFilter<DOGFilterType::TOutputImage> TBPFilter;
  typedef TBPFilter::TOutputImage BloxBPImageType;
  typedef itk::BloxCoreAtomImage<dim> CoreAtomType;
  typedef itk::BloxBoundaryPointToCoreAtomImageFilter<dim> TCAFilter;
  typedef TCAFilter::TOutputImage BloxCAImageType;

  // Create a sphere and find core atoms. This loop creates two sets of data and
  // core atom images. Required for Medial Node Correspondence testing.
  for(int i = 0; i < 2; i++)
    {
    // Creates the sourceImage (but doesn't set the size or allocate memory)
    ImageType::Pointer sourceImage = ImageType::New();
    sourceImage->SetOrigin(sourceImageOrigin);
    sourceImage->SetSpacing(sourceImageSpacing);

    printf("New sourceImage created\n");

    //-----The following block allocates the sourceImage-----

    // Create a size object native to the sourceImage type
    ImageType::SizeType sourceImageSizeObject;
    // Set the size object to the array defined earlier
    sourceImageSizeObject.SetSize( sourceImageSize );
    // Create a region object native to the sourceImage type
    ImageType::RegionType largestPossibleRegion;
    // Resize the region
    largestPossibleRegion.SetSize( sourceImageSizeObject );
    // Set the largest legal region size (i.e. the size of the whole sourceImage) to what we just defined
    sourceImage->SetLargestPossibleRegion( largestPossibleRegion );
    // Set the buffered region
    sourceImage->SetBufferedRegion( largestPossibleRegion );
    // Set the requested region
    sourceImage->SetRequestedRegion( largestPossibleRegion );
    // Now allocate memory for the sourceImage
    sourceImage->Allocate();

    printf("New sourceImage allocated\n");

    // Initialize the image to hold all 0's
    itk::ImageRegionIterator<ImageType> it = 
      itk::ImageRegionIterator<ImageType>(sourceImage, largestPossibleRegion);

    for(it.GoToBegin(); !it.IsAtEnd(); ++it)
      {
      it.Set(0);
      }

    //---------Create and initialize a spatial function-----------

    // Create and initialize a new sphere function
    FunctionType::Pointer spatialFunc = FunctionType::New();
    spatialFunc->SetRadius( 5 );

    FunctionPositionType center;
    center[0]=10;
    center[1]=10;
    center[2]=10;
    spatialFunc->SetCenter(center);

    printf("Sphere spatial function created\n");

    //---------Create and initialize a spatial function iterator-----------
    ImageType::IndexType seedPos;
    const ImageType::IndexValueType pos[] = {10,10,10};
    seedPos.SetIndex(pos);


    ItType sfi = ItType(sourceImage, spatialFunc, seedPos);

    // Iterate through the entire image and set interior pixels to 255
    for( ; !( sfi.IsAtEnd() ); ++sfi)
      {
      if(i==0)
        {
        //std::cerr << "Set Intensity 1" << std::endl;
        sfi.Set(intensity1);
        }
       else
        {
        //std::cerr << "Set Intensity 2" << std::endl;
        sfi.Set(intensity2);
        }
      }

    printf("Spatial function iterator created, sphere drawn\n");

    //--------------------Do blurring and edge detection----------------
  
    // Create a binomial blur filter
    itk::BinomialBlurImageFilter<ImageType, OutputType>::Pointer binfilter;
    binfilter = itk::BinomialBlurImageFilter<ImageType, OutputType>::New();

    sourceImage->SetRequestedRegion(sourceImage->GetLargestPossibleRegion() );

    // Set filter parameters
    binfilter->SetInput(sourceImage);
    if(i == 0)
      binfilter->SetRepetitions(4);
    else
      binfilter->SetRepetitions(2);

    // Set up the output of the filter
    ImageType::Pointer blurredImage = binfilter->GetOutput();

    // Create a differennce of gaussians gradient filter
    DOGFilterType::Pointer DOGFilter = DOGFilterType::New();

    // We're filtering the output of the binomial filter
    DOGFilter->SetInput(blurredImage);

    // Get the output of the gradient filter
    DOGFilterType::TOutputImage::Pointer gradientImage = DOGFilter->GetOutput();

    //------------------------Blox Boundary Point Analysis-------------------------


    TBPFilter::Pointer bpFilter= TBPFilter::New();
    bpFilter->SetThreshold(10);
    bpFilter->SetInput( DOGFilter->GetOutput() );

    BloxBPImageType::Pointer bloxBoundaryPointImage = bpFilter->GetOutput();

    bpFilter->Update();

    //----------------------Find core atoms-------------------------

    CoreAtomType::Pointer coreAtomImage = CoreAtomType::New();


    TCAFilter::Pointer caFilter = TCAFilter::New();
    caFilter->SetInput(bloxBoundaryPointImage);
    caFilter->SetDistanceMin(8.0);
    caFilter->SetDistanceMax(12.0);
    if(i == 0)
      caFilter->SetEpsilon(0.05);
    else
      caFilter->SetEpsilon(0.20);

    caFilter->SetPolarity(0);

    BloxCAImageType::Pointer bloxCoreAtomImage = caFilter->GetOutput();

    caFilter->Update();
  
    // Test the macros in the image
    bloxCoreAtomImage->GetMedialNodeCount();
    bloxCoreAtomImage->GetNodePointerList();

    //--------------------Analyze core atom population---------------------

    std::cout << "Performing Eigenanalysis\n";
  
    bloxCoreAtomImage->DoEigenanalysis();
  
    //-----------------------Do core atom voting---------------------------
  
    std::cout << "Doing core atom voting\n";
  
    bloxCoreAtomImage->DoCoreAtomVoting();

    if(i == 0)
      {bloxCoreAtomImage1 = bloxCoreAtomImage;}
    else
      {bloxCoreAtomImage2 = bloxCoreAtomImage;}
    }

  int numberNodes1 = bloxCoreAtomImage1->GetMedialNodeCount();
  int numberNodes2 = bloxCoreAtomImage2->GetMedialNodeCount();

  //-------Test CoreAtomImageToDistanceMatrixProcess-----------

  std::cout << "Testing CoreAtomImageToDistanceMatrixProcess" << std::endl;

  itk::CoreAtomImageToDistanceMatrixProcess<BloxCAImageType>::Pointer distanceMatrixProcess;
  distanceMatrixProcess = itk::CoreAtomImageToDistanceMatrixProcess<BloxCAImageType>::New();

  // Stores the distances between nodes in an image.
  typedef itk::MatrixResizeableDataObject<double> MatrixType;
  typedef MatrixType::Pointer DistanceMatrixPointer;

  DistanceMatrixPointer distanceMatrix = MatrixType::New();
  DistanceMatrixPointer correctDistance = MatrixType::New(); // The ground truth values for this test

  // Set the correct values of the distance matrix to test against.
  correctDistance->resize(numberNodes1, numberNodes1);
  correctDistance->put(0,0,0); 
  correctDistance->put(0,1,2.42792);
  correctDistance->put(0,2,2.42792);
  correctDistance->put(0,3,3.62853);
  correctDistance->put(0,4,2.42792);
  correctDistance->put(0,5,3.62853);
  correctDistance->put(0,6,3.62853);

  correctDistance->put(1,0,2.42792);
  correctDistance->put(1,1,0);
  correctDistance->put(1,2,3.7268);
  correctDistance->put(1,3,2.78767);
  correctDistance->put(1,4,3.7268);
  correctDistance->put(1,5,2.78767);
  correctDistance->put(1,6,4.94715);

  correctDistance->put(2,0,2.42792);
  correctDistance->put(2,1,3.7268);
  correctDistance->put(2,2,0);
  correctDistance->put(2,3,2.78767);
  correctDistance->put(2,4,3.7268);
  correctDistance->put(2,5,4.94715);
  correctDistance->put(2,6,2.78767);

  correctDistance->put(3,0,3.62853);
  correctDistance->put(3,1,2.78767);
  correctDistance->put(3,2,2.78767);
  correctDistance->put(3,3,0);
  correctDistance->put(3,4,4.94715);
  correctDistance->put(3,5,4.48191);
  correctDistance->put(3,6,4.48191);

  correctDistance->put(4,0,2.42792);
  correctDistance->put(4,1,3.7268);
  correctDistance->put(4,2,3.7268);
  correctDistance->put(4,3,4.94715);
  correctDistance->put(4,4,0);
  correctDistance->put(4,5,2.78767);
  correctDistance->put(4,6,2.78767);

  correctDistance->put(5,0,3.62853);
  correctDistance->put(5,1,2.78767);
  correctDistance->put(5,2,4.94715);
  correctDistance->put(5,3,4.48191);
  correctDistance->put(5,4,2.78767);
  correctDistance->put(5,5,0);
  correctDistance->put(5,6,4.48191);

  correctDistance->put(6,0,3.62853);
  correctDistance->put(6,1,4.94715);
  correctDistance->put(6,2,2.78767);
  correctDistance->put(6,3,4.48191);
  correctDistance->put(6,4,2.78767);
  correctDistance->put(6,5,4.48191);
  correctDistance->put(6,6,0);

  // Set bloxCoreAtomImage1 as the input to the process.
  distanceMatrixProcess->SetInput1(bloxCoreAtomImage1);

  // Get the resulting output of the process.
  distanceMatrix = distanceMatrixProcess->GetOutput();

  // Update the pipeline.
  distanceMatrixProcess->Update();

  // Pass/fail flag.
  bool distanceSuccess = true;

  int indexI = 0;
  int indexJ = 0;
  double difference = 0;

  // Iterate through the distance matrix to test that the distance values computed are correct.
  for(int i=0;i<numberNodes1;++i)
    {
    for(int j=0;j<numberNodes1;++j)
      {
      if( fabs(distanceMatrix->get(i,j) - correctDistance->get(i,j)) >= 0.0001) 
        {
        indexI = i;
        indexJ = j;
        difference = (distanceMatrix->get(i,j) - correctDistance->get(i,j));
        distanceSuccess = false;
        break;
        }
      }
    }

  // Print results of test.
  if(distanceSuccess)
    std::cerr << "CoreAtomImageToDistanceMatrixProcess Test Passed!" << std::endl;
  else
    {
    std::cerr << "CoreAtomImageToDistanceMatrixProcess Test failed at index (" << indexI << ", " 
      << indexJ << ") with difference: " << difference << std::endl;
    return EXIT_FAILURE;
    }

  std::cerr << "All tests Passed!" << std::endl;
  return EXIT_SUCCESS;
}
