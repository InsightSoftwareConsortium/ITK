/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkLevelSetShapeDetectionTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


Copyright (c) 2000 National Library of Medicine
All rights reserved.

See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "itkFastMarching.h"
#include "itkLevelSetShapeDetection.h"
#include "itkImage.h"
#include "itkScalar.h"
#include "itkImageRegionIterator.h"

int main()
{

  /* -------------------------------------------------
   * Create an initial level set of dimension 64x64
   * with the zero set being a circle of radius 8
   * centered at (28,35).
   */

  // create a fastmarching object
  typedef itk::Image<itk::Scalar<float>,2> FloatImage;
  typedef itk::FastMarching<FloatImage> FastMarcherType;

  FastMarcherType::Pointer marcher = FastMarcherType::New();
  
  // setup trial points
  typedef FastMarcherType::NodeType NodeType;
  typedef FastMarcherType::NodeContainer NodeContainer;

  NodeContainer::Pointer trialPoints = NodeContainer::New();

  NodeType node;

  FloatImage::IndexType index0 = {{28,35}};
  
  node.value = 0.0;
  node.index = index0;
  trialPoints->InsertElement(0, node);
  
  marcher->SetTrialPoints( trialPoints );

  // specify the size of the output image
  FloatImage::SizeType size = {{64,64}};
  marcher->SetOutputSize( size );

  // update the marcher
  marcher->Update();

  // walk the marcher output
  FloatImage::Pointer levelSet = marcher->GetOutput();
  itk::ImageRegionIterator<itk::Scalar<float>,2>
    iterator( levelSet, levelSet->GetBufferedRegion() );

  iterator = iterator.Begin();
  for( ; !iterator.IsAtEnd(); ++iterator )
    {
      *iterator -= 8.0;
    }


  /* -------------------------------------------------
   * Create a edge potential image with all ones
   */
  FloatImage::Pointer edgeImg = FloatImage::New();
  edgeImg->CopyInformation( levelSet );
  edgeImg->SetBufferedRegion(
    levelSet->GetBufferedRegion() );
  edgeImg->Allocate();

  itk::ImageRegionIterator<itk::Scalar<float>,2>
    edgeIter( edgeImg, edgeImg->GetBufferedRegion() );

  for(edgeIter = edgeIter.Begin() ; !edgeIter.IsAtEnd(); ++edgeIter )
    {
    *edgeIter = 1.0;
    }

  /* -----------------------------------------------
   * Create a shape detection object
   */

  typedef itk::LevelSetShapeDetection<FloatImage,FloatImage>
    ShapeDetectorType;

  ShapeDetectorType::Pointer detector = ShapeDetectorType::New();

  detector->SetInput( levelSet );
  detector->SetEdgeImage( edgeImg );
  detector->SetNumberOfIterations( 10 );

  detector->SetDebugOn( true );

  /* -------------------------------------------------
   * Test the full-band version of the algorithm
   */
  std::cout << "Running the full-band version" << std::endl;
  detector->Update();


  /* -------------------------------------------------
   * Test the narrow-band version of the algorithm
   */
  std::cout << "Running the narrow-band version" << std::endl;
  detector->NarrowBandingOn();
  detector->SetNarrowBandwidth( 10.0 );
  detector->Update();

  return EXIT_SUCCESS;

}
