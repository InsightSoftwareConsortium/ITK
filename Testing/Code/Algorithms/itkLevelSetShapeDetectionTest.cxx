/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLevelSetShapeDetectionTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkFastMarchingImageFilter.h"
#include "itkShapeDetectionLevelSetFilter.h"
#include "itkImage.h"
#include "itkImageRegionIterator.h"
#include "itkCommand.h"

// The following classe is used to support callbacks
// on the filter in the pipeline that follows later
class ShowProgressObject
{
public:
  ShowProgressObject(itk::ProcessObject* o)
    {m_Process = o;}
  void ShowProgress()
    {std::cout << "Progress " << m_Process->GetProgress() << std::endl;}
  itk::ProcessObject::Pointer m_Process;
};


int main()
{

  /* -------------------------------------------------
   * Create an initial level set of dimension 64x64
   * with the zero set being a circle of radius 8
   * centered at (28,35).
   */

  // create a fastmarching object
  typedef itk::Image<float,2> FloatImage;
  typedef itk::FastMarchingImageFilter<FloatImage> FastMarcherType;

  FastMarcherType::Pointer marcher = FastMarcherType::New();
  
  // setup trial points
  typedef FastMarcherType::NodeType NodeType;
  typedef FastMarcherType::NodeContainer NodeContainer;

  NodeContainer::Pointer trialPoints = NodeContainer::New();

  NodeType node;

  FloatImage::IndexType index0 = {{28,35}};
  
  node.SetValue( 0.0 );
  node.SetIndex( index0 );
  trialPoints->InsertElement(0, node);
  
  marcher->SetTrialPoints( trialPoints );

  // specify the size of the output image
  FloatImage::SizeType size = {{64,64}};
  marcher->SetOutputSize( size );

  // update the marcher
  marcher->Update();

  // walk the marcher output
  FloatImage::Pointer levelSet = marcher->GetOutput();
  itk::ImageRegionIterator<FloatImage>
    iterator( levelSet, levelSet->GetBufferedRegion() );

  for( ; !iterator.IsAtEnd(); ++iterator )
    {
    iterator.Set( iterator.Get() - 8.0 );
    }


  /* -------------------------------------------------
   * Create a edge potential image with all ones
   */
  FloatImage::Pointer edgeImg = FloatImage::New();
  edgeImg->CopyInformation( levelSet );
  edgeImg->SetBufferedRegion(
    levelSet->GetBufferedRegion() );
  edgeImg->Allocate();

  itk::ImageRegionIterator<FloatImage>
    edgeIter( edgeImg, edgeImg->GetBufferedRegion() );

  for(; !edgeIter.IsAtEnd(); ++edgeIter )
    {
    edgeIter.Set( 1.0 );
    }


  /* -----------------------------------------------
   * Create a shape detection object
   */

  typedef itk::ShapeDetectionLevelSetFilter<FloatImage,FloatImage>
    ShapeDetectorType;

  ShapeDetectorType::Pointer detector = ShapeDetectorType::New();

  // create progress observer
  ShowProgressObject progressWatch( detector );
  itk::SimpleMemberCommand<ShowProgressObject>::Pointer command;
  command = itk::SimpleMemberCommand<ShowProgressObject>::New();
  command->SetCallbackFunction(&progressWatch,
                               &ShowProgressObject::ShowProgress);
  detector->AddObserver( itk::ProgressEvent(), command);
  

  detector->SetInput( levelSet );
  detector->SetEdgeImage( edgeImg );
  detector->SetNumberOfIterations( 10 );

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

  // Exercise various member variables
  detector->Print( std::cout );

  return EXIT_SUCCESS;

}
