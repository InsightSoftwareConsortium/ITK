/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFastMarchingUpwindGradientTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif
#include "itkFastMarchingUpwindGradientImageFilter.h"
#include "itkImage.h"
#include "itkImageRegionIterator.h"
#include "itkTextOutput.h"
#include "itkCommand.h"

#include "vnl/vnl_math.h"

namespace{
// The following class is used to support callbacks
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
}

int itkFastMarchingUpwindGradientTest(int, char* [] )
{

  itk::OutputWindow::SetInstance(itk::TextOutput::New().GetPointer());

  // create a fastmarching object
  typedef float PixelType;
  typedef itk::Image<PixelType,2> FloatImage;
  typedef itk::FastMarchingUpwindGradientImageFilter<FloatImage,FloatImage> FloatFMType;

  FloatFMType::Pointer marcher = FloatFMType::New();

  ShowProgressObject progressWatch(marcher);
  itk::SimpleMemberCommand<ShowProgressObject>::Pointer command;
  command = itk::SimpleMemberCommand<ShowProgressObject>::New();
  command->SetCallbackFunction(&progressWatch,
                               &ShowProgressObject::ShowProgress);
  marcher->AddObserver( itk::ProgressEvent(), command);
  
  typedef FloatFMType::NodeType NodeType;
  typedef FloatFMType::NodeContainer NodeContainer;

  // setup alive points
  NodeContainer::Pointer alivePoints = NodeContainer::New();

  NodeType node;

  FloatImage::OffsetType offset0 = {{28,35}};

  itk::Index<2> index;
  index.Fill(0);

  node.SetValue( 0.0 );
  node.SetIndex( index + offset0 );
  alivePoints->InsertElement(0, node);

  node.SetValue( 42.0 );
  index.Fill( 200 );
  node.SetIndex( index ); // this node is out of range
  alivePoints->InsertElement(1, node);
  
  marcher->SetAlivePoints( alivePoints );


  // setup trial points
  NodeContainer::Pointer trialPoints = NodeContainer::New();

  node.SetValue( 1.0 );

  index.Fill(0);
  index += offset0;

  index[0] += 1;
  node.SetIndex( index );
  trialPoints->InsertElement(0, node);

  index[0] -= 1;
  index[1] += 1;
  node.SetIndex( index );
  trialPoints->InsertElement(1, node);

  index[0] -= 1;
  index[1] -= 1;
  node.SetIndex( index );
  trialPoints->InsertElement(2, node);

  index[0] += 1;
  index[1] -= 1;
  node.SetIndex( index );
  trialPoints->InsertElement(3, node);

  node.SetValue( 42.0 );
  index.Fill( 300 ); // this node is out of ranage
  node.SetIndex( index );
  trialPoints->InsertElement(4, node);

  marcher->SetTrialPoints( trialPoints );

  // specify the size of the output image
  FloatImage::SizeType size = {{64,64}};
  marcher->SetOutputSize( size );

  // setup a speed image of ones
  FloatImage::Pointer speedImage = FloatImage::New();
  FloatImage::RegionType region;
  region.SetSize( size );
  speedImage->SetLargestPossibleRegion( region );
  speedImage->SetBufferedRegion( region );
  speedImage->Allocate();

  itk::ImageRegionIterator<FloatImage>
    speedIter( speedImage, speedImage->GetBufferedRegion() );
  for ( ; !speedIter.IsAtEnd(); ++speedIter )
    {
    speedIter.Set( 1.0 );
    }

  speedImage->Print( std::cout );
  marcher->SetInput( speedImage );
  marcher->SetStoppingValue( 100.0 );
  marcher->GenerateGradientImageOn();

  // turn on debugging
  marcher->DebugOn();

  // update the marcher
  marcher->Update();

  // check the results
  typedef FloatFMType::GradientImageType FloatGradientImage;
  typedef FloatGradientImage::PixelType GradientPixelType;
  FloatGradientImage::Pointer gradientOutput = marcher->GetGradientImage();
  itk::ImageRegionIterator<FloatGradientImage>
    iterator( gradientOutput, gradientOutput->GetBufferedRegion() );
 
  bool passed = true;

  for ( ; !iterator.IsAtEnd(); ++iterator )
    {

    FloatGradientImage::IndexType tempIndex;
    double distance;
    GradientPixelType outputPixel;

    tempIndex = iterator.GetIndex();
    tempIndex -= offset0;
    distance = 0.0;
    for ( int j = 0; j < 2; j++ )
      {
        distance += tempIndex[j] * tempIndex[j];
      }
    distance = vcl_sqrt( distance );

    outputPixel = iterator.Get();

    double outputPixelNorm = (double) outputPixel.GetNorm();

    if (distance == 0)
      {
      continue;
      }

    // for test to pass, gradient vectors must have norm = 1
    // (equal to the rhs of the Eikonal equation)
    // and must be oriented radially from the seed point

    double dot = 0.0;
    for ( int j = 0; j < 2; j++ )
      {
      dot += tempIndex[j] / distance * outputPixel[j];
      }
     
    if ( outputPixelNorm < 0.9999 || outputPixelNorm > 1.0001 || 
         dot < 0.99 || dot > 1.01 )
      {
      std::cout << iterator.GetIndex() << " ";
      std::cout << outputPixelNorm << " ";
      std::cout << dot << std::endl;
      passed = false;
      }
    
    }

  // Exercise other member functions
  std::cout << "SpeedConstant: " << marcher->GetSpeedConstant() << std::endl;
  std::cout << "StoppingValue: " << marcher->GetStoppingValue() << std::endl;
  std::cout << "CollectPoints: " << marcher->GetCollectPoints() << std::endl;

  marcher->SetNormalizationFactor( 2.0 );
  std::cout << "NormalizationFactor: " << marcher->GetNormalizationFactor();
  std::cout << std::endl;

  std::cout << "SpeedImage: " << marcher->GetInput();
  std::cout << std::endl;

  //TODO: test target points and reached mode (test if outside inner or outer circle by setting two targets)

  marcher->Print( std::cout );

  if ( passed )
    {
    std::cout << "Fast Marching Upwind Gradient test passed" << std::endl;
    return EXIT_SUCCESS;
    }
  else
    {
    std::cout << "Fast Marching Upwind Gradient test failed" << std::endl;
    return EXIT_FAILURE;
    }

}
