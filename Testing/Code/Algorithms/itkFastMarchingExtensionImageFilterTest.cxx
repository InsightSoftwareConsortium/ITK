/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFastMarchingExtensionImageFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkFastMarchingExtensionImageFilter.h"
#include "itkImage.h"
#include "itkImageRegionIterator.h"
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

int itkFastMarchingExtensionImageFilterTest(int, char* [] )
{
  // create a fastmarching object
  typedef float PixelType;
  typedef itk::Image<PixelType,2> FloatImage;
  typedef itk::FastMarchingExtensionImageFilter<FloatImage,unsigned char,1,
    FloatImage> MarcherType;

  MarcherType::Pointer marcher = MarcherType::New();

  ShowProgressObject progressWatch(marcher);
  itk::SimpleMemberCommand<ShowProgressObject>::Pointer command;
  command = itk::SimpleMemberCommand<ShowProgressObject>::New();
  command->SetCallbackFunction(&progressWatch,
                               &ShowProgressObject::ShowProgress);
  marcher->AddObserver( itk::ProgressEvent(), command);


  bool passed;
  
  // setup trial points
  typedef MarcherType::NodeType NodeType;
  typedef MarcherType::NodeContainer NodeContainer;

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

  marcher->SetInput( speedImage );
  marcher->SetStoppingValue( 100.0 );

  // deliberately cause an exception by not setting AuxAliveValues
  passed = false;
  try
    {
    marcher->Update();
    }
  catch ( itk::ExceptionObject & err )
    {
    passed = true;
    marcher->ResetPipeline();
    std::cout << err << std::endl;
    }
  if ( !passed ) { return EXIT_FAILURE; }

  typedef MarcherType::AuxValueVectorType VectorType;
  typedef MarcherType::AuxValueContainer AuxValueContainer;

  AuxValueContainer::Pointer auxAliveValues = AuxValueContainer::New();

  // deliberately cause an exception setting AuxAliveValues of the wrong size
  marcher->SetAuxiliaryAliveValues( auxAliveValues );

  passed = false;
  try
    {
    marcher->Update();
    }
  catch ( itk::ExceptionObject & err )
    {
    passed = true;
    marcher->ResetPipeline();
    std::cout << err << std::endl;
    }
  if ( !passed ) { return EXIT_FAILURE; }


  VectorType vector;
  vector[0] = 48;

  auxAliveValues->InsertElement(0,vector);
  auxAliveValues->InsertElement(1,vector);

  // deliberately cause an exception by not setting AuxTrialValues
  passed = false;
  try
    {
    marcher->Update();
    }
  catch ( itk::ExceptionObject & err )
    {
    passed = true;
    marcher->ResetPipeline();
    std::cout << err << std::endl;
    }
  if ( !passed ) { return EXIT_FAILURE; }

  AuxValueContainer::Pointer auxTrialValues = AuxValueContainer::New();

  // deliberately cause an exception setting AuxTrialValues of the wrong size
  marcher->SetAuxiliaryTrialValues( auxTrialValues );

  passed = false;
  try
    {
    marcher->Update();
    }
  catch ( itk::ExceptionObject & err )
    {
    passed = true;
    marcher->ResetPipeline();
    std::cout << err << std::endl;
    }
  if ( !passed ) { return EXIT_FAILURE; }


  auxTrialValues->InsertElement(0,vector);
  auxTrialValues->InsertElement(1,vector);
  auxTrialValues->InsertElement(2,vector);
  auxTrialValues->InsertElement(3,vector);
  auxTrialValues->InsertElement(4,vector);
 

  // run the algorithm
  passed = true;
  try
    {
    marcher->Update();
    }
  catch ( itk::ExceptionObject & err )
    {
    passed = false;
    marcher->ResetPipeline();
    std::cout << err << std::endl;
    }
  if ( !passed ) { return EXIT_FAILURE; }


  // check the results
  passed = true;
  FloatImage::Pointer output = marcher->GetOutput();
  itk::ImageRegionIterator<FloatImage>
    iterator( output, output->GetBufferedRegion() );

  typedef MarcherType::AuxImageType AuxImageType;
  AuxImageType::Pointer auxImage = marcher->GetAuxiliaryImage(0);
  itk::ImageRegionIterator<AuxImageType>
    auxIterator( auxImage, auxImage->GetBufferedRegion() );


  for ( ; !iterator.IsAtEnd(); ++iterator, ++auxIterator )
    {

    FloatImage::IndexType tempIndex;
    double distance;
    float output;

    tempIndex = iterator.GetIndex();
    tempIndex -= offset0;
    distance = 0.0;
    for ( int j = 0; j < 2; j++ )
      {
      distance += tempIndex[j] * tempIndex[j];
      }
    distance = vcl_sqrt( distance );

    output = (float) iterator.Get();

    if (distance == 0)
      {
      continue;
      }
    if ( vnl_math_abs( output ) / distance > 1.42 )
      {
      std::cout << iterator.GetIndex() << " ";
      std::cout << vnl_math_abs( output ) / distance << " ";
      std::cout << vnl_math_abs( output ) << " " << distance << std::endl;
      passed = false;
      break;
      }

    if ( auxIterator.Get() != vector[0] )
      {
      std::cout << auxIterator.GetIndex()
                << " got aux value of " << (double) auxIterator.Get()
                << " but it should be  " << (double) vector[0]
                << std::endl;
      passed = false;
      break;
      }
    
    }

  // Exercise other member functions
  std::cout << "Auxiliary alive values: " << marcher->GetAuxiliaryAliveValues();
  std::cout << std::endl;

  std::cout << "Auxiliary trial values: " << marcher->GetAuxiliaryTrialValues();
  std::cout << std::endl;

  marcher->Print( std::cout );

  if ( marcher->GetAuxiliaryImage(2) )
    {
    std::cout << "GetAuxiliaryImage(2) should have returned NULL";
    std::cout << std::endl;
    passed = false;
    } 

  if ( passed )
    {
    std::cout << "Fast Marching test passed" << std::endl;
    return EXIT_SUCCESS;
    }
  else
    {
    std::cout << "Fast Marching test failed" << std::endl;
    return EXIT_FAILURE;
    }


return EXIT_SUCCESS;

}

