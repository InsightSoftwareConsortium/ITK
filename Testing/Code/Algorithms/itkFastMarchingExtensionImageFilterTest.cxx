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

#include "vnl/vnl_math.h"

int itkFastMarchingExtensionImageFilterTest(int, char* [] )
{
  // create a fastmarching object
  typedef float PixelType;
  typedef itk::Image<PixelType,2> FloatImage;
  typedef itk::FastMarchingExtensionImageFilter<FloatImage,unsigned char,1,
    FloatImage> MarcherType;

  MarcherType::Pointer marcher = MarcherType::New();
  bool passed;
  
  // setup trial points
  typedef MarcherType::NodeType NodeType;
  typedef MarcherType::NodeContainer NodeContainer;

  NodeContainer::Pointer trialPoints = NodeContainer::New();

  NodeType node;

  FloatImage::OffsetType offset0 = {{28,35}};
  itk::Index<2> index;
  index.Fill(0);

  node.SetValue( 0.0 );
  node.SetIndex( index + offset0 );
  trialPoints->InsertElement(0, node);
  
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

  typedef MarcherType::AuxValueVectorType VectorType;
  typedef MarcherType::AuxValueContainer AuxValueContainer;

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


  VectorType vector;
  vector[0] = 48;

  auxTrialValues->InsertElement(0,vector);

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

