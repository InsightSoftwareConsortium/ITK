/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkFastMarchingTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


Copyright (c) 2000 National Library of Medicine
All rights reserved.

See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "itkFastMarchingImageFilter.h"
#include "itkImage.h"
#include "itkScalar.h"
#include "itkImageRegionIterator.h"
#include "itkPixelTraits.h"

#include "vnl/vnl_math.h"

int main()
{
  // create a fastmarching object
  typedef itk::Scalar<float> PixelType;
  typedef itk::Image<PixelType,2> FloatImage;
  typedef itk::FastMarchingImageFilter<FloatImage> FloatFMType;

  FloatFMType::Pointer marcher = FloatFMType::New();
  
  // setup trial points
  typedef FloatFMType::NodeType NodeType;
  typedef FloatFMType::NodeContainer NodeContainer;

  NodeContainer::Pointer trialPoints = NodeContainer::New();

  NodeType node;

  FloatImage::OffsetType offset0 = {{28,35}};
  
  node.value = 0.0;
  node.index += offset0;
  node.index = itk::Index<2>::ZeroIndex + offset0;
  trialPoints->InsertElement(0, node);
  
  marcher->SetTrialPoints( trialPoints );

  // specify the size of the output image
  FloatImage::SizeType size = {{64,64}};
  marcher->SetOutputSize( size );

  // update the marcher
  marcher->Update();

  // check the results
  FloatImage::Pointer output = marcher->GetOutput();
  itk::ImageRegionIterator<FloatImage>
    iterator( output, output->GetBufferedRegion() );

  iterator = iterator.Begin();
  bool passed = true;

  for( ; !iterator.IsAtEnd(); ++iterator )
    {

    FloatImage::IndexType tempIndex;
    double distance;
    float output;

    tempIndex = iterator.GetIndex();
    tempIndex -= offset0;
    distance = 0.0;
    for( int j = 0; j < 2; j++ )
      {
        distance += tempIndex[j] * tempIndex[j];
      }
    distance = vnl_math_sqrt( distance );

    output = itk::ScalarTraits<PixelType>::GetScalar( iterator.Get() );

    if( vnl_math_abs( output ) / distance > 1.42 )
      {
        std::cout << iterator.GetIndex() << " ";
        std::cout << vnl_math_abs( output ) / distance << " ";
        std::cout << vnl_math_abs( output ) << " " << distance << std::endl;
        passed = false;
      }
    
    }

  if( passed )
    {
    std::cout << "Fast Marching test passed" << std::endl;
    return EXIT_SUCCESS;
    }
  else
    {
    std::cout << "Fast Marching test failed" << std::endl;
    return EXIT_FAILURE;
    }

}
