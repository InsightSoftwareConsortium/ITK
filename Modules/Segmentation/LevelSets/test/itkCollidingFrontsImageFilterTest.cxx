/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include "itkCollidingFrontsImageFilter.h"

#include "itkCastImageFilter.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkImageFileWriter.h"

int itkCollidingFrontsImageFilterTest(int argc, char* argv[] )
{

  const   unsigned int    ImageDimension = 2;
  typedef unsigned char   PixelType;
  typedef float           InternalPixelType;

  typedef itk::Image<PixelType,ImageDimension>         ImageType;
  typedef itk::Image<InternalPixelType,ImageDimension> InternalImageType;

  //setup uniform image

  ImageType::SizeType imageSize;
  imageSize[0] = 128;
  imageSize[1] = 128;

  ImageType::RegionType imageRegion;
  imageRegion.SetSize( imageSize );

  PixelType background = 64;

  ImageType::Pointer inputImage = ImageType::New();
  inputImage->SetRegions( imageRegion );
  inputImage->Allocate();
  inputImage->FillBuffer( background );

  typedef itk::CastImageFilter< ImageType, InternalImageType > CastFilterType;
  CastFilterType::Pointer caster = CastFilterType::New();
  caster->SetInput( inputImage );

  typedef itk::CollidingFrontsImageFilter<InternalImageType,InternalImageType> CollidingFrontsFilterType;
  CollidingFrontsFilterType::Pointer collidingFronts = CollidingFrontsFilterType::New();

  typedef CollidingFrontsFilterType::NodeContainer NodeContainer;
  typedef CollidingFrontsFilterType::NodeType      NodeType;

  //select seeds 20 pixels apart

  NodeContainer::Pointer seeds1 = NodeContainer::New();

  InternalImageType::IndexType seedPosition1;
  seedPosition1[0] = 50;
  seedPosition1[1] = 60;

  NodeType node1;
  node1.SetIndex( seedPosition1 );
  node1.SetValue( 0.0 );

  seeds1->Initialize();
  seeds1->InsertElement( 0, node1 );

  NodeContainer::Pointer seeds2 = NodeContainer::New();

  InternalImageType::IndexType seedPosition2;
  seedPosition2[0] = 70;
  seedPosition2[1] = 60;

  NodeType node2;
  node2.SetIndex( seedPosition2 );
  node2.SetValue( 0.0 );

  seeds2->Initialize();
  seeds2->InsertElement( 0, node2 );

  InternalImageType::OffsetType offset = {{60,60}};
  double radius = seedPosition2[0] - offset[0];

  collidingFronts->SetInput( caster->GetOutput() );
  collidingFronts->SetSeedPoints1( seeds1 );
  collidingFronts->SetSeedPoints2( seeds2 );
  collidingFronts->ApplyConnectivityOn();
  try
    {
    collidingFronts->Update();
    }
  catch ( itk::ExceptionObject& err )
    {
    std::cout << err << std::endl;
    }

  InternalImageType::Pointer output = collidingFronts->GetOutput();

  itk::ImageRegionIterator<InternalImageType>
      iterator ( output, output->GetBufferedRegion() );

  bool passed = true;

  for (; !iterator.IsAtEnd(); ++iterator )
    {
    InternalImageType::IndexType tempIndex;
    tempIndex = iterator.GetIndex();
    tempIndex -= offset;
    double distance = 0.0;
    for ( int j = 0; j < 2; j++ )
      {
      distance += tempIndex[j] * tempIndex[j];
      }
    distance = std::sqrt( distance );
    InternalImageType::PixelType outputPixel = iterator.Get();

    // for test to pass, the circle of radius 10 centered in offset
    // must be made up only of negative pixels and vice-versa

    if (outputPixel < 0.0)
      {
      //allow half a pixel diagonal tolerance
      if (distance > radius + 1.414 / 2.0)
        {
        std::cout<<outputPixel<<" "<<distance<<std::endl;
        passed = false;
        }
      }
    else
      {
       if (distance < radius)
        {
        std::cout<<outputPixel<<" "<<distance<<std::endl;
        passed = false;
        }
      }
    }

  // Optionally writing out the two images
  if( argc > 2 )
    {
    typedef itk::ImageFileWriter< ImageType > WriterType;
    WriterType::Pointer writer = WriterType::New();

    typedef itk::RescaleIntensityImageFilter< InternalImageType,
      ImageType > RescaleFilterType;
    RescaleFilterType::Pointer rescaler = RescaleFilterType::New();

    writer->SetFileName( argv[1] );
    writer->SetInput( inputImage );
    writer->Update();

    rescaler->SetInput( collidingFronts->GetOutput() );
    rescaler->SetOutputMinimum( 0 );
    rescaler->SetOutputMaximum( 255 );

    writer->SetFileName( argv[2] );
    writer->SetInput( rescaler->GetOutput() );
    writer->Update();
    }

  if (!passed)
    {
    std::cout << "Colliding Fronts test failed. " << std::endl;
    return EXIT_FAILURE;
    }

  collidingFronts->StopOnTargetsOn();
  try
    {
    collidingFronts->Update();
    }
  catch ( itk::ExceptionObject& err )
    {
    std::cout << err << std::endl;
    }

  std::cout << "Colliding Fronts test passed. " << std::endl;

  typedef itk::Image<double,ImageDimension> DoubleImageType;
  typedef itk::CollidingFrontsImageFilter<DoubleImageType,InternalImageType> CollidingFrontsFilterType2;
  CollidingFrontsFilterType2::Pointer collidingFronts2 = CollidingFrontsFilterType2::New();

  return EXIT_SUCCESS;

}
