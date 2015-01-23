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

#include "itkLevelSetNeighborhoodExtractor.h"
#include "itkFastMarchingImageFilter.h"

int itkLevelSetNeighborhoodExtractorTest(int, char* [] )
{
  const unsigned int ImageDimension = 2;
  typedef float                                PixelType;
  typedef itk::Image<PixelType,ImageDimension> ImageType;

  // Create an input image using fastmarching
  typedef itk::FastMarchingImageFilter<ImageType> SourceType;
  SourceType::Pointer source = SourceType::New();

  ImageType::SizeType size;
  size.Fill( 17 );

  source->SetOutputSize( size );

  SourceType::NodeType node;
  ImageType::IndexType index;
  index.Fill( 8 );

  node.SetIndex( index );
  node.SetValue( -4.0 );

  typedef SourceType::NodeContainer NodeContainerType;
  NodeContainerType::Pointer container = NodeContainerType::New();

  container->InsertElement( 0, node );

  source->SetTrialPoints( container );
  source->CollectPointsOn();
  source->Update();

  typedef itk::LevelSetNeighborhoodExtractor<ImageType> ExtractorType;
  ExtractorType::Pointer extractor = ExtractorType::New();

  extractor->SetInputLevelSet( source->GetOutput() );
  extractor->SetLevelSetValue( 0.0 );
  extractor->NarrowBandingOff();

  extractor->Locate();

  typedef NodeContainerType::ConstIterator Iterator;
  Iterator iter;
  Iterator iterEnd;

  std::cout << "Inside Points" << std::endl;
  iter    = extractor->GetInsidePoints()->Begin();
  iterEnd = extractor->GetInsidePoints()->End();
  for(; iter != iterEnd; iter++ )
    {
    std::cout << iter.Value().GetIndex() << " ";
    std::cout << iter.Value().GetValue() << std::endl;
    }

  std::cout << "Outside Points" << std::endl;
  iter    = extractor->GetOutsidePoints()->Begin();
  iterEnd = extractor->GetOutsidePoints()->End();
  for(; iter != iterEnd; iter++ )
    {
    std::cout << iter.Value().GetIndex() << " ";
    std::cout << iter.Value().GetValue() << std::endl;
    }

  // exercise Print
  extractor->Print( std::cout );

  // exercise Get methods
  std::cout << "InputLevelSet: " << extractor->GetInputLevelSet() << std::endl;
  std::cout << "LevelSetValue: " << extractor->GetLevelSetValue() << std::endl;
  std::cout << "NarrowBandwidth: " << extractor->GetNarrowBandwidth() << std::endl;
  std::cout << "NarrowBanding: " << extractor->GetNarrowBanding() << std::endl;
  std::cout << "InputNarrowBand: " << extractor->GetInputNarrowBand() << std::endl;

  // exercise error handling
  bool passed;
  std::cout << "Testing ITK_NULLPTR inputs" << std::endl;

  try
    {
    passed = false;
    extractor->SetInputLevelSet( ITK_NULLPTR );
    extractor->Locate();
    }
  catch( itk::ExceptionObject& err )
    {
    std::cout << err << std::endl;
    passed = true;
    extractor->SetInputLevelSet( source->GetOutput() );
    }

  if ( !passed )
    {
    std::cout << "Test failed" << std::endl;
    return EXIT_FAILURE;
    }

  try
    {
    passed = false;
    extractor->NarrowBandingOn();
    extractor->SetInputNarrowBand( ITK_NULLPTR );
    extractor->Locate();
    }
  catch( itk::ExceptionObject& err )
    {
    std::cout << err << std::endl;
    passed = true;
    extractor->NarrowBandingOff();
    }

  if ( !passed )
    {
    std::cout << "Test failed" << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;

}
