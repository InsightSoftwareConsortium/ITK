/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkLevelSetVelocityNeighborhoodExtractorTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "itkLevelSetVelocityNeighborhoodExtractor.h"
#include "itkFastMarchingImageFilter.h"

int itkLevelSetVelocityNeighborhoodExtractorTest(int, char* [] )
{
  const unsigned int ImageDimension = 2;
  typedef float PixelType;
  typedef itk::Image<PixelType,ImageDimension> ImageType;
  typedef double AuxValueType;

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
  
  typedef itk::LevelSetVelocityNeighborhoodExtractor<ImageType,AuxValueType,2> ExtractorType;
  ExtractorType::Pointer extractor = ExtractorType::New();

  extractor->SetInputLevelSet( source->GetOutput() );
  extractor->SetLevelSetValue( 0.0 );
  extractor->NarrowBandingOff();

  // create some dummy auxiliary variable images
  typedef ExtractorType::AuxImageType AuxImageType;
  
  AuxImageType::Pointer aux0 = AuxImageType::New();
  aux0->SetRegions( source->GetOutput()->GetBufferedRegion() );
  aux0->Allocate();
  aux0->FillBuffer( 1.0 );

  extractor->SetAuxImage( aux0, 0 );

  AuxImageType::Pointer aux1 = AuxImageType::New();
  aux1->SetRegions( source->GetOutput()->GetBufferedRegion() );
  aux1->Allocate();
  aux1->FillBuffer( 5.0 );

  extractor->SetAuxImage( aux1, 1 );

  extractor->Locate();

  typedef NodeContainerType::ConstIterator Iterator;
  Iterator iter;
  Iterator iterEnd;

  typedef ExtractorType::AuxValueContainer AuxValueContainer;
  typedef AuxValueContainer::ConstIterator AuxIterator;
  AuxIterator aIter;
  AuxIterator aIterEnd;

  std::cout << "Inside Points" << std::endl;
  iter     = extractor->GetInsidePoints()->Begin();
  iterEnd  = extractor->GetInsidePoints()->End();
  aIter    = extractor->GetAuxInsideValues()->Begin();
  aIterEnd = extractor->GetAuxInsideValues()->End();
  for( ; iter != iterEnd; iter++, aIter++ )
    {
    std::cout << iter.Value().GetIndex() << " ";
    std::cout << iter.Value().GetValue() << " ";
    std::cout << aIter.Value() << std::endl;
    }
  
  std::cout << "Outside Points" << std::endl;
  iter     = extractor->GetOutsidePoints()->Begin();
  iterEnd  = extractor->GetOutsidePoints()->End();
  aIter    = extractor->GetAuxOutsideValues()->Begin();
  aIterEnd = extractor->GetAuxOutsideValues()->End();

  for( ; iter != iterEnd; iter++, aIter++ )
    {
    std::cout << iter.Value().GetIndex() << " ";
    std::cout << iter.Value().GetValue() << " ";
    std::cout << aIter.Value() << std::endl;
    }

  // exercise Print
  extractor->Print( std::cout );

  std::cout << "AuxImage0: " << extractor->GetAuxImage(0).GetPointer() << std::endl;

  // exercise error handling
  extractor->SetAuxImage( aux0, 2 );
  
  if ( extractor->GetAuxImage( 2 ) )
    {
    std::cout << "Out of range index should return NULL pointer" << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;

}
