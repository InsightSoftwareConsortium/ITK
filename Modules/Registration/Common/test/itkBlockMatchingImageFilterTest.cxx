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

#include <iostream>
#include "itkIndex.h"
#include "itkImage.h"
#include "itkRGBPixel.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkImageRegionIterator.h"
#include "itkImageDuplicator.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkLineIterator.h"
#include "itkMultiThreader.h"
#include "itkRegionOfInterestImageFilter.h"
#include "itkMaskFeaturePointSelectionFilter.h"
#include "itkBlockMatchingImageFilter.h"
#include "itkScalarToRGBColormapImageFilter.h"
#include "itkTranslationTransform.h"
#include "itkResampleImageFilter.h"


int itkBlockMatchingImageFilterTest( int argc, char * argv[] )
{
  if( argc < 2 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << " itkitkBlockMatchingImageFilterTest inputImageFile outputImageFile [Mask File]" << std::endl;
    return EXIT_FAILURE;
    }

  const double selectFraction = 0.01;

  typedef unsigned char                  InputPixelType;
  typedef itk::RGBPixel<InputPixelType>  OutputPixelType;
  static ITK_CONSTEXPR_VAR unsigned int Dimension = 3;

  typedef itk::Image< InputPixelType,  Dimension >  InputImageType;
  typedef itk::Image< OutputPixelType, Dimension >  OutputImageType;

  // Parameters used for FS and BM
  typedef InputImageType::SizeType RadiusType;
  RadiusType blockRadius;
  blockRadius.Fill( 2 );

  RadiusType searchRadius;
  searchRadius.Fill( 7 );

  typedef itk::ImageFileReader< InputImageType >  ReaderType;

  //Set up the reader
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );
  try
    {
    reader->Update();
    }
  catch( itk::ExceptionObject & e )
    {
    std::cerr << "Error in reading the input image: " << e << std::endl;
    return EXIT_FAILURE;
    }

  // Reduce region of interest by SEARCH_RADIUS
  typedef itk::RegionOfInterestImageFilter< InputImageType, InputImageType >  RegionOfInterestFilterType;

  RegionOfInterestFilterType::Pointer regionOfInterestFilter = RegionOfInterestFilterType::New();

  regionOfInterestFilter->SetInput( reader->GetOutput() );

  RegionOfInterestFilterType::RegionType regionOfInterest = reader->GetOutput()->GetLargestPossibleRegion();

  RegionOfInterestFilterType::RegionType::IndexType regionOfInterestIndex = regionOfInterest.GetIndex();
  regionOfInterestIndex += searchRadius;
  regionOfInterest.SetIndex( regionOfInterestIndex );

  RegionOfInterestFilterType::RegionType::SizeType regionOfInterestSize = regionOfInterest.GetSize();
  regionOfInterestSize -= searchRadius + searchRadius;
  regionOfInterest.SetSize( regionOfInterestSize );

  regionOfInterestFilter->SetRegionOfInterest( regionOfInterest );
  regionOfInterestFilter->Update();

  typedef itk::MaskFeaturePointSelectionFilter< InputImageType >  FeatureSelectionFilterType;
  typedef FeatureSelectionFilterType::FeaturePointsType           PointSetType;

  // Feature Selection
  FeatureSelectionFilterType::Pointer featureSelectionFilter = FeatureSelectionFilterType::New();

  featureSelectionFilter->SetInput( regionOfInterestFilter->GetOutput() );
  featureSelectionFilter->SetSelectFraction( selectFraction );
  featureSelectionFilter->SetBlockRadius( blockRadius );
  featureSelectionFilter->ComputeStructureTensorsOff();

  // Create transformed image from input to match with
  typedef itk::TranslationTransform< double, Dimension > TranslationTransformType;
  TranslationTransformType::Pointer transform = TranslationTransformType::New();
  TranslationTransformType::OutputVectorType translation;
  // move each pixel in input image 5 pixels along first(0) dimension
  translation[0] = 20.0;
  translation[1] = 0.0;
  translation[2] = 0.0;
  transform->Translate(translation);

  typedef itk::ResampleImageFilter< InputImageType, InputImageType > ResampleImageFilterType;
  ResampleImageFilterType::Pointer resampleFilter = ResampleImageFilterType::New();
  resampleFilter->SetTransform( transform.GetPointer() );
  resampleFilter->SetInput( reader->GetOutput() );
  resampleFilter->SetReferenceImage( reader->GetOutput() );
  resampleFilter->UseReferenceImageOn();

  typedef itk::BlockMatchingImageFilter< InputImageType >  BlockMatchingFilterType;
  BlockMatchingFilterType::Pointer blockMatchingFilter = BlockMatchingFilterType::New();

  // inputs (all required)
  blockMatchingFilter->SetFixedImage( resampleFilter->GetOutput() );
  blockMatchingFilter->SetMovingImage( reader->GetOutput() );
  blockMatchingFilter->SetFeaturePoints( featureSelectionFilter->GetOutput() );

  // parameters (all optional)
  blockMatchingFilter->SetBlockRadius( blockRadius );
  blockMatchingFilter->SetSearchRadius( searchRadius );

  std::cout << "Block matching: " << blockMatchingFilter << std::endl;
  try
    {
    blockMatchingFilter->Update();
    }
  catch ( itk::ExceptionObject &err )
    {
    std::cerr << err << std::endl;
    return EXIT_FAILURE;
    }

  // Exercise the following methods
  BlockMatchingFilterType::DisplacementsType * displacements = blockMatchingFilter->GetDisplacements();
  if( displacements == ITK_NULLPTR )
    {
    std::cerr << "GetDisplacements() failed." << std::endl;
    return EXIT_FAILURE;
    }
  BlockMatchingFilterType::SimilaritiesType * similarities = blockMatchingFilter->GetSimilarities();
  if( similarities == ITK_NULLPTR )
    {
    std::cerr << "GetSimilarities() failed." << std::endl;
    return EXIT_FAILURE;
    }

  // create RGB copy of input image
  typedef itk::ScalarToRGBColormapImageFilter< InputImageType, OutputImageType >  RGBFilterType;
  RGBFilterType::Pointer colormapImageFilter = RGBFilterType::New();

  colormapImageFilter->SetColormap( RGBFilterType::Grey );
  colormapImageFilter->SetInput( reader->GetOutput() );
  try
    {
    colormapImageFilter->Update();
    }
  catch ( itk::ExceptionObject &err )
    {
    std::cerr << err << std::endl;
    return EXIT_FAILURE;
    }

  OutputImageType::Pointer outputImage = colormapImageFilter->GetOutput();

  // Highlight the feature points identified in the output image
  typedef PointSetType::PointsContainer::ConstIterator                                   PointIteratorType;
  typedef BlockMatchingFilterType::DisplacementsType::PointDataContainer::ConstIterator  PointDataIteratorType;

  PointIteratorType pointItr = featureSelectionFilter->GetOutput()->GetPoints()->Begin();
  PointIteratorType pointEnd = featureSelectionFilter->GetOutput()->GetPoints()->End();
  PointDataIteratorType displItr = displacements->GetPointData()->Begin();

  // define colors
  OutputPixelType red;
  red.SetRed( 255 );
  red.SetGreen( 0 );
  red.SetBlue( 0 );

  OutputPixelType green;
  green.SetRed( 0 );
  green.SetGreen( 255 );
  green.SetBlue( 0 );

  OutputPixelType blue;
  blue.SetRed( 0 );
  blue.SetGreen( 0 );
  blue.SetBlue( 255 );

  OutputImageType::IndexType index;
  while ( pointItr != pointEnd )
    {
    if ( outputImage->TransformPhysicalPointToIndex(pointItr.Value(), index) )
      {
      OutputImageType::IndexType displ;
      outputImage->TransformPhysicalPointToIndex( pointItr.Value() + displItr.Value(), displ );

      // draw line between old and new location of a point in blue
      itk::LineIterator< OutputImageType > lineIter( outputImage, index, displ );
      for ( lineIter.GoToBegin(); !lineIter.IsAtEnd(); ++lineIter )
        {
        lineIter.Set( blue );
        }

      // mark old location of a point in green
      outputImage->SetPixel(index, green);

      // mark new location of a point in red
      outputImage->SetPixel(displ, red);
      }
    pointItr++;
    displItr++;
    }

  //Set up the writer
  typedef itk::ImageFileWriter< OutputImageType >  WriterType;
  WriterType::Pointer writer = WriterType::New();

  writer->SetFileName( argv[2] );
  writer->SetInput( outputImage );
  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & e )
    {
    std::cerr << "Error in writing the output image:" << e << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
