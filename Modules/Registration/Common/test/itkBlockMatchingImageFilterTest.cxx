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


  // Parameters used for FS and BM
  itk::Size< 3 > blockRadius;
  blockRadius.Fill( 2 );

  itk::Size< 3 > searchRadius;
  searchRadius.Fill( 7 );

  double selectFraction = 0.01;

  typedef unsigned char                  InputPixelType;
  typedef itk::RGBPixel<InputPixelType>  OutputPixelType;

  typedef itk::Image< InputPixelType,  3 >       InputImageType;
  typedef itk::Image< OutputPixelType, 3 >       OutputImageType;

  typedef itk::ImageFileReader< InputImageType >  ReaderType;

  typedef itk::MaskFeaturePointSelectionFilter< InputImageType >  FilterType;
  typedef FilterType::FeaturePointsType                           PointSetType;

  typedef FilterType::PointType       PointType;
  typedef FilterType::InputImageType  ImageType;

  //Set up the reader
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );
  reader->Update();

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

  // Feature Selection
  FilterType::Pointer filter = FilterType::New();

  filter->SetInput( regionOfInterestFilter->GetOutput() );
  filter->SetSelectFraction( selectFraction );
  filter->SetBlockRadius( blockRadius );
  filter->ComputeStructureTensorsOff();

  // Create transformed image from input to match with
  typedef itk::TranslationTransform< double, 3> TranslationTransformType;
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

  resampleFilter->Update();

  // at this time we have feature points
  typedef itk::BlockMatchingImageFilter< InputImageType >  BMFilterType;

  BMFilterType::Pointer BMFilter = BMFilterType::New();

  // inputs (all required)
  BMFilter->SetFixedImage( resampleFilter->GetOutput() );
  BMFilter->SetMovingImage( reader->GetOutput() );
  BMFilter->SetFeaturePoints( filter->GetOutput() );

  // parameters (all optional)
  BMFilter->SetBlockRadius( blockRadius );
  BMFilter->SetSearchRadius( searchRadius );

  std::cout << "Block matching: " << BMFilter << std::endl;
  try
    {
    BMFilter->Update();
    }
  catch ( itk::ExceptionObject &err )
    {
    std::cerr << err << std::endl;
    return EXIT_FAILURE;
    }


  // create RGB copy of input image
  typedef itk::ScalarToRGBColormapImageFilter< InputImageType, OutputImageType >  RGBFilterType;
  RGBFilterType::Pointer colormapImageFilter = RGBFilterType::New();

  colormapImageFilter->SetColormap( RGBFilterType::Grey );
  colormapImageFilter->SetInput( reader->GetOutput() );
  colormapImageFilter->Update();

  OutputImageType::Pointer outputImage = colormapImageFilter->GetOutput();

  //Highlight the feature points identified in the output image
  typedef PointSetType::PointsContainer::ConstIterator                        PointIteratorType;
  typedef BMFilterType::DisplacementsType::PointDataContainer::ConstIterator  PointDataIteratorType;

  PointIteratorType pointItr = filter->GetOutput()->GetPoints()->Begin();
  PointIteratorType pointEnd = filter->GetOutput()->GetPoints()->End();
  PointDataIteratorType displItr = BMFilter->GetOutput()->GetPointData()->Begin();

  // define colors
  OutputPixelType red;
  red.SetRed( 255.0 );
  red.SetGreen( 0.0 );
  red.SetBlue( 0.0 );

  OutputPixelType green;
  green.SetRed( 0.0 );
  green.SetGreen( 255.0 );
  green.SetBlue( 0.0 );

  OutputPixelType blue;
  blue.SetRed( 0.0 );
  blue.SetGreen( 0.0 );
  blue.SetBlue( 255.0 );

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
  writer->Update();


  return EXIT_SUCCESS;
}
