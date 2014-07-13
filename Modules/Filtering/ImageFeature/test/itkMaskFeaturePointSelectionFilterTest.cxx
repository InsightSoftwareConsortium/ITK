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

#include <ctime>
#include <iostream>
#include "itkIndex.h"
#include "itkImage.h"
#include "itkImageRegionIterator.h"
#include "itkImageFileWriter.h"
#include "itkMaskFeaturePointSelectionFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkRGBPixel.h"


int itkMaskFeaturePointSelectionFilterTest( int argc, char * argv[] )
{
  if( argc < 2 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << " itkMaskFeaturePointSelectionFilterTest inputImageFile outputImageFile [Mask File] ";
    return EXIT_FAILURE;
    }

  typedef unsigned char                  InputPixelType;
  typedef itk::RGBPixel<InputPixelType>  OutputPixelType;

  typedef itk::Image< InputPixelType,  3 > InputImageType;
  typedef itk::Image< OutputPixelType, 3 > OutputImageType;

  typedef itk::Matrix< itk::SpacePrecisionType, 3, 3 > PointSetPixelType;
  typedef itk::PointSet< PointSetPixelType, 3 >        PointSetType;

  typedef itk::ImageFileReader< InputImageType >  ReaderType;

  typedef itk::MaskFeaturePointSelectionFilter< InputImageType, InputImageType, PointSetType >  FilterType;

  //Set up the reader
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  // Set up filter
  FilterType::Pointer filter = FilterType::New();

  filter->SetInput( reader->GetOutput() );

  filter->SetSelectFraction( 0.01 );
  filter->ComputeStructureTensorsOff();

  std::cout << "Filter: " << filter << std::endl;

  try
    {
      filter->Update();
    }
  catch ( itk::ExceptionObject &err )
    {
      std::cerr << err << std::endl;
      return EXIT_FAILURE;
    }

  //Set up the writer
  typedef itk::ImageFileWriter< OutputImageType >  WriterType;
  WriterType::Pointer writer = WriterType::New();

  typedef itk::ImageRegionConstIterator< InputImageType >         InputIteratorType;
  InputIteratorType inputIterator( reader->GetOutput(), reader->GetOutput()->GetBufferedRegion() );
  typedef itk::ImageRegionIterator< OutputImageType > OutputIteratorType;

  OutputImageType::Pointer outputImage = OutputImageType::New();
  outputImage->CopyInformation( reader->GetOutput() );
  outputImage->SetBufferedRegion( reader->GetOutput()->GetBufferedRegion() );
  outputImage->SetRequestedRegion( reader->GetOutput()->GetRequestedRegion() );
  outputImage->Allocate();

  OutputIteratorType outputIterator( outputImage, outputImage->GetBufferedRegion() );
  inputIterator.GoToBegin();
  outputIterator.GoToBegin();

  // Copy input image to output image
  while (!outputIterator.IsAtEnd())
    {
    OutputPixelType rgbPixel;
    rgbPixel.SetRed( inputIterator.Get() );
    rgbPixel.SetGreen( inputIterator.Get());
    rgbPixel.SetBlue( inputIterator.Get() );
    outputIterator.Set( rgbPixel );
    ++outputIterator;
    ++inputIterator;
    }

  //Highlight the feature points identified in the output image
  typedef PointSetType::PointsContainer::ConstIterator PointIteratorType;

  PointIteratorType pointItr =
          filter->GetOutput()->GetPoints()->Begin();
  PointIteratorType pointEnd =
          filter->GetOutput()->GetPoints()->End();

  OutputImageType::IndexType index;

  //Highlight the feature point in red color
  OutputPixelType colorValue;
  colorValue.SetRed( 255u );
  colorValue.SetGreen( 0u );
  colorValue.SetBlue(  0u );

  while ( pointItr != pointEnd )
    {
    if ( outputImage->TransformPhysicalPointToIndex(pointItr.Value(), index) )
      {
      outputImage->SetPixel(index, colorValue);
      }
    pointItr++;
    }
  writer->SetFileName( argv[2] );
  writer->SetInput( outputImage );
  writer->Update();

  return EXIT_SUCCESS;
}
