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

// Ensure we do not get NaN's with a constant image

#include "itkScalarImageToCooccurrenceMatrixFilter.h"
#include "itkHistogramToTextureFeaturesFilter.h"
#include "itkMath.h"

int itkHistogramToTextureFeaturesFilterNaNTest( int, char * [])
{
  const unsigned int Dimension = 2;
  typedef unsigned char                      PixelType;
  typedef itk::Image< PixelType, Dimension > ImageType;

  // Build a constant image
  ImageType::Pointer image = ImageType::New();
  ImageType::RegionType region;
  ImageType::SizeType size;
  size.Fill( 256 );
  region.SetSize( size );
  image->SetRegions( region );
  image->Allocate();
  image->FillBuffer( 128 );

  // Generate co-occurence matrix
  typedef itk::Statistics::ScalarImageToCooccurrenceMatrixFilter< ImageType > MatrixGeneratorType;
  MatrixGeneratorType::Pointer generator = MatrixGeneratorType::New();
  MatrixGeneratorType::OffsetType offset;
  offset.Fill( 1 );
  generator->SetOffset( offset );
  generator->SetInput( image );
  generator->Update();

  typedef itk::Statistics::HistogramToTextureFeaturesFilter< MatrixGeneratorType::HistogramType > TextureFilterType;
  TextureFilterType::Pointer filter = TextureFilterType::New();
  filter->SetInput( generator->GetOutput() );
  filter->Update();

  TextureFilterType::MeasurementType correlation = filter->GetCorrelation();
  std::cout << "Correlation: " << correlation << std::endl;
  if( itk::Math::isnan( correlation ) )
    {
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
