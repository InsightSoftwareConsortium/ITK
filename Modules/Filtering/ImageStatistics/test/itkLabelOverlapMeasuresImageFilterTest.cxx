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

#include "itkImageFileReader.h"
#include "itkLabelOverlapMeasuresImageFilter.h"

#include <iomanip>

template <unsigned int VImageDimension>
int LabelOverlapMeasures( int , char * argv[] )
{
  typedef unsigned int                           PixelType;
  typedef itk::Image<PixelType, VImageDimension> ImageType;

  typedef itk::ImageFileReader<ImageType>  ReaderType;
  typename ReaderType::Pointer reader1 = ReaderType::New();
  reader1->SetFileName( argv[2] );
  typename ReaderType::Pointer reader2 = ReaderType::New();
  reader2->SetFileName( argv[3] );

  typedef itk::LabelOverlapMeasuresImageFilter<ImageType> FilterType;
  typename FilterType::Pointer filter = FilterType::New();
  filter->SetSourceImage( reader1->GetOutput() );
  filter->SetTargetImage( reader2->GetOutput() );
  filter->Update();

  std::cout << "                                          "
            << "************ All Labels *************" << std::endl;
  std::cout << std::setw( 10 ) << "   "
            << std::setw( 17 ) << "Total"
            << std::setw( 17 ) << "Union (jaccard)"
            << std::setw( 17 ) << "Mean (dice)"
            << std::setw( 17 ) << "Volume sim."
            << std::setw( 17 ) << "False negative"
            << std::setw( 17 ) << "False positive" << std::endl;
  std::cout << std::setw( 10 ) << "   ";
  std::cout << std::setw( 17 ) << filter->GetTotalOverlap();
  std::cout << std::setw( 17 ) << filter->GetUnionOverlap();
  std::cout << std::setw( 17 ) << filter->GetMeanOverlap();
  std::cout << std::setw( 17 ) << filter->GetVolumeSimilarity();
  std::cout << std::setw( 17 ) << filter->GetFalseNegativeError();
  std::cout << std::setw( 17 ) << filter->GetFalsePositiveError();
  std::cout << std::endl;

  std::cout << "                                       "
            << "************ Individual Labels *************" << std::endl;
  std::cout << std::setw( 10 ) << "Label"
            << std::setw( 17 ) << "Target"
            << std::setw( 17 ) << "Union (jaccard)"
            << std::setw( 17 ) << "Mean (dice)"
            << std::setw( 17 ) << "Volume sim."
            << std::setw( 17 ) << "False negative"
            << std::setw( 17 ) << "False positive" << std::endl;

  typename FilterType::MapType labelMap = filter->GetLabelSetMeasures();
  typename FilterType::MapType::const_iterator it;
  for( it = labelMap.begin(); it != labelMap.end(); ++it )
    {
    if( (*it).first == 0 )
      {
      continue;
      }

    int label = (*it).first;

    std::cout << std::setw( 10 ) << label;
    std::cout << std::setw( 17 ) << filter->GetTargetOverlap( label );
    std::cout << std::setw( 17 ) << filter->GetUnionOverlap( label );
    std::cout << std::setw( 17 ) << filter->GetMeanOverlap( label );
    std::cout << std::setw( 17 ) << filter->GetVolumeSimilarity( label );
    std::cout << std::setw( 17 ) << filter->GetFalseNegativeError( label );
    std::cout << std::setw( 17 ) << filter->GetFalsePositiveError( label );
    std::cout << std::endl;
    }

  return EXIT_SUCCESS;
}

int itkLabelOverlapMeasuresImageFilterTest( int argc, char *argv[] )
{
  if( argc < 4 )
    {
    std::cerr << "Usage: " << argv[0] << " imageDimension sourceImage "
              << "targetImage" << std::endl;
    return EXIT_FAILURE;
    }

  switch( atoi( argv[1] ) )
    {
    case 2:
      LabelOverlapMeasures<2>( argc, argv );
      break;
    case 3:
      LabelOverlapMeasures<3>( argc, argv );
      break;
    default:
      std::cerr << "Unsupported dimension" << std::endl;
      return EXIT_FAILURE;
    }
  return EXIT_SUCCESS;
}
