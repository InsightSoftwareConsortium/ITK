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

#include "itkLabelStatisticsImageFilter.h"
#include "itkImageFileReader.h"

#include "itkFilterWatcher.h"


int itkLabelStatisticsImageFilterTest(int argc, char* argv [] )
{
  std::cout << "itkLabelStatisticsImageFilterTest Start" << std::endl;

  if( argc < 2 )
  {
    std::cerr << "Missing Arguments" << std::endl;
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << " inputImage labeledImage " << std::endl;
    return EXIT_FAILURE;
  }
  typedef itk::Image<unsigned char,2> ImageType;

  typedef itk::ImageFileReader< ImageType >    ReaderType;

  ReaderType::Pointer reader1 = ReaderType::New();
  ReaderType::Pointer reader2 = ReaderType::New();

  reader1->SetFileName( argv[1] );
  reader2->SetFileName( argv[2] );

  typedef itk::LabelStatisticsImageFilter< ImageType, ImageType > FilterType;

  FilterType::Pointer filter = FilterType::New();

  FilterWatcher filterWatch( filter );

  filter->SetInput (      reader1->GetOutput() );
  filter->SetLabelInput ( reader2->GetOutput() );
  filter->UseHistogramsOn();
  try
    {
    filter->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Exception caught ! " << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int numberOfObjects  = filter->GetNumberOfObjects();
  const unsigned int numberOfLabels   = filter->GetNumberOfLabels();

  typedef FilterType::RealType                      RealType;
  typedef FilterType::BoundingBoxType               BoundingBoxType;
  typedef FilterType::RegionType                    RegionType;
  typedef FilterType::LabelPixelType                LabelPixelType;
  typedef FilterType::ValidLabelValuesContainerType ValidLabelValuesType;

  LabelPixelType labelValue;

  std::cout << "There are " << numberOfLabels << " labels" << std::endl;
  std::cout << "There are " << numberOfObjects << " objects" << std::endl;

  unsigned int labelCount=0;
  // Try to validate that the numberOfLabels in the ValidLabelList is
  // equal to the number of labels reported
  for(ValidLabelValuesType::const_iterator vIt=filter->GetValidLabelValues().begin();
      vIt != filter->GetValidLabelValues().end();
      ++vIt)
    {
    if ( filter->HasLabel(*vIt) )
      {
      ++labelCount;
      }

    }
  if( labelCount != numberOfLabels )
    {
    std::cerr << "Valid Labels Mismatch found!" << std::endl;
    std::cerr << labelCount << " != " << numberOfLabels << std::endl;
    return EXIT_FAILURE;
    }

  // Try two labels: one that exists and one that does not
  for (int i = 0; i < 2; i++)
    {
    // Find an existing label
    if (i == 0)
      {
      labelValue = 0;
      while (!filter->HasLabel(labelValue))
        {
        labelValue++;
        }
      std::cout << "Label Statistics for label "
                << static_cast<itk::NumericTraits<LabelPixelType>::PrintType>(labelValue)
                << " which exists" << std::endl;
      }
    // Find a non existent label
    if (i != 0)
      {
      labelValue = 0;
      while (filter->HasLabel(labelValue))
        {
        labelValue++;
        }
      std::cout << "Label Statistics for label "
                << static_cast<itk::NumericTraits<LabelPixelType>::PrintType>(labelValue)
                << " which does not exist" << std::endl;
      }


    const RealType min      =  filter->GetMinimum( labelValue );
    const RealType max      =  filter->GetMaximum( labelValue );
    const RealType median   =  filter->GetMedian( labelValue );
    const RealType mean     =  filter->GetMean( labelValue );
    const RealType sigma    =  filter->GetSigma( labelValue );
    const RealType variance =  filter->GetVariance( labelValue );
    const RealType sum      =  filter->GetSum( labelValue );
    const BoundingBoxType box = filter->GetBoundingBox( labelValue );
    const RegionType region = filter->GetRegion( labelValue );

    std::cout << "Minimum   = " << min      << std::endl;
    std::cout << "Maximum   = " << max      << std::endl;
    std::cout << "Median    = " << median   << std::endl;
    std::cout << "Mean      = " << mean     << std::endl;
    std::cout << "Sigma     = " << sigma    << std::endl;
    std::cout << "Variance  = " << variance << std::endl;
    std::cout << "Sum       = " << sum      << std::endl;
    std::cout << "Region    = " << region   << std::endl;

    BoundingBoxType::const_iterator itr = box.begin();
    while( itr != box.end() )
      {
      std::cout << "Index = " << *itr << std::endl;
      ++itr;
      }
    }
  return EXIT_SUCCESS;
}
