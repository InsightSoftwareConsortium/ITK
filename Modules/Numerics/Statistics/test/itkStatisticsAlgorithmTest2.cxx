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

#include "itkImageRegionIteratorWithIndex.h"
#include "itkImageToListSampleAdaptor.h"
#include "itkStatisticsAlgorithm.h"

#include <vector>
#include <algorithm>

typedef itk::FixedArray< int, 3 >  PixelType;
typedef itk::Image< PixelType, 3 > ImageType;

typedef itk::Statistics::ImageToListSampleAdaptor< ImageType > SampleType;
typedef itk::Statistics::Subsample< SampleType >               SubsampleType;

const unsigned int testDimension = 1;

void resetData(::itk::Image<PixelType, 3>::Pointer image,  std::vector<int> &refVector)
{
  ImageType::IndexType index;
  ImageType::SizeType  size;
  size = image->GetLargestPossibleRegion().GetSize();

  unsigned long x;
  unsigned long y;
  unsigned long z;
  PixelType temp;

  // fill the image with random values
  for( z = 0; z < size[2]; z++ )
    {
    index[2] = z;
    temp[2] = rand();
    for( y = 0; y < size[1]; y++ )
      {
      index[1] = y;
      temp[1] = rand();
      for( x = 0; x < size[0]; x++ )
        {
        index[0] = x;
        temp[0] = rand();
        image->SetPixel(index, temp);
        }
      }
    }

  // fill the vector
  itk::ImageRegionIteratorWithIndex< ImageType >
    i_iter(image, image->GetLargestPossibleRegion());
  i_iter.GoToBegin();
  std::vector< int >::iterator viter;

  refVector.resize(size[0] * size[1] * size[2]);
  viter = refVector.begin();
  while( viter != refVector.end() )
    {
    *viter = i_iter.Get()[testDimension];
    ++viter;
    ++i_iter;
    }

  // sort result using stl vector for reference
  std::sort( refVector.begin(), refVector.end() );
}

bool isSortedOrderCorrect(std::vector<int> &ref,
                          ::itk::Statistics::Subsample<SampleType>::Pointer subsample)
{
  bool ret = true;
  std::vector<int>::iterator viter = ref.begin();
  SubsampleType::Iterator siter = subsample->Begin();
  while( siter != subsample->End() )
    {
    if( *viter != siter.GetMeasurementVector()[testDimension] )
      {
      ret = false;
      }
    ++siter;
    ++viter;
    }

  return ret;
}


int itkStatisticsAlgorithmTest2(int, char* [] )
{
  std::cout << "Statistics Algorithm Test \n \n";
  bool pass = true;
  std::string whereFail = "";

  // creats an image and allocate memory
  ImageType::Pointer image = ImageType::New();

  ImageType::SizeType size;
  size.Fill(5);

  ImageType::IndexType index;
  index.Fill(0);

  ImageType::RegionType region;
  region.SetSize(size);
  region.SetIndex(index);

  image->SetLargestPossibleRegion(region);
  image->SetBufferedRegion(region);
  image->Allocate();

  // creates an ImageToListSampleAdaptor object
  SampleType::Pointer sample = SampleType::New();
  sample->SetImage(image);

  // creates a Subsample obeject using the ImageToListSampleAdaptor object
  SubsampleType::Pointer subsample = SubsampleType::New();
  subsample->SetSample(sample);

  PixelType temp;

  // each algorithm test will be compared with the sorted
  // refVector
  std::vector< int > refVector;

  // creats a subsample with all instances in the image
  subsample->InitializeWithAllInstances();

  // InsertSort algorithm test

  // fill the image with random values and fill and sort the
  // refVector
  resetData(image, refVector);

  itk::Statistics::Algorithm::InsertSort< SubsampleType >(subsample, testDimension,
                                    0, subsample->Size());
  if( !isSortedOrderCorrect(refVector, subsample) )
    {
    pass = false;
    whereFail = "InsertSort";
    }

  // HeapSort algorithm test
  resetData(image, refVector);
  itk::Statistics::Algorithm::HeapSort< SubsampleType >(subsample, testDimension,
                                  0, subsample->Size());
  if( !isSortedOrderCorrect(refVector, subsample) )
    {
    pass = false;
    whereFail = "HeapSort";
    }

  // IntospectiveSort algortihm test
  resetData(image, refVector);
  itk::Statistics::Algorithm::IntrospectiveSort< SubsampleType >
    (subsample, testDimension, 0, subsample->Size(), 16);
  if( !isSortedOrderCorrect(refVector, subsample) )
    {
    pass = false;
    whereFail = "IntrospectiveSort";
    }

  // QuickSelect algorithm test
  resetData(image, refVector);
  SubsampleType::MeasurementType median =
    itk::Statistics::Algorithm::QuickSelect< SubsampleType >(subsample, testDimension,
                                                  0, subsample->Size(),
                                                  subsample->Size()/2);
  if( refVector[subsample->Size()/2] != median )
    {
    pass = false;
    whereFail = "QuickSelect";
    }

  if( !pass )
    {
    std::cerr << "Test failed in " << whereFail << "." << std::endl;
    return EXIT_FAILURE;
    }


  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;
}
