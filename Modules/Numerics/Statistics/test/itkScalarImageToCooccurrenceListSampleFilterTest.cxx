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


#include "itkScalarImageToCooccurrenceListSampleFilter.h"

int itkScalarImageToCooccurrenceListSampleFilterTest( int , char *[] )
{
  //Data definitions
  const unsigned int  IMGWIDTH         =  5;
  const unsigned int  IMGHEIGHT        =  5;
  const unsigned int  NDIMENSION       =  2;


  //------------------------------------------------------
  //Create a simple test images
  //------------------------------------------------------
  typedef itk::Image<int, NDIMENSION> InputImageType;

  typedef itk::ImageRegionIterator< InputImageType > InputImageIterator;


  InputImageType::Pointer image = InputImageType::New();

  InputImageType::SizeType inputImageSize = {{ IMGWIDTH, IMGHEIGHT }};

  InputImageType::IndexType index;
  index.Fill(0);
  InputImageType::RegionType region;

  region.SetSize( inputImageSize );
  region.SetIndex( index );

  //--------------------------------------------------------------------------
  // Set up the image first. It looks like:
  //  4 5 6 7 8
  //  3 4 5 6 7
  //  2 3 4 5 6
  //  1 2 3 4 5
  //  0 1 2 3 4
  //--------------------------------------------------------------------------

  image->SetRegions( region );
  image->Allocate();

  // setup the iterator
  InputImageIterator imageIt( image, image->GetBufferedRegion() );

  for(int i = 0; i < 5; i++)
    for(int j = 0; j < 5; j++, ++imageIt)
      {
      imageIt.Set(i % 5 + j);
      }

  imageIt.GoToBegin();
  for(int i = 0; i < 5; i++)
    {
    for(int j = 0; j < 5; j++, ++imageIt)
      {
      std::cout << imageIt.Get() << "\t";
      }
    std::cout << "\n";
    }


  typedef itk::Statistics::ScalarImageToCooccurrenceListSampleFilter <
                                  InputImageType > CooccurrenceListType;

  CooccurrenceListType::Pointer filter = CooccurrenceListType::New();

  filter->Print( std::cout );

  //Invoke update before adding an input. An exception should be
  //thrown.
  try
    {
    filter->Update();
    std::cerr << "Failed to throw expected exception due to ITK_NULLPTR input: " << std::endl;
    return EXIT_FAILURE;
    }
  catch ( itk::ExceptionObject & excp )
    {
    std::cout << "Expected exception caught: " << excp << std::endl;
    }

  filter->ResetPipeline();

  if ( filter->GetInput() != ITK_NULLPTR )
    {
    std::cerr << "GetInput() should return ITK_NULLPTR since the input is\
                  not set yet " << std::endl;
    return EXIT_FAILURE;
    }

  filter->SetInput(image);

  CooccurrenceListType::OffsetType offset = {{1,0}};

  filter->UseNeighbor(offset);

  filter->Update();

  const CooccurrenceListType::SampleType * sample = filter->GetOutput();

  typedef CooccurrenceListType::SampleType::ConstIterator ConstIteratorType;

  ConstIteratorType s_iter = sample->Begin();

  typedef CooccurrenceListType::SampleType::MeasurementVectorType MeasurementVectorType;

  std::vector< MeasurementVectorType > baselineVectorList;

  int  val[2];

  val[0] = 2;
  val[1] = 3;
  baselineVectorList.push_back( val );

  val[0] = 3;
  val[1] = 4;
  baselineVectorList.push_back( val );

  val[0] = 4;
  val[1] = 5;
  baselineVectorList.push_back( val );

  val[0] = 3;
  val[1] = 4;
  baselineVectorList.push_back( val );


  val[0] = 4;
  val[1] = 5;
  baselineVectorList.push_back( val );

  val[0] = 5;
  val[1] = 6;
  baselineVectorList.push_back( val );

  val[0] = 4;
  val[1] = 5;
  baselineVectorList.push_back( val );

  val[0] = 5;
  val[1] = 6;
  baselineVectorList.push_back( val );

  val[0] = 6;
  val[1] = 7;
  baselineVectorList.push_back( val );

  val[0] = 0;
  val[1] = 1;
  baselineVectorList.push_back( val );

  val[0] = 1;
  val[1] = 2;
  baselineVectorList.push_back( val );

  val[0] = 2;
  val[1] = 3;
  baselineVectorList.push_back( val );

  val[0] = 3;
  val[1] = 4;
  baselineVectorList.push_back( val );

  val[0] = 4;
  val[1] = 5;
  baselineVectorList.push_back( val );

  val[0] = 1;
  val[1] = 2;
  baselineVectorList.push_back( val );

  val[0] = 2;
  val[1] = 3;
  baselineVectorList.push_back( val );

  val[0] = 3;
  val[1] = 4;
  baselineVectorList.push_back( val );

  val[0] = 5;
  val[1] = 6;
  baselineVectorList.push_back( val );

  val[0] = 6;
  val[1] = 7;
  baselineVectorList.push_back( val );

  val[0] = 7;
  val[1] = 8;
  baselineVectorList.push_back( val );

  std::vector< MeasurementVectorType >::const_iterator it;

  it = baselineVectorList.begin();

  while ( s_iter != sample->End() )
    {

    MeasurementVectorType  v = s_iter.GetMeasurementVector();
    MeasurementVectorType  vbase = *it;

    if ( vbase != v )
      {
      std::cerr << "Cooccurrence list sample content is not correct " << std::endl;
      return EXIT_FAILURE;
      }
    ++s_iter;
    ++it;
    }

  return EXIT_SUCCESS;

}
