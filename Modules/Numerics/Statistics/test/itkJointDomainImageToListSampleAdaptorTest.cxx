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
// The example tests the class itk::Statistics::JointDomainImageToListSampleAdaptor.


#include "itkJointDomainImageToListSampleAdaptor.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkMath.h"

int itkJointDomainImageToListSampleAdaptorTest(int, char* [] )
{
  const unsigned int MeasurementVectorSize = 8;
  typedef unsigned long                                                      MeasurementComponentType;
  typedef itk::FixedArray< MeasurementComponentType, MeasurementVectorSize > PixelType;

  const unsigned int ImageDimension = 3;
  typedef itk::Image< PixelType, ImageDimension > ImageType;

  ImageType::Pointer image = ImageType::New();

  ImageType::IndexType start;
  ImageType::SizeType  size;

  start.Fill(0);
  size.Fill(10);

  unsigned long totalSize = size[0] * size[1] * size[2];
  ImageType::RegionType region( start, size );
  image->SetRegions( region );
  image->Allocate();

  typedef itk::ImageRegionIteratorWithIndex< ImageType > RegionIteratorType;

  RegionIteratorType it( image, region );

  it.GoToBegin();

  while (!it.IsAtEnd())
    {
    PixelType value;
    for( unsigned int i=0; i< MeasurementVectorSize; i++ )
      {
      value[i] = i + it.GetIndex()[0];
      }
    it.Set( value );
    ++it;
    }

  //define an adaptor type
  typedef itk::Statistics::JointDomainImageToListSampleAdaptor<
    ImageType > JointDomainImageToListSampleAdaptorType;
  JointDomainImageToListSampleAdaptorType::Pointer adaptor
                              = JointDomainImageToListSampleAdaptorType::New();
 //Test if the methods throw exceptions if invoked before setting the image
  try
    {
    // calling Size() method prematurely in order to trigger an exception.
    adaptor->Size();
    std::cerr << "Exception should have been thrown since the input image \
                  is not set yet" << std::endl;
    }
  catch ( itk::ExceptionObject & excp )
    {
    std::cerr << "Caught expected exception: " << excp << std::endl;
    }
  try
    {
    // calling GetTotalFrequency() method prematurely in order to trigger an exception.
    adaptor->GetTotalFrequency();
    std::cerr << "Exception should have been thrown since the input image \
                  is not set yet" << std::endl;
    }
  catch ( itk::ExceptionObject & excp )
    {
    std::cerr << "Caught expected exception: " << excp << std::endl;
    }

  try
    {
    // calling GetMeasurementVector() method prematurely in order to trigger an exception.
    adaptor->GetMeasurementVector( 0 );
    std::cerr << "Exception should have been thrown since the input image \
                  is not set yet" << std::endl;
    }
  catch ( itk::ExceptionObject & excp )
    {
    std::cerr << "Caught expected exception: " << excp << std::endl;
    }

  try
    {
    // calling GetImage() method prematurely in order to trigger an exception.
    adaptor->GetImage( );
    std::cerr << "Exception should have been thrown since the input image \
                  is not set yet" << std::endl;
    }
  catch ( itk::ExceptionObject & excp )
    {
    std::cerr << "Caught expected exception: " << excp << std::endl;
    }

  try
    {
    // calling GetFrequency() method prematurely in order to trigger an exception.
    adaptor->GetFrequency(0 );
    std::cerr << "Exception should have been thrown since the input image \
                  is not set yet" << std::endl;
    }
  catch ( itk::ExceptionObject & excp )
    {
    std::cerr << "Caught expected exception: " << excp << std::endl;
    }


  adaptor->SetImage ( image );

  //check size
  if (totalSize != adaptor->Size())
    {
    std::cerr << "Size() is not returning the correct size"<< std::endl;
    return EXIT_FAILURE;
    }

  //check frequency
  if (totalSize != adaptor->GetTotalFrequency())
    {
    std::cerr << "GetTotalFrequency() is not returning the correct frequency"<< std::endl;
    return EXIT_FAILURE;
    }


  adaptor->Print( std::cout );

  ImageType::IndexType index;
  ImageType::PixelType pixel;

  JointDomainImageToListSampleAdaptorType::InstanceIdentifier    iid;
  typedef JointDomainImageToListSampleAdaptorType::MeasurementVectorType MeasurementVectorType;
  JointDomainImageToListSampleAdaptorType::PointType             tempPoint;


  MeasurementVectorType   measurementVector;

  for ( unsigned int i=0; i < size[2]; i++ )
    {
    for ( unsigned int j=0; j < size[1]; j++ )
      {
      for ( unsigned int k=0; k < size[0]; k++ )
        {
        index[0]=k;
        index[1]=j;
        index[2]=i;

        adaptor->GetImage()->TransformIndexToPhysicalPoint( index, tempPoint );

        pixel = adaptor->GetImage()->GetPixel( index );

        measurementVector[0] = tempPoint[0];
        measurementVector[1] = tempPoint[1];
        measurementVector[2] = tempPoint[2];
        measurementVector[3] = pixel[0];
        measurementVector[4] = pixel[1];
        measurementVector[5] = pixel[2];

        iid = adaptor->GetImage()->ComputeOffset( index );

        MeasurementVectorType measurementVectorFromAdaptor = adaptor->GetMeasurementVector(iid);
        for ( unsigned int m=0; m < 5; m ++ )
          {
          if ( !itk::Math::FloatAlmostEqual(measurementVectorFromAdaptor[m],measurementVector[m],4,1.0E-6) )
            {
            std::cerr << "Error in measurment vector value accessed using the adaptor "
                      << (measurementVectorFromAdaptor[m] - measurementVector[m])
                      << std::endl;
            return EXIT_FAILURE;
            }
          }
        }
      }
    }

  //Test the Use_PixelContainer boolean

  adaptor->SetUsePixelContainer( false );
  if ( adaptor->GetUsePixelContainer() != false )
    {
    std::cerr << "Error in Set/Get UsePixelContainer methods" << std::endl;
    return EXIT_FAILURE;
    }

  adaptor->UsePixelContainerOn(  );
  if ( adaptor->GetUsePixelContainer() != true )
    {
    std::cerr << "Error in Set/Get UsePixelContainerOn method" << std::endl;
    return EXIT_FAILURE;
    }

  //Get measurement vector from the pixel container and using ComputeIndex and compare
  //the result
  JointDomainImageToListSampleAdaptorType::MeasurementVectorType  v1 = adaptor->GetMeasurementVector( 4 );
  adaptor->UsePixelContainerOff();
  JointDomainImageToListSampleAdaptorType::MeasurementVectorType  v2 = adaptor->GetMeasurementVector( 4 );

  const double epsilon = 1e-3;

  for ( unsigned int m=0; m < 5; m ++ )
    {
    if ( !itk::Math::FloatAlmostEqual(v1[m],v2[m],4,epsilon) )
      {
      std::cerr << "Accessing the measurement vector using the two method produced different \
                  result " << std::endl;
      return EXIT_FAILURE;
      }
    }

  //Exercise GetFrequency method on the first and last element
  if ( adaptor->GetFrequency( 0 ) != 1 ||
       adaptor->GetFrequency( adaptor->Size() ) != 1 )
    {
    std::cerr << "GetFrequency is not returning the expected value: " << std::endl;
    return EXIT_FAILURE;
    }

  // iterator tests
  //
  std::cerr << "Iterators..." << std::endl;
    {
    // forward iterator
    typedef JointDomainImageToListSampleAdaptorType::Iterator IteratorType;

    IteratorType s_iter = adaptor->Begin();

    // copy constructor
    IteratorType bs_iter(s_iter);
    if (bs_iter != s_iter)
      {
      std::cerr << "Iterator::Copy Constructor failed" << std::endl;
      return EXIT_FAILURE;
      }

    // assignment operator
    IteratorType assignment_iter( bs_iter );
    assignment_iter = s_iter;
    if (assignment_iter != s_iter)
      {
      std::cerr << "Iterator::assignment operator failed" << std::endl;
      return EXIT_FAILURE;
      }

    JointDomainImageToListSampleAdaptorType::InstanceIdentifier iid2 = 0;
    while (s_iter != adaptor->End())
      {
      if (adaptor->GetMeasurementVector(iid2) !=
          s_iter.GetMeasurementVector())
        {
        std::cerr << "Iterator::GetMeasurementVector (forward) failed"
                  << std::endl;
        return EXIT_FAILURE;
        }
      if (iid2 != s_iter.GetInstanceIdentifier())
        {
        std::cerr << "Iterator::GetInstanceIdentifier (forward) failed"
                  << std::endl;
        return EXIT_FAILURE;
        }
      if (s_iter.GetFrequency() != 1)
        {
        std::cerr << "Iterator::GetFrequency (forward) failed" << std::endl;
        return EXIT_FAILURE;
        }
      if (adaptor->GetFrequency(iid2) != 1)
        {
        std::cerr << "GetFrequency (forward) failed" << std::endl;
        return EXIT_FAILURE;
        }
      ++iid2;
      ++s_iter;
      }

    if (s_iter != adaptor->End())
      {
      std::cerr << "Iterator::End (forward) failed" << std::endl;
      return EXIT_FAILURE;
      }

    }


  //Test the iterators
  std::cerr << "Const Iterators..." << std::endl;
    {
    // forward iterator
    typedef JointDomainImageToListSampleAdaptorType::ConstIterator  ConstIteratorType;

    ConstIteratorType s_iter = adaptor->Begin();

    // copy constructor
    ConstIteratorType bs_iter(s_iter);
    if (bs_iter != s_iter)
      {
      std::cerr << "Iterator::Copy Constructor (from const) failed"
                << std::endl;
      return EXIT_FAILURE;
      }

    // assignment operator
    ConstIteratorType assignment_iter( bs_iter );
    assignment_iter = s_iter;
    if (assignment_iter != s_iter)
      {
      std::cerr << "Const Iterator::operator= () failed"
                << std::endl;
      return EXIT_FAILURE;
      }

    // copy from non-const iterator
    JointDomainImageToListSampleAdaptorType::Iterator nonconst_iter = adaptor->Begin();
    JointDomainImageToListSampleAdaptorType::ConstIterator s2_iter(nonconst_iter);
    if (s2_iter != s_iter)
      {
      std::cerr << "Iterator::Copy Constructor (from non-const) failed"
                << std::endl;
      return EXIT_FAILURE;
      }
    // assignment from non-const iterator
    s2_iter = nonconst_iter;
    if (s2_iter != s_iter)
      {
      std::cerr << "Iterator::assignment (from non-const) failed" << std::endl;
      return EXIT_FAILURE;
      }

    JointDomainImageToListSampleAdaptorType::InstanceIdentifier iid3 = 0;
    while (s_iter != adaptor->End())
      {
      if (adaptor->GetMeasurementVector(iid3) !=
          s_iter.GetMeasurementVector())
        {
        std::cerr << "Iterator::GetMeasurementVector (forward) failed"
                  << std::endl;
        return EXIT_FAILURE;
        }
      if (iid3 != s_iter.GetInstanceIdentifier())
        {
        std::cerr << "Iterator::GetInstanceIdentifier (forward) failed"
                  << std::endl;
        return EXIT_FAILURE;
        }
      if (s_iter.GetFrequency() != 1)
        {
        std::cerr << "Iterator::GetFrequency (forward) failed" << std::endl;
        return EXIT_FAILURE;
        }
      ++iid3;
      ++s_iter;
      }

    if (s_iter != adaptor->End())
      {
      std::cerr << "Iterator::End (forward) failed" << std::endl;
      return EXIT_FAILURE;
      }

    }


  std::cerr << "[PASSED]" << std::endl;
  return EXIT_SUCCESS;
}
