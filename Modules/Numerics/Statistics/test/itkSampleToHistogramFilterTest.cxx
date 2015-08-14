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

#include "itkListSample.h"
#include "itkHistogram.h"
#include "itkSampleToHistogramFilter.h"
#include "itkIntTypes.h"
#include "itkMath.h"


int itkSampleToHistogramFilterTest( int , char * [] )
{

  const unsigned int numberOfComponents = 3;
  typedef float      MeasurementType;

  typedef itk::Array< MeasurementType > MeasurementVectorType;

  typedef itk::Statistics::ListSample< MeasurementVectorType > SampleType;

  typedef itk::Statistics::Histogram< MeasurementType,
          itk::Statistics::DenseFrequencyContainer2 > HistogramType;

  typedef itk::Statistics::SampleToHistogramFilter<
    SampleType, HistogramType > FilterType;

  typedef FilterType::InputHistogramSizeObjectType         InputHistogramSizeObjectType;
  typedef FilterType::HistogramSizeType                    HistogramSizeType;
  typedef FilterType::HistogramMeasurementType             HistogramMeasurementType;
  typedef FilterType::HistogramMeasurementVectorType       HistogramMeasurementVectorType;
  typedef FilterType::InputHistogramMeasurementObjectType  InputHistogramMeasurementObjectType;
  typedef FilterType::
    InputHistogramMeasurementVectorObjectType  InputHistogramMeasurementVectorObjectType;

  FilterType::Pointer filter = FilterType::New();

  SampleType::Pointer sample = SampleType::New();

  // Test GetInput() before setting the input
  if( filter->GetInput() != ITK_NULLPTR )
    {
    std::cerr << "GetInput() should have returned ITK_NULLPTR" << std::endl;
    return EXIT_FAILURE;
    }

  // Test GetOutput() before creating the output
  if( filter->GetOutput() == ITK_NULLPTR )
    {
    std::cerr << "GetOutput() should have returned NON-ITK_NULLPTR" << std::endl;
    return EXIT_FAILURE;
    }

  filter->SetInput( sample );

  if( filter->GetInput() != sample.GetPointer() )
    {
    std::cerr << "GetInput() didn't matched SetInput()" << std::endl;
    return EXIT_FAILURE;
    }

  // Exercise the Print method.
  filter->Print( std::cout );


  // Test exception when calling Update() without having
  // defined the size of the histogram in the filter.
  try
    {
    filter->Update();
    std::cerr << "Failure to throw expected exception due to lack";
    std::cerr << " of calling SetHistogramSize() in the filter ";
    return EXIT_FAILURE;
    }
  catch( itk::MissingHistogramSizeInput &e )
    {
    std::cout << "Exception received:" << std::endl;
    std::cout << e << std::endl;
    }
  catch( ... )
    {
    std::cerr << "Histogram Size input exception not received.\n";
    return EXIT_FAILURE;
    }

  HistogramSizeType histogramSize0( numberOfComponents );
  histogramSize0.Fill(1);
  filter->SetHistogramSize( histogramSize0 );

  // Test exception when calling Update() without having
  // defined the number of components in the sample
  try
    {
    filter->Update();
    std::cerr << "Failure to throw expected exception due to lack";
    std::cerr << " of calling SetMeasurementVectorSize() in the sample";
    }
  catch( itk::NullSizeHistogramInputMeasurementVectorSize &e )
    {
    std::cout << "Exception received:" << std::endl;
    std::cout << e << std::endl;
    }
  catch( ... )
    {
    std::cerr << "MeasurementVectorSize exception not received.\n";
    return EXIT_FAILURE;
    }

  sample->SetMeasurementVectorSize( numberOfComponents );

  try
    {
    filter->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  HistogramSizeType histogramSize1( numberOfComponents );
  histogramSize1[0] = 256;
  histogramSize1[1] = 256;
  histogramSize1[2] = 256;

  HistogramSizeType histogramSize2( numberOfComponents );
  histogramSize2[0] = 128;
  histogramSize2[1] = 128;
  histogramSize2[2] = 128;


  filter->SetHistogramSize( histogramSize1 );

  const InputHistogramSizeObjectType * returnedHistogramSizeObject =
    filter->GetHistogramSizeInput();

  if( returnedHistogramSizeObject == ITK_NULLPTR )
    {
    std::cerr << "SetHistogramSize() failed pointer consistency test" << std::endl;
    return EXIT_FAILURE;
    }

  HistogramSizeType returnedHistogramSize = returnedHistogramSizeObject->Get();

  for( unsigned int k1 = 0; k1 < numberOfComponents; k1++ )
    {
    if( returnedHistogramSize[k1] != histogramSize1[k1] )
      {
      std::cerr << "Get/Set HistogramSize() failed value consistency test" << std::endl;
      return EXIT_FAILURE;
      }
    }

  filter->SetHistogramSize( histogramSize2 );

  returnedHistogramSizeObject =
      filter->GetHistogramSizeInput();

  returnedHistogramSize = returnedHistogramSizeObject->Get();

  for( unsigned int k2 = 0; k2 < numberOfComponents; k2++ )
    {
    if( returnedHistogramSize[k2] != histogramSize2[k2] )
      {
      std::cerr << "Get/Set HistogramSize() failed value consistency test" << std::endl;
      return EXIT_FAILURE;
      }
    }


  InputHistogramSizeObjectType::Pointer histogramSizeObject =
    InputHistogramSizeObjectType::New();

  histogramSizeObject->Set( histogramSize1 );

  filter->SetHistogramSizeInput( histogramSizeObject );

  returnedHistogramSizeObject = filter->GetHistogramSizeInput();

  if( returnedHistogramSizeObject != histogramSizeObject )
    {
    std::cerr << "Get/Set HistogramSizeInput() failed pointer consistency test" << std::endl;
    return EXIT_FAILURE;
    }

  returnedHistogramSize = returnedHistogramSizeObject->Get();

  for( unsigned int k3 = 0; k3 < numberOfComponents; k3++ )
    {
    if( returnedHistogramSize[k3] != histogramSize1[k3] )
      {
      std::cerr << "Get/Set HistogramSizeInput() failed value consistency test" << std::endl;
      return EXIT_FAILURE;
      }
    }

  histogramSizeObject->Set( histogramSize2 );

  filter->SetHistogramSizeInput( histogramSizeObject );

  returnedHistogramSizeObject = filter->GetHistogramSizeInput();

  if( returnedHistogramSizeObject != histogramSizeObject )
    {
    std::cerr << "Get/Set HistogramSizeInput() failed pointer consistency test" << std::endl;
    return EXIT_FAILURE;
    }

  returnedHistogramSize = returnedHistogramSizeObject->Get();

  for( unsigned int k4 = 0; k4 < numberOfComponents; k4++ )
    {
    if( returnedHistogramSize[k4] != histogramSize2[k4] )
      {
      std::cerr << "Get/Set HistogramSizeInput() failed value consistency test" << std::endl;
      return EXIT_FAILURE;
      }
    }


  filter->SetHistogramSize( histogramSize1 );
  filter->Update();
  itk::ModifiedTimeType modifiedTime = filter->GetMTime();
  filter->SetHistogramSize( histogramSize1 );

  if( filter->GetMTime() != modifiedTime )
    {
    std::cerr << "SetHistogramSize() failed modified Test 1" << std::endl;
    return EXIT_FAILURE;
    }

  filter->SetHistogramSize( histogramSize2 );

  if( filter->GetMTime() == modifiedTime )
    {
    std::cerr << "SetHistogramSize() failed modified Test 2" << std::endl;
    return EXIT_FAILURE;
    }


  // Testing the settings of the MarginalScale.
  const HistogramMeasurementType marginalScale1 = 237;
  const HistogramMeasurementType marginalScale2 = 179;

  filter->SetMarginalScale( marginalScale1 );

  const InputHistogramMeasurementObjectType * recoveredMarginalScaleObject =
    filter->GetMarginalScaleInput();

  if( recoveredMarginalScaleObject == ITK_NULLPTR )
    {
    std::cerr << "GetMarginalScaleInput() returned ITK_NULLPTR object." << std::endl;
    return EXIT_FAILURE;
    }

  if( itk::Math::NotExactlyEquals(recoveredMarginalScaleObject->Get(), marginalScale1) )
    {
    std::cerr << "GetMarginalScaleInput() test for value consistency 1 failed." << std::endl;
    return EXIT_FAILURE;
    }

  filter->SetMarginalScale( marginalScale2 );

  recoveredMarginalScaleObject = filter->GetMarginalScaleInput();

  if( recoveredMarginalScaleObject == ITK_NULLPTR )
    {
    std::cerr << "GetMarginalScaleInput() returned ITK_NULLPTR object." << std::endl;
    return EXIT_FAILURE;
    }

  if( itk::Math::NotExactlyEquals(recoveredMarginalScaleObject->Get(), marginalScale2) )
    {
    std::cerr << "GetMarginalScaleInput() test for value consistency 2 failed." << std::endl;
    return EXIT_FAILURE;
    }


  InputHistogramMeasurementObjectType::Pointer marginalScaleObject1 =
    InputHistogramMeasurementObjectType::New();

  marginalScaleObject1->Set( marginalScale1 );

  filter->SetMarginalScaleInput( marginalScaleObject1 );

  recoveredMarginalScaleObject = filter->GetMarginalScaleInput();

  if( recoveredMarginalScaleObject != marginalScaleObject1 )
    {
    std::cerr << "GetMarginalScaleInput() test for pointer consistency 1 failed." << std::endl;
    return EXIT_FAILURE;
    }

  if( itk::Math::NotExactlyEquals(recoveredMarginalScaleObject->Get(), marginalScale1) )
    {
    std::cerr << "GetMarginalScaleInput() test for value consistency 3 failed." << std::endl;
    return EXIT_FAILURE;
    }

  InputHistogramMeasurementObjectType::Pointer marginalScaleObject2 =
    InputHistogramMeasurementObjectType::New();

  marginalScaleObject2->Set( marginalScale2 );

  filter->SetMarginalScaleInput( marginalScaleObject2 );

  recoveredMarginalScaleObject = filter->GetMarginalScaleInput();

  if( recoveredMarginalScaleObject != marginalScaleObject2 )
    {
    std::cerr << "GetMarginalScaleInput() test for pointer consistency 2 failed." << std::endl;
    return EXIT_FAILURE;
    }

  if( itk::Math::NotExactlyEquals(recoveredMarginalScaleObject->Get(), marginalScale2) )
    {
    std::cerr << "GetMarginalScaleInput() test for value consistency 4 failed." << std::endl;
    return EXIT_FAILURE;
    }

  filter->SetMarginalScaleInput( marginalScaleObject1 );

  recoveredMarginalScaleObject = filter->GetMarginalScaleInput();

  if( recoveredMarginalScaleObject != marginalScaleObject1 )
    {
    std::cerr << "GetMarginalScaleInput() test for pointer consistency 3 failed." << std::endl;
    return EXIT_FAILURE;
    }

  if( itk::Math::NotExactlyEquals(recoveredMarginalScaleObject->Get(), marginalScale1) )
    {
    std::cerr << "GetMarginalScaleInput() test for value consistency 5 failed." << std::endl;
    return EXIT_FAILURE;
    }


  // Testing the settings of the BinMaximum and BinMinimum methods.
  HistogramMeasurementVectorType histogramBinMinimum1( numberOfComponents );
  histogramBinMinimum1[0] = 0;
  histogramBinMinimum1[1] = 0;
  histogramBinMinimum1[2] = 0;

  HistogramMeasurementVectorType histogramBinMinimum2( numberOfComponents );
  histogramBinMinimum2[0] = 17;
  histogramBinMinimum2[1] = 17;
  histogramBinMinimum2[2] = 17;


  filter->SetHistogramBinMinimum( histogramBinMinimum1 );

  const InputHistogramMeasurementVectorObjectType * returnedHistogramBinMinimumObject =
    filter->GetHistogramBinMinimumInput();

  if( returnedHistogramBinMinimumObject == ITK_NULLPTR )
    {
    std::cerr << "SetHistogramSize() failed pointer consistency test" << std::endl;
    return EXIT_FAILURE;
    }

  HistogramMeasurementVectorType returnedHistogramBinMinimum =
    returnedHistogramBinMinimumObject->Get();

  for( unsigned int k1 = 0; k1 < numberOfComponents; k1++ )
    {
    if( itk::Math::NotExactlyEquals(returnedHistogramBinMinimum[k1], histogramBinMinimum1[k1]) )
      {
      std::cerr << "Get/Set HistogramBinMinimum() failed value consistency test" << std::endl;
      return EXIT_FAILURE;
      }
    }


  filter->SetHistogramBinMinimum( histogramBinMinimum2 );

  returnedHistogramBinMinimumObject = filter->GetHistogramBinMinimumInput();

  returnedHistogramBinMinimum = returnedHistogramBinMinimumObject->Get();

  for( unsigned int k2 = 0; k2 < numberOfComponents; k2++ )
    {
    if( itk::Math::NotExactlyEquals(returnedHistogramBinMinimum[k2], histogramBinMinimum2[k2]) )
      {
      std::cerr << "Get/Set HistogramSize() failed value consistency test" << std::endl;
      return EXIT_FAILURE;
      }
    }


  InputHistogramMeasurementVectorObjectType::Pointer histogramBinMinimumObject =
    InputHistogramMeasurementVectorObjectType::New();

  histogramBinMinimumObject->Set( histogramBinMinimum1 );

  filter->SetHistogramBinMinimumInput( histogramBinMinimumObject );

  returnedHistogramBinMinimumObject = filter->GetHistogramBinMinimumInput();

  if( returnedHistogramBinMinimumObject != histogramBinMinimumObject )
    {
    std::cerr << "Get/Set HistogramBinMinimum() failed pointer consistency test" << std::endl;
    return EXIT_FAILURE;
    }

  returnedHistogramBinMinimum = returnedHistogramBinMinimumObject->Get();

  for( unsigned int k3 = 0; k3 < numberOfComponents; k3++ )
    {
    if( itk::Math::NotExactlyEquals(returnedHistogramBinMinimum[k3], histogramBinMinimum1[k3]) )
      {
      std::cerr << "Get/Set HistogramBinMinimum() failed value consistency test" << std::endl;
      return EXIT_FAILURE;
      }
    }

  histogramBinMinimumObject->Set( histogramBinMinimum2 );

  filter->SetHistogramBinMinimumInput( histogramBinMinimumObject );

  returnedHistogramBinMinimumObject = filter->GetHistogramBinMinimumInput();

  if( returnedHistogramBinMinimumObject != histogramBinMinimumObject )
    {
    std::cerr << "Get/Set HistogramBinMinimum() failed pointer consistency test" << std::endl;
    return EXIT_FAILURE;
    }

  returnedHistogramBinMinimum = returnedHistogramBinMinimumObject->Get();

  for( unsigned int k4 = 0; k4 < numberOfComponents; k4++ )
    {
    if( itk::Math::NotExactlyEquals(returnedHistogramBinMinimum[k4], histogramBinMinimum2[k4]) )
      {
      std::cerr << "Get/Set HistogramBinMinimum() failed value consistency test" << std::endl;
      return EXIT_FAILURE;
      }
    }


  filter->SetHistogramBinMinimum( histogramBinMinimum1 );
  filter->Update();
  modifiedTime = filter->GetMTime();
  filter->SetHistogramBinMinimum( histogramBinMinimum1 );

  if( filter->GetMTime() != modifiedTime )
    {
    std::cerr << "SetHistogramBinMinimum() failed modified Test 1" << std::endl;
    return EXIT_FAILURE;
    }

  filter->SetHistogramBinMinimum( histogramBinMinimum2 );

  if( filter->GetMTime() == modifiedTime )
    {
    std::cerr << "SetHistogramBinMinimum() failed modified Test 2" << std::endl;
    return EXIT_FAILURE;
    }


  HistogramMeasurementVectorType histogramBinMaximum1( numberOfComponents );
  histogramBinMaximum1[0] = 0;
  histogramBinMaximum1[1] = 0;
  histogramBinMaximum1[2] = 0;

  HistogramMeasurementVectorType histogramBinMaximum2( numberOfComponents );
  histogramBinMaximum2[0] = 17;
  histogramBinMaximum2[1] = 17;
  histogramBinMaximum2[2] = 17;


  filter->SetHistogramBinMaximum( histogramBinMaximum1 );

  const InputHistogramMeasurementVectorObjectType * returnedHistogramBinMaximumObject =
    filter->GetHistogramBinMaximumInput();

  if( returnedHistogramBinMaximumObject == ITK_NULLPTR )
    {
    std::cerr << "SetHistogramSize() failed pointer consistency test" << std::endl;
    return EXIT_FAILURE;
    }

  HistogramMeasurementVectorType returnedHistogramBinMaximum =
    returnedHistogramBinMaximumObject->Get();

  for( unsigned int k1 = 0; k1 < numberOfComponents; k1++ )
    {
    if( itk::Math::NotExactlyEquals(returnedHistogramBinMaximum[k1], histogramBinMaximum1[k1]) )
      {
      std::cerr << "Get/Set HistogramBinMaximum() failed value consistency test" << std::endl;
      return EXIT_FAILURE;
      }
    }


  filter->SetHistogramBinMaximum( histogramBinMaximum2 );

  returnedHistogramBinMaximumObject = filter->GetHistogramBinMaximumInput();

  returnedHistogramBinMaximum = returnedHistogramBinMaximumObject->Get();

  for( unsigned int k2 = 0; k2 < numberOfComponents; k2++ )
    {
    if( itk::Math::NotExactlyEquals(returnedHistogramBinMaximum[k2], histogramBinMaximum2[k2]) )
      {
      std::cerr << "Get/Set HistogramSize() failed value consistency test" << std::endl;
      return EXIT_FAILURE;
      }
    }


  InputHistogramMeasurementVectorObjectType::Pointer histogramBinMaximumObject =
    InputHistogramMeasurementVectorObjectType::New();

  histogramBinMaximumObject->Set( histogramBinMaximum1 );

  filter->SetHistogramBinMaximumInput( histogramBinMaximumObject );

  returnedHistogramBinMaximumObject = filter->GetHistogramBinMaximumInput();

  if( returnedHistogramBinMaximumObject != histogramBinMaximumObject )
    {
    std::cerr << "Get/Set HistogramBinMaximum() failed pointer consistency test" << std::endl;
    return EXIT_FAILURE;
    }

  returnedHistogramBinMaximum = returnedHistogramBinMaximumObject->Get();

  for( unsigned int k3 = 0; k3 < numberOfComponents; k3++ )
    {
    if( itk::Math::NotExactlyEquals(returnedHistogramBinMaximum[k3], histogramBinMaximum1[k3]) )
      {
      std::cerr << "Get/Set HistogramBinMaximum() failed value consistency test" << std::endl;
      return EXIT_FAILURE;
      }
    }

  histogramBinMaximumObject->Set( histogramBinMaximum2 );

  filter->SetHistogramBinMinimumInput( histogramBinMaximumObject );

  returnedHistogramBinMaximumObject = filter->GetHistogramBinMinimumInput();

  if( returnedHistogramBinMaximumObject != histogramBinMaximumObject )
    {
    std::cerr << "Get/Set HistogramBinMaximum() failed pointer consistency test" << std::endl;
    return EXIT_FAILURE;
    }

  returnedHistogramBinMaximum = returnedHistogramBinMaximumObject->Get();

  for( unsigned int k4 = 0; k4 < numberOfComponents; k4++ )
    {
    if( itk::Math::NotExactlyEquals(returnedHistogramBinMaximum[k4], histogramBinMaximum2[k4]) )
      {
      std::cerr << "Get/Set HistogramBinMaximum() failed value consistency test" << std::endl;
      return EXIT_FAILURE;
      }
    }


  filter->SetHistogramBinMaximum( histogramBinMaximum1 );
  filter->Update();
  modifiedTime = filter->GetMTime();
  filter->SetHistogramBinMaximum( histogramBinMaximum1 );

  if( filter->GetMTime() != modifiedTime )
    {
    std::cerr << "SetHistogramBinMaximum() failed modified Test 1" << std::endl;
    return EXIT_FAILURE;
    }

  filter->SetHistogramBinMaximum( histogramBinMaximum2 );

  if( filter->GetMTime() == modifiedTime )
    {
    std::cerr << "SetHistogramBinMaximum() failed modified Test 2" << std::endl;
    return EXIT_FAILURE;
    }

  // Testing the settings of the AutoMinimumMaximum Flag.
  const bool autoMinimumMaximum1 = true;
  const bool autoMinimumMaximum2 = false;

  filter->SetAutoMinimumMaximum( autoMinimumMaximum1 );

  typedef FilterType::InputBooleanObjectType InputBooleanObjectType;

  const InputBooleanObjectType * recoveredAutoMinimumMaximumObject =
    filter->GetAutoMinimumMaximumInput();

  if( recoveredAutoMinimumMaximumObject == ITK_NULLPTR )
    {
    std::cerr << "GetAutoMinimumMaximumInput() returned ITK_NULLPTR object." << std::endl;
    return EXIT_FAILURE;
    }

  if( recoveredAutoMinimumMaximumObject->Get() != autoMinimumMaximum1 )
    {
    std::cerr << "GetAutoMinimumMaximumInput() test for value consistency 1 failed." << std::endl;
    return EXIT_FAILURE;
    }

  filter->SetAutoMinimumMaximum( autoMinimumMaximum2 );

  recoveredAutoMinimumMaximumObject = filter->GetAutoMinimumMaximumInput();

  if( recoveredAutoMinimumMaximumObject == ITK_NULLPTR )
    {
    std::cerr << "GetAutoMinimumMaximumInput() returned ITK_NULLPTR object." << std::endl;
    return EXIT_FAILURE;
    }

  if( recoveredAutoMinimumMaximumObject->Get() != autoMinimumMaximum2 )
    {
    std::cerr << "GetAutoMinimumMaximumInput() test for value consistency 2 failed." << std::endl;
    return EXIT_FAILURE;
    }


  InputBooleanObjectType::Pointer autoMinimumMaximumObject1 =
    InputBooleanObjectType::New();

  autoMinimumMaximumObject1->Set( autoMinimumMaximum1 );

  filter->SetAutoMinimumMaximumInput( autoMinimumMaximumObject1 );

  recoveredAutoMinimumMaximumObject = filter->GetAutoMinimumMaximumInput();

  if( recoveredAutoMinimumMaximumObject != autoMinimumMaximumObject1 )
    {
    std::cerr << "GetAutoMinimumMaximumInput() test for pointer consistency 1 failed." << std::endl;
    return EXIT_FAILURE;
    }

  if( recoveredAutoMinimumMaximumObject->Get() != autoMinimumMaximum1 )
    {
    std::cerr << "GetAutoMinimumMaximumInput() test for value consistency 3 failed." << std::endl;
    return EXIT_FAILURE;
    }

  InputBooleanObjectType::Pointer autoMinimumMaximumObject2 =
    InputBooleanObjectType::New();

  autoMinimumMaximumObject2->Set( autoMinimumMaximum2 );

  filter->SetAutoMinimumMaximumInput( autoMinimumMaximumObject2 );

  recoveredAutoMinimumMaximumObject = filter->GetAutoMinimumMaximumInput();

  if( recoveredAutoMinimumMaximumObject != autoMinimumMaximumObject2 )
    {
    std::cerr << "GetAutoMinimumMaximumInput() test for pointer consistency 2 failed." << std::endl;
    return EXIT_FAILURE;
    }

  if( recoveredAutoMinimumMaximumObject->Get() != autoMinimumMaximum2 )
    {
    std::cerr << "GetAutoMinimumMaximumInput() test for value consistency 4 failed." << std::endl;
    return EXIT_FAILURE;
    }

  filter->SetAutoMinimumMaximumInput( autoMinimumMaximumObject1 );

  recoveredAutoMinimumMaximumObject = filter->GetAutoMinimumMaximumInput();

  if( recoveredAutoMinimumMaximumObject != autoMinimumMaximumObject1 )
    {
    std::cerr << "GetAutoMinimumMaximumInput() test for pointer consistency 3 failed." << std::endl;
    return EXIT_FAILURE;
    }

  if( recoveredAutoMinimumMaximumObject->Get() != autoMinimumMaximum1 )
    {
    std::cerr << "GetAutoMinimumMaximumInput() test for value consistency 5 failed." << std::endl;
    return EXIT_FAILURE;
    }


  //
  // Testing exception cases in the GenerateData() method.
  //
  filter->SetHistogramSizeInput( ITK_NULLPTR );

  std::cout << "GetHistogramSizeInput() =  " <<  filter->GetHistogramSizeInput() << std::endl;

  try
    {
    filter->Update();
    std::cerr << "Failure to throw expected exception ";
    std::cerr << " due to ITK_NULLPTR SetHistogramSizeInput()";
    return EXIT_FAILURE;
    }
  catch( itk::MissingHistogramSizeInput &e )
    {
    std::cout << "Exception received:" << std::endl;
    std::cout << e << std::endl;
    }
  catch(...)
    {
    std::cerr << "Histogram Size input exception not received.\n";
    return EXIT_FAILURE;
    }

  filter->SetHistogramSizeInput( histogramSizeObject );

  //
  // Testing exception cases in the GenerateData() method.
  //
  filter->SetMarginalScaleInput( ITK_NULLPTR );

  std::cout << "GetMarginalScaleInput() =  " <<  filter->GetMarginalScaleInput() << std::endl;

  try
    {
    filter->Update();
    std::cerr << "Failure to throw expected exception ";
    std::cerr << " due to ITK_NULLPTR SetMarginalScaleInput()";
    return EXIT_FAILURE;
    }
  catch( itk::MissingHistogramMarginalScaleInput &e )
    {
    std::cout << "Exception received:" << std::endl;
    std::cout << e << std::endl;
    }
  catch(...)
    {
    std::cerr << "Marginal scale input exception not received.\n";
    return EXIT_FAILURE;
    }

  marginalScaleObject1->Set( 100 );
  filter->SetMarginalScaleInput( marginalScaleObject1 );


  //
  // Testing exception cases in the GenerateData() method.
  //

  // First, force to use the minimum and maximum provided by the user.
  filter->SetAutoMinimumMaximum( false );

  filter->SetHistogramBinMinimumInput( ITK_NULLPTR );

  std::cout << "GetHistogramBinMinimumInput() =  " <<  filter->GetHistogramBinMinimumInput() << std::endl;

  try
    {
    filter->Update();
    std::cerr << "Failure to throw expected exception ";
    std::cerr << " due to ITK_NULLPTR SetHistogramBinMinimumInput()";
    return EXIT_FAILURE;
    }
  catch( itk::MissingHistogramBinMinimumInput &e )
    {
    std::cout << "Exception received:" << std::endl;
    std::cout << e << std::endl;
    }
  catch(...)
    {
    std::cerr << "Histogram Bin Minimum input exception not received.\n";
    return EXIT_FAILURE;
    }

  histogramBinMinimumObject->Set( histogramBinMinimum1 );
  filter->SetHistogramBinMinimumInput( histogramBinMinimumObject );

  //
  // Testing exception cases in the GenerateData() method.
  //
  filter->SetHistogramBinMaximumInput( ITK_NULLPTR );

  std::cout << "GetHistogramBinMaximumInput() =  " <<  filter->GetHistogramBinMaximumInput() << std::endl;

  try
    {
    filter->Update();
    std::cerr << "Failure to throw expected exception ";
    std::cerr << " due to ITK_NULLPTR SetHistogramBinMaximumInput()";
    return EXIT_FAILURE;
    }
  catch( itk::MissingHistogramBinMaximumInput &e )
    {
    std::cout << "Exception received:" << std::endl;
    std::cout << e << std::endl;
    }
  catch(...)
    {
    std::cerr << "Histogram Bin Maximum input exception not received.\n";
    return EXIT_FAILURE;
    }

  histogramBinMaximumObject->Set( histogramBinMaximum1 );
  filter->SetHistogramBinMaximumInput( histogramBinMaximumObject );


  try
    {
    filter->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }


  // Test GetOutput() after creating the output
  if( filter->GetOutput() == ITK_NULLPTR )
    {
    std::cerr << "GetOutput() should have returned NON-ITK_NULLPTR" << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;
}
