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
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include <iostream>
#include "itkDataObject.h"
#include "itkProcessObject.h"
#include "itkTestingMacros.h"
#include "itkNumericTraits.h"
#include "itkIntTypes.h"


namespace itk
{

class TestDataObject: public DataObject
{
public:
  /** Standard class typedefs. */
  typedef TestDataObject                                   Self;
  typedef DataObject                                       Superclass;
  typedef SmartPointer< Self >                             Pointer;
  typedef SmartPointer< const Self >                       ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);
};

class TestProcessObject: public ProcessObject
{
public:
  /** Standard class typedefs. */
  typedef TestProcessObject                                Self;
  typedef ProcessObject                                    Superclass;
  typedef SmartPointer< Self >                             Pointer;
  typedef SmartPointer< const Self >                       ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  // expose the protected method so we can test them
  using Superclass::GetInput;
  using Superclass::SetInput;
  using Superclass::GetPrimaryInput;
  using Superclass::SetPrimaryInput;
  using Superclass::GetOutput;
  using Superclass::SetOutput;
  using Superclass::GetPrimaryOutput;
  using Superclass::SetPrimaryOutput;
  using Superclass::SetPrimaryInputName;
  using Superclass::SetPrimaryOutputName;
  using Superclass::GetPrimaryInputName;
  using Superclass::GetPrimaryOutputName;
  using Superclass::MakeNameFromIndex;
  using Superclass::MakeIndexFromName;
  using Superclass::SetNthInput;
  using Superclass::AddInput;
  using Superclass::RemoveInput;
  using Superclass::SetNumberOfRequiredInputs;
  using Superclass::GetNumberOfRequiredInputs;
  using Superclass::PushBackInput;
  using Superclass::PopBackInput;
  using Superclass::PushFrontInput;
  using Superclass::PopFrontInput;
  using Superclass::SetNumberOfIndexedInputs;
  using Superclass::SetNthOutput;
  using Superclass::AddOutput;
  using Superclass::RemoveOutput;
  using Superclass::SetNumberOfRequiredOutputs;
  using Superclass::GetNumberOfRequiredOutputs;
  using Superclass::SetNumberOfIndexedOutputs;
  using Superclass::GenerateInputRequestedRegion;
  using Superclass::GenerateOutputRequestedRegion;
  using Superclass::GenerateOutputInformation;
  using Superclass::GenerateData;
  using Superclass::PropagateResetPipeline;
  using Superclass::ReleaseInputs;
  using Superclass::CacheInputReleaseDataFlags;
  using Superclass::RestoreInputReleaseDataFlags;
  using Superclass::GetRequiredInputNames;
  using Superclass::AddRequiredInputName;
  using Superclass::RemoveRequiredInputName;
  using Superclass::IsRequiredInputName;
  using Superclass::SetRequiredInputNames;
  using Superclass::VerifyPreconditions;
#if !defined(ITK_LEGACY_REMOVE)
  using Superclass::SetNumberOfInputs;
  using Superclass::SetNumberOfOutputs;
#endif
};

}

int itkDataObjectAndProcessObjectTest(int, char* [] )
{

  // create a TestProcessObject
  itk::TestProcessObject::Pointer process = itk::TestProcessObject::New();

  // some vars to test the methods
  itk::TestProcessObject::NameArray names;
  itk::TestProcessObject::DataObjectPointerArray dataObjects;
  itk::DataObject::Pointer dataObject;
  itk::ModifiedTimeType mtime;

  // and exercise various methods
  names = process->GetInputNames();
  TEST_SET_GET_VALUE( 0, names.size() );
  dataObjects = process->GetInputs();
  TEST_SET_GET_VALUE( 0, dataObjects.size() );
//   constDataObjects = process->GetInputs();
//   TEST_SET_GET_VALUE( 0, constDataObjects.size() );
  TEST_SET_GET_VALUE( false, process->HasInput("toto") );
  TEST_SET_GET_VALUE( 0, process->GetNumberOfInputs() );

  names = process->GetOutputNames();
  TEST_SET_GET_VALUE( 0, names.size() );
  dataObjects = process->GetOutputs();
  TEST_SET_GET_VALUE( 0, dataObjects.size() );
//   constDataObjects = process->GetOutputs();
//   TEST_SET_GET_VALUE( 0, constDataObjects.size() );
  TEST_SET_GET_VALUE( false, process->HasOutput("toto") );
  TEST_SET_GET_VALUE( 0, process->GetNumberOfOutputs() );

  dataObjects = process->GetIndexedInputs();
  TEST_SET_GET_VALUE( 0, dataObjects.size() );
  TEST_SET_GET_VALUE( 0, process->GetNumberOfIndexedInputs() );
  TEST_SET_GET_VALUE( 0, process->GetNumberOfValidRequiredInputs() );

  dataObjects = process->GetIndexedOutputs();
  TEST_SET_GET_VALUE( 0, dataObjects.size() );
  TEST_SET_GET_VALUE( 0, process->GetNumberOfIndexedOutputs() );
//   TEST_SET_GET_VALUE( 0, process->GetNumberOfValidRequiredOutputs() );

  dataObject = process->MakeOutput( 0 );
  TEST_SET_GET_VALUE( true, dataObject.IsNotNull() );
  dataObject = process->MakeOutput( 1 );
  TEST_SET_GET_VALUE( true, dataObject.IsNotNull() );
  dataObject = process->MakeOutput( 2 );
  TEST_SET_GET_VALUE( true, dataObject.IsNotNull() );

  TEST_SET_GET_VALUE( false, process->GetAbortGenerateData() );
  process->SetAbortGenerateData( true );
  TEST_SET_GET_VALUE( true, process->GetAbortGenerateData() );
  process->AbortGenerateDataOff();
  TEST_SET_GET_VALUE( false, process->GetAbortGenerateData() );
  process->AbortGenerateDataOn();
  TEST_SET_GET_VALUE( true, process->GetAbortGenerateData() );
  process->AbortGenerateDataOff();
  TEST_SET_GET_VALUE( false, process->GetAbortGenerateData() );

  TEST_SET_GET_VALUE( 0.0, process->GetProgress() );
  process->SetProgress( 1.0 );
  TEST_SET_GET_VALUE( 1.0, process->GetProgress() );
  process->SetProgress( 10000.0 );
  TEST_SET_GET_VALUE( 1.0, process->GetProgress() );
  process->SetProgress( -1.0 );
  TEST_SET_GET_VALUE( 0.0, process->GetProgress() );
  process->SetProgress( 0.0 );
  TEST_SET_GET_VALUE( 0.0, process->GetProgress() );

  // UpdateProgress() doesn't clamp the value - is it really what we want?
  process->UpdateProgress( 0.5 );
  TEST_SET_GET_VALUE( 0.5, process->GetProgress() );
  process->UpdateProgress( 100.0 );
  TEST_SET_GET_VALUE( 100.0, process->GetProgress() );
  process->UpdateProgress( -1.0 );
  TEST_SET_GET_VALUE( -1.0, process->GetProgress() );
  process->UpdateProgress( 0.0 );
  TEST_SET_GET_VALUE( 0.0, process->GetProgress() );

  // shouldn't do anything: there is no output at this point
  mtime = process->GetMTime();
  process->Update();
  TEST_SET_GET_VALUE( mtime, process->GetMTime() );

  // shouldn't do anything: there is no output at this point
  mtime = process->GetMTime();
  process->UpdateLargestPossibleRegion();
  TEST_SET_GET_VALUE( mtime, process->GetMTime() );

  // shouldn't do anything: there is no output at this point
  mtime = process->GetMTime();
  process->UpdateOutputInformation();
  TEST_SET_GET_VALUE( mtime, process->GetMTime() );

  // TODO: how to test those ones?
  // PropagateRequestedRegion(DataObject *output);
  // UpdateOutputData(DataObject *output);
  // EnlargeOutputRequestedRegion( DataObject *itkNotUsed(output) ){}

  // TODO: doesn't do anything for now, but should probably do something even without
  // input nor outout
  mtime = process->GetMTime();
  process->ResetPipeline();
  TEST_SET_GET_VALUE( mtime, process->GetMTime() );

  // nothing should change: there is no output
  TEST_SET_GET_VALUE( false, process->GetReleaseDataFlag() );
  process->SetReleaseDataFlag( true );
  TEST_SET_GET_VALUE( false, process->GetReleaseDataFlag() );
  process->ReleaseDataFlagOff();
  TEST_SET_GET_VALUE( false, process->GetReleaseDataFlag() );
  process->ReleaseDataFlagOn();
  TEST_SET_GET_VALUE( false, process->GetReleaseDataFlag() );

  TEST_SET_GET_VALUE( true, process->GetReleaseDataBeforeUpdateFlag() );
  process->SetReleaseDataBeforeUpdateFlag( false );
  TEST_SET_GET_VALUE( false, process->GetReleaseDataBeforeUpdateFlag() );
  process->ReleaseDataBeforeUpdateFlagOn();
  TEST_SET_GET_VALUE( true, process->GetReleaseDataBeforeUpdateFlag() );
  process->ReleaseDataBeforeUpdateFlagOff();
  TEST_SET_GET_VALUE( false, process->GetReleaseDataBeforeUpdateFlag() );
  process->ReleaseDataBeforeUpdateFlagOn();
  TEST_SET_GET_VALUE( true, process->GetReleaseDataBeforeUpdateFlag() );

  TEST_SET_GET_VALUE( itk::MultiThreader::GetGlobalDefaultNumberOfThreads(), process->GetNumberOfThreads() );
  process->SetNumberOfThreads( 11 );
  TEST_SET_GET_VALUE( 11, process->GetNumberOfThreads() );
  process->SetNumberOfThreads( 0 );
  TEST_SET_GET_VALUE( 1, process->GetNumberOfThreads() );
  process->SetNumberOfThreads( itk::NumericTraits<long>::max() );
  TEST_SET_GET_VALUE( itk::MultiThreader::GetGlobalMaximumNumberOfThreads(), process->GetNumberOfThreads() );
  process->SetNumberOfThreads( itk::MultiThreader::GetGlobalDefaultNumberOfThreads() );
  TEST_SET_GET_VALUE( itk::MultiThreader::GetGlobalDefaultNumberOfThreads(), process->GetNumberOfThreads() );

  // not sure what to test with that method - at least test that it exist
  itk::MultiThreader::Pointer multiThreader = process->GetMultiThreader();
  TEST_SET_GET_VALUE( true, multiThreader.IsNotNull() );

  // create some data object that will be used as input and output
  itk::TestDataObject::Pointer input0 = itk::TestDataObject::New();
  itk::TestDataObject::Pointer input1 = itk::TestDataObject::New();

  // default input values
  TEST_SET_GET_NULL_VALUE( process->GetPrimaryInput() );
  TEST_SET_GET_NULL_VALUE( process->GetInput("Primary") );
  TEST_SET_GET_NULL_VALUE( process->GetInput("toto") );
  TEST_SET_GET_NULL_VALUE( process->GetInput(0) );
  TEST_SET_GET_NULL_VALUE( process->GetInput(1) );

  process->Print(std::cout);
  process->SetInput( "Primary", input0 );
  names = process->GetInputNames();
  TEST_SET_GET_VALUE( 1, names.size() );

  TEST_SET_GET( input0, process->GetPrimaryInput() );
  TEST_SET_GET( input0, process->GetInput(0) );
  TEST_SET_GET( input0, process->GetInput("Primary") );
  TEST_SET_GET_VALUE( 1, process->GetNumberOfIndexedInputs() );
  process->SetPrimaryInput( NULL );

  TEST_SET_GET_NULL_VALUE( process->GetPrimaryInput() );
  TEST_SET_GET_NULL_VALUE( process->GetInput(0) );
  TEST_SET_GET_NULL_VALUE( process->GetInput("Primary") );
  process->SetNthInput( 0, input0 );

  TEST_SET_GET( input0, process->GetPrimaryInput() );
  TEST_SET_GET( input0, process->GetInput(0) );
  TEST_SET_GET( input0, process->GetInput("Primary") );
  process->SetPrimaryInputName("First");

  TEST_SET_GET( input0, process->GetPrimaryInput() );
  TEST_SET_GET( input0, process->GetInput(0) );
  TEST_SET_GET( input0, process->GetInput("First") );


  process->SetNthInput( 1, input1 );
  TEST_SET_GET( input1, process->GetInput(1) );
  TEST_SET_GET_VALUE( 2, process->GetNumberOfIndexedInputs() );
  process->SetNthInput( 1, NULL );
  TEST_SET_GET_NULL_VALUE( process->GetInput(1) );
  process->SetNthInput( 1, input1 );

  process->Print(std::cout);
  process->RemoveInput(1);
  TEST_SET_GET_VALUE( 1, process->GetNumberOfIndexedInputs() );
  process->SetNthInput( 1, input1 );
  TEST_SET_GET_VALUE( 2, process->GetNumberOfIndexedInputs() );
  TEST_SET_GET( input1, process->GetInput(1) );
  process->RemoveInput(10);
  TEST_SET_GET_VALUE( 2, process->GetNumberOfIndexedInputs() );
  process->PopBackInput();
  TEST_SET_GET_VALUE( 1, process->GetNumberOfIndexedInputs() );

  itk::TestDataObject::Pointer output = itk::TestDataObject::New();
  process->SetNthOutput( 0, output );

  process->SetNumberOfRequiredInputs(1);
  process->AddRequiredInputName( "Foo" );
  process->Print( std::cout );
  TRY_EXPECT_EXCEPTION( process->Update() );
  process->SetInput( "Foo", input1 );
  process->Print( std::cout );
  TRY_EXPECT_NO_EXCEPTION( process->Update() );
  process->AddRequiredInputName( "Bar" );
  process->SetInput( "Bar", input1 );
  process->SetNumberOfRequiredInputs(0);
  process->Print( std::cout );
  TRY_EXPECT_NO_EXCEPTION( process->Update() );

  names = process->GetRequiredInputNames();
  TEST_SET_GET_VALUE( 2, names.size() );
  process->RemoveRequiredInputName( "Foo" );
  process->RemoveRequiredInputName( "Bar" );
  TEST_SET_GET_VALUE( 0, process->GetRequiredInputNames().size() );
  process->SetRequiredInputNames( names );
  process->Print( std::cout );
  TEST_SET_GET_VALUE( 2, process->GetRequiredInputNames().size() );
  process->SetPrimaryOutputName( "Outy" );
  const std::string primaryOutputName = process->GetPrimaryOutputName();
  TEST_EXPECT_EQUAL( "Outy", primaryOutputName );

  process = itk::TestProcessObject::New();
  process->SetPrimaryInputName("Image1");
  TRY_EXPECT_EXCEPTION( process->AddRequiredInputName( "" ) );
  process->Print(std::cout);
  TEST_EXPECT_EQUAL( 1, process->GetNumberOfRequiredInputs() );
  process->AddRequiredInputName("Image2");
  TEST_EXPECT_EQUAL( 1, process->GetNumberOfRequiredInputs() );
  TEST_EXPECT_EQUAL( 0, process->GetNumberOfValidRequiredInputs() );
  TRY_EXPECT_EXCEPTION( process->VerifyPreconditions() );
  process->SetInput( "Image1", input0 );
  TEST_EXPECT_EQUAL( 1, process->GetNumberOfValidRequiredInputs() );

  TRY_EXPECT_EXCEPTION( process->VerifyPreconditions() );
  process->SetInput( "Image2", input0 );
  TEST_EXPECT_EQUAL( 1, process->GetNumberOfValidRequiredInputs() );
  TRY_EXPECT_NO_EXCEPTION(process->VerifyPreconditions() );

  process->SetInput( "Image2", NULL );
  process->AddInput( input0 );
  TRY_EXPECT_EXCEPTION(process->VerifyPreconditions() );

  process = itk::TestProcessObject::New();
  process->SetNumberOfRequiredInputs(2);
  process->SetPrimaryInputName("Image1");
  process->AddRequiredInputName("Image2");
  TEST_EXPECT_EQUAL( 2, process->GetNumberOfRequiredInputs() );
  TEST_EXPECT_EQUAL( 0, process->GetNumberOfValidRequiredInputs() );
  process->SetInput( "Image1", input0 );
  TEST_EXPECT_EQUAL( 1, process->GetNumberOfValidRequiredInputs() );
  process->SetInput( "Image2", input0 );
  process->RemoveRequiredInputName( "Image2" );

  TRY_EXPECT_EXCEPTION( process->AddRequiredInputName( "", 1 ) );
  process->AddRequiredInputName("Image2", 1 );
  process->Print(std::cout);
  TEST_EXPECT_EQUAL( 2, process->GetNumberOfRequiredInputs() );
  TEST_EXPECT_EQUAL( 2, process->GetNumberOfValidRequiredInputs() );
  TRY_EXPECT_NO_EXCEPTION(process->VerifyPreconditions() );

  // testing SetNumberOfIndexedInputs
  process = itk::TestProcessObject::New();
  process->SetNumberOfIndexedInputs( 1 );
  TEST_SET_GET_VALUE( 0, process->GetNumberOfIndexedInputs() );
  TEST_SET_GET_NULL_VALUE( process->GetPrimaryInput() );

  process->SetPrimaryInput( input0 );
  TEST_SET_GET_VALUE( 1, process->GetNumberOfIndexedInputs() );
  TEST_SET_GET_VALUE( input0, process->GetPrimaryInput() );

  process->SetNumberOfIndexedInputs( 0 );
  TEST_SET_GET_VALUE( 0, process->GetNumberOfIndexedInputs() );
  TEST_SET_GET_NULL_VALUE( process->GetPrimaryInput() );

   process->SetNumberOfIndexedInputs( 2 );
   TEST_SET_GET_VALUE( 2, process->GetNumberOfIndexedInputs() );
   TEST_SET_GET_NULL_VALUE( process->GetPrimaryInput() );
   process->SetNthInput( 1, input1 );
   TEST_SET_GET_VALUE( input1, process->GetInput(1) );

   process->SetNumberOfIndexedInputs( 1 );
   TEST_SET_GET_VALUE( 0, process->GetNumberOfIndexedInputs() );
   TEST_SET_GET_NULL_VALUE( process->GetPrimaryInput() );

   // testing RemoveInput
   process = itk::TestProcessObject::New();
   process->SetNumberOfIndexedInputs( 2 );
   process->SetPrimaryInput( input0 );
   TEST_SET_GET_VALUE( 2, process->GetNumberOfIndexedInputs() );
   process->RemoveInput( "Primary" );
   TEST_SET_GET_NULL_VALUE( process->GetPrimaryInput() );
   TEST_SET_GET_VALUE( 2, process->GetNumberOfIndexedInputs() );

   process->AddRequiredInputName("Req");
   process->SetInput( "Req", input0 );
   TEST_SET_GET_VALUE( input0, process->GetInput("Req") );
   process->RemoveInput( "Req" );
   TEST_SET_GET_NULL_VALUE( process->GetInput("Req") );
   TEST_EXPECT_TRUE( process->IsRequiredInputName("Req") );

   process->RemoveInput( 99 );
   TEST_SET_GET_NULL_VALUE( process->GetInput("Req") );
   TEST_EXPECT_TRUE( process->IsRequiredInputName("Req") );
   TEST_SET_GET_VALUE( 2, process->GetNumberOfIndexedInputs() );

   process->RemoveInput( 1 );
   TEST_SET_GET_VALUE( 0, process->GetNumberOfIndexedInputs() );

  return (EXIT_SUCCESS);
}
