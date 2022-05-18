/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#include <iostream>
#include "itkDataObject.h"
#include "itkProcessObject.h"
#include "itkTestingMacros.h"
#include "itkNumericTraits.h"
#include "itkIntTypes.h"
#include "itkMultiThreaderBase.h"

namespace itk
{

class TestDataObject : public DataObject
{
public:
  /** Standard class type aliases. */
  using Self = TestDataObject;
  using Superclass = DataObject;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);
};

class TestProcessObject : public ProcessObject
{
public:
  /** Standard class type aliases. */
  using Self = TestProcessObject;
  using Superclass = ProcessObject;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

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
  using Superclass::AddOptionalInputName;
  using Superclass::RemoveRequiredInputName;
  using Superclass::IsRequiredInputName;
  using Superclass::SetRequiredInputNames;
  using Superclass::VerifyPreconditions;
};

} // namespace itk

int
itkDataObjectAndProcessObjectTest(int, char *[])
{
  // create a TestProcessObject
  itk::TestProcessObject::Pointer process = itk::TestProcessObject::New();

  // some vars to test the methods
  itk::TestProcessObject::NameArray              names;
  itk::TestProcessObject::DataObjectPointerArray dataObjects;
  itk::DataObject::Pointer                       dataObject;
  itk::ModifiedTimeType                          mtime;

  // and exercise various methods
  names = process->GetInputNames();
  ITK_TEST_SET_GET_VALUE(0, names.size());
  dataObjects = process->GetInputs();
  ITK_TEST_SET_GET_VALUE(0, dataObjects.size());
  //   constDataObjects = process->GetInputs();
  //   ITK_TEST_SET_GET_VALUE( 0, constDataObjects.size() );
  ITK_TEST_SET_GET_VALUE(false, process->HasInput("toto"));
  ITK_TEST_SET_GET_VALUE(0, process->GetNumberOfInputs());

  names = process->GetOutputNames();
  ITK_TEST_SET_GET_VALUE(0, names.size());
  dataObjects = process->GetOutputs();
  ITK_TEST_SET_GET_VALUE(0, dataObjects.size());
  //   constDataObjects = process->GetOutputs();
  //   ITK_TEST_SET_GET_VALUE( 0, constDataObjects.size() );
  ITK_TEST_SET_GET_VALUE(false, process->HasOutput("toto"));
  ITK_TEST_SET_GET_VALUE(0, process->GetNumberOfOutputs());

  dataObjects = process->GetIndexedInputs();
  ITK_TEST_SET_GET_VALUE(0, dataObjects.size());
  ITK_TEST_SET_GET_VALUE(0, process->GetNumberOfIndexedInputs());
  ITK_TEST_SET_GET_VALUE(0, process->GetNumberOfValidRequiredInputs());

  dataObjects = process->GetIndexedOutputs();
  ITK_TEST_SET_GET_VALUE(0, dataObjects.size());
  ITK_TEST_SET_GET_VALUE(0, process->GetNumberOfIndexedOutputs());
  //   ITK_TEST_SET_GET_VALUE( 0, process->GetNumberOfValidRequiredOutputs() );

  dataObject = process->MakeOutput(0);
  ITK_TEST_SET_GET_VALUE(true, dataObject.IsNotNull());
  dataObject = process->MakeOutput(1);
  ITK_TEST_SET_GET_VALUE(true, dataObject.IsNotNull());
  dataObject = process->MakeOutput(2);
  ITK_TEST_SET_GET_VALUE(true, dataObject.IsNotNull());
  dataObject = process->MakeOutput("Not an Index");
  ITK_TEST_SET_GET_VALUE(true, dataObject.IsNotNull());

  ITK_TEST_SET_GET_VALUE(false, process->GetAbortGenerateData());
  process->SetAbortGenerateData(true);
  ITK_TEST_SET_GET_VALUE(true, process->GetAbortGenerateData());
  process->AbortGenerateDataOff();
  ITK_TEST_SET_GET_VALUE(false, process->GetAbortGenerateData());
  process->AbortGenerateDataOn();
  ITK_TEST_SET_GET_VALUE(true, process->GetAbortGenerateData());
  process->AbortGenerateDataOff();
  ITK_TEST_SET_GET_VALUE(false, process->GetAbortGenerateData());

  ITK_TEST_SET_GET_VALUE(0.0, process->GetProgress());
  process->UpdateProgress(1.0);
  ITK_TEST_SET_GET_VALUE(1.0, process->GetProgress());
  process->UpdateProgress(10000.0);
  ITK_TEST_SET_GET_VALUE(1.0, process->GetProgress());
  process->UpdateProgress(-1.0);
  ITK_TEST_SET_GET_VALUE(0.0, process->GetProgress());
  process->UpdateProgress(0.0);
  ITK_TEST_SET_GET_VALUE(0.0, process->GetProgress());

  // verify no progress overflow
  for (size_t i = 0; i < 10; ++i)
  {
    process->IncrementProgress(0.1);
  }
  if (!itk::Math::FloatAlmostEqual(process->GetProgress(), 1.0f))
  {
    std::cerr << "Progress is not reported correctly!" << std::endl;
    return EXIT_FAILURE;
  }

  // shouldn't do anything: there is no output at this point
  mtime = process->GetMTime();
  process->Update();
  ITK_TEST_SET_GET_VALUE(mtime, process->GetMTime());

  // shouldn't do anything: there is no output at this point
  mtime = process->GetMTime();
  process->UpdateLargestPossibleRegion();
  ITK_TEST_SET_GET_VALUE(mtime, process->GetMTime());

  // shouldn't do anything: there is no output at this point
  mtime = process->GetMTime();
  process->UpdateOutputInformation();
  ITK_TEST_SET_GET_VALUE(mtime, process->GetMTime());

  // TODO: how to test those ones?
  // PropagateRequestedRegion(DataObject *output);
  // UpdateOutputData(DataObject *output);
  // EnlargeOutputRequestedRegion( DataObject *itkNotUsed(output) ){}

  // TODO: doesn't do anything for now, but should probably do something even without
  // input nor output
  mtime = process->GetMTime();
  process->ResetPipeline();
  ITK_TEST_SET_GET_VALUE(mtime, process->GetMTime());

  // nothing should change: there is no output
  ITK_TEST_SET_GET_VALUE(false, process->GetReleaseDataFlag());
  process->SetReleaseDataFlag(true);
  ITK_TEST_SET_GET_VALUE(false, process->GetReleaseDataFlag());
  process->ReleaseDataFlagOff();
  ITK_TEST_SET_GET_VALUE(false, process->GetReleaseDataFlag());
  process->ReleaseDataFlagOn();
  ITK_TEST_SET_GET_VALUE(false, process->GetReleaseDataFlag());

  ITK_TEST_SET_GET_VALUE(true, process->GetReleaseDataBeforeUpdateFlag());
  process->SetReleaseDataBeforeUpdateFlag(false);
  ITK_TEST_SET_GET_VALUE(false, process->GetReleaseDataBeforeUpdateFlag());
  process->ReleaseDataBeforeUpdateFlagOn();
  ITK_TEST_SET_GET_VALUE(true, process->GetReleaseDataBeforeUpdateFlag());
  process->ReleaseDataBeforeUpdateFlagOff();
  ITK_TEST_SET_GET_VALUE(false, process->GetReleaseDataBeforeUpdateFlag());
  process->ReleaseDataBeforeUpdateFlagOn();
  ITK_TEST_SET_GET_VALUE(true, process->GetReleaseDataBeforeUpdateFlag());

  ITK_TEST_EXPECT_TRUE(itk::MultiThreaderBase::GetGlobalDefaultNumberOfThreads() <= process->GetNumberOfWorkUnits());
  process->SetNumberOfWorkUnits(11);
  ITK_TEST_SET_GET_VALUE(11, process->GetNumberOfWorkUnits());
  process->SetNumberOfWorkUnits(0);
  ITK_TEST_SET_GET_VALUE(1, process->GetNumberOfWorkUnits());
  process->SetNumberOfWorkUnits(itk::NumericTraits<itk::ThreadIdType>::max());
  ITK_TEST_EXPECT_TRUE(itk::MultiThreaderBase::GetGlobalMaximumNumberOfThreads() <= process->GetNumberOfWorkUnits());
  process->SetNumberOfWorkUnits(itk::MultiThreaderBase::GetGlobalDefaultNumberOfThreads());
  ITK_TEST_SET_GET_VALUE(itk::MultiThreaderBase::GetGlobalDefaultNumberOfThreads(), process->GetNumberOfWorkUnits());

  // not sure what to test with that method - at least test that it exist
  itk::MultiThreaderBase::Pointer multiThreader = process->GetMultiThreader();
  ITK_TEST_SET_GET_VALUE(true, multiThreader.IsNotNull());

  // create some data object that will be used as input and output
  itk::TestDataObject::Pointer input0 = itk::TestDataObject::New();
  itk::TestDataObject::Pointer input1 = itk::TestDataObject::New();

  itk::TestDataObject::Pointer output1 = itk::TestDataObject::New();

  // default input values
  ITK_TEST_SET_GET_NULL_VALUE(process->GetPrimaryInput());
  ITK_TEST_SET_GET_NULL_VALUE(process->GetInput("Primary"));
  ITK_TEST_SET_GET_NULL_VALUE(process->GetInput("toto"));
  ITK_TEST_SET_GET_NULL_VALUE(process->GetInput(0));
  ITK_TEST_SET_GET_NULL_VALUE(process->GetInput(1));

  process->Print(std::cout);
  process->SetInput("Primary", input0);
  names = process->GetInputNames();
  ITK_TEST_SET_GET_VALUE(1, names.size());

  ITK_TEST_SET_GET(input0, process->GetPrimaryInput());
  ITK_TEST_SET_GET(input0, process->GetInput(0));
  ITK_TEST_SET_GET(input0, process->GetInput("Primary"));
  ITK_TEST_SET_GET_VALUE(1, process->GetNumberOfIndexedInputs());
  process->SetPrimaryInput(nullptr);

  ITK_TEST_SET_GET_NULL_VALUE(process->GetPrimaryInput());
  ITK_TEST_SET_GET_NULL_VALUE(process->GetInput(0));
  ITK_TEST_SET_GET_NULL_VALUE(process->GetInput("Primary"));
  process->SetNthInput(0, input0);

  ITK_TEST_SET_GET(input0, process->GetPrimaryInput());
  ITK_TEST_SET_GET(input0, process->GetInput(0));
  ITK_TEST_SET_GET(input0, process->GetInput("Primary"));
  process->SetPrimaryInputName("First");

  ITK_TEST_SET_GET(input0, process->GetPrimaryInput());
  ITK_TEST_SET_GET(input0, process->GetInput(0));
  ITK_TEST_SET_GET(input0, process->GetInput("First"));

  process->SetNthInput(1, input1);
  ITK_TEST_SET_GET(input1, process->GetInput(1));
  ITK_TEST_SET_GET_VALUE(2, process->GetNumberOfIndexedInputs());
  process->SetNthInput(1, nullptr);
  ITK_TEST_SET_GET_NULL_VALUE(process->GetInput(1));
  process->SetNthInput(1, input1);

  process->Print(std::cout);
  process->RemoveInput(1);
  ITK_TEST_SET_GET_VALUE(1, process->GetNumberOfIndexedInputs());
  process->SetNthInput(1, input1);
  ITK_TEST_SET_GET_VALUE(2, process->GetNumberOfIndexedInputs());
  ITK_TEST_SET_GET(input1, process->GetInput(1));
  process->RemoveInput(10);
  ITK_TEST_SET_GET_VALUE(2, process->GetNumberOfIndexedInputs());
  process->PopBackInput();
  ITK_TEST_SET_GET_VALUE(1, process->GetNumberOfIndexedInputs());

  itk::TestDataObject::Pointer output = itk::TestDataObject::New();
  process->SetNthOutput(0, output);

  process->SetNumberOfRequiredInputs(1);
  process->AddRequiredInputName("Foo");
  process->Print(std::cout);
  ITK_TRY_EXPECT_EXCEPTION(process->Update());
  process->SetInput("Foo", input1);
  process->Print(std::cout);
  ITK_TRY_EXPECT_NO_EXCEPTION(process->Update());
  process->AddRequiredInputName("Bar");
  process->SetInput("Bar", input1);
  process->SetNumberOfRequiredInputs(0);
  process->Print(std::cout);
  ITK_TRY_EXPECT_NO_EXCEPTION(process->Update());
  ITK_TRY_EXPECT_EXCEPTION(process->SetInput("", input1));

  names = process->GetRequiredInputNames();
  ITK_TEST_SET_GET_VALUE(2, names.size());
  process->RemoveRequiredInputName("Foo");
  process->RemoveRequiredInputName("Bar");
  ITK_TEST_SET_GET_VALUE(0, process->GetRequiredInputNames().size());
  process->SetRequiredInputNames(names);
  process->Print(std::cout);
  ITK_TEST_SET_GET_VALUE(2, process->GetRequiredInputNames().size());
  process->SetPrimaryOutputName("Outy");
  const std::string primaryOutputName = process->GetPrimaryOutputName();
  ITK_TEST_EXPECT_EQUAL("Outy", primaryOutputName);

  process = itk::TestProcessObject::New();
  process->SetPrimaryInputName("Image1");
  ITK_TRY_EXPECT_EXCEPTION(process->AddRequiredInputName(""));
  process->Print(std::cout);
  ITK_TEST_EXPECT_EQUAL(1, process->GetNumberOfRequiredInputs());
  process->AddRequiredInputName("Image2");
  ITK_TEST_EXPECT_EQUAL(1, process->GetNumberOfRequiredInputs());
  ITK_TEST_EXPECT_EQUAL(0, process->GetNumberOfValidRequiredInputs());
  ITK_TRY_EXPECT_EXCEPTION(process->VerifyPreconditions());
  process->SetInput("Image1", input0);
  ITK_TEST_EXPECT_EQUAL(1, process->GetNumberOfValidRequiredInputs());
  ITK_TRY_EXPECT_EXCEPTION(process->VerifyPreconditions());
  process->SetInput("Image2", input0);
  ITK_TEST_EXPECT_EQUAL(1, process->GetNumberOfValidRequiredInputs());
  ITK_TRY_EXPECT_NO_EXCEPTION(process->VerifyPreconditions());

  process->SetNumberOfRequiredInputs(2);
  process->SetInput("Image2", nullptr);
  process->SetNthInput(10, input0);
  ITK_TEST_EXPECT_EQUAL(1, process->GetNumberOfValidRequiredInputs());
  ITK_TRY_EXPECT_EXCEPTION(process->VerifyPreconditions());
  process->AddInput(input0);
  ITK_TEST_EXPECT_EQUAL(2, process->GetNumberOfValidRequiredInputs());
  ITK_TRY_EXPECT_EXCEPTION(process->VerifyPreconditions());
  process->SetInput("Image2", input0);
  ITK_TEST_EXPECT_EQUAL(2, process->GetNumberOfValidRequiredInputs());
  ITK_TRY_EXPECT_NO_EXCEPTION(process->VerifyPreconditions());

  // Test AddOptionalInputName
  process = itk::TestProcessObject::New();
  process->AddOptionalInputName("OptImage");
  ITK_TEST_EXPECT_EQUAL(0, process->GetRequiredInputNames().size());
  ITK_TEST_EXPECT_EQUAL(1, process->GetInputNames().size());
  ITK_TEST_EXPECT_EQUAL("OptImage", process->GetInputNames().front());
  ITK_TEST_EXPECT_TRUE(!process->IsRequiredInputName("OptImage"));

  process = itk::TestProcessObject::New();
  process->SetNumberOfRequiredInputs(1);
  process->AddOptionalInputName("OptImage", 1);
  ITK_TEST_EXPECT_EQUAL(1, process->GetRequiredInputNames().size());
  ITK_TEST_EXPECT_EQUAL(2, process->GetInputNames().size());
  ITK_TEST_EXPECT_TRUE(!process->IsRequiredInputName("OptImage"));
  process->SetInput("OptImage", input0);
  ITK_TEST_EXPECT_EQUAL(input0, process->GetInput("OptImage"));

  process = itk::TestProcessObject::New();
  process->SetNumberOfRequiredInputs(1);
  process->AddOptionalInputName("OptImage", 1);
  ITK_TEST_EXPECT_EQUAL(1, process->GetRequiredInputNames().size());
  ITK_TEST_EXPECT_EQUAL(2, process->GetInputNames().size());
  ITK_TEST_EXPECT_TRUE(!process->IsRequiredInputName("OptImage"));

  process->SetInput("OptImage", input0);
  ITK_TEST_EXPECT_EQUAL(input0, process->GetInput("OptImage"));
  ITK_TEST_EXPECT_EQUAL(input0, process->GetInput(1));
  ITK_TEST_EXPECT_EQUAL(0, process->GetNumberOfValidRequiredInputs());
  ITK_TRY_EXPECT_EXCEPTION(process->VerifyPreconditions());

  process->SetPrimaryInput(input0);
  ITK_TEST_EXPECT_EQUAL(1, process->GetNumberOfValidRequiredInputs());
  ITK_TRY_EXPECT_NO_EXCEPTION(process->VerifyPreconditions());

  process->SetInput("OptImage", nullptr);
  ITK_TEST_EXPECT_EQUAL(1, process->GetNumberOfValidRequiredInputs());
  ITK_TRY_EXPECT_NO_EXCEPTION(process->VerifyPreconditions());

  process = itk::TestProcessObject::New();
  process->SetNthInput(1, input0);
  process->AddOptionalInputName("OptImage", 1);
  ITK_TEST_EXPECT_EQUAL(input0, process->GetInput("OptImage"));
  ITK_TEST_EXPECT_EQUAL(input0, process->GetInput(1));

  process = itk::TestProcessObject::New();
  process->SetInput("OptImage", input0);
  process->SetNthInput(1, input1);
  process->AddOptionalInputName("OptImage", 1);
  ITK_TEST_EXPECT_EQUAL(input0, process->GetInput("OptImage"));
  ITK_TEST_EXPECT_EQUAL(input0, process->GetInput(1));

  process = itk::TestProcessObject::New();
  process->SetNumberOfRequiredInputs(2);
  process->SetPrimaryInputName("Image1");
  process->AddRequiredInputName("Image2");
  ITK_TEST_EXPECT_EQUAL(2, process->GetNumberOfRequiredInputs());
  ITK_TEST_EXPECT_EQUAL(0, process->GetNumberOfValidRequiredInputs());
  process->SetInput("Image1", input0);
  ITK_TEST_EXPECT_EQUAL(1, process->GetNumberOfValidRequiredInputs());
  process->SetInput("Image2", input0);
  process->RemoveRequiredInputName("Image2");

  ITK_TRY_EXPECT_EXCEPTION(process->AddRequiredInputName("", 1));
  process->AddRequiredInputName("Image2", 1);
  process->Print(std::cout);
  ITK_TEST_EXPECT_EQUAL(2, process->GetNumberOfRequiredInputs());
  ITK_TEST_EXPECT_EQUAL(2, process->GetNumberOfValidRequiredInputs());
  ITK_TRY_EXPECT_NO_EXCEPTION(process->VerifyPreconditions());

  // testing SetNumberOfIndexedInputs
  process = itk::TestProcessObject::New();
  process->SetNumberOfIndexedInputs(1);
  ITK_TEST_SET_GET_VALUE(0, process->GetNumberOfIndexedInputs());
  ITK_TEST_SET_GET_NULL_VALUE(process->GetPrimaryInput());

  process->SetPrimaryInput(input0);
  ITK_TEST_SET_GET_VALUE(1, process->GetNumberOfIndexedInputs());
  ITK_TEST_SET_GET_VALUE(input0, process->GetPrimaryInput());

  process->SetNumberOfIndexedInputs(0);
  ITK_TEST_SET_GET_VALUE(0, process->GetNumberOfIndexedInputs());
  ITK_TEST_SET_GET_NULL_VALUE(process->GetPrimaryInput());

  process->SetNumberOfIndexedInputs(3);
  ITK_TEST_SET_GET_VALUE(3, process->GetNumberOfIndexedInputs());
  ITK_TEST_SET_GET_NULL_VALUE(process->GetPrimaryInput());
  process->SetNthInput(1, input1);
  ITK_TEST_SET_GET_VALUE(input1, process->GetInput(1));
  process->SetNthInput(2, input1);
  ITK_TEST_SET_GET_VALUE(input1, process->GetInput(2));
  process->SetNumberOfIndexedInputs(1);
  ITK_TEST_SET_GET_VALUE(0, process->GetNumberOfIndexedInputs());
  ITK_TEST_SET_GET_NULL_VALUE(process->GetPrimaryInput());

  // testing RemoveInput
  process = itk::TestProcessObject::New();
  process->SetNumberOfIndexedInputs(2);
  process->SetPrimaryInput(input0);
  ITK_TEST_SET_GET_VALUE(2, process->GetNumberOfIndexedInputs());
  process->RemoveInput("Primary");
  ITK_TEST_SET_GET_NULL_VALUE(process->GetPrimaryInput());
  ITK_TEST_SET_GET_VALUE(2, process->GetNumberOfIndexedInputs());
  ITK_TEST_SET_GET_VALUE(1, process->GetNumberOfInputs());

  process->AddRequiredInputName("Req");
  process->SetInput("Req", input0);
  ITK_TEST_SET_GET_VALUE(input0, process->GetInput("Req"));
  process->RemoveInput("Req");
  ITK_TEST_SET_GET_NULL_VALUE(process->GetInput("Req"));
  ITK_TEST_EXPECT_TRUE(process->IsRequiredInputName("Req"));

  process->SetInput("name", input0);
  ITK_TEST_SET_GET_VALUE(input0, process->GetInput("name"));
  process->RemoveInput("name");
  ITK_TEST_SET_GET_NULL_VALUE(process->GetInput("name"));
  ITK_TEST_SET_GET_VALUE(2, process->GetNumberOfInputs());

  process->RemoveInput(99);
  ITK_TEST_SET_GET_NULL_VALUE(process->GetInput("Req"));
  ITK_TEST_EXPECT_TRUE(process->IsRequiredInputName("Req"));
  ITK_TEST_SET_GET_VALUE(2, process->GetNumberOfIndexedInputs());

  process->RemoveInput(1);
  ITK_TEST_SET_GET_VALUE(0, process->GetNumberOfIndexedInputs());

  // testing SetNumberOfIndexedOutputs
  process = itk::TestProcessObject::New();
  process->SetNumberOfIndexedOutputs(1);
  ITK_TEST_SET_GET_VALUE(0, process->GetNumberOfIndexedOutputs());
  ITK_TEST_SET_GET_NULL_VALUE(process->GetPrimaryOutput());

  process->SetPrimaryOutput(input0);
  ITK_TEST_SET_GET_VALUE(1, process->GetNumberOfIndexedOutputs());
  ITK_TEST_SET_GET_VALUE(input0, process->GetPrimaryOutput());

  process->SetNumberOfIndexedOutputs(0);
  ITK_TEST_SET_GET_VALUE(0, process->GetNumberOfIndexedOutputs());
  ITK_TEST_SET_GET_NULL_VALUE(process->GetPrimaryOutput());

  process->SetNumberOfIndexedOutputs(3);
  ITK_TEST_SET_GET_VALUE(3, process->GetNumberOfIndexedOutputs());
  ITK_TEST_SET_GET_NULL_VALUE(process->GetPrimaryOutput());
  process->SetNthOutput(1, input1);
  ITK_TEST_SET_GET_VALUE(input1, process->GetOutput(1));
  process->SetNthOutput(2, input1);
  ITK_TEST_SET_GET_VALUE(input1, process->GetOutput(2));
  process->SetNumberOfIndexedOutputs(1);
  ITK_TEST_SET_GET_VALUE(0, process->GetNumberOfIndexedOutputs());
  ITK_TEST_SET_GET_NULL_VALUE(process->GetPrimaryOutput());

  process->SetNumberOfRequiredOutputs(1);
  std::cout << process;
  process->SetPrimaryOutput(input0);
  std::cout << process;

  // test default state where null  is primary output
  process = itk::TestProcessObject::New();
  ITK_TEST_SET_GET_VALUE(0, process->GetNumberOfOutputs());
  ITK_TEST_SET_GET_NULL_VALUE(process->GetPrimaryOutput());
  process->SetOutput("Out", output1);
  process->SetPrimaryOutputName("Out");
  ITK_TEST_SET_GET_VALUE(output1, process->GetPrimaryOutput());
  ITK_TEST_SET_GET_NULL_VALUE(process->GetOutput("Primary"));
  process->RemoveOutput("Primary");

  process = itk::TestProcessObject::New();
  process->SetOutput("New", nullptr);

  std::cout << "Test PASSED" << std::endl;
  return (EXIT_SUCCESS);
}
