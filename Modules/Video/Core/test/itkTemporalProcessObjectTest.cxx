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

#include "itkTemporalProcessObject.h"
#include "itkTemporalDataObject.h"

/** Set up dummy implementations of TemporalProcessObject and
 * TemporalDataObject for testing
 */
namespace itk
{
namespace TemporalProcessObjectTest
{

typedef ::itk::SizeValueType       SizeValueType;
typedef ::itk::OffsetValueType     OffsetValueType;
/** \class CallRecord
 * Record of a start or end of a GenerateDataCall from a
 * DummyTemporalProcessObject instance
 */
class CallRecord
{
public:
  enum RecordTypeEnum {START_CALL, END_CALL, MAX_RECORD_TYPE};
  enum MethodTypeEnum {GENERATE_DATA, STREAMING_GENERATE_DATA, MAX_METHOD_TYPE};

  /** Constructor that takes necessary info */
  CallRecord(SizeValueType callerId, SizeValueType recordType, SizeValueType methodType)
  {
    if (recordType >= MAX_RECORD_TYPE || methodType >= MAX_METHOD_TYPE)
      {
      throw;
      }
    m_CallerId = callerId;
    m_RecordType = recordType;
    m_MethodType = methodType;
  }

  /** Access members */
  SizeValueType GetCallerId() const
  {
    return m_CallerId;
  }
  SizeValueType GetRecordType() const
  {
    return m_RecordType;
  }
  SizeValueType GetMethodType() const
  {
    return m_MethodType;
  }

  /** Print out nicely */
  void Print()
  {
    std::cout << "ID: " << m_CallerId << " -> ";
    if (m_MethodType == GENERATE_DATA)
      {
      std::cout << "GenerateData - ";
      }
    else if(m_MethodType == STREAMING_GENERATE_DATA)
      {
      std::cout << "TemporalStreamingGenerateData - ";
      }

    if (m_RecordType == START_CALL)
      {
      std::cout << " START";
      }
    else if(m_RecordType == END_CALL)
      {
      std::cout << " END";
      }
    std::cout << std::endl;
  }

  /** Comparison operators */
  bool operator==(const CallRecord& other) const
  {
    return (m_CallerId == other.GetCallerId() &&
            m_RecordType == other.GetRecordType() &&
            m_MethodType == other.GetMethodType() );
  }

  bool operator!=(const CallRecord& other) const
  {
    return !(*this == other);
  }

protected:
  SizeValueType m_CallerId;
  SizeValueType m_RecordType;
  SizeValueType m_MethodType;
};

/**
 * Static list of CallRecord items representing the stack trace of
 * calls to GenerateData and TemporalStreamingGenerateData
 */
std::vector<CallRecord> m_CallStack;

/** \class DummyTemporalDataObject
 * Create TemporaDataObject subclass that does nothing, but overrides some
 * methods to provide debug output
 */
class DummyTemporalDataObject : public TemporalDataObject
{
public:

  /** typedefs */
  typedef DummyTemporalDataObject    Self;
  typedef TemporalDataObject         Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Class macros */
  itkNewMacro(Self);
  itkTypeMacro(DummyTemporalDataObject, TemporalDataObject);

  /** Override update for debug output */
  virtual void Update() ITK_OVERRIDE
  {
    //std::cout << "Calling Update from temporal data object" << std::endl;
    Superclass::Update();
  }

  /** Override UpdateOutputInformation for debug output */
  virtual void UpdateOutputInformation() ITK_OVERRIDE
  {
    //std::cout << "Calling UpdateOutputInformation from temporal data object"
    // << std::endl;
    Superclass::UpdateOutputInformation();
  }

  /** Override PropagateRequestedRegion for debug output */
  virtual void PropagateRequestedRegion() throw (itk::InvalidRequestedRegionError) ITK_OVERRIDE
  {
    Superclass::PropagateRequestedRegion();
  }

  /** Override UpdateOutputData for debug output */
  virtual void UpdateOutputData() ITK_OVERRIDE
  {
    std::cout << "      UpdateOutputData from temporal data object" << std::endl;

    //DEBUG
    std::cout << "Buffered region outside: " << this->RequestedRegionIsOutsideOfTheBufferedRegion()
              << std::endl;
    Superclass::UpdateOutputData();
  }

  /** Fill buffer with X new frames */
  void SetBufferToXNewFrames(SizeValueType  x)
  {
    // Set the internal number of buffers
    m_DataObjectBuffer->SetNumberOfBuffers(x);

    for (SizeValueType i = 0; i < x; ++i)
      {
      // Create a new DataObject
      DataObject::Pointer obj = dynamic_cast<DataObject*>(DataObject::New().GetPointer() );

      // Append to the end of the buffer
      m_DataObjectBuffer->MoveHeadForward();
      m_DataObjectBuffer->SetBufferContents(0, obj);
      }

    // Set buffered region info
    m_BufferedTemporalRegion.SetFrameStart(0);
    m_BufferedTemporalRegion.SetFrameDuration(x);
  }

  /** Append the supplied data object */
  void SetObjectAtFrame(SizeValueType frameNumber, DataObject* obj)
  {
    m_DataObjectBuffer->SetBufferContents(frameNumber, obj);
  }

  /** Get a bufferd frame */
  DataObject::Pointer GetFrame(SizeValueType frameNumber)
  {
    // if nothing buffered, just fail
    if (m_BufferedTemporalRegion.GetFrameDuration() == 0)
      {
      return ITK_NULLPTR;
      }

    // make sure we have the desired frame buffered
    SizeValueType bufStart = m_BufferedTemporalRegion.GetFrameStart();
    SizeValueType bufEnd = bufStart + m_BufferedTemporalRegion.GetFrameDuration() - 1;
    if (frameNumber < bufStart || frameNumber > bufEnd)
      {
      return ITK_NULLPTR;
      }

    // If we can, fetch the desired frame
    OffsetValueType frameOffset = frameNumber - bufEnd;  // Should be negative
    return m_DataObjectBuffer->GetBufferContents(frameOffset);
  }

};

/** \class DummyTemporalProcessObject
 * Create TemporalProcessObject subclass that does nothing, but implements
 * New() and TemporalStreamingGenerateData()
 */
class DummyTemporalProcessObject : public TemporalProcessObject
{
public:
  /** typedefs */
  typedef DummyTemporalProcessObject Self;
  typedef TemporalProcessObject      Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Class macros */
  itkNewMacro(Self);
  itkTypeMacro(DummyTemporalProcessObject, TemporalProcessObject);

  /*-REQUIRED IMPLEMENTATIONS------------------------------------------------*/

  /** TemporalStreamingGenerateData */
  virtual void TemporalStreamingGenerateData() ITK_OVERRIDE
  {
    // Create a START entry in the stack trace
    m_CallStack.push_back(CallRecord(m_IdNumber,
                                     CallRecord::START_CALL, CallRecord::STREAMING_GENERATE_DATA) );

    // Report
    SizeValueType outputStart = this->GetOutput()->GetRequestedTemporalRegion().GetFrameStart();
    std::cout << "**(ID = " << m_IdNumber << ") - TemporalStreamingGenerateData" << std::endl;
    std::cout << "  -> output requested from: " << outputStart << " to "
              << this->GetOutput()->GetRequestedTemporalRegion().GetFrameDuration() +
    outputStart - 1
              << std::endl;

    SizeValueType inputStart = this->GetInput()->GetRequestedTemporalRegion().GetFrameStart();
    SizeValueType inputEnd = inputStart +
      this->GetInput()->GetRequestedTemporalRegion().GetFrameDuration() - 1;
    std::cout << "  -> input requested from " << inputStart << " to " << inputEnd << std::endl;
    std::cout << "  -> input buffered from "
              << this->GetInput()->GetBufferedTemporalRegion().GetFrameStart() << " to "
              << this->GetInput()->GetBufferedTemporalRegion().GetFrameStart() +
    this->GetInput()->GetBufferedTemporalRegion().GetFrameDuration() - 1
              << std::endl;

    // Get the list of unbuffered frames
    TemporalRegion unbufferedRegion = this->GetOutput()->GetUnbufferedRequestedTemporalRegion();
    std::cout << unbufferedRegion << std::endl;

    // Make sure that the requested output duration matches the unit output
    // duration
    SizeValueType numFramesOut =
      this->GetOutput()->GetRequestedTemporalRegion().GetFrameDuration();
    if (numFramesOut != m_UnitOutputNumberOfFrames)
      {
      itkExceptionMacro("Requested non-unit number of output frames");
      }

    // Just pass frames from the input through to the output and add debug info
    for (SizeValueType i = outputStart; i < outputStart + numFramesOut; ++i)
      {
      DataObject::Pointer newObj = dynamic_cast<DataObject*>(DataObject::New().GetPointer() );

      // Set the output
      this->GetOutput()->SetObjectAtFrame(i, newObj);
      }

    // Set the buffered region to match the requested region
    //this->GetOutput()->SetBufferedTemporalRegion(this->GetOutput()->GetRequestedTemporalRegion());

    // Create an END entry in the stack trace
    m_CallStack.push_back(CallRecord(m_IdNumber,
                                     CallRecord::END_CALL, CallRecord::STREAMING_GENERATE_DATA) );

  }

  /** Allow the UnitInputNumberOfFrames to be set */
  virtual void SetUnitInputNumberOfFrames( const SizeValueType numberOfFrames ) ITK_OVERRIDE
    {
    itkDebugMacro("setting UnitInputNumberOfFrames to " << numberOfFrames);
    if ( this->m_UnitInputNumberOfFrames != numberOfFrames )
      {
      this->m_UnitInputNumberOfFrames = numberOfFrames;
      this->Modified();
      }
    }

  /** Allow the UnitOutputNumberOfFrames to be set */
  virtual void SetUnitOutputNumberOfFrames( const SizeValueType numberOfFrames ) ITK_OVERRIDE
    {
    itkDebugMacro("setting UnitOutputNumberOfFrames to " << numberOfFrames);
    if ( this->m_UnitOutputNumberOfFrames != numberOfFrames )
      {
      this->m_UnitOutputNumberOfFrames = numberOfFrames;
      this->Modified();
      }
    }

  /** GetOutput will return the output on port 0 */
  DummyTemporalDataObject::Pointer GetOutput()
  {
    return dynamic_cast<DummyTemporalDataObject*>(this->TemporalProcessObject::GetOutput(0) );
  }

  /** SetInput will set the 0th input */
  using Superclass::SetInput;
  void SetInput(TemporalDataObject* tdo)
  {
    this->ProcessObject::SetNthInput(0, tdo);
  }

  /** GetInput gets the 0th input as a DummyTemporalDataObject */
  DummyTemporalDataObject::Pointer GetInput()
  {
    return dynamic_cast<DummyTemporalDataObject*>(this->TemporalProcessObject::GetInput(0) );
  }

  /** Get/Set IdNumber */
  itkSetMacro(IdNumber, SizeValueType);
  itkGetMacro(IdNumber, SizeValueType);

  /** Provide access to m_FrameSkipPerOutput */
  virtual void SetFrameSkipPerOutput ( const OffsetValueType frameSkip ) ITK_OVERRIDE
    {
    itkDebugMacro("setting FrameSkipPerOutput to " << frameSkip);
    if ( this->m_FrameSkipPerOutput != frameSkip )
      {
      this->m_FrameSkipPerOutput = frameSkip;
      this->Modified();
      }
    }

  itkGetMacro(FrameSkipPerOutput, OffsetValueType);

  /** Provide access to m_InputStencilCurrentFrameIndex */
  virtual void SetInputStencilCurrentFrameIndex ( const SizeValueType inputStencil ) ITK_OVERRIDE
    {
    itkDebugMacro("setting InputStencilCurrentFrameIndex to " << inputStencil);
    if ( this->m_InputStencilCurrentFrameIndex != inputStencil )
      {
      this->m_InputStencilCurrentFrameIndex = inputStencil;
      this->Modified();
      }
    }
  virtual SizeValueType GetInputStencilCurrentFrameIndex() ITK_OVERRIDE
    {
    return this->m_InputStencilCurrentFrameIndex;
    }

  /*-DEBUG OVERRIDES---------------------------------------------------------*/

  /** Override Update for debug output */
  virtual void Update() ITK_OVERRIDE
  {
    std::cout << "(ID = " << m_IdNumber << ") - Update" << std::endl;
    Superclass::Update();
  }

  /** Override UpdateOutputData for debug output */
  virtual void UpdateOutputData(DataObject* dobj) ITK_OVERRIDE
  {
    std::cout << "(ID = " << m_IdNumber << ") - UpdateOutputData" << std::endl;
    Superclass::UpdateOutputData(dobj);
  }

  /** Override GenerateData for debug output */
  virtual void GenerateData() ITK_OVERRIDE
  {
    // Create a START entry in the stack trace
    m_CallStack.push_back(CallRecord(m_IdNumber,
                                     CallRecord::START_CALL, CallRecord::GENERATE_DATA) );

    std::cout << "*(ID = " << m_IdNumber << ") - GenerateData" << std::endl;
    Superclass::GenerateData();

    // Create an END entry in the stack trace
    m_CallStack.push_back(CallRecord(m_IdNumber,
                                     CallRecord::END_CALL, CallRecord::GENERATE_DATA) );

  }

  /** Override EnlargeOutputRequestedTemporalRegion for debug output */
  virtual void EnlargeOutputRequestedTemporalRegion(TemporalDataObject* output) ITK_OVERRIDE
  {
    std::cout << "(ID = " << m_IdNumber << ") - EnlargeOutputRequestedTemporalRegion" << std::endl;
    Superclass::EnlargeOutputRequestedTemporalRegion(output);
  }

  /** Override GenerateInputRequestedTemporalRegion for debug output */
  virtual void GenerateInputRequestedTemporalRegion() ITK_OVERRIDE
  {
    std::cout << "(ID = " << m_IdNumber << ") - GenerateInputRequestedTemporalRegion" << std::endl;
    Superclass::GenerateInputRequestedTemporalRegion();
  }

protected:

  /** Constructor */
  DummyTemporalProcessObject()
    : m_IdNumber(0)
  {
    DummyTemporalDataObject::Pointer po = DummyTemporalDataObject::New();

    this->SetNthOutput(0, po.GetPointer() );
  }

private:

  /** ID number used for debugging */
  SizeValueType m_IdNumber;

};

} // end namespace TemporalProcessObjectTest
} // end namespace itk

/**
 * Test functionality of itkTemporalProcessObject
 */
int itkTemporalProcessObjectTest( int ,
                                  char* [] )
{

  typedef ::itk::SizeValueType       SizeValueType;
  typedef ::itk::OffsetValueType     OffsetValueType;
  //////
  // Set up pipeline
  //////

  // Create 3 new DummyTemporalProcessObjects
  typedef itk::TemporalProcessObjectTest::DummyTemporalProcessObject TPOType;
  TPOType::Pointer tpo1 = TPOType::New();
  tpo1->SetIdNumber(1);
  TPOType::Pointer tpo2 = TPOType::New();
  tpo2->SetIdNumber(2);
  TPOType::Pointer tpo3 = TPOType::New();
  tpo3->SetIdNumber(3);

  // Set up the Process Objects in a pipeline
  tpo2->SetInput(tpo1->GetOutput() );
  tpo3->SetInput(tpo2->GetOutput() );

  // Set up the Unit input/output numbers of frames
  tpo1->SetUnitInputNumberOfFrames(3);
  tpo1->SetUnitOutputNumberOfFrames(1);
  tpo2->SetUnitInputNumberOfFrames(3);
  tpo2->SetUnitOutputNumberOfFrames(3);
  tpo3->SetUnitInputNumberOfFrames(2);
  tpo3->SetUnitOutputNumberOfFrames(1);
  tpo3->SetFrameSkipPerOutput(2);
  tpo2->GetOutput()->SetNumberOfBuffers(6);

  // Set up frame stencils
  tpo1->SetInputStencilCurrentFrameIndex(1); // "current frame" centered in
                                             // group of 3
  tpo2->SetInputStencilCurrentFrameIndex(0); // "current frame" at start of
                                             // group of 3
  tpo3->SetInputStencilCurrentFrameIndex(1); // "current frame" at end of group
                                             // of 2

  // Create a new TemporalDataObject to pass through the pipeline
  typedef itk::TemporalProcessObjectTest::DummyTemporalDataObject TDOType;
  TDOType::Pointer tdo = TDOType::New();
  tpo1->SetInput(tdo);

  // Set up regions for TemporalDataObject
  itk::TemporalRegion largestRegion;
  itk::TemporalRegion requestedRegion;
  itk::TemporalRegion bufferedRegion;
  largestRegion.SetFrameStart(0);
  largestRegion.SetFrameDuration(20);
  tdo->SetLargestPossibleTemporalRegion(largestRegion);
  requestedRegion.SetFrameStart(0);
  requestedRegion.SetFrameDuration(1);
  tdo->SetRequestedTemporalRegion(requestedRegion);
  bufferedRegion.SetFrameStart(0);
  bufferedRegion.SetFrameDuration(0);
  tdo->SetBufferedTemporalRegion(bufferedRegion);

  // Fill the TemporalDataObject input with frames for the entire region
  tdo->SetBufferToXNewFrames(largestRegion.GetFrameDuration() );

  //////
  // Test results of LargestTemporalRegion computation
  //////

  // Update to get largest possible temporal region information
  tpo3->UpdateOutputInformation();

  // Check largest possible temporal region after propagation
  if (tpo1->GetOutput()->GetLargestPossibleTemporalRegion().GetFrameDuration() != 18)
    {
    std::cerr << "tpo1 largest possible region duration not correct" << std::endl;
    return EXIT_FAILURE;
    }
  if (tpo1->GetOutput()->GetLargestPossibleTemporalRegion().GetFrameStart() != 1)
    {
    std::cerr << "tpo1 largest possible region start not correct" << std::endl;
    return EXIT_FAILURE;
    }
  if (tpo2->GetOutput()->GetLargestPossibleTemporalRegion().GetFrameDuration() != 48)
    {
    std::cerr << "tpo2 largest possible region duration not correct" << std::endl;
    return EXIT_FAILURE;
    }
  if (tpo2->GetOutput()->GetLargestPossibleTemporalRegion().GetFrameStart() != 1)
    {
    std::cerr << "tpo2 largest possible region start not correct" << std::endl;
    return EXIT_FAILURE;
    }
  itk::TemporalRegion endLargestPossibleRegion =
    tpo3->GetOutput()->GetLargestPossibleTemporalRegion();
  if (endLargestPossibleRegion.GetFrameDuration() != 24)
    {
    std::cerr << "tpo3 largest possible region duration not correct" << std::endl;
    return EXIT_FAILURE;
    }
  if (endLargestPossibleRegion.GetFrameStart() != 2)
    {
    std::cerr << "tpo3 largest possible region start not correct" << std::endl;
    return EXIT_FAILURE;
    }

  //////
  // Test results of requested region propagation
  //////

  // Set up requested region for the end of the pipeline
  itk::TemporalRegion finalRequest;
  finalRequest.SetFrameStart(endLargestPossibleRegion.GetFrameStart() );
  finalRequest.SetFrameDuration(1);
  itk::TemporalProcessObjectTest::DummyTemporalDataObject* finalOutput = tpo3->GetOutput();
  finalOutput->SetRequestedTemporalRegion(finalRequest);

  // Update to propagate the requested temporal region
  finalOutput->PropagateRequestedRegion();

  // Check requested region up the pipeline

  // for tpo3, the requested input region should be size 3 because tpo2 can
  // only output in groups of 3
  if (tpo3->GetInput()->GetRequestedTemporalRegion().GetFrameDuration() != 3)
    {
    std::cout << tpo3->GetInput()->GetRequestedTemporalRegion().GetFrameDuration() << std::endl;
    std::cerr << "tpo3 requested region duration not correct" << std::endl;
    return EXIT_FAILURE;
    }
  if (tpo3->GetInput()->GetRequestedTemporalRegion().GetFrameStart() != 3)
    {
    std::cerr << tpo3->GetInput()->GetRequestedTemporalRegion().GetFrameStart() << std::endl;
    std::cerr << "tpo3 requested region start not correct" << std::endl;
    return EXIT_FAILURE;
    }

  // tpo2 is 3->3, so an initial request of 2 gets enlarged to 3 which results
  // in propagating a request for 3 to tpo1
  if (tpo2->GetInput()->GetRequestedTemporalRegion().GetFrameDuration() != 3)
    {
    std::cerr << "tpo2 requested region duration not correct" << std::endl;
    return EXIT_FAILURE;
    }
  if (tpo2->GetInput()->GetRequestedTemporalRegion().GetFrameStart() != 3)
    {
    std::cerr << tpo2->GetInput()->GetRequestedTemporalRegion().GetFrameStart() << std::endl;
    std::cerr << "tpo2 requested region start not correct" << std::endl;
    return EXIT_FAILURE;
    }

  // tpo1 is 3->1 and skips 1 frame for each output, so a request for 3
  // requires 5 as input
  if (tpo1->GetInput()->GetRequestedTemporalRegion().GetFrameDuration() != 5)
    {
    std::cerr << "tpo1 requested region duration not correct" << std::endl;
    return EXIT_FAILURE;
    }
  if (tpo1->GetInput()->GetRequestedTemporalRegion().GetFrameStart() != 2)
    {
    std::cerr << tpo1->GetInput()->GetRequestedTemporalRegion().GetFrameStart() << std::endl;
    std::cerr << "tpo1 requested region start not correct" << std::endl;
    return EXIT_FAILURE;
    }

  //////
  // Test Generation of data
  //////

  // Call update to execute the entire pipeline and track the call stack
  itk::TemporalProcessObjectTest::m_CallStack.clear();
  tpo3->Update();

  // Print out duration of buffered output region
  itk::TemporalProcessObjectTest::DummyTemporalDataObject::Pointer outputObject = tpo3->GetOutput();
  OffsetValueType                                                    outputStart =
    outputObject->GetBufferedTemporalRegion().GetFrameStart();
  SizeValueType outputDuration =
    outputObject->GetBufferedTemporalRegion().GetFrameDuration();
  std::cout << "Buffered Output Region: "
            << outputStart << "->" << outputStart + outputDuration - 1 << std::endl;

  // Create a list of CallRecord items representing the correct
  // stack trace
  typedef itk::TemporalProcessObjectTest::CallRecord RecordType;
  std::vector<RecordType> correctCallStack;

  // GenDat - START - obj 3
  correctCallStack.push_back(
    RecordType(3, RecordType::START_CALL, RecordType::GENERATE_DATA) );

  // GenDat - START - obj 2
  correctCallStack.push_back(
    RecordType(2, RecordType::START_CALL, RecordType::GENERATE_DATA) );

  // GenDat - START - obj 1
  correctCallStack.push_back(
    RecordType(1, RecordType::START_CALL, RecordType::GENERATE_DATA) );

  // TempStreamGenDat - START - obj 1
  correctCallStack.push_back(
    RecordType(1, RecordType::START_CALL, RecordType::STREAMING_GENERATE_DATA) );

  // TempStreamGenDat - END - obj 1
  correctCallStack.push_back(
    RecordType(1, RecordType::END_CALL, RecordType::STREAMING_GENERATE_DATA) );

  // TempStreamGenDat - START - obj 1
  correctCallStack.push_back(
    RecordType(1, RecordType::START_CALL, RecordType::STREAMING_GENERATE_DATA) );

  // TempStreamGenDat - END - obj 1
  correctCallStack.push_back(
    RecordType(1, RecordType::END_CALL, RecordType::STREAMING_GENERATE_DATA) );

  // TempStreamGenDat - START - obj 1
  correctCallStack.push_back(
    RecordType(1, RecordType::START_CALL, RecordType::STREAMING_GENERATE_DATA) );

  // TempStreamGenDat - END - obj 1
  correctCallStack.push_back(
    RecordType(1, RecordType::END_CALL, RecordType::STREAMING_GENERATE_DATA) );

  // GenDat - END - obj 1
  correctCallStack.push_back(
    RecordType(1, RecordType::END_CALL, RecordType::GENERATE_DATA) );

  // TempStreamGenDat - START - obj 2
  correctCallStack.push_back(
    RecordType(2, RecordType::START_CALL, RecordType::STREAMING_GENERATE_DATA) );

  // TempStreamGenDat - END - obj 2
  correctCallStack.push_back(
    RecordType(2, RecordType::END_CALL, RecordType::STREAMING_GENERATE_DATA) );

  // GenDat - END - obj 2
  correctCallStack.push_back(
    RecordType(2, RecordType::END_CALL, RecordType::GENERATE_DATA) );

  // TempStreamGenDat - START - obj 3
  correctCallStack.push_back(
    RecordType(3, RecordType::START_CALL, RecordType::STREAMING_GENERATE_DATA) );

  // TempStreamGenDat - END - obj 3
  correctCallStack.push_back(
    RecordType(3, RecordType::END_CALL, RecordType::STREAMING_GENERATE_DATA) );

  // GenDat - END - obj 3
  correctCallStack.push_back(
    RecordType(3, RecordType::END_CALL, RecordType::GENERATE_DATA) );

  // Check that correct number of calls made
  if (itk::TemporalProcessObjectTest::m_CallStack.size() != correctCallStack.size() )
    {
    std::cerr << "Incorrect number of items in call stack" << std::endl;
    return EXIT_FAILURE;
    }

  // Check that call lists match
  std::cout << std::endl;
  for (SizeValueType i = 0; i < itk::TemporalProcessObjectTest::m_CallStack.size(); ++i)
    {
    std::cout << "Got: ";
    itk::TemporalProcessObjectTest::m_CallStack[i].Print();
    std::cout << "Expected: ";
    correctCallStack[i].Print();
    std::cout << std::endl;

    if (itk::TemporalProcessObjectTest::m_CallStack[i] != correctCallStack[i])
      {
      std::cerr << "Call stacks don't match" << std::endl;
      return EXIT_FAILURE;
      }
    }

  //////
  // Test Generation of next output frame -- Since tpo3 skips two frames of
  // input for every frame of output and tpo2 can only generate 3 outputs at a
  // time, tpo2 must generate 6,7,8 (none of which are already buffered), so
  // the entire pipeline runs again (so the call stack should be the same).
  //////

  // Set the requested region to the next output frame
  finalRequest.SetFrameStart(finalRequest.GetFrameStart() + 1);
  finalOutput = tpo3->GetOutput();
  finalOutput->SetRequestedTemporalRegion(finalRequest);

  // Call update to execute the entire pipeline and track the call stack
  itk::TemporalProcessObjectTest::m_CallStack.clear();
  tpo3->Update();

  // Check that correct number of calls made
  if (itk::TemporalProcessObjectTest::m_CallStack.size() != correctCallStack.size() )
    {
    std::cerr << "Incorrect number of items in call stack. Got: "
              << itk::TemporalProcessObjectTest::m_CallStack.size() << " Expected: "
              << correctCallStack.size() << std::endl;
    return EXIT_FAILURE;
    }

  // Check that call lists match
  std::cout << std::endl;
  for (SizeValueType i = 0; i < itk::TemporalProcessObjectTest::m_CallStack.size(); ++i)
    {
    std::cout << "Got: ";
    itk::TemporalProcessObjectTest::m_CallStack[i].Print();
    std::cout << "Expected: ";
    correctCallStack[i].Print();
    std::cout << std::endl;

    if (itk::TemporalProcessObjectTest::m_CallStack[i] != correctCallStack[i])
      {
      std::cerr << "Call stacks don't match" << std::endl;
      return EXIT_FAILURE;
      }
    }

  //////
  // Call Update again and make sure that nothing happens except one call to
  // GenerateData at the bottom which doesn't end up needing to do anything
  //////
  itk::TemporalProcessObjectTest::m_CallStack.clear();
  tpo3->Update();

  correctCallStack.clear();

  // GenDat - START - obj 3
  correctCallStack.push_back(
    RecordType(3, RecordType::START_CALL, RecordType::GENERATE_DATA) );

  // GenDat - END - obj 3
  correctCallStack.push_back(
    RecordType(3, RecordType::END_CALL, RecordType::GENERATE_DATA) );

  // Check that correct number of calls made
  if (itk::TemporalProcessObjectTest::m_CallStack.size() != correctCallStack.size() )
    {
    std::cerr << "Incorrect number of items in call stack. Got: "
              << itk::TemporalProcessObjectTest::m_CallStack.size() << " Expected: "
              << correctCallStack.size() << std::endl;
    return EXIT_FAILURE;
    }

  // Check that call lists match
  std::cout << std::endl;
  for (SizeValueType i = 0; i < itk::TemporalProcessObjectTest::m_CallStack.size(); ++i)
    {
    std::cout << "Got: ";
    itk::TemporalProcessObjectTest::m_CallStack[i].Print();
    std::cout << "Expected: ";
    correctCallStack[i].Print();
    std::cout << std::endl;

    if (itk::TemporalProcessObjectTest::m_CallStack[i] != correctCallStack[i])
      {
      std::cerr << "Call stacks don't match" << std::endl;
      return EXIT_FAILURE;
      }
    }

  //////
  // Test that the requested temporal region for the output of a temporal
  // process object gets set to the largest possible temporal region if no
  // temporal region has been set
  //////

  // Reset tpo1 and the requsted temporal region of tdo
  tpo1 = TPOType::New();
  itk::TemporalRegion emptyRegion;
  tdo->SetRequestedTemporalRegion(emptyRegion);
  tpo1->SetInput(tdo);
  tpo1->UpdateOutputInformation();

  // Make sure the requested temporal region of tpo1's output is empty
  if (tpo1->GetOutput()->GetRequestedTemporalRegion() != emptyRegion)
    {
    std::cerr << "tpo1's output's requested temporal region not empty before propagate" << std::endl;
    return EXIT_FAILURE;
    }

  tpo1->PropagateRequestedRegion(tpo1->GetOutput() );
  if (tpo1->GetOutput()->GetRequestedTemporalRegion() !=
      tpo1->GetOutput()->GetLargestPossibleTemporalRegion() ||
      tpo1->GetOutput()->GetRequestedTemporalRegion() == emptyRegion)
    {
    std::cerr << "tpo1's output's requested temporal region not set correctly after propagate" << std::endl;
    return EXIT_FAILURE;
    }

  // Test that if largest possible temporal region has infinte duration,
  // request gets set to duration 1
  tpo1 = TPOType::New();
  largestRegion = tdo->GetLargestPossibleTemporalRegion();
  largestRegion.SetFrameDuration(ITK_INFINITE_FRAME_DURATION);
  tdo->SetLargestPossibleTemporalRegion(largestRegion);
  tpo1->SetInput(tdo);
  tpo1->UpdateOutputInformation();
  tpo1->PropagateRequestedRegion(tpo1->GetOutput() );
  if (tpo1->GetOutput()->GetLargestPossibleTemporalRegion().GetFrameDuration() !=
      ITK_INFINITE_FRAME_DURATION ||
      tpo1->GetOutput()->GetRequestedTemporalRegion().GetFrameDuration() != 1)
    {
    std::cerr << "tpo1's output's temporal regions not properly set for infinite input" << std::endl;
    std::cerr << "Requested region duration: "
              << tpo1->GetOutput()->GetRequestedTemporalRegion().GetFrameDuration() << std::endl;
    return EXIT_FAILURE;
    }

  //////
  // Return successfully
  //////
  return EXIT_SUCCESS;
}
