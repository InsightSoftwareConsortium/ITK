#include <iostream>
#include <math.h>

#include "itkTemporalProcessObject.h"
#include "itkTemporalDataObject.h"

/** Set up dummy implementations of TemporalProcessObject and
 * TemporalDataObject for testing
 */
namespace itk
{
namespace test
{


/** \class DummyTemporalDataObject
 * Create TemporaDataObject subclass that does nothing, but overrides some
 * methods to provide debug output
 */
class DummyTemporalDataObject:public TemporalDataObject
{
public:

  /** typedefs */
  typedef DummyTemporalDataObject     Self;
  typedef TemporalDataObject          Superclass;
  typedef SmartPointer< Self >        Pointer;
  typedef SmartPointer< const Self >  ConstPointer;

  /** Class macros */
  itkNewMacro(Self);
  itkTypeMacro(DummyTemporalDataObject, TemporalDataObject);

  /** Override update for debug output */
  virtual void Update()
    {
    //std::cout << "Calling Update from temporal data object" << std::endl;
    Superclass::Update();
    }

  /** Override UpdateOutputInformation for debug output */
  virtual void UpdateOutputInformation()
    {
    //std::cout << "Calling UpdateOutputInformation from temporal data object" << std::endl;
    Superclass::UpdateOutputInformation();
    }

  /** Override PropagateRequestedRegion for debug output */
  virtual void PropagateRequestedRegion() throw (itk::InvalidRequestedRegionError)
    {
    //std::cout << "Calling PropagateRequestedRegion from temporal data object" << std::endl;
    Superclass::PropagateRequestedRegion();
    }

  /** Override UpdateOutputData for debug output */
  virtual void UpdateOutputData()
    {
    std::cout << "      UpdateOutputData from temporal data object" << std::endl;
    //std::cout << "UpdateMTime = " << this->GetUpdateMTime() << " PipelineMTime = "
    //          << this->GetPipelineMTime() << std::endl;
    Superclass::UpdateOutputData();
    }

  /** Fill buffer with X new frames */
  void SetBufferToXNewFrames(unsigned int x)
    {
    // Set the internal number of buffers
    m_DataObjectBuffer->SetNumberOfBuffers(x);

    for (unsigned int i = 0; i < x; ++i)
      {
      // Create a new DataObject
      DataObject::Pointer obj = dynamic_cast<DataObject*>(DataObject::New().GetPointer());

      // Append to the end of the buffer
      m_DataObjectBuffer->MoveHeadForward();
      m_DataObjectBuffer->SetBufferContents(0, obj);
      }

    // Set buffered region info
    m_BufferedTemporalRegion.SetFrameStart(0);
    m_BufferedTemporalRegion.SetFrameDuration(x);
    }

  /** Append the supplied data object */
  void AppendDataObject(DataObject* obj)
    {
    m_DataObjectBuffer->MoveHeadForward();
    m_DataObjectBuffer->SetBufferContents(0, obj);

    if (m_BufferedTemporalRegion.GetFrameDuration() < m_DataObjectBuffer->GetNumberOfBuffers())
      {
      m_BufferedTemporalRegion.SetFrameDuration(
        m_BufferedTemporalRegion.GetFrameDuration() + 1);
      }
    else
      {
      m_BufferedTemporalRegion.SetFrameStart(
        m_BufferedTemporalRegion.GetFrameStart() + 1);
      }
    }

  /** Get a bufferd frame */
  DataObject::Pointer GetFrame(unsigned long frameNumber)
    {
    // if nothing buffered, just fail
    if (m_BufferedTemporalRegion.GetFrameDuration() == 0)
      {
      return NULL;
      }

    // make sure we have the desired frame buffered
    unsigned long bufStart = m_BufferedTemporalRegion.GetFrameStart();
    unsigned long bufEnd = bufStart + m_BufferedTemporalRegion.GetFrameDuration() - 1;
    if (frameNumber < bufStart || frameNumber > bufEnd)
      {
      return NULL;
      }

    // If we can, fetch the desired frame
    long frameOffset = frameNumber - bufEnd;  // Should be negative
    return m_DataObjectBuffer->GetBufferContents(frameOffset);
    }

};


/** \class DummyTemporalProcessObject
 * Create TemporalProcessObject subclass that does nothing, but implements
 * New() and TemporalStreamingGenerateData()
 */
class DummyTemporalProcessObject:public TemporalProcessObject
{
public:
  /** typedefs */
  typedef DummyTemporalProcessObject  Self;
  typedef TemporalProcessObject       Superclass;
  typedef SmartPointer< Self >        Pointer;
  typedef SmartPointer< const Self >  ConstPointer;

  /** Class macros */
  itkNewMacro(Self);
  itkTypeMacro(DummyTemporalProcessObject, TemporalProcessObject);

  /*-REQUIRED IMPLEMENTATIONS------------------------------------------------*/

  /** TemporalStreamingGenerateData */
  virtual void TemporalStreamingGenerateData(unsigned long outputFrameStart)
    {
    // Report
    std::cout << "**(ID = " << m_IdNumber << ") - TemporalStreamingGenerateData" << std::endl;
    std::cout << "  -> output start frame: " << outputFrameStart << std::endl;

    unsigned long inputStart = this->GetInput()->GetRequestedTemporalRegion().GetFrameStart();
    unsigned long inputEnd = inputStart +
                      this->GetInput()->GetRequestedTemporalRegion().GetFrameDuration() - 1;
    std::cout << "  -> input requested from " << inputStart << " to " << inputEnd << std::endl;

    // Just pass frames from the input through to the output and add debug info
    for (unsigned int i = 0; i < m_UnitOutputNumberOfFrames; ++i)
      {
      DataObject::Pointer newObj = dynamic_cast<DataObject*>(DataObject::New().GetPointer());

      // Set the output
      this->GetOutput()->AppendDataObject(newObj);
      }

    // Set the buffered output region to match the requested output region
    this->GetOutput()->SetBufferedTemporalRegion(
      this->GetOutput()->GetRequestedTemporalRegion());

    }

  /** GetOutput will return the output on port 0 */
  DummyTemporalDataObject::Pointer GetOutput()
    {
    return dynamic_cast<DummyTemporalDataObject*>(this->TemporalProcessObject::GetOutput(0));
    }

  /** SetInput will set the 0th input */
  void SetInput(TemporalDataObject* tdo)
    {
    this->ProcessObject::SetNthInput(0, tdo);
    }

  /** GetInput gets the 0th input as a DummyTemporalDataObject */
  DummyTemporalDataObject::Pointer GetInput()
    {
    return dynamic_cast<DummyTemporalDataObject*>(this->TemporalProcessObject::GetInput(0));
    }

  /** Get/Set IdNumber */
  itkSetMacro(IdNumber, unsigned int);
  itkGetMacro(IdNumber, unsigned int);

  /** Provide access to m_FrameSkipPerOutput */
  itkSetMacro(FrameSkipPerOutput, int);
  itkGetMacro(FrameSkipPerOutput, int);

  /** Provide access to m_InputStencilCurrentFrameIndex */
  itkSetMacro(InputStencilCurrentFrameIndex, long);
  itkGetMacro(InputStencilCurrentFrameIndex, long);


  /*-DEBUG OVERRIDES---------------------------------------------------------*/

  /** Override Update for debug output */
  virtual void Update()
    {
    std::cout << "(ID = " << m_IdNumber << ") - Update" << std::endl;
    Superclass::Update();
    }

  /** Override UpdateOutputData for debug output */
  virtual void UpdateOutputData(DataObject* dobj)
    {
    std::cout << "(ID = " << m_IdNumber << ") - UpdateOutputData" << std::endl;
    Superclass::UpdateOutputData(dobj);
    }

  /** Override GenerateData for debug output */
  virtual void GenerateData()
    {
    std::cout << "*(ID = " << m_IdNumber << ") - GenerateData" << std::endl;
    //std::cout << "Output Buffered temporal region duration = "
    //          << this->GetOutput()->GetBufferedTemporalRegion().GetFrameDuration() << std::endl;
    Superclass::GenerateData();
    }

  /** Override EnlargeOutputRequestedTemporalRegion for debug output */
  virtual void EnlargeOutputRequestedTemporalRegion(TemporalDataObject* output)
    {
    std::cout << "(ID = " << m_IdNumber << ") - EnlargeOutputRequestedTemporalRegion" << std::endl;
    Superclass::EnlargeOutputRequestedTemporalRegion(output);
    }

  /** Override GenerateInputRequestedTemporalRegion for debug output */
  virtual void GenerateInputRequestedTemporalRegion()
    {
    std::cout << "(ID = " << m_IdNumber << ") - GenerateInputRequestedTemporalRegion" << std::endl;
    Superclass::GenerateInputRequestedTemporalRegion();
    }

protected:

  /** Constructor */
  DummyTemporalProcessObject()
    {
    DummyTemporalDataObject::Pointer po = DummyTemporalDataObject::New();
    this->SetNthOutput(0, po.GetPointer());
    }

private:

  /** ID number used for debugging */
  unsigned int m_IdNumber;

};


} // end namespace test
} // end namespace itk


/**
 * Test functionality of itkTemporalProcessObject
 */
int itkTemporalProcessObjectTest ( int argc, char *argv[] )
{

  //////
  // Set up pipeline
  //////

  // Create 3 new DummyTemporalProcessObjects
  typedef itk::test::DummyTemporalProcessObject TPOType;
  TPOType::Pointer tpo1 = TPOType::New();
  tpo1->SetIdNumber(1);
  TPOType::Pointer tpo2 = TPOType::New();
  tpo2->SetIdNumber(2);
  TPOType::Pointer tpo3 = TPOType::New();
  tpo3->SetIdNumber(3);

  // Set up the Process Objects in a pipeline
  tpo2->SetInput(tpo1->GetOutput());
  tpo3->SetInput(tpo2->GetOutput());

  // Set up the Unit input/output numbers of frames
  tpo1->SetUnitInputNumberOfFrames(3);
  tpo1->SetUnitOutputNumberOfFrames(1);
  tpo2->SetUnitInputNumberOfFrames(3);
  tpo2->SetUnitOutputNumberOfFrames(3);
  tpo3->SetUnitInputNumberOfFrames(2);
  tpo3->SetUnitOutputNumberOfFrames(1);
  tpo3->SetFrameSkipPerOutput(2);

  // Set up frame stencils
  tpo1->SetInputStencilCurrentFrameIndex(1); // "current frame" centered in group of 3
  tpo2->SetInputStencilCurrentFrameIndex(0); // "current frame" at start of group of 3
  tpo3->SetInputStencilCurrentFrameIndex(1); // "current frame" at end of group of 2

  // Create a new TemporalDataObject to pass through the pipeline
  typedef itk::test::DummyTemporalDataObject TDOType;
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
  tdo->SetBufferToXNewFrames(largestRegion.GetFrameDuration());


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
  finalRequest.SetFrameStart(endLargestPossibleRegion.GetFrameStart());
  finalRequest.SetFrameDuration(1);
  itk::test::DummyTemporalDataObject* finalOutput = tpo3->GetOutput();
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
  if (tpo3->GetInput()->GetRequestedTemporalRegion().GetFrameStart() != 1)
    {
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
  if (tpo2->GetInput()->GetRequestedTemporalRegion().GetFrameStart() != 1)
    {
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
  if (tpo1->GetInput()->GetRequestedTemporalRegion().GetFrameStart() != 0)
    {
    std::cerr << "tpo1 requested region start not correct" << std::endl;
    return EXIT_FAILURE;
    }


  //////
  // Test Generation of data
  //////

  //DEBUG
  std::cout << std::endl << "Requested frames at end = "
    << tpo3->GetOutput()->GetRequestedTemporalRegion().GetFrameDuration() << std::endl << std::endl;

  // Call update to execute the entire pipeline
  tpo3->Update();

  itk::test::DummyTemporalDataObject::Pointer outputObject = tpo3->GetOutput();

  std::cout << "Buffered Output Region: "
    << outputObject->GetBufferedTemporalRegion().GetFrameStart()
    << "->" << outputObject->GetBufferedTemporalRegion().GetFrameDuration() << std::endl;

  itk::DataObject::Pointer computedFrame =
    outputObject->GetFrame(endLargestPossibleRegion.GetFrameStart());

  // Return successfully
  return EXIT_SUCCESS;
}
