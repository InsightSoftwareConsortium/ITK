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
    std::cout << "Calling Update from temporal data object" << std::endl;
    Superclass::Update();
    }

  /** Override UpdateOutputInformation for debug output */
  virtual void UpdateOutputInformation()
    {
    std::cout << "Calling UpdateOutputInformation from temporal data object" << std::endl;
    Superclass::UpdateOutputInformation();
    }

  /** Override PropagateRequestedRegion for debug output */
  virtual void PropagateRequestedRegion() throw (itk::InvalidRequestedRegionError)
    {
    std::cout << "Calling PropagateRequestedRegion from temporal data object" << std::endl;
    Superclass::PropagateRequestedRegion();
    }

  /** Override UpdateOutputData for debug output */
  virtual void UpdateOutputData()
    {
    std::cout << "Calling UpdateOutputData from temporal data object" << std::endl;
    std::cout << "UpdateMTime = " << this->GetUpdateMTime() << " PipelineMTime = "
              << this->GetPipelineMTime() << std::endl;
    Superclass::UpdateOutputData();
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
    //DEBUG
    std::cout << "Calling TemporalStreamingGenerateData from process object with ID = "
              << m_IdNumber << std::endl;
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
    std::cout << "Calling Update for process object with ID = " << m_IdNumber << std::endl;
    Superclass::Update();
    }

  /** Override UpdateOutputData for debug output */
  virtual void UpdateOutputData(DataObject* dobj)
    {
    std::cout << "Calling UpdateOutputData for process object with ID = "
              << m_IdNumber << std::endl;
    Superclass::UpdateOutputData(dobj);
    }

  /** Override GenerateData for debug output */
  virtual void GenerateData()
    {
    std::cout << "Calling GenerateData for process object with ID = "
              << m_IdNumber << std::endl;
    Superclass::GenerateData();
    }

  /** Override EnlargeOutputRequestedTemporalRegion for debug output */
  virtual void EnlargeOutputRequestedTemporalRegion(TemporalDataObject* output)
    {
    std::cout << "Calling EnlargeOutputRequestedTemporalRegion for process object with ID = "
              << m_IdNumber << std::endl;
    Superclass::EnlargeOutputRequestedTemporalRegion(output);
    }

  /** Override GenerateInputRequestedTemporalRegion for debug output */
  virtual void GenerateInputRequestedTemporalRegion()
    {
    std::cout << "Calling GenerateInputRequestedTemporalRegion for process object with ID = "
              << m_IdNumber << std::endl;
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


  // Return successfully
  return EXIT_SUCCESS;
}
