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

  /** Set the input requested temporal region */
  //virtual void GenerateInputRequestedTemporalRegion(TemporalDataObject* output)
  //  {
  //  // Given the
  //  }

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

  /** Get/Set IdNumber */
  itkSetMacro(IdNumber, unsigned int);
  itkGetMacro(IdNumber, unsigned int);

  /** Provide access to m_FrameSkipPerOutput */
  itkSetMacro(FrameSkipPerOutput, int);
  itkGetMacro(FrameSkipPerOutput, int);


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
  tpo2->SetUnitOutputNumberOfFrames(2);
  tpo3->SetUnitInputNumberOfFrames(2);
  tpo3->SetUnitOutputNumberOfFrames(1);
  tpo3->SetFrameSkipPerOutput(2);

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
    std::cerr << "tpo1 largest possible region not correct" << std::endl;
    std::cout << tpo1->GetOutput()->GetLargestPossibleTemporalRegion().GetFrameDuration() << std::endl;
    return EXIT_FAILURE;
    }
  if (tpo2->GetOutput()->GetLargestPossibleTemporalRegion().GetFrameDuration() != 32)
    {
    std::cerr << "tpo2 largest possible region not correct" << std::endl;
    return EXIT_FAILURE;
    }
  if (tpo3->GetOutput()->GetLargestPossibleTemporalRegion().GetFrameDuration() != 16)
    {
    std::cerr << "tpo3 largest possible region not correct" << std::endl;
    return EXIT_FAILURE;
    }


  //////
  // Test results of requested region propagation
  //////

  // Set up requested region for the end of the pipeline
  itk::TemporalRegion finalRequest;
  finalRequest.SetFrameStart(0);
  finalRequest.SetFrameDuration(1);
  itk::test::DummyTemporalDataObject* finalOutput = tpo3->GetOutput();
  finalOutput->SetRequestedTemporalRegion(finalRequest);

  // Update to propagate the requested temporal region
  finalOutput->PropagateRequestedRegion();

  // Check requested region up the pipeline

  // Return successfully
  return EXIT_SUCCESS;
}
