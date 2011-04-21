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
#ifndef __itkTemporalProcessObject_cxx
#define __itkTemporalProcessObject_cxx

#include "itkTemporalProcessObject.h"
#include "itkTemporalDataObject.h"

#include <math.h>

namespace itk
{

//-CONSTRUCTOR PRINT-----------------------------------------------------------

//
// Constructor
//
TemporalProcessObject::TemporalProcessObject()
  : m_UnitInputNumberOfFrames(1),
    m_UnitOutputNumberOfFrames(1),
    m_FrameSkipPerOutput(1),
    m_InputStencilCurrentFrameIndex(0)
{}

//
// PrintSelf
//
void
TemporalProcessObject::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "TemporalProcessObject" << std::endl;
}

//-PROPAGATE REQUESTED REGION CALLBACKS----------------------------------------

//
// EnlargeOutputRequestedRegion
//
void
TemporalProcessObject::EnlargeOutputRequestedRegion(DataObject* output)
{
  // Check that output is a TemporalDataObject
  TemporalDataObject* tOutput = dynamic_cast<TemporalDataObject*>(output);

  if (tOutput)
    {
    this->EnlargeOutputRequestedTemporalRegion(tOutput);
    }
  else
    {
    itkExceptionMacro(<< "itk::TemporalProcessObject::EnlargeOutputRequestedRegion() "
                      << "cannot cast " << typeid(output).name() << " to "
                      << typeid(TemporalDataObject*).name() );
    }
}

//
// EnlargeOutputRequestedTemporalRegion
// TODO: Hanle RealTime
//
void
TemporalProcessObject::EnlargeOutputRequestedTemporalRegion(TemporalDataObject* output)
{
  // Make sure the requested output temporal region duration is a multiple of
  // the unit number of output frames
  TemporalRegion outReqTempRegion = output->GetRequestedTemporalRegion();
  unsigned long outFrameDuration = outReqTempRegion.GetFrameDuration();

  unsigned int remainder = outFrameDuration % m_UnitOutputNumberOfFrames;
  if (remainder > 0)
    {
    outFrameDuration += (m_UnitOutputNumberOfFrames - remainder);
    }
  outReqTempRegion.SetFrameDuration(outFrameDuration);
  output->SetRequestedTemporalRegion(outReqTempRegion);
}

//
// GenerateOutputRequestedRegion
//
void
TemporalProcessObject::GenerateOutputRequestedRegion(DataObject* output)
{
  // Check that output is a TemporalDataObject
  TemporalDataObject* tOutput = dynamic_cast<TemporalDataObject*>(output);

  if (tOutput)
    {
    this->GenerateOutputRequestedTemporalRegion(tOutput);
    }
  else
    {
    itkExceptionMacro(<< "itk::TemporalProcessObject::GenerateOutputRequestedRegion() "
                      << "cannot cast " << typeid(output).name() << " to "
                      << typeid(TemporalDataObject*).name() );
    }
}

//
// GenerateInputRequestedRegion
//
void
TemporalProcessObject::GenerateInputRequestedRegion()
{
  // Only continue if we have an input
  if (this->GetNumberOfInputs() == 0)
    {
    return;
    }

  // Check that output and input are a TemporalDataObjects
  TemporalDataObject* tOutput = dynamic_cast<TemporalDataObject*>(this->GetOutput(0));
  TemporalDataObject* tInput = dynamic_cast<TemporalDataObject*>(this->GetInput(0));

  if (!tOutput)
    {
    itkExceptionMacro(<< "itk::TemporalProcessObject::GenerateInputRequestedRegion() "
                      << "cannot cast " << typeid(this->GetOutput(0)).name() << " to "
                      << typeid(TemporalDataObject*).name() );
    }
  else if (!tInput)
    {
    itkExceptionMacro(<< "itk::TemporalProcessObject::GenerateInputRequestedRegion() "
                      << "cannot cast " << typeid(this->GetInput(0)).name() << " to "
                      << typeid(TemporalDataObject*).name() );
    }
  else
    {
    this->GenerateInputRequestedTemporalRegion();
    }
}

//
// GenerateInputRequestedTemporalRegion
// TODO: Hanle RealTime
//
void
TemporalProcessObject::GenerateInputRequestedTemporalRegion()
{
  // This should only get called after verifying that input(0) and output(0)
  // can validly be cast to TemporalDataObjects, so don't check cast here
  TemporalDataObject* input = dynamic_cast<TemporalDataObject*>(this->GetInput(0));
  TemporalDataObject* output = dynamic_cast<TemporalDataObject*>(this->GetOutput(0));

  TemporalRegion outReqTempRegion = output->GetRequestedTemporalRegion();

  // This should always be a whole number because of EnlargeOutputRequestedTemporalRegion
  // but do it safely in case the subclass overrides it
  unsigned long numInputRequests =
    (unsigned long)ceil((double)outReqTempRegion.GetFrameDuration() /
                        (double)m_UnitOutputNumberOfFrames);

  // The number of input requests indicates the number of times the process
  // will have to request a temporal region of size m_UnitInputNumberOfFrames.
  // Each request besides the last will require m_FrameSkipPerOutput new frames
  // to be loaded.
  unsigned long inputDuration = m_FrameSkipPerOutput * (numInputRequests - 1) +
                                  m_UnitInputNumberOfFrames;

  // Compute the start of the input requested temporal region based on
  // m_InputStencilCurrentFrameIndex
  long inputStart = outReqTempRegion.GetFrameStart() - m_InputStencilCurrentFrameIndex;

  // Make sure we're not requesting a negative frame (this may be replaced by
  // boundary conditions at some point)
  if (inputStart < 0)
    {
    itkExceptionMacro(<< "itk::TemporalProcessObject::GenerateInputRequestedTemporalRegion() "
                      << "cannot request a region with a starting frame of " << inputStart);
    }

  // Set up the region and assign it to input
  TemporalRegion inReqTempRegion;
  inReqTempRegion.SetFrameStart(inputStart);
  inReqTempRegion.SetFrameDuration(inputDuration);
  input->SetRequestedTemporalRegion(inReqTempRegion);
}

//
// GenerateDefaultLargestPossibleRegion
//
TemporalRegion
TemporalProcessObject::GenerateDefaultLargestPossibleRegion()
{
  TemporalRegion out;
  out.SetFrameStart(0);
  out.SetFrameDuration(ITK_INFINITE_FRAME_DURATION);
  out.SetRealStart(RealTimeStamp());
  out.SetRealDuration(ITK_INFINITE_REAL_DURATION);
  return out;
}

//
// UpdateOutputInformation
// TODO: Hanle RealTime
//
void
TemporalProcessObject::UpdateOutputInformation()
{
  // Update using inherited system
  Superclass::UpdateOutputInformation();

  TemporalDataObject* input = dynamic_cast<TemporalDataObject*>(this->GetInput(0));
  TemporalDataObject* output = dynamic_cast<TemporalDataObject*>(this->GetOutput(0));
  if (!output)
    {
    itkExceptionMacro(<< "itk::TemporalProcessObject::GenerateOutputRequestedTemporalRegion() "
                      << "cannot cast " << typeid(output).name() << " to "
                      << typeid(TemporalDataObject*).name() );
    }

  // Compute duration for output largest possible region
  TemporalRegion inputLargestRegion;
  if (!input)
    {
    // If there is no input, use the default LargestTemporalRegion
    inputLargestRegion = this->GenerateDefaultLargestPossibleRegion();
    }
  else
    {
    inputLargestRegion = input->GetLargestPossibleTemporalRegion();
    }
  long scannableDuration = inputLargestRegion.GetFrameDuration() -
                            m_UnitInputNumberOfFrames + 1;
  long outputDuration = m_UnitOutputNumberOfFrames *
    ((double)(scannableDuration - 1) / (double)(m_FrameSkipPerOutput) + 1);

  // Compute the start of the output region
  long outputStart = inputLargestRegion.GetFrameStart() + m_InputStencilCurrentFrameIndex;

  // Set up output largets possible region
  TemporalRegion largestRegion = output->GetLargestPossibleTemporalRegion();
  largestRegion.SetFrameDuration(outputDuration);
  largestRegion.SetFrameStart(outputStart);
  output->SetLargestPossibleTemporalRegion(largestRegion);
}


//-TEMPORAL STREAMING----------------------------------------------------------

//
// UpdateOutputData
//
void
TemporalProcessObject::UpdateOutputData(DataObject* itkNotUsed(output))
{

  // This implementation mirrors the one in ProcessObject with the exception
  // that it does not propagate the call to its inputs before calling
  // GenerateData. This is done because the temporal streaming system that is
  // active by default will re-set the requested temporal region on the input
  // multiple times in order to stream the data without requireing all data to
  // be loaded at once.

  // Prevent chasing the tail
  if (m_Updating)
    {
    return;
    }

  // Prepare outputs
  this->PrepareOutputs();

  // Mark that we are updating
  m_Updating = true;

  // Cache ReleaseDataFlag(s)
  this->CacheInputReleaseDataFlags();

  // Notify observers of start
  this->InvokeEvent( StartEvent() );

  // Process GenerateData for this object (which will start temporal streaming)
  this->SetAbortGenerateData(false);
  this->UpdateProgress(0.0f);

  try
    {
    // Make sure all requred input ports full
    DataObjectPointerArraySizeType ninputs = this->GetNumberOfValidRequiredInputs();
    if ( ninputs < this->GetNumberOfRequiredInputs() )
      {
      itkExceptionMacro(<< "At least " << this->GetNumberOfRequiredInputs()
                        << " inputs are required but only " << ninputs
                        << " are specified.");
      }
    this->GenerateData();
    }
  catch (ProcessAborted & excp)
    {
    this->InvokeEvent( AbortEvent() );
    this->ResetPipeline();
    this->RestoreInputReleaseDataFlags();
    throw excp;
    }
  catch (...)
    {
    this->ResetPipeline();
    this->RestoreInputReleaseDataFlags();
    throw;
    }

  // If aborted, push progress to 1.0
  if (this->GetAbortGenerateData())
    {
    this->UpdateProgress(1.0f);
    }

  // Now, mark the data up to date
  DataObjectPointerArraySizeType idx;
  for ( idx = 0; idx < this->GetNumberOfOutputs(); ++idx )
    {
    if ( this->GetOutput(idx) )
      {
      this->GetOutput(idx)->DataHasBeenGenerated();
      }
    }

  // Restore the state of any input ReleaseDataFlags
  this->RestoreInputReleaseDataFlags();

  // Release any inputs if marked for release
  this->ReleaseInputs();

  // Mark that we are no longer updating the data in this filter
  m_Updating = false;

}

//
// GenerateData
// TODO: Handle Real Time
//
void
TemporalProcessObject::GenerateData()
{
  // Call Pre-processing method
  this->BeforeTemporalStreamingGenerateData();

  // Split up the requested output temporal region
  std::vector<TemporalRegion> inputTemporalRegionRequests = this->SplitRequestedTemporalRegion();

  // Get the first output frame location
  TemporalDataObject* output = dynamic_cast<TemporalDataObject*>(this->GetOutput(0));
  if (!output)
    {
    itkExceptionMacro(<< "itk::TemporalProcessObject::GenerateData() "
                      << "cannot cast " << typeid(output).name() << " to "
                      << typeid(TemporalDataObject*).name() );
    }
  unsigned long outputStartFrame = output->GetUnbufferedRequestedTemporalRegion().GetFrameStart();

  // Set the start frame of the buffered region to match the first requested
  // frame. The duration must be handled by the TemporalDataObject because of
  // the RingBuffer used underneath.
  TemporalRegion updatedBufferedRegion = output->GetBufferedTemporalRegion();
  updatedBufferedRegion.SetFrameStart(output->GetRequestedTemporalRegion().GetFrameStart());
  output->SetBufferedTemporalRegion(updatedBufferedRegion);

  // Process each of the temporal sub-regions in sequence
  for (unsigned int i = 0; i < inputTemporalRegionRequests.size(); ++i)
    {
    // If we have an input, set the requested region and make sure its data is ready
    if (this->GetNumberOfInputs())
      {
      // Set Input's requested region to the new reqest at i
      TemporalDataObject* input = dynamic_cast<TemporalDataObject*>(this->GetInput(0));
      if (!input)
        {
        itkExceptionMacro(<< "itk::TemporalProcessObject::GenerateData() "
                          << "cannot cast " << typeid(input).name() << " to "
                          << typeid(TemporalDataObject*).name() );
        }
      input->SetRequestedTemporalRegion(inputTemporalRegionRequests[i]);

      // Call Input's UpdateOutputData()
      input->UpdateOutputData();
      }

    // Call TemporalStreamingGenerateData to process the chunk of data
    this->TemporalStreamingGenerateData(outputStartFrame);

    // Increment outputStartFrame
    outputStartFrame += this->m_UnitOutputNumberOfFrames;
    }

  // Call post-processing method
  this->AfterTemporalStreamingGenerateData();
}


//
// TemporalStreamingGenerateData
//
void
TemporalProcessObject::TemporalStreamingGenerateData(unsigned long outputFrameStart)
{
  itkExceptionMacro(<< "itk::Error: " << this->GetNameOfClass()
                    << "(" << this << "): Subclass should override this method!!!" );
}


//
// SplitRequestedTemporalRegion
// TODO: Hanle RealTime
//
std::vector<TemporalRegion>
TemporalProcessObject::SplitRequestedTemporalRegion()
{

  // Get the current output TemporalDataObject
  TemporalDataObject* outputObject = dynamic_cast<TemporalDataObject*>(this->GetOutput(0));
  if (!outputObject)
    {
    itkExceptionMacro(<< "itk::TemporalProcessObject::SplitRequestedTemporalRegion() "
                      << "cannot cast " << typeid(outputObject).name() << " to "
                      << typeid(TemporalDataObject*).name() );
    }

  // Get the TemporalRegion representing the difference between the output's
  // requested temporal region and its buffered temporal region. This
  // difference is defined as any time that is covered by the requested region
  // but not by the buffered region
  TemporalRegion unbufferedRegion = outputObject->GetUnbufferedRequestedTemporalRegion();

  // Calculate the number of input requests that will be needed
  unsigned long numRequests = (unsigned long)(ceil(
                                              (double)(unbufferedRegion.GetFrameDuration() /
                                              (double)(m_UnitOutputNumberOfFrames)) ));

  // Set up the requested input temporal region set (TODO: NOT PROPERLY HANDLING REAL TIME!!!!!!!!)
  std::vector<TemporalRegion> inputTemporalRegionRequests;

  // if there are no requests, just return now
  if (numRequests == 0)
    {
    return inputTemporalRegionRequests;
    }

  long regionStartFrame = 1;
  if (this->m_FrameSkipPerOutput > 0)
    {
    regionStartFrame = unbufferedRegion.GetFrameStart() - m_InputStencilCurrentFrameIndex;
    }
  else if (this->m_FrameSkipPerOutput < 0)
    {
    regionStartFrame = unbufferedRegion.GetFrameStart() + unbufferedRegion.GetFrameDuration() + 1 -
                        (this->m_UnitOutputNumberOfFrames - m_InputStencilCurrentFrameIndex);
    }

  // Make sure we're not trying to get a negative frame
  if (regionStartFrame < 0)
    {
    itkExceptionMacro(<< "itk::TemporalProcessObject::SplitRequestedTemporalRegion() "
                      << "cannot start at frame number " << regionStartFrame);
    }

  for (unsigned int i = 0; i < numRequests; ++i)
    {
    // Create the requested region
    TemporalRegion r;
    r.SetFrameStart(regionStartFrame);
    r.SetFrameDuration(this->m_UnitInputNumberOfFrames);
    inputTemporalRegionRequests.push_back(r);

    // Move the start position for the next requested region
    regionStartFrame += this->m_FrameSkipPerOutput;
    }

  // Return the set of regions
  return inputTemporalRegionRequests;

}

} // end namespace itk

#endif
