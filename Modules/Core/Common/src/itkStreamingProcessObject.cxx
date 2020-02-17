/*=========================================================================
 *
 *  Copyright NumFOCUS
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

#include "itkStreamingProcessObject.h"

namespace itk
{


void
StreamingProcessObject::GenerateData()
{
  // The m_Updating flag used in StreamingProcessObject::UpdateOutputData
  // should prevent recursive execution.

  this->BeforeStreamedGenerateData();


  //
  // Determine number of pieces to divide the input.  This will be the
  // minimum of what the user specified via SetNumberOfStreamDivisions()
  // and what the Splitter thinks is a reasonable value.
  //
  unsigned int numberOfInputRequestRegion = this->GetNumberOfInputRequestedRegions();

  //
  // Loop over the number of pieces, execute the upstream pipeline on each
  // piece, and execute StreamedGenerateData on each region
  //
  for (unsigned int piece = 0; piece < numberOfInputRequestRegion && !this->GetAbortGenerateData(); piece++)
  {
    this->m_CurrentRequestNumber = piece;

    this->GenerateNthInputRequestedRegion(piece);

    //
    // Now that we know the input requested region, propagate this
    // through all the inputs.
    // ;
    for (auto & inputName : this->GetInputNames())
    {
      if (this->GetInput(inputName))
      {
        this->GetInput(inputName)->PropagateRequestedRegion();
      }
    }

    //
    // Propagate the update call - make sure everything we
    // might rely on is up-to-date
    // Must call PropagateRequestedRegion before UpdateOutputData if multiple
    // inputs since they may lead back to the same data object.
    m_Updating = true;
    for (auto & inputName : this->GetInputNames())
    {
      if (this->GetInput(inputName))
      {
        if (inputName != this->GetPrimaryInputName() && this->GetNumberOfInputs() > 1)
        {
          this->GetInput(inputName)->PropagateRequestedRegion();
        }
        this->GetInput(inputName)->UpdateOutputData();
      }
    }

    //
    try
    {
      this->StreamedGenerateData(piece);
      this->UpdateProgress(float(piece + 1) / numberOfInputRequestRegion);
    }
    catch (ProcessAborted &)
    {
      this->InvokeEvent(AbortEvent());
      this->ResetPipeline();
      this->RestoreInputReleaseDataFlags();
      throw;
    }
    catch (...)
    {
      this->ResetPipeline();
      this->RestoreInputReleaseDataFlags();
      throw;
    }
  }

  m_CurrentRequestNumber = -1;

  this->AfterStreamedGenerateData();
}


void
StreamingProcessObject::UpdateOutputData(DataObject * itkNotUsed(output))
{

  //
  // prevent chasing our tail
  //
  if (this->m_Updating)
  {
    return;
  }


  //
  // Prepare all the outputs. This may deallocate previous bulk data.
  //
  this->PrepareOutputs();

  /*
   * Cache the state of any ReleaseDataFlag's on the inputs. While the
   * filter is executing, we need to set the ReleaseDataFlag's on the
   * inputs to false in case the current filter is implemented using a
   * mini-pipeline (which will try to release the inputs).  After the
   * filter finishes, we restore the state of the ReleaseDataFlag's
   * before the call to ReleaseInputs().
   */
  this->CacheInputReleaseDataFlags();

  /*
   * Make sure we have the necessary inputs
   */
  const DataObjectPointerArraySizeType ninputs = this->GetNumberOfValidRequiredInputs();
  if (ninputs < this->GetNumberOfRequiredInputs())
  {
    itkExceptionMacro(<< "At least " << this->GetNumberOfRequiredInputs() << " inputs are required but only " << ninputs
                      << " are specified.");
  }

  this->SetAbortGenerateData(false);
  this->UpdateProgress(0.0);
  this->m_Updating = true;

  /*
   * Tell all Observers that the filter is starting
   */
  this->InvokeEvent(StartEvent());

  this->Self::GenerateData();
  /*
   * If we ended due to aborting, push the progress up to 1.0 (since
   * it probably didn't end there)
   */
  if (this->GetAbortGenerateData())
  {
    this->UpdateProgress(1.0);
  }

  // Notify end event observers
  this->InvokeEvent(EndEvent());


  /*
   * Now we have to mark the data as up to data.
   */
  for (auto & outputName : this->GetOutputNames())
  {
    if (this->GetOutput(outputName))
    {
      this->GetOutput(outputName)->DataHasBeenGenerated();
    }
  }

  /* DO NOT Restore the state of any input ReleaseDataFlags
   */
  // this->RestoreInputReleaseDataFlags();

  /**
   * Release any inputs if marked for release
   */
  this->ReleaseInputs();

  // Mark that we are no longer updating the data in this filter
  this->m_Updating = false;
}


int
StreamingProcessObject::GetCurrentRequestNumber() const
{
  return m_CurrentRequestNumber;
}


void
StreamingProcessObject::ResetPipeline()
{
  Superclass::ResetPipeline();
  m_CurrentRequestNumber = -1;
}


StreamingProcessObject::StreamingProcessObject(void) = default;


StreamingProcessObject::~StreamingProcessObject() = default;


void
StreamingProcessObject::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Current Request Number: " << this->m_CurrentRequestNumber << std::endl;
}


void
StreamingProcessObject::PropagateRequestedRegion(DataObject * output)
{

  /**
   * check flag to avoid executing forever if there is a loop
   */
  if (this->m_Updating)
  {
    return;
  }

  /**
   * Give the subclass a chance to indicate that it will provide
   * more data than required for the output. This can happen, for
   * example, when a source can only produce the whole output.
   * Although this is being called for a specific output, the source
   * may need to enlarge all outputs.
   */
  this->EnlargeOutputRequestedRegion(output);


  /**
   * Give the subclass a chance to define how to set the requested
   * regions for each of its outputs, given this output's requested
   * region.  The default implementation is to make all the output
   * requested regions the same.  A subclass may need to override this
   * method if each output is a different resolution.
   */
  this->GenerateOutputRequestedRegion(output);

  // we don't call GenerateInputRequestedRegion since the requested
  // regions are managed when the pipeline is execute

  // we don't call inputs PropagateRequestedRegion either
  // because the pipeline managed later
}


void
StreamingProcessObject::BeforeStreamedGenerateData()
{}

void
StreamingProcessObject::AfterStreamedGenerateData()
{}

} // end namespace itk
