/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkProcessObject.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkProcessObject.h"
#include "itkDataObject.h"
#include "itkObjectFactory.h"
#include "itkCommand.h"
#include <algorithm>

namespace itk
{

/**
 * Instantiate object with no start, end, or progress methods.
 */
ProcessObject
::ProcessObject()
{
  m_NumberOfRequiredInputs = 0;

  m_NumberOfRequiredOutputs = 0;

  m_AbortGenerateData = false;
  m_Progress = 0.0;
  m_Updating = false;
  
  m_Threader = MultiThreader::New();
  m_NumberOfThreads = m_Threader->GetNumberOfThreads();
}

/**
 * This is a default implementation to make sure we have something.
 * Once all the subclasses of ProcessObject provide an appopriate
 * MakeOutput(), then ProcessObject::MakeOutput() can be made pure
 * virtual.
 */
DataObject::Pointer
ProcessObject
::MakeOutput(unsigned int)
{
  return static_cast<DataObject*>(DataObject::New().GetPointer());
}
  
/**
 * Destructor for the ProcessObject class. We've got to
 * UnRegister() the use of any input classes.
 */
ProcessObject
::~ProcessObject()
{
  // Tell each output that we are going away.  If other objects have a 
  // reference to one of these outputs, the data object will not be deleted
  // when the process object is deleted.  However, the data object's source
  // will still point back to the now nonexistent process object if we do not
  // clean things up now.
  unsigned int idx;
  for (idx = 0; idx < m_Outputs.size(); ++idx)
    {
    if (m_Outputs[idx])
      {
      // let the output know we no longer want to associate with the object
      m_Outputs[idx]->DisconnectSource(this, idx);
      // let go of our reference to the data object
      m_Outputs[idx] = 0;
      }
    }
}

//typedef DataObject *DataObjectPointer;

/**
 * Called by constructor to set up input array.
 */
void 
ProcessObject
::SetNumberOfInputs(unsigned int num)
{
  // in case nothing has changed.
  if (num == m_Inputs.size())
    {
    return;
    }
  m_Inputs.resize(num);
  this->Modified();
}


/**
 * Adds an input to the first null position in the input list.
 * Expands the list memory if necessary
 */
void 
ProcessObject
::AddInput(DataObject *input)
{
  std::vector<DataObjectPointer>::size_type idx;
  
  this->Modified();
  
  for (idx = 0; idx < m_Inputs.size(); ++idx)
    {
    if (!m_Inputs[idx])
      {
      m_Inputs[idx] = input;
      return;
      }
    }
  
  this->SetNumberOfInputs( static_cast<int>( m_Inputs.size() + 1 ) );
  m_Inputs[ static_cast<int>( m_Inputs.size() ) - 1] = input;
}


/**
 * Remove an input.
 *
 * Removes the first occurence of the given OutputObject from the
 * inputs to this ProcessObject.  If it's the last object on the
 * list, shortens the list.
 */
void 
ProcessObject
::RemoveInput(DataObject *input)
{
  if (!input)
    {
    return;
    }
  
  // find the input in the list of inputs
  DataObjectPointerArray::iterator pos = 
    std::find(m_Inputs.begin(), m_Inputs.end(), input);

  if(pos == m_Inputs.end())
    {
    itkDebugMacro("tried to remove an input that was not in the list");
    return;
    }

  // Set the position in the m_Inputs containing input to 0
  *pos = 0;

  // if that was the last input, then shrink the list
  if (pos == m_Inputs.end() - 1 )
    {
    this->SetNumberOfInputs( static_cast<int>( m_Inputs.size() ) - 1);
    }

  this->Modified();
}


/**
 * Set an Input of this filter. This method 
 * does Register()/UnRegister() manually to
 * deal with the fact that smart pointers aren't
 * around to do the reference counting.
 */
void 
ProcessObject
::SetNthInput(unsigned int idx, DataObject *input)
{
  // does this change anything?
  if ( idx < m_Inputs.size() && m_Inputs[idx] == input )
    {
    return;
    }
  
  // Expand array if necessary.
  if (idx >= m_Inputs.size())
    {
    this->SetNumberOfInputs(idx + 1);
    }
  
  m_Inputs[idx] = input;

  this->Modified();
}

void 
ProcessObject
::RemoveOutput(DataObject *output)
{
  if (!output)
    {
    return;
    }
  
  // find the input in the list of inputs
  DataObjectPointerArray::iterator pos = 
    std::find(m_Outputs.begin(), m_Outputs.end(), output);

  if(pos == m_Outputs.end())
    {
    itkDebugMacro("tried to remove an output that was not in the list");
    return;
    }

  // let the output know we no longer want to associate with the object
  (*pos)->DisconnectSource(this, pos - m_Outputs.begin());
  // let go of our reference to the data object
  *pos = 0;

  // if that was the last output, then shrink the list
  if (pos == m_Outputs.end() - 1 )
    {
    this->SetNumberOfOutputs( static_cast<int>( m_Outputs.size() ) - 1);
    }

  this->Modified();
}


/**
 * Set an output of this filter. This method specifically
 * does not do a Register()/UnRegister() because of the 
 * desire to break the reference counting loop.
 */
void 
ProcessObject
::SetNthOutput(unsigned int idx, DataObject *output)
{
  // does this change anything?
  if ( idx < m_Outputs.size() && output == m_Outputs[idx])
    {
    return;
    }

  // Expand array if necessary.
  if (idx >= m_Outputs.size())
    {
    this->SetNumberOfOutputs(idx + 1);
    }

  // Keep a handle to the original output and disconnect the old output from
  // the pipeline
  DataObjectPointer oldOutput;
  if ( m_Outputs[idx] )
    {
    oldOutput = m_Outputs[idx];
    m_Outputs[idx]->DisconnectSource(this, idx);
    }

  if (output)
    {
    output->ConnectSource(this, idx);
    }
  // save the current reference (which releases the previous reference)
  m_Outputs[idx] = output;

  // if we are clearing an output, we need to create a new blank output
  // so we are prepared for the next Update(). this copies the requested
  // region ivar
  if (!m_Outputs[idx])
    {
    itkDebugMacro( " creating new output object." );
    DataObjectPointer newOutput = this->MakeOutput(idx);
    this->SetNthOutput(idx, newOutput);

    // If we had an output object before, copy the requested region
    // ivars to the the new output
    if (oldOutput)
      {
      newOutput->SetRequestedRegion( oldOutput );
      }
    }

  this->Modified();
}

/**
 * Adds an output to the first null position in the output list.
 * Expands the list memory if necessary
 */
void 
ProcessObject
::AddOutput(DataObject *output)
{
  unsigned int idx;
  
  for (idx = 0; idx < m_Outputs.size(); ++idx)
    {
    if ( m_Outputs[idx].IsNull() )
      {
      m_Outputs[idx] = output;

      if (output)
        {
        output->ConnectSource(this, idx);
        }
      this->Modified();
  
      return;
      }
    }
  
  this->SetNumberOfOutputs( static_cast<int>( m_Outputs.size() ) + 1);
  m_Outputs[ static_cast<int>( m_Outputs.size() ) - 1] = output;
  if (output)
    {
    output->ConnectSource(this, static_cast<int>( m_Outputs.size() ) - 1 );
    }
  this->Modified();
}

/**
 * Called by constructor to set up output array.
 */
void 
ProcessObject
::SetNumberOfOutputs(unsigned int num)
{

  // in case nothing has changed.
  if (num == m_Outputs.size())
    {
    return;
    }
  m_Outputs.resize(num);
  this->Modified();
}


/**
 *
 */
DataObject *
ProcessObject
::GetOutput(unsigned int i)
{
  if (m_Outputs.size() < i+1)
    {
    return NULL;
    }
  
  return m_Outputs[i].GetPointer();
}


/**
 *
 */
DataObject *
ProcessObject
::GetInput(unsigned int i)
{
  if (m_Inputs.size() < i+1)
    {
    return NULL;
    }
  
  return m_Inputs[i].GetPointer();
}


/**
 * Update the progress of the process object. If a ProgressMethod exists, 
 * execute it. Then set the Progress ivar to amount. The parameter amount 
 * should range between (0,1).
 */
void 
ProcessObject
::UpdateProgress(float amount)
{
  m_Progress = amount;
  this->InvokeEvent( ProgressEvent() );
}


/**
 *
 */
bool 
ProcessObject
::GetReleaseDataFlag()
{
  if (this->GetOutput(0))
    {
    return this->GetOutput(0)->GetReleaseDataFlag();
    }
  itkWarningMacro(<<"Output doesn't exist!");
  return false;
}


/**
 *
 */
void 
ProcessObject
::SetReleaseDataFlag(bool val)
{
  unsigned int idx;
  
  for (idx = 0; idx < m_Outputs.size(); idx++)
    {
    if (m_Outputs[idx])
      {
      m_Outputs[idx]->SetReleaseDataFlag(val);
      }
    }
}


/**
 *
 */
void 
ProcessObject
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Number Of Required Inputs: "
     << m_NumberOfRequiredInputs << std::endl;

  os << indent << "Number Of Required Outputs: "
     << m_NumberOfRequiredOutputs << std::endl;

  os << indent << "Number Of Threads: "
     << m_NumberOfThreads << std::endl;

  if ( m_Inputs.size())
    {
    std::vector<DataObjectPointer>::size_type idx;
    for (idx = 0; idx < m_Inputs.size(); ++idx)
      {
      os << indent << "Input " << static_cast<int>( idx );
    os << ": (" << m_Inputs[idx].GetPointer() << ")\n";
      }
    }
  else
    {
    os << indent <<"No Inputs\n";
    }
  if ( m_Outputs.size())
    {
    std::vector<DataObjectPointer>::size_type idx;
    for (idx = 0; idx < m_Outputs.size(); ++idx)
      {
      os << indent << "Output " << static_cast<int>( idx );
    os << ": (" << m_Outputs[idx].GetPointer() << ")\n";
      }
    }
  else
    {
    os << indent <<"No Output\n";
    }

  os << indent << "AbortGenerateData: " << (m_AbortGenerateData ? "On\n" : "Off\n");
  os << indent << "Progress: " << m_Progress << "\n";

  os << indent << "Multithreader: " << std::endl;
  m_Threader->PrintSelf(os, indent.GetNextIndent());
}



/**
 * The following methods are used to coordinate the execution of the
 * data processing pipeline.
 */


/**
 *
 */
void 
ProcessObject
::Update()
{
  if (this->GetOutput(0))
    {
    this->GetOutput(0)->Update();
    }
}

void
ProcessObject
::ResetPipeline()
{
  if (this->GetOutput(0))
    {
    this->GetOutput(0)->ResetPipeline();
    }
}

void
ProcessObject
::PropagateResetPipeline()
{
  // 
  // Reset this object.
  //
  // Clear the updating flag.
  m_Updating = 0;

  //
  // Loop through the inputs
  //
  unsigned int idx;
  DataObject::Pointer input;
  for (idx = 0; idx < m_Inputs.size(); ++idx)
    {
    if (m_Inputs[idx])
      {
      input = m_Inputs[idx];

      /**
       * Propagate the ResetPipeline call
       */
      input->PropagateResetPipeline();
      }
    }
}


/**
 *
 */
void 
ProcessObject
::UpdateOutputInformation()
{
  unsigned long t1, t2;
  std::vector<DataObjectPointer>::size_type idx;
  DataObject *input;
  DataObject *output;

  /**
   * Watch out for loops in the pipeline
   */
  if ( m_Updating )
    {
    /**
     * Since we are in a loop, we will want to update. But if
     * we don't modify this filter, then we will not execute
     * because our InformationTime will be more recent than
     * the MTime of our output.
     */
    this->Modified();
    return;
    }

  /**
   * We now wish to set the PipelineMTime of each output DataObject to
   * the largest of this ProcessObject's InformationMTime, all input
   * DataObject's PipelineMTime, and all input's MTime.  We begin with
   * the InformationMTime of this ProcessObject.
   */
  t1 = this->GetMTime();

  /**
   * Loop through the inputs
   */
  for (idx = 0; idx < m_Inputs.size(); ++idx)
    {
    if (m_Inputs[idx])
      {
      input = m_Inputs[idx];

      /**
       * Propagate the UpdateOutputInformation call
       */
      m_Updating = true;
      input->UpdateOutputInformation();
      m_Updating = false;
      
      /**
       * What is the PipelineMTime of this input? Compare this against
       * our current computation to find the largest one.
       */
      t2 = input->GetPipelineMTime();

      if (t2 > t1)
        {
        t1 = t2;
        }

      /**
       * Pipeline MTime of the input does not include the MTime of the 
       * data object itself. Factor these mtimes into the next PipelineMTime
       */
      t2 = input->GetMTime();
      if (t2 > t1)
        {
        t1 = t2;
        }
      }
    }

  /**
   * Call GenerateOutputInformation for subclass specific information.
   * Since UpdateOutputInformation propagates all the way up the pipeline,
   * we need to be careful here to call GenerateOutputInformation only if
   * necessary. Otherwise, we may cause this source to be modified which
   * will cause it to execute again on the next update.
   */
  if (t1 > m_InformationTime.GetMTime())
    {
    for (idx = 0; idx < m_Outputs.size(); ++idx)
      {
      output = this->GetOutput( static_cast<int>( idx ) );
      if (output)
        {
        output->SetPipelineMTime(t1);
        }  
      }
    
    this->GenerateOutputInformation();
    }
}


/**
 *
 */
void 
ProcessObject
::PropagateRequestedRegion(DataObject *output)
{
  /**
   * check flag to avoid executing forever if there is a loop
   */
  if (m_Updating)
    {
    return;
    }

  /**
   * Give the subclass a chance to indicate that it will provide
   * more data then required for the output. This can happen, for
   * example, when a source can only produce the whole output.
   * Although this is being called for a specific output, the source
   * may need to enlarge all outputs.
   */
  this->EnlargeOutputRequestedRegion( output );

  /**
   * Give the subclass a chance to define how to set the requested
   * regions for each of its outputs, given this output's requested
   * region.  The default implementation is to make all the output
   * requested regions the same.  A subclass may need to override this
   * method if each output is a different resolution.
   */
  this->GenerateOutputRequestedRegion( output );
  
  /**
   * Give the subclass a chance to request a larger requested region on 
   * the inputs. This is necessary when, for example, a filter
   * requires more data at the "internal" boundaries to 
   * produce the boundary values - such as an image filter that
   * derives a new pixel value by applying some operation to a 
   * neighborhood of surrounding original values. 
   */
  this->GenerateInputRequestedRegion();

  /**
   * Now that we know the input requested region, propagate this
   * through all the inputs.
   */
  m_Updating = true;
  std::vector<DataObjectPointer>::size_type idx;
  for (idx = 0; idx < m_Inputs.size(); ++idx)
    {
    if (m_Inputs[idx])
      {
      m_Inputs[idx]->PropagateRequestedRegion();
      }
    }
  m_Updating = false;
}


/**
 * By default we require all the input to produce the output. This is
 * overridden in the subclasses since we can often produce the output with
 * just a portion of the input data.
 */
void 
ProcessObject
::GenerateInputRequestedRegion()
{
  std::vector<DataObjectPointer>::size_type idx;
  for (idx = 0; idx < m_Inputs.size(); ++idx)
    {
    if (m_Inputs[idx])
      {
      m_Inputs[idx]->SetRequestedRegionToLargestPossibleRegion();
      }
    }  
}


/**
 * By default we set all the output requested regions to be the same.
 */
void 
ProcessObject
::GenerateOutputRequestedRegion(DataObject *output)
{
  std::vector<DataObjectPointer>::size_type idx;
  for (idx = 0; idx < m_Outputs.size(); ++idx)
    {
    if (m_Outputs[idx] && m_Outputs[idx] != output)
      {
      m_Outputs[idx]->SetRequestedRegion(output);
      }
    }  
}


/**
 *
 */
void 
ProcessObject
::PrepareOutputs()
{  
  unsigned int idx;
  
  for (idx = 0; idx < m_Outputs.size(); idx++)
    {
    if (m_Outputs[idx])
      {
      m_Outputs[idx]->PrepareForNewData(); 
      }
    }
}


/**
 *
 */
void 
ProcessObject
::ReleaseInputs()
{  
  unsigned int idx;

  for (idx = 0; idx < m_Inputs.size(); ++idx)
    {
    if (m_Inputs[idx])
      {
      if ( m_Inputs[idx]->ShouldIReleaseData() )
        {
        m_Inputs[idx]->ReleaseData();
        }
      }  
    }
}


/**
 *
 */
void 
ProcessObject
::UpdateOutputData(DataObject *itkNotUsed(output))
{
  std::vector<DataObjectPointer>::size_type idx;

  /**
   * prevent chasing our tail
   */
  if (m_Updating)
    {
    return;
    }

  /**
   * Propagate the update call - make sure everything we
   * might rely on is up-to-date
   * Must call PropagateRequestedRegion before UpdateOutputData if multiple 
   * inputs since they may lead back to the same data object.
   */
  m_Updating = true;
  if ( m_Inputs.size() == 1 )
    {
    if (m_Inputs[0])
      {
      m_Inputs[0]->UpdateOutputData();
      }
    }
  else
    {
    for (idx = 0; idx < m_Inputs.size(); ++idx)
      {
      if (m_Inputs[idx])
        {
        m_Inputs[idx]->PropagateRequestedRegion();
        m_Inputs[idx]->UpdateOutputData();
        }
      }
    }
  m_Updating = false;     
    
  /**
   * Prepare all the outputs. This may Allocate/Deallocate bulk data.
   */
  this->PrepareOutputs();

  /**
   * Tell all Observers that the filter is starting
   */
  this->InvokeEvent( StartEvent() );

  /**
   * GenerateData this object - we have not aborted yet, and our progress
   * before we start to execute is 0.0.
   */
  m_AbortGenerateData = false;
  m_Progress = 0.0;
  if (m_Inputs.size() < m_NumberOfRequiredInputs)
    {
    itkExceptionMacro(<< "At least " << m_NumberOfRequiredInputs 
                  << " inputs are required but only " << static_cast<int>( m_Inputs.size() )
                  << " are specified");
    }
  else
    {
    try
      {
      this->GenerateData();
      }
    catch( ProcessAborted & excp )
      {
        excp = excp;
      this->InvokeEvent( AbortEvent() );
      this->ResetPipeline();
      throw ProcessAborted(__FILE__,__LINE__);
      }
    }

  /**
   * If we ended due to aborting, push the progress up to 1.0 (since
   * it probably didn't end there)
   *
   */
  if ( m_AbortGenerateData )
    {
    this->UpdateProgress(1.0);
    }

  /**
   * Notify end event observers
   */
  this->InvokeEvent( EndEvent() );

  /**
   * Now we have to mark the data as up to date.
   */
  for (idx = 0; idx < m_Outputs.size(); ++idx)
    {
    if (m_Outputs[idx])
      {
      m_Outputs[idx]->DataHasBeenGenerated();
      }
    }
  
  /**
   * Release any inputs if marked for release
   */
  this->ReleaseInputs();
  
  /**
   * Information gets invalidated as soon as Update is called,
   * so validate it again here.
   */
  m_InformationTime.Modified();
}



/**
 * Default implementation - copy information from first input to all outputs
 */
void 
ProcessObject
::GenerateOutputInformation()
{
  DataObjectPointer input, output;

  if (m_Inputs.size() && m_Inputs[0])
    {
    input = m_Inputs[0];

    for (unsigned int idx = 0; idx < m_Outputs.size(); ++idx)
      {
      output = this->GetOutput(idx);
      if (output)
        {
        output->CopyInformation(input);
        }  
      }
    }
}


/**
 *
 */
void 
ProcessObject
::UpdateLargestPossibleRegion()
{
  this->UpdateOutputInformation();

  if (this->GetOutput(0))
    {
    this->GetOutput(0)->SetRequestedRegionToLargestPossibleRegion();
    this->GetOutput(0)->Update();
    }
}




} // end namespace itk
