/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkProcessObject.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "itkProcessObject.h"
#include "itkDataObject.h"
#include "itkObjectFactory.h"

namespace itk
{

/**
 * Instantiate object with no start, end, or progress methods.
 */
ProcessObject
::ProcessObject()
{
  m_NumberOfInputs = 0;
  m_NumberOfRequiredInputs = 0;
  m_Inputs = NULL;

  m_NumberOfOutputs = 0;
  m_NumberOfRequiredOutputs = 0;
  m_Outputs = NULL;

  m_StartMethod = NULL;
  m_StartMethodArgDelete = NULL;
  m_StartMethodArg = NULL;
  m_ProgressMethod = NULL;
  m_ProgressMethodArgDelete = NULL;
  m_ProgressMethodArg = NULL;
  m_EndMethod = NULL;
  m_EndMethodArgDelete = NULL;
  m_EndMethodArg = NULL;

  m_AbortExecute = false;
  m_Progress = 0.0;
  m_Updating = false;
}


/**
 * Destructor for the ProcessObject class
 */
ProcessObject
::~ProcessObject()
{
  if ((m_StartMethodArg)&&(m_StartMethodArgDelete))
    {
    (*m_StartMethodArgDelete)(m_StartMethodArg);
    }
  if ((m_ProgressMethodArg)&&(m_ProgressMethodArgDelete))
    {
    (*m_ProgressMethodArgDelete)(m_ProgressMethodArg);
    }
  if ((m_EndMethodArg)&&(m_EndMethodArgDelete))
    {
    (*m_EndMethodArgDelete)(m_EndMethodArg);
    }

  unsigned int idx;
  
  for (idx = 0; idx < m_NumberOfInputs; ++idx)
    {
    if (m_Inputs[idx])
      {
      m_Inputs[idx]->UnRegister();
      m_Inputs[idx] = NULL;
      }
    }
  if (m_Inputs)
    {
    delete [] m_Inputs;
    m_Inputs = NULL;
    m_NumberOfInputs = 0;
    }

  for (idx = 0; idx < m_NumberOfOutputs; ++idx)
    {
    if (m_Outputs[idx])
      {
      m_Outputs[idx]->UnRegister();
      m_Outputs[idx] = NULL;
      }
    }
  if (m_Outputs)
    {
    delete [] m_Outputs;
    m_Outputs = NULL;
    m_NumberOfOutputs = 0;
    }
}

typedef DataObject *DataObjectPointer;

/**
 * Called by constructor to set up input array.
 */
void 
ProcessObject
::SetNumberOfInputs(unsigned int num)
{
  unsigned int idx;
  DataObjectPointer *inputs;

  /**
   * in case nothing has changed.
   */
  if (num == m_NumberOfInputs)
    {
    return;
    }
  
  /**
   * Allocate new arrays.
   */
  inputs = new DataObjectPointer[num];

  /**
   * Initialize with NULLs.
   */
  for (idx = 0; idx < num; ++idx)
    {
    inputs[idx] = NULL;
    }

  /**
   * Copy old inputs
   */
  for (idx = 0; idx < num && idx < m_NumberOfInputs; ++idx)
    {
    inputs[idx] = m_Inputs[idx];
    }
  
  /**
   * delete the previous arrays
   */
  if (m_Inputs)
    {
    delete [] m_Inputs;
    m_Inputs = NULL;
    m_NumberOfInputs = 0;
    }
  
  /**
   * Set the new arrays
   */
  m_Inputs = inputs;
  
  m_NumberOfInputs = num;
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
  unsigned int idx;
  
  if (input)
    {
    input->Register();
    }
  this->Modified();
  
  for (idx = 0; idx < m_NumberOfInputs; ++idx)
    {
    if (m_Inputs[idx] == NULL)
      {
      m_Inputs[idx] = input;
      return;
      }
    }
  
  this->SetNumberOfInputs(m_NumberOfInputs + 1);
  m_Inputs[m_NumberOfInputs - 1] = input;
}


/**
 * Adds an input to the first null position in the input list.
 * Expands the list memory if necessary
 */
void 
ProcessObject
::RemoveInput(DataObject *input)
{
  unsigned int idx;
  
  if (!input)
    {
    return;
    }
  
  /**
   * find the input in the list of inputs
   */
  int loc = -1;
  for (idx = 0; idx < m_NumberOfInputs; ++idx)
    {
    if (m_Inputs[idx] == input)
      {
      loc = idx;
      }
    }
  if (loc == -1)
    {
    itkDebugMacro("tried to remove an input that was not in the list");
    return;
    }
  
  m_Inputs[loc]->UnRegister();
  m_Inputs[loc] = NULL;

  /**
   * if that was the last input, then shrink the list
   */
  if (loc == int(m_NumberOfInputs) - 1 )
    {
    this->SetNumberOfInputs(m_NumberOfInputs - 1);
    }
  
  this->Modified();
}


/**
 * Set an Input of this filter. 
 */
void 
ProcessObject
::SetNthInput(unsigned int idx, DataObject *input)
{
  if (idx < 0)
    {
    itkErrorMacro(<< "SetNthInput: " << idx << ", cannot set input. ");
    return;
    }
  /**
   * Expand array if necessary.
   */
  if (idx >= m_NumberOfInputs)
    {
    this->SetNumberOfInputs(idx + 1);
    }
  
  /**
   * does this change anything?
   */
  if (input == m_Inputs[idx])
    {
    return;
    }
  
  if (m_Inputs[idx])
    {
    m_Inputs[idx]->UnRegister();
    m_Inputs[idx] = NULL;
    }
  
  if (input)
    {
    input->Register();
    }

  m_Inputs[idx] = input;
  this->Modified();
}

void 
ProcessObject
::RemoveOutput(DataObject *output)
{
  unsigned int idx;
  
  if (!output)
    {
    return;
    }
  
  /**
   * find the output in the list of outputs
   */
  int loc = -1;
  for (idx = 0; idx < m_NumberOfOutputs; ++idx)
    {
    if ( m_Outputs[idx] == output)
      {
      loc = idx;
      }
    }
  if (loc == -1)
    {
    itkDebugMacro("tried to remove an output that was not in the list");
    return;
    }
  
  m_Outputs[loc]->SetSource(NULL);
  m_Outputs[loc]->UnRegister();
  m_Outputs[loc] = NULL;

  /**
   * if that was the last output, then shrink the list
   */
  if (loc == int(m_NumberOfOutputs) - 1)
    {
    this->SetNumberOfOutputs(m_NumberOfOutputs - 1);
    }
  
  this->Modified();
}


/**
 * Set an Output of this filter. 
 * tricky because we have to manage the double pointers and keep
 * them consistent.
 */
void 
ProcessObject
::SetNthOutput(unsigned int idx, DataObject *newOutput)
{
  DataObject *oldOutput;
  
  if (idx < 0)
    {
    itkErrorMacro(<< "SetNthOutput: " << idx << ", cannot set output. ");
    return;
    }
  /**
   * Expand array if necessary.
   */
  if (idx >= m_NumberOfOutputs)
    {
    this->SetNumberOfOutputs(idx + 1);
    }
  
  /**
   * does this change anything?
   */
  oldOutput = m_Outputs[idx];
  if (newOutput == oldOutput)
    {
    return;
    }
  
  /**
   * disconnect first existing source-output relationship.
   */
  if (oldOutput)
    {
    oldOutput->SetSource(NULL);
    oldOutput->UnRegister();
    m_Outputs[idx] = NULL;
    }
  
  if (newOutput)
    {
    ProcessObject *newOutputOldSource = newOutput->GetSource();

    /**
     * Register the newOutput so it does not get deleted.
     * Don't set the link yet until previous links is disconnected.
     */
    newOutput->Register();
    
    /**
     * disconnect second existing source-output relationship
     */
    if (newOutputOldSource)
      {
      newOutputOldSource->RemoveOutput(newOutput);
      }
    newOutput->SetSource(this);
    }
  /**
   * now actually make the link that was registered previously.
   */
  m_Outputs[idx] = newOutput;
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
  
  if (output)
    {
    output->SetSource(this);
    output->Register();
    }
  this->Modified();
  
  for (idx = 0; idx < m_NumberOfOutputs; ++idx)
    {
    if (m_Outputs[idx] == NULL)
      {
      m_Outputs[idx] = output;
      return;
      }
    }
  
  this->SetNumberOfOutputs(m_NumberOfOutputs + 1);
  m_Outputs[m_NumberOfOutputs - 1] = output;
}

/**
 * Called by constructor to set up output array.
 */
void 
ProcessObject
::SetNumberOfOutputs(unsigned int num)
{
  unsigned int idx;
  DataObject **outputs;

  /**
   * in case nothing has changed.
   */
  if (num == m_NumberOfOutputs)
    {
    return;
    }
  
  /**
   * Allocate new arrays.
   */
  outputs = new DataObject *[num];

  /**
   * Initialize with NULLs.
   */
  for (idx = 0; idx < num; ++idx)
    {
    outputs[idx] = NULL;
    }

  /**
   * Copy old outputs
   */
  for (idx = 0; idx < num && idx < m_NumberOfOutputs; ++idx)
    {
    outputs[idx] = m_Outputs[idx];
    }
  
  /**
   * delete the previous arrays
   */
  if (m_Outputs)
    {
    delete [] m_Outputs;
    m_Outputs = NULL;
    m_NumberOfOutputs = 0;
    }
  
  /**
   * Set the new arrays
   */
  m_Outputs = outputs;
  
  m_NumberOfOutputs = num;
  this->Modified();
}


/**
 *
 */
DataObject *
ProcessObject
::GetOutput(unsigned int i)
{
  if (m_NumberOfOutputs < i+1)
    {
    return NULL;
    }
  
  return m_Outputs[i];
}


/**
 *
 */
DataObject *
ProcessObject
::GetInput(unsigned int i)
{
  if (m_NumberOfInputs < i+1)
    {
    return NULL;
    }
  
  return m_Inputs[i];
}


/**
 * Update the progress of the process object. If a ProgressMethod exists, 
 * executes it. Then set the Progress ivar to amount. The parameter amount 
 * should range between (0,1).
 */
void 
ProcessObject
::UpdateProgress(float amount)
{
  m_Progress = amount;
  if ( m_ProgressMethod )
    {
    (*m_ProgressMethod)(m_ProgressMethodArg);
    }
}


/**
 * Specify function to be called before object executes.
 */
void 
ProcessObject
::SetStartMethod(void (*f)(void *), void *arg)
{
  if ( f != m_StartMethod || arg != m_StartMethodArg )
    {
    /**
     * delete the current arg if there is one and a delete meth
     */
    if ((m_StartMethodArg)&&(m_StartMethodArgDelete))
      {
      (*m_StartMethodArgDelete)(m_StartMethodArg);
      }
    m_StartMethod = f;
    m_StartMethodArg = arg;
    this->Modified();
    }
}


/**
 * Specify function to be called to show progress of filter
 */
void 
ProcessObject
::SetProgressMethod(void (*f)(void *), void *arg)
{
  if ( f != m_ProgressMethod || arg != m_ProgressMethodArg )
    {
    /**
     * delete the current arg if there is one and a delete meth
     */
    if ((m_ProgressMethodArg)&&(m_ProgressMethodArgDelete))
      {
      (*m_ProgressMethodArgDelete)(m_ProgressMethodArg);
      }
    m_ProgressMethod = f;
    m_ProgressMethodArg = arg;
    this->Modified();
    }
}


/**
 * Specify function to be called after object executes.
 */
void 
ProcessObject
::SetEndMethod(void (*f)(void *), void *arg)
{
  if ( f != m_EndMethod || arg != m_EndMethodArg )
    {
    /**
     * delete the current arg if there is one and a delete meth
     */
    if ((m_EndMethodArg)&&(m_EndMethodArgDelete))
      {
      (*m_EndMethodArgDelete)(m_EndMethodArg);
      }
    m_EndMethod = f;
    m_EndMethodArg = arg;
    this->Modified();
    }
}


/**
 * Set the arg delete method. This is used to free user memory.
 */
void 
ProcessObject
::SetStartMethodArgDelete(void (*f)(void *))
{
  if ( f != m_StartMethodArgDelete)
    {
    m_StartMethodArgDelete = f;
    this->Modified();
    }
}


/**
 * Set the arg delete method. This is used to free user memory.
 */
void 
ProcessObject
::SetProgressMethodArgDelete(void (*f)(void *))
{
  if ( f != m_ProgressMethodArgDelete)
    {
    m_ProgressMethodArgDelete = f;
    this->Modified();
    }
}


/**
 * Set the arg delete method. This is used to free user memory.
 */
void 
ProcessObject
::SetEndMethodArgDelete(void (*f)(void *))
{
  if ( f != m_EndMethodArgDelete)
    {
    m_EndMethodArgDelete = f;
    this->Modified();
    }
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
  
  for (idx = 0; idx < m_NumberOfOutputs; idx++)
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
::PrintSelf(std::ostream& os, Indent indent)
{
  Object::PrintSelf(os,indent);

  os << indent << "Number Of Required Inputs: "
     << m_NumberOfRequiredInputs << std::endl;

  if ( m_NumberOfInputs)
    {
    unsigned int idx;
    for (idx = 0; idx < m_NumberOfInputs; ++idx)
      {
      os << indent << "Input " << idx << ": (" << m_Inputs[idx] << ")\n";
      }
    }
  else
    {
    os << indent <<"No Inputs\n";
    }

  if ( m_StartMethod )
    {
    os << indent << "Start Method defined\n";
    }
  else
    {
    os << indent <<"No Start Method\n";
    }

  if ( m_ProgressMethod )
    {
    os << indent << "Progress Method defined\n";
    }
  else
    {
    os << indent << "No Progress Method\n";
    }

  if ( m_EndMethod )
    {
    os << indent << "End Method defined\n";
    }
  else
    {
    os << indent << "No End Method\n";
    }

  os << indent << "AbortExecute: " << (m_AbortExecute ? "On\n" : "Off\n");
  os << indent << "Progress: " << m_Progress << "\n";
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


/**
 *
 */
void 
ProcessObject
::UpdateInformation()
{
  unsigned long t1, t2;
  unsigned int idx;
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
   * The MTime of this source will be used in determine the PipelineMTime
   * for the outputs
   */
  t1 = this->GetMTime();

  /**
   * Loop through the inputs
   */
  for (idx = 0; idx < m_NumberOfInputs; ++idx)
    {
    if (m_Inputs[idx] != NULL)
      {
      input = m_Inputs[idx];

      /**
       * Propagate the UpdateInformation call
       */
      m_Updating = true;
      input->UpdateInformation();
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
   * Call ExecuteInformation for subclass specific information.
   * Since UpdateInformation propagates all the way up the pipeline,
   * we need to be careful here to call ExecuteInformation only if necessary.
   * Otherwise, we may cause this source to be modified which will cause it
   * to execute again on the next update.
   */
  if (t1 > m_InformationTime.GetMTime())
    {
    for (idx = 0; idx < m_NumberOfOutputs; ++idx)
      {
      output = this->GetOutput(idx);
      if (output)
        {
        output->SetPipelineMTime(t1);
        }  
      }
    
    this->ExecuteInformation();
    }
}


/**
 *
 */
void 
ProcessObject
::PropagateUpdateExtent(DataObject *output)
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
   * more data then require for the output. This can happen, for
   * example, when a source can only produce the whole output.
   * Although this is being called for a specific output, the source
   * may need to enlarge all outputs.
   */
  this->EnlargeOutputUpdateExtents( output );

  /**
   * Give the subclass a chance to request a larger extent on 
   * the inputs. This is necessary when, for example, a filter
   * requires more data at the "internal" boundaries to 
   * produce the boundary values - such as an image filter that
   * derives a new pixel value by applying some operation to a 
   * neighborhood of surrounding original values. 
   */
  this->ComputeInputUpdateExtents( output );

  /**
   * Now that we know the input update extent, propogate this
   * through all the inputs.
   */
  m_Updating = true;
  for (unsigned int idx = 0; idx < m_NumberOfInputs; ++idx)
    {
    if (m_Inputs[idx] != NULL)
      {
      m_Inputs[idx]->PropagateUpdateExtent();
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
::ComputeInputUpdateExtents( DataObject *itkNotUsed(output) )
{
  for (unsigned int idx = 0; idx < m_NumberOfInputs; ++idx)
    {
    if (m_Inputs[idx] != NULL)
      {
      m_Inputs[idx]->SetUpdateExtentToWholeExtent();
      }
    }  
}


/**
 *
 */
void 
ProcessObject
::TriggerAsynchronousUpdate()
{
  /**
   * check flag to avoid executing forever if there is a loop
   */
  if (m_Updating)
    {
    return;
    }

  /**
   * Propagate the trigger to all the inputs
   */
  m_Updating = true;
  for (unsigned int idx = 0; idx < m_NumberOfInputs; ++idx)
    {
    if (m_Inputs[idx] != NULL)
      {
      m_Inputs[idx]->TriggerAsynchronousUpdate();
      }
    }
  m_Updating = false;
}


/**
 *
 */
void 
ProcessObject
::UpdateData(DataObject *itkNotUsed(output))
{
  unsigned int idx;

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
   * Must call PropagateUpdateExtent before UpdateData if multiple 
   * inputs since they may lead back to the same data object.
   */
  m_Updating = true;
  if ( m_NumberOfInputs == 1 )
    {
    if (m_Inputs[0] != NULL)
      {
      m_Inputs[0]->UpdateData();
      }
    }
  else
    {
    for (idx = 0; idx < m_NumberOfInputs; ++idx)
      {
      if (m_Inputs[idx] != NULL)
        {
        m_Inputs[idx]->PropagateUpdateExtent();
        m_Inputs[idx]->UpdateData();
        }
      }
    }
  m_Updating = false;     
    
  /**
   * Initialize all the outputs
   */
  for (idx = 0; idx < m_NumberOfOutputs; idx++)
    {
    if (m_Outputs[idx])
      {
      m_Outputs[idx]->PrepareForNewData(); 
      }
    }
 
  /**
   * If there is a start method, call it
   */
  if ( m_StartMethod )
    {
    (*m_StartMethod)(m_StartMethodArg);
    }

  /**
   * Execute this object - we have not aborted yet, and our progress
   * before we start to execute is 0.0.
   */
  m_AbortExecute = 0;
  m_Progress = 0.0;
  if (m_NumberOfInputs < m_NumberOfRequiredInputs)
    {
    itkErrorMacro(<< "At least " << m_NumberOfRequiredInputs << " inputs are required but only " << m_NumberOfInputs << " are specified");
    }
  else
    {
    this->Execute();
    }

  /**
   * If we ended due to aborting, push the progress up to 1.0 (since
   * it probably didn't end there)
   */
  if ( !m_AbortExecute )
    {
    this->UpdateProgress(1.0);
    }

  /**
   * Call the end method, if there is one
   */
  if ( m_EndMethod )
    {
    (*m_EndMethod)(m_EndMethodArg);
    }
    
  /**
   * Now we have to mark the data as up to data.
   */
  for (idx = 0; idx < m_NumberOfOutputs; ++idx)
    {
    if (m_Outputs[idx])
      {
      m_Outputs[idx]->DataHasBeenGenerated();
      }
    }
  
  /**
   * Release any inputs if marked for release
   */
  for (idx = 0; idx < m_NumberOfInputs; ++idx)
    {
    if (m_Inputs[idx] != NULL)
      {
      if ( m_Inputs[idx]->ShouldIReleaseData() )
        {
        m_Inputs[idx]->ReleaseData();
        }
      }  
    }
  
  /**
   * Information gets invalidated as soon as Update is called,
   * so validate it again here.
   */
  m_InformationTime.Modified();
}


/**
 *
 */
void 
ProcessObject
::ComputeEstimatedPipelineMemorySize(DataObject *output,
                                     unsigned long size[3] )
{
  unsigned long outputSize[2];
  unsigned long inputPipelineSize[3];
  unsigned long mySize = 0;
  unsigned long maxSize = 0;
  unsigned long goingDownstreamSize = 0;
  unsigned long *inputSize = NULL;
  unsigned int idx;

  /**
   * We need some space to store the input sizes if there are any inputs
   */
  if ( m_NumberOfInputs > 0 )
    {
    inputSize = new unsigned long[m_NumberOfInputs];
    }

  /**
   * Get the pipeline size propagated down each input. Keep track of max
   * pipeline size, how much memory will be required downstream from here,
   * the size of each input, and the memory required by this filter when
   * it executes.
   */
  for (idx = 0; idx < m_NumberOfInputs; ++idx)
    {
    if (m_Inputs[idx])
      {

      /**
       * Get the upstream size of the pipeline, the estimated size of this
       * input, and the maximum size seen upstream from here.
       */
      m_Inputs[idx]->ComputeEstimatedPipelineMemorySize(inputPipelineSize);

      /**
       * Save this input size to possibly be used when estimating output size
       */
      inputSize[idx] = inputPipelineSize[1];

      /**
       * Is the max returned bigger than the max we've seen so far?
       */
      if ( inputPipelineSize[2] > maxSize )
        {
        maxSize = inputPipelineSize[2];
        }

      /**
       * If we are going to release this input, then its size won't matter
       * downstream from here.
       */
      if ( m_Inputs[idx]->ShouldIReleaseData() )
        {
        goingDownstreamSize += inputPipelineSize[0] - inputPipelineSize[1];
        }
      else
        {
        goingDownstreamSize += inputPipelineSize[0];
        }

      /**
       * During execution this filter will need all the input data 
       */
      mySize += inputPipelineSize[0];
      }

    /**
     * The input was null, so it has no size
     */
    else
      {
      inputSize[idx] = 0;
      }
    }

  /**
   * Now the we know the size of all input, compute the output size
   */
  this->ComputeEstimatedOutputMemorySize( output, inputSize, outputSize );

  /**
   * This filter will produce all output so it needs all that memory.
   * Also, all this data will flow downstream to the next source (if it is
   * the requested output) or will still exist with no chance of being
   * released (if it is the non-requested output)
   */
  mySize += outputSize[1];
  goingDownstreamSize += outputSize[1];

  /**
   * Is the state of the pipeline during this filter's execution the
   * largest that it has been so far?
   */
  if ( mySize > maxSize )
    {
    maxSize = mySize;
    }

  /**
   * The first size is the memory going downstream from here - which is all
   * the memory coming in minus any data realeased. The second size is the
   * size of the specified output (which can be used by the downstream 
   * filter when determining how much data it might release). The final size
   * is the maximum pipeline size encountered here and upstream from here.
   */
  size[0] = goingDownstreamSize;
  size[1] = outputSize[0];
  size[2] = maxSize;

  /**
   * Delete the space we may have created
   */
  if ( inputSize )
    {
    delete [] inputSize;
    }
}


/**
 * This default implementation can be used by any source that will produce
 * only image output. This method should be overridden by anything
 * that will produce mesh (unstructured) output since the
 * output itself cannot estimate its own size.
 */
void 
ProcessObject
::ComputeEstimatedOutputMemorySize( DataObject *output,
                                          unsigned long *itkNotUsed(inputSize),
                                          unsigned long size[2] )
{
  unsigned int idx;
  unsigned int tmp;

  size[0] = 0;
  size[1] = 0;

  /**
   * loop through all the outputs asking them how big they are given the
   * information that they have on their update extent. Keep track of 
   * the size of the specified output in size[0], and the sum of all
   * output size in size[1]. Ignore input sizes in this default implementation.
   */
  for (idx = 0; idx < m_NumberOfOutputs; ++idx)
    {
    if (m_Outputs[idx])
      {
      tmp = m_Outputs[idx]->GetEstimatedMemorySize();
      if ( m_Outputs[idx] == output )
        {
        size[0] = tmp;
        }
      size[1] += tmp;
      }
    }
}


/**
 * Default implementation - copy information from first input to all outputs
 */
void 
ProcessObject
::ExecuteInformation()
{
  DataObject *input, *output;

  if (m_Inputs && m_Inputs[0])
    {
    input = m_Inputs[0];

    for (unsigned int idx = 0; idx < m_NumberOfOutputs; ++idx)
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
::UpdateWholeExtent()
{
  this->UpdateInformation();

  if (this->GetOutput(0))
    {
    this->GetOutput(0)->SetUpdateExtentToWholeExtent();
    this->GetOutput(0)->Update();
    }
}

} // namespace itk
