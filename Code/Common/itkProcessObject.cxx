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


//------------------------------------------------------------------------
itkProcessObject::Pointer itkProcessObject::New()
{
  return itkProcessObject::Pointer(new itkProcessObject);
}

// Instantiate object with no start, end, or progress methods.
itkProcessObject::itkProcessObject()
{
  m_NumberOfInputs = 0;
  m_NumberOfRequiredInputs = 0;
  m_Inputs = NULL;

  m_StartMethod = NULL;
  m_StartMethodArgDelete = NULL;
  m_StartMethodArg = NULL;
  m_ProgressMethod = NULL;
  m_ProgressMethodArgDelete = NULL;
  m_ProgressMethodArg = NULL;
  m_EndMethod = NULL;
  m_EndMethodArgDelete = NULL;
  m_EndMethodArg = NULL;

  m_AbortExecute = 0;
  m_Progress = 0.0;
}

// Destructor for the itkProcessObject class
itkProcessObject::~itkProcessObject()
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

  int idx;
  
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
}

typedef itkDataObject *itkDataObjectPointer;
//----------------------------------------------------------------------------
// Called by constructor to set up input array.
void itkProcessObject::SetNumberOfInputs(int num)
{
  int idx;
  itkDataObjectPointer *inputs;

  // in case nothing has changed.
  if (num == m_NumberOfInputs)
    {
    return;
    }
  
  // Allocate new arrays.
  inputs = new itkDataObjectPointer[num];

  // Initialize with NULLs.
  for (idx = 0; idx < num; ++idx)
    {
    inputs[idx] = NULL;
    }

  // Copy old inputs
  for (idx = 0; idx < num && idx < m_NumberOfInputs; ++idx)
    {
    inputs[idx] = m_Inputs[idx];
    }
  
  // delete the previous arrays
  if (m_Inputs)
    {
    delete [] m_Inputs;
    m_Inputs = NULL;
    m_NumberOfInputs = 0;
    }
  
  // Set the new arrays
  m_Inputs = inputs;
  
  m_NumberOfInputs = num;
  this->Modified();
}

//----------------------------------------------------------------------------
// Adds an input to the first null position in the input list.
// Expands the list memory if necessary
void itkProcessObject::AddInput(itkDataObject *input)
{
  int idx;
  
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

//----------------------------------------------------------------------------
// Adds an input to the first null position in the input list.
// Expands the list memory if necessary
void itkProcessObject::RemoveInput(itkDataObject *input)
{
  int idx, loc;
  
  if (!input)
    {
    return;
    }
  
  // find the input in the list of inputs
  loc = -1;
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

  // if that was the last input, then shrink the list
  if (loc == m_NumberOfInputs - 1)
    {
    this->SetNumberOfInputs(m_NumberOfInputs - 1);
    }
  
  this->Modified();
}

//----------------------------------------------------------------------------
// Set an Input of this filter. 
void itkProcessObject::SetNthInput(int idx, itkDataObject *input)
{
  if (idx < 0)
    {
    itkErrorMacro(<< "SetNthInput: " << idx << ", cannot set input. ");
    return;
    }
  // Expand array if necessary.
  if (idx >= m_NumberOfInputs)
    {
    this->SetNumberOfInputs(idx + 1);
    }
  
  // does this change anything?
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

void itkProcessObject::RemoveOutput(itkDataObject *output)
{
  int idx, loc;
  
  if (!output)
    {
    return;
    }
  
  // find the output in the list of outputs
  loc = -1;
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

  // if that was the last output, then shrink the list
  if (loc == m_NumberOfOutputs - 1)
    {
    this->SetNumberOfOutputs(m_NumberOfOutputs - 1);
    }
  
  this->Modified();
}

//----------------------------------------------------------------------------
// Set an Output of this filter. 
// tricky because we have to manage the double pointers and keep
// them consistent.
void itkProcessObject::SetNthOutput(int idx, itkDataObject *newOutput)
{
  itkDataObject *oldOutput;
  
  if (idx < 0)
    {
    itkErrorMacro(<< "SetNthOutput: " << idx << ", cannot set output. ");
    return;
    }
  // Expand array if necessary.
  if (idx >= m_NumberOfOutputs)
    {
    this->SetNumberOfOutputs(idx + 1);
    }
  
  // does this change anything?
  oldOutput = m_Outputs[idx];
  if (newOutput == oldOutput)
    {
    return;
    }
  
  // disconnect first existing source-output relationship.
  if (oldOutput)
    {
    oldOutput->SetSource(NULL);
    oldOutput->UnRegister();
    m_Outputs[idx] = NULL;
    }
  
  if (newOutput)
    {
    itkProcessObject *newOutputOldSource = newOutput->GetSource();

    // Register the newOutput so it does not get deleted.
    // Don't set the link yet until previous links is disconnected.
    newOutput->Register();
    
    // disconnect second existing source-output relationship
    if (newOutputOldSource)
      {
      newOutputOldSource->RemoveOutput(newOutput);
      }
    newOutput->SetSource(this);
    }
  // now actually make the link that was registered previously.
  m_Outputs[idx] = newOutput;
}

//----------------------------------------------------------------------------
// Adds an output to the first null position in the output list.
// Expands the list memory if necessary
void itkProcessObject::AddOutput(itkDataObject *output)
{
  int idx;
  
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

// Called by constructor to set up output array.
void itkProcessObject::SetNumberOfOutputs(int num)
{
  int idx;
  itkDataObject **outputs;

  // in case nothing has changed.
  if (num == m_NumberOfOutputs)
    {
    return;
    }
  
  // Allocate new arrays.
  outputs = new itkDataObject *[num];

  // Initialize with NULLs.
  for (idx = 0; idx < num; ++idx)
    {
    outputs[idx] = NULL;
    }

  // Copy old outputs
  for (idx = 0; idx < num && idx < m_NumberOfOutputs; ++idx)
    {
    outputs[idx] = m_Outputs[idx];
    }
  
  // delete the previous arrays
  if (m_Outputs)
    {
    delete [] m_Outputs;
    m_Outputs = NULL;
    m_NumberOfOutputs = 0;
    }
  
  // Set the new arrays
  m_Outputs = outputs;
  
  m_NumberOfOutputs = num;
  this->Modified();
}

//----------------------------------------------------------------------------
itkDataObject *itkProcessObject::GetOutput(int i)
{
  if (m_NumberOfOutputs < i+1)
    {
    return NULL;
    }
  
  return m_Outputs[i];
}

// Update the progress of the process object. If a ProgressMethod exists, 
// executes it. Then set the Progress ivar to amount. The parameter amount 
// should range between (0,1).
void itkProcessObject::UpdateProgress(float amount)
{
  m_Progress = amount;
  if ( m_ProgressMethod )
    {
    (*m_ProgressMethod)(m_ProgressMethodArg);
    }
}

// Specify function to be called before object executes.
void itkProcessObject::SetStartMethod(void (*f)(void *), void *arg)
{
  if ( f != m_StartMethod || arg != m_StartMethodArg )
    {
    // delete the current arg if there is one and a delete meth
    if ((m_StartMethodArg)&&(m_StartMethodArgDelete))
      {
      (*m_StartMethodArgDelete)(m_StartMethodArg);
      }
    m_StartMethod = f;
    m_StartMethodArg = arg;
    this->Modified();
    }
}

// Specify function to be called to show progress of filter
void itkProcessObject::SetProgressMethod(void (*f)(void *), void *arg)
{
  if ( f != m_ProgressMethod || arg != m_ProgressMethodArg )
    {
    // delete the current arg if there is one and a delete meth
    if ((m_ProgressMethodArg)&&(m_ProgressMethodArgDelete))
      {
      (*m_ProgressMethodArgDelete)(m_ProgressMethodArg);
      }
    m_ProgressMethod = f;
    m_ProgressMethodArg = arg;
    this->Modified();
    }
}

// Specify function to be called after object executes.
void itkProcessObject::SetEndMethod(void (*f)(void *), void *arg)
{
  if ( f != m_EndMethod || arg != m_EndMethodArg )
    {
    // delete the current arg if there is one and a delete meth
    if ((m_EndMethodArg)&&(m_EndMethodArgDelete))
      {
      (*m_EndMethodArgDelete)(m_EndMethodArg);
      }
    m_EndMethod = f;
    m_EndMethodArg = arg;
    this->Modified();
    }
}


// Set the arg delete method. This is used to free user memory.
void itkProcessObject::SetStartMethodArgDelete(void (*f)(void *))
{
  if ( f != m_StartMethodArgDelete)
    {
    m_StartMethodArgDelete = f;
    this->Modified();
    }
}

// Set the arg delete method. This is used to free user memory.
void itkProcessObject::SetProgressMethodArgDelete(void (*f)(void *))
{
  if ( f != m_ProgressMethodArgDelete)
    {
    m_ProgressMethodArgDelete = f;
    this->Modified();
    }
}

// Set the arg delete method. This is used to free user memory.
void itkProcessObject::SetEndMethodArgDelete(void (*f)(void *))
{
  if ( f != m_EndMethodArgDelete)
    {
    m_EndMethodArgDelete = f;
    this->Modified();
    }
}

//----------------------------------------------------------------------------
bool itkProcessObject::GetReleaseDataFlag()
{
  if (this->GetOutput(0))
    {
    return this->GetOutput(0)->GetReleaseDataFlag();
    }
  itkWarningMacro(<<"Output doesn't exist!");
  return false;
}

//----------------------------------------------------------------------------
void itkProcessObject::SetReleaseDataFlag(bool val)
{
  int idx;
  
  for (idx = 0; idx < m_NumberOfOutputs; idx++)
    {
    if (m_Outputs[idx])
      {
      m_Outputs[idx]->SetReleaseDataFlag(val);
      }
    }
}

void itkProcessObject::PrintSelf(std::ostream& os, itkIndent indent)
{
  itkObject::PrintSelf(os,indent);

  os << indent << "Number Of Required Inputs: "
     << m_NumberOfRequiredInputs << endl;

  if ( m_NumberOfInputs)
    {
    int idx;
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



