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
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#include "itkProcessObject.h"
#include <mutex>

#include <cstdio>
#include <sstream>
#include <algorithm>
#include "itkMultiThreaderBase.h"

namespace itk
{


namespace
{ // local namespace for managing globals
constexpr size_t ITK_GLOBAL_INDEX_NAMES_NUMBER = 10;
constexpr size_t ITK_GLOBAL_INDEX_NAMES_LENGTH = 3;
constexpr char   globalIndexNames[ITK_GLOBAL_INDEX_NAMES_NUMBER][ITK_GLOBAL_INDEX_NAMES_LENGTH] = {
  "_0", "_1", "_2", "_3", "_4", "_5", "_6", "_7", "_8", "_9"
};

} // namespace


ProcessObject::ProcessObject()
  : m_Inputs()
  , m_Outputs()
  , m_CachedInputReleaseDataFlags()
  , m_RequiredInputNames()
{
  /*
   * Instantiate object with no start, end, or progress methods.
   */

  m_NumberOfRequiredInputs = 0;
  m_NumberOfRequiredOutputs = 0;

  m_AbortGenerateData = false;
  m_Progress = 0u;
  m_Updating = false;

  DataObjectPointerMap::value_type p("Primary", DataObjectPointer());
  m_IndexedInputs.push_back(m_Inputs.insert(p).first);
  m_IndexedOutputs.push_back(m_Outputs.insert(std::move(p)).first);

  this->Self::SetMultiThreader(MultiThreaderType::New());

  m_ReleaseDataBeforeUpdateFlag = true;
}


DataObject::Pointer
ProcessObject::MakeOutput(const DataObjectIdentifierType & name)
{
  /*
   * This is a default implementation to make sure we have something.
   * Once all the subclasses of ProcessObject provide an appropriate
   * MakeOutput(), then ProcessObject::MakeOutput() can be made pure
   * virtual.
   */

  itkDebugMacro("MakeOutput(" << name << ")");
  if (this->IsIndexedOutputName(name))
  {
    return this->MakeOutput(this->MakeIndexFromOutputName(name));
  }

  return DataObject::New().GetPointer();
}


DataObject::Pointer ProcessObject::MakeOutput(DataObjectPointerArraySizeType)
{
  return static_cast<DataObject *>(DataObject::New().GetPointer());
}


ProcessObject::~ProcessObject()
{
  /*
   * Destructor for the ProcessObject class. We've got to
   * UnRegister() the use of any input classes.
   */

  // Tell each output that we are going away.  If other objects have a
  // reference to one of these outputs, the data object will not be deleted
  // when the process object is deleted.  However, the data object's source
  // will still point back to the now nonexistent process object if we do not
  // clean things up now.
  for (auto & output : m_Outputs)
  {
    if (output.second)
    {
      // let the output know we no longer want to associate with the object
      output.second->DisconnectSource(this, output.first);
      // let go of our reference to the data object
      output.second = nullptr;
    }
  }
}


void
ProcessObject::SetNumberOfIndexedInputs(DataObjectPointerArraySizeType num)
{
  /*
   * Called by constructor to set up input array.
   */

  if (this->GetNumberOfIndexedInputs() != num)
  {
    if (num < this->GetNumberOfIndexedInputs())
    {
      // NB: The primary input must never be removed from the map, or
      // the indexed inputs array!

      // remove the extra inputs
      for (DataObjectPointerArraySizeType i = std::max<DataObjectPointerArraySizeType>(num, 1);
           i < this->GetNumberOfIndexedInputs();
           ++i)
      {
        m_Inputs.erase(m_IndexedInputs[i]->first);
      }
      m_IndexedInputs.resize(std::max<DataObjectPointerArraySizeType>(num, 1));


      if (num < 1)
      {
        m_IndexedInputs[0]->second = nullptr;
      }
    }
    else
    {
      for (DataObjectPointerArraySizeType i = m_IndexedInputs.size(); i < num; ++i)
      {
        auto iter = m_Inputs.emplace(this->MakeNameFromInputIndex(i), DataObjectPointer()).first;
        // note: insert will not change value if it's already there.
        m_IndexedInputs.push_back(std::move(iter));
      }
    }

    this->Modified();
  }
}


ProcessObject::DataObjectPointerArraySizeType
ProcessObject::GetNumberOfValidRequiredInputs() const
{
  /*
   * Get the number of specified inputs
   */
  DataObjectPointerArraySizeType count = 0;
  for (DataObjectPointerArraySizeType i = 0; i < m_NumberOfRequiredInputs; ++i)
  {
    if (this->GetInput(i))
    {
      ++count;
    }
  }
  return count;
}


void
ProcessObject::AddInput(DataObject * input)
{
  /*
   * Adds an input to the first null position in the input list.
   * Expands the list memory if necessary
   */
  for (unsigned int idx = 0; idx < this->GetNumberOfIndexedInputs(); ++idx)
  {
    if (!this->GetInput(idx))
    {
      this->SetNthInput(idx, input);
      return;
    }
  }
  this->SetNthInput(this->GetNumberOfIndexedInputs(), input);
}


void
ProcessObject::RemoveInput(const DataObjectIdentifierType & key)
{
  /*
   * Remove an input.
   */

  // if primary or required set to null
  if (key == m_IndexedInputs[0]->first || this->IsRequiredInputName(key))
  {
    this->SetInput(key, nullptr);
    return;
  }

  // set indexed input to null, remove if last
  for (DataObjectPointerArraySizeType i = 1; i < m_IndexedInputs.size(); ++i)
  {
    if (m_IndexedInputs[i]->first == key)
    {
      this->SetNthInput(i, nullptr);
      if (i == m_IndexedInputs.size() - 1)
      {
        // remove the last indexed input
        this->SetNumberOfIndexedInputs(this->GetNumberOfIndexedInputs() - 1);
      }
      return;
    }
  }

  auto it = m_Inputs.find(key);

  if (it != m_Inputs.end())
  {
    m_Inputs.erase(it);
    this->Modified();
  }
}


void
ProcessObject::RemoveInput(DataObjectPointerArraySizeType idx)
{
  if (idx < this->GetNumberOfIndexedInputs())
  {
    this->RemoveInput(m_IndexedInputs[idx]->first);
  }
  else
  {
    this->RemoveInput(this->MakeNameFromInputIndex(idx));
  }
}


void
ProcessObject::SetInput(const DataObjectIdentifierType & key, DataObject * input)
{
  if (key.empty())
  {
    itkExceptionMacro("An empty string can't be used as an input identifier");
  }
  auto it = m_Inputs.find(key);
  if (it == m_Inputs.end())
  {
    // this is a whole new entry
    m_Inputs[key] = input;
    this->Modified();
  }
  else if (it->second.GetPointer() != input)
  {
    // there is already an entry, but not with the right object
    it->second = input;
    this->Modified();
  }
  // the entry is already there - there is nothing to do
}


void
ProcessObject::SetNthInput(DataObjectPointerArraySizeType idx, DataObject * input)
{
  /* Set an Input of this filter. This method
   * does Register()/UnRegister() manually to
   * deal with the fact that smart pointers aren't
   * around to do the reference counting.
   */
  if (idx >= this->GetNumberOfIndexedInputs())
  {
    this->SetNumberOfIndexedInputs(idx + 1);
  }
  if (m_IndexedInputs[idx]->second != input)
  {
    m_IndexedInputs[idx]->second = input;
    this->Modified();
  }
}


void
ProcessObject::PushBackInput(const DataObject * input)
{
  /*
   * Model a queue on the input list by providing a push back
   */

  this->SetNthInput(this->GetNumberOfIndexedInputs(), const_cast<DataObject *>(input));
}


void
ProcessObject::PopBackInput()
{
  /*
   * Model a stack on the input list by providing a pop back
   */

  if (this->GetNumberOfIndexedInputs() > 0)
  {
    this->SetNumberOfIndexedInputs(this->GetNumberOfIndexedInputs() - 1);
  }
}


void
ProcessObject::PushFrontInput(const DataObject * input)
{
  const DataObjectPointerArraySizeType nb = this->GetNumberOfIndexedInputs();
  for (DataObjectPointerArraySizeType i = nb; i > 0; i--)
  {
    this->SetNthInput(i, this->GetInput(i - 1));
  }
  this->SetNthInput(0, const_cast<DataObject *>(input));
}


void
ProcessObject::PopFrontInput()
{
  DataObjectPointerArraySizeType nb = this->GetNumberOfIndexedInputs();
  if (nb > 0)
  {
    for (DataObjectPointerArraySizeType i = 1; i < nb; ++i)
    {
      this->SetNthInput(i - 1, this->GetInput(i));
    }
    this->SetNumberOfIndexedInputs(nb - 1);
  }
}


void
ProcessObject::RemoveOutput(const DataObjectIdentifierType & key)
{
  // if primary or required set to null
  if (key == m_IndexedOutputs[0]->first)
  {
    this->SetOutput(key, nullptr);
    return;
  }

  // set indexed output to null, remove if last
  for (DataObjectPointerArraySizeType i = 1; i < m_IndexedOutputs.size(); ++i)
  {
    if (m_IndexedOutputs[i]->first == key)
    {
      this->SetNthOutput(i, nullptr);
      if (i == m_IndexedOutputs.size() - 1)
      {
        // remove the last indexed input
        this->SetNumberOfIndexedOutputs(this->GetNumberOfIndexedOutputs() - 1);
      }
      return;
    }
  }


  auto it = m_Outputs.find(key);

  if (it != m_Outputs.end())
  {
    if (it->second)
    {
      // let the output know we no longer want to associate with the object
      it->second->DisconnectSource(this, it->first);
    }
    m_Outputs.erase(it);
    // let go of our reference to the data object
    this->Modified();
  }
}


void
ProcessObject::RemoveOutput(DataObjectPointerArraySizeType idx)
{
  if (idx == this->GetNumberOfIndexedOutputs() - 1)
  {
    // just remove the last indexed output
    this->SetNumberOfIndexedOutputs(this->GetNumberOfIndexedOutputs() - 1);
  }
  else
  {
    this->RemoveOutput(this->MakeNameFromOutputIndex(idx));
  }
}


void
ProcessObject::SetOutput(const DataObjectIdentifierType & name, DataObject * output)
{
  /*
    Set an Output of this filter. This method
   * does Register()/UnRegister() manually to
   * deal with the fact that smart pointers aren't
   * around to do the reference counting.
   */

  // copy the key, because it might be destroyed in that method, so a reference
  // is not enough.
  DataObjectIdentifierType key = name;

  if (key.empty())
  {
    itkExceptionMacro("An empty string can't be used as an output identifier");
  }

  // does this change anything?
  DataObjectPointerMap::const_iterator it = m_Outputs.find(key);
  if (it != m_Outputs.end() && it->second.GetPointer() == output)
  {
    return;
  }

  // Keep a handle to the original output and disconnect the old output from
  // the pipeline
  DataObjectPointer oldOutput;
  if (m_Outputs[key])
  {
    oldOutput = m_Outputs[key];
    m_Outputs[key]->DisconnectSource(this, key);
  }

  if (output)
  {
    output->ConnectSource(this, key);
  }
  // save the current reference (which releases the previous reference)
  m_Outputs[key] = output;

  // if we are clearing an output, we need to create a new blank output
  // so we are prepared for the next Update(). this copies the requested
  // region ivar
  if (!m_Outputs[key])
  {
    itkDebugMacro(" creating new output object.");
    DataObjectPointer newOutput = this->MakeOutput(key);
    this->SetOutput(key, newOutput);

    // If we had an output object before, copy the requested region
    // ivars and release data flag to the the new output
    if (oldOutput)
    {
      newOutput->SetRequestedRegion(oldOutput);
      newOutput->SetReleaseDataFlag(oldOutput->GetReleaseDataFlag());
    }
  }

  this->Modified();
}


void
ProcessObject::SetNthOutput(DataObjectPointerArraySizeType idx, DataObject * output)
{
  if (idx >= this->GetNumberOfIndexedOutputs())
  {
    this->SetNumberOfIndexedOutputs(idx + 1);
  }
  this->SetOutput(m_IndexedOutputs[idx]->first, output);
}


void
ProcessObject::AddOutput(DataObject * output)
{
  /*
   * Adds an output to the first null position in the output list.
   * Expands the list memory if necessary
   */

  for (DataObjectPointerArraySizeType idx = 0; idx < this->GetNumberOfIndexedOutputs(); ++idx)
  {
    if (!this->GetOutput(idx))
    {
      this->SetNthOutput(idx, output);
      return;
    }
  }
  this->SetNthOutput(this->GetNumberOfIndexedOutputs(), output);
}


void
ProcessObject::SetNumberOfIndexedOutputs(DataObjectPointerArraySizeType num)
{
  /*
   * Called by constructor to set up output array.
   */

  if (this->GetNumberOfIndexedOutputs() != num)
  {
    if (num < this->GetNumberOfIndexedOutputs())
    {
      // NB: The primary output must never be removed from the map, or
      // the indexed inputs array!

      // remove the extra outputs
      for (DataObjectPointerArraySizeType i = std::max<DataObjectPointerArraySizeType>(num, 1);
           i < this->GetNumberOfIndexedOutputs();
           ++i)
      {
        // an output should never be nullptr
        itkAssertInDebugAndIgnoreInReleaseMacro(m_IndexedOutputs[i]->second);

        // let the output know we no longer want to associate with the
        // object
        m_IndexedOutputs[i]->second->DisconnectSource(this, m_IndexedOutputs[i]->first);

        m_Outputs.erase(m_IndexedOutputs[i]->first);
      }

      m_IndexedOutputs.resize(std::max<DataObjectPointerArraySizeType>(num, 1));

      if (num < 1)
      {
        m_IndexedOutputs[0]->second = nullptr;
      }
    }
    else
    {
      for (DataObjectPointerArraySizeType i = m_IndexedOutputs.size(); i < num; ++i)
      {
        auto iter = m_Outputs.emplace(this->MakeNameFromOutputIndex(i), DataObjectPointer()).first;
        // note: insert will not change value if it's already there.
        m_IndexedOutputs.push_back(std::move(iter));
      }
    }

    this->Modified();
  }
}


DataObject *
ProcessObject::GetOutput(const DataObjectIdentifierType & key)
{
  auto it = m_Outputs.find(key);
  if (it == m_Outputs.end())
  {
    return nullptr;
  }
  return it->second.GetPointer();
}


const DataObject *
ProcessObject::GetOutput(const DataObjectIdentifierType & key) const
{
  auto it = m_Outputs.find(key);
  if (it == m_Outputs.end())
  {
    return nullptr;
  }
  return it->second.GetPointer();
}


DataObject *
ProcessObject::GetOutput(DataObjectPointerArraySizeType i)
{
  return m_IndexedOutputs[i]->second;
}


const DataObject *
ProcessObject::GetOutput(DataObjectPointerArraySizeType i) const
{
  return m_IndexedOutputs[i]->second;
}


void
ProcessObject::SetPrimaryOutput(DataObject * object)
{
  this->SetOutput(m_IndexedOutputs[0]->first, object);
}


void
ProcessObject::SetPrimaryOutputName(const DataObjectIdentifierType & key)
{
  if (key != this->m_IndexedOutputs[0]->first)
  {

    // note: insert will not change value if it's already there.
    auto it = this->m_Outputs.emplace(key, DataObjectPointer()).first;

    if (it->second.IsNull())
    {
      it->second = this->m_IndexedOutputs[0]->second;
      m_Outputs.erase(this->m_IndexedOutputs[0]);
    }
    this->m_IndexedOutputs[0] = it;

    this->Modified();
  }
}


bool
ProcessObject::HasOutput(const DataObjectIdentifierType & key) const
{
  auto it = m_Outputs.find(key);
  return it != m_Outputs.end();
}


ProcessObject::NameArray
ProcessObject::GetOutputNames() const
{
  NameArray res;
  res.reserve(m_Outputs.size());
  for (const auto & output : m_Outputs)
  {
    // only include the primary if it's required or set
    if (output.first != m_IndexedOutputs[0]->first || output.second.IsNotNull())
    {
      res.push_back(output.first);
    }
  }

  return res;
}

// ProcessObject::ConstDataObjectPointerArray
// ProcessObject
// ::GetOutputs() const
// {
//   ConstDataObjectPointerArray res;
//   res.reserve(m_Outputs.size());
//   for ( DataObjectPointerMap::const_iterator it = m_Outputs.begin(); it != m_Outputs.end(); ++it )
//     {
//     res.push_back( it->second.GetPointer() );
//     }
//   return res;
// }

ProcessObject::DataObjectPointerArray
ProcessObject::GetOutputs()
{
  DataObjectPointerArray res;
  res.reserve(m_Outputs.size());
  for (auto & output : m_Outputs)
  {
    // only include the primary if it's required or set
    if (output.first != m_IndexedOutputs[0]->first || output.second.IsNotNull())
    {
      res.push_back(output.second.GetPointer());
    }
  }
  return res;
}


ProcessObject::DataObjectPointerArraySizeType
ProcessObject::GetNumberOfOutputs() const
{
  // only include the primary if it's required or set
  if (m_IndexedOutputs[0]->second.IsNotNull())
  {
    return m_Outputs.size();
  }
  return m_Outputs.size() - 1;
}


ProcessObject::DataObjectPointerArraySizeType
ProcessObject::GetNumberOfIndexedOutputs() const
{
  // this first element should always contain the primary output's
  // name, if this is not true there is an internal logic error.
  itkAssertInDebugAndIgnoreInReleaseMacro(m_IndexedOutputs.size() >= 1);

  if (m_IndexedOutputs.size() > 1)
  {
    return m_IndexedOutputs.size();
  }
  return this->GetPrimaryOutput() != nullptr;
}

// ProcessObject::ConstDataObjectPointerArray
// ProcessObject
// ::GetIndexedOutputs() const
// {
//   ConstDataObjectPointerArray res(this->GetNumberOfIndexedOutputs());
//   for ( unsigned int i = 0; i < this->GetNumberOfIndexedOutputs(); i++ )
//     {
//     res[i] = this->GetOutput(i);
//     }
//   return res;
// }

ProcessObject::DataObjectPointerArray
ProcessObject::GetIndexedOutputs()
{
  DataObjectPointerArray res(this->GetNumberOfIndexedOutputs());
  for (DataObjectPointerArraySizeType i = 0; i < this->GetNumberOfIndexedOutputs(); ++i)
  {
    res[i] = this->GetOutput(i);
  }
  return res;
}


DataObject *
ProcessObject::GetInput(const DataObjectIdentifierType & key)
{
  auto it = m_Inputs.find(key);
  if (it == m_Inputs.end())
  {
    return nullptr;
  }
  return it->second.GetPointer();
}


const DataObject *
ProcessObject::GetInput(const DataObjectIdentifierType & key) const
{
  auto it = m_Inputs.find(key);
  if (it == m_Inputs.end())
  {
    return nullptr;
  }
  return it->second.GetPointer();
}


void
ProcessObject::SetPrimaryInput(DataObject * object)
{
  if (m_IndexedInputs[0]->second != object)
  {
    m_IndexedInputs[0]->second = object;
    this->Modified();
  }
}


void
ProcessObject::SetPrimaryInputName(const DataObjectIdentifierType & key)
{
  this->RemoveRequiredInputName(m_IndexedInputs[0]->first);
  this->AddRequiredInputName(key, 0);
}


bool
ProcessObject::HasInput(const DataObjectIdentifierType & key) const
{
  auto it = m_Inputs.find(key);
  return it != m_Inputs.end();
}


ProcessObject::NameArray
ProcessObject::GetInputNames() const
{
  NameArray res;
  res.reserve(m_Inputs.size());
  for (const auto & input : m_Inputs)
  {
    // only include the primary if it's required or set
    if (input.first != m_IndexedInputs[0]->first || input.second.IsNotNull() || this->IsRequiredInputName(input.first))
    {
      res.push_back(input.first);
    }
  }
  return res;
}


bool
ProcessObject::AddRequiredInputName(const DataObjectIdentifierType & name)
{
  if (name.empty())
  {
    itkExceptionMacro("An empty string can't be used as an input identifier");
  }

  if (!m_RequiredInputNames.insert(name).second)
  {
    return false;
  }


  this->AddOptionalInputName(name);

  if (name == m_IndexedInputs[0]->first && m_NumberOfRequiredInputs == 0)
  {
    m_NumberOfRequiredInputs = 1;
  }

  return true;
}

void
ProcessObject::AddOptionalInputName(const DataObjectIdentifierType & name)
{

  if (name.empty())
  {
    itkExceptionMacro("An empty string can't be used as an input identifier");
  }

  // note: insert will not change value if it's already there.
  m_Inputs.emplace(name, DataObjectPointer());

  this->Modified();
}


bool
ProcessObject::AddRequiredInputName(const DataObjectIdentifierType & name, DataObjectPointerArraySizeType idx)
{

  if (name.empty())
  {
    itkExceptionMacro("An empty string can't be used as an input identifier");
  }

  if (!m_RequiredInputNames.insert(name).second)
  {
    itkWarningMacro(<< "Input already \"" << name << "\" already required!");
    // Input already required, but it is not added as indexed input?
    return false;
  }

  this->AddOptionalInputName(name, idx);

  if (name == m_IndexedInputs[0]->first && m_NumberOfRequiredInputs == 0)
  {
    m_NumberOfRequiredInputs = 1;
  }

  return true;
}

void
ProcessObject::AddOptionalInputName(const DataObjectIdentifierType & name, DataObjectPointerArraySizeType idx)
{

  if (name.empty())
  {
    itkExceptionMacro("An empty string can't be used as an input identifier");
  }

  // note: insert will not change value if it's already in named inputs.
  auto it = m_Inputs.emplace(name, DataObjectPointer()).first;

  if (idx >= this->GetNumberOfIndexedInputs())
  {
    this->SetNumberOfIndexedInputs(idx + 1);
  }
  else if (!it->second)
  {
    // if the old index had a data object move that to the new name
    it->second = this->GetInput(m_IndexedInputs[idx]->first);
  }

  // remove name of the old input ( may be new default index name
  // i.e. _1 )
  m_Inputs.erase(m_IndexedInputs[idx]->first);

  m_IndexedInputs[idx] = it;

  this->Modified();
}


bool
ProcessObject::RemoveRequiredInputName(const DataObjectIdentifierType & name)
{
  if (m_RequiredInputNames.erase(name))
  {
    if (name == m_IndexedInputs[0]->first && m_NumberOfRequiredInputs == 1)
    {
      m_NumberOfRequiredInputs = 0;
    }
    this->Modified();
    return true;
  }
  return false;
}


bool
ProcessObject::IsRequiredInputName(const DataObjectIdentifierType & name) const
{
  return m_RequiredInputNames.find(name) != m_RequiredInputNames.end();
}


void
ProcessObject::SetRequiredInputNames(const NameArray & names)
{
  m_RequiredInputNames.clear();
  for (const auto & name : names)
  {
    this->AddRequiredInputName(name);
  }
  this->Modified();
}


ProcessObject::NameArray
ProcessObject::GetRequiredInputNames() const
{
  NameArray res;
  res.reserve(m_RequiredInputNames.size());
  for (const auto & requiredInputName : m_RequiredInputNames)
  {
    res.push_back(requiredInputName);
  }
  return res;
}

// ProcessObject::ConstDataObjectPointerArray
// ProcessObject
// ::GetInputs() const
// {
//   ConstDataObjectPointerArray res;
//   res.reserve(m_Inputs.size());
//   for ( DataObjectPointerMap::const_iterator it = m_Inputs.begin(); it != m_Inputs.end(); ++it )
//     {
//     res.push_back( it->second.GetPointer() );
//     }
//   return res;
// }

ProcessObject::DataObjectPointerArray
ProcessObject::GetInputs()
{
  DataObjectPointerArray res;
  res.reserve(m_Inputs.size());
  for (auto & input : m_Inputs)
  {
    // only include the primary if it's required or set
    if (input.first != m_IndexedInputs[0]->first || input.second.IsNotNull() || this->IsRequiredInputName(input.first))
    {
      res.push_back(input.second.GetPointer());
    }
  }
  return res;
}


ProcessObject::DataObjectPointerArraySizeType
ProcessObject::GetNumberOfInputs() const
{
  // only include the primary if it's required or set
  if (m_IndexedInputs[0]->second.IsNotNull() || this->IsRequiredInputName(m_IndexedInputs[0]->first))
  {
    return m_Inputs.size();
  }
  return m_Inputs.size() - 1;
}


ProcessObject::DataObjectPointerArraySizeType
ProcessObject::GetNumberOfIndexedInputs() const
{
  // this first element should always contain the primary input's
  // name, if this is not true there is an internal logic error.
  itkAssertInDebugAndIgnoreInReleaseMacro(m_IndexedInputs.size() >= 1);

  if (m_IndexedInputs.size() > 1)
  {
    return m_IndexedInputs.size();
  }
  return static_cast<ProcessObject::DataObjectPointerArraySizeType>(this->GetPrimaryInput() != nullptr);
}

// ProcessObject::ConstDataObjectPointerArray
// ProcessObject
// ::GetIndexedInputs() const
// {
//   ConstDataObjectPointerArray res(this->GetNumberOfIndexedInputs());
//   for ( DataObjectPointerArraySizeType i = 0; i < this->GetNumberOfIndexedInputs(); i++ )
//     {
//     res[i] = this->GetInput(i);
//     }
//   return res;
// }

ProcessObject::DataObjectPointerArray
ProcessObject::GetIndexedInputs()
{
  DataObjectPointerArray res(this->GetNumberOfIndexedInputs());
  for (DataObjectPointerArraySizeType i = 0; i < this->GetNumberOfIndexedInputs(); ++i)
  {
    res[i] = this->GetInput(i);
  }
  return res;
}


ProcessObject::DataObjectIdentifierType
ProcessObject::MakeNameFromInputIndex(DataObjectPointerArraySizeType idx) const
{
  if (idx == 0)
  {
    return m_IndexedInputs[0]->first;
  }
  return this->MakeNameFromIndex(idx);
}


ProcessObject::DataObjectIdentifierType
ProcessObject::MakeNameFromOutputIndex(DataObjectPointerArraySizeType idx) const
{
  if (idx == 0)
  {
    return this->m_IndexedOutputs[0]->first;
  }
  return this->MakeNameFromIndex(idx);
}


ProcessObject::DataObjectIdentifierType
ProcessObject::MakeNameFromIndex(DataObjectPointerArraySizeType idx) const
{
  if (idx < ITK_GLOBAL_INDEX_NAMES_NUMBER)
  {
    return ProcessObject::DataObjectIdentifierType(globalIndexNames[idx]);
  }
  else
  {
    return '_' + std::to_string(idx);
  }
}


ProcessObject::DataObjectPointerArraySizeType
ProcessObject::MakeIndexFromInputName(const DataObjectIdentifierType & name) const
{
  if (name == m_IndexedInputs[0]->first)
  {
    itkDebugMacro("MakeIndexFromName(" << name << ") -> 0");
    return 0;
  }
  return this->MakeIndexFromName(name);
}


ProcessObject::DataObjectPointerArraySizeType
ProcessObject::MakeIndexFromOutputName(const DataObjectIdentifierType & name) const
{
  if (name == this->m_IndexedOutputs[0]->first)
  {
    itkDebugMacro("MakeIndexFromName(" << name << ") -> 0");
    return 0;
  }
  return this->MakeIndexFromName(name);
}


ProcessObject::DataObjectPointerArraySizeType
ProcessObject::MakeIndexFromName(const DataObjectIdentifierType & name) const
{
  DataObjectIdentifierType       baseName = "_";
  DataObjectPointerArraySizeType baseSize = baseName.size();
  if (name.size() <= baseSize || name.substr(0, baseSize) != baseName)
  {
    itkDebugMacro("MakeIndexFromName(" << name << ") -> exception bad base name");
    itkExceptionMacro(<< "Not an indexed data object: " << name);
  }
  DataObjectIdentifierType       idxStr = name.substr(baseSize);
  DataObjectPointerArraySizeType idx;
  if (!(std::istringstream(idxStr) >> idx))
  {
    itkDebugMacro("MakeIndexFromName(" << name << ") -> exception not an index");
    itkExceptionMacro(<< "Not an indexed data object: " << name);
  }
  itkDebugMacro("MakeIndexFromName(" << name << ") -> " << idx);
  return idx;
}


bool
ProcessObject::IsIndexedInputName(const DataObjectIdentifierType & name) const
{
  if (name == m_IndexedInputs[0]->first)
  {
    return true;
  }
  for (const auto & indexedInput : m_IndexedInputs)
  {
    if (indexedInput->first == name)
    {
      return true;
    }
  }
  return false;
}


bool
ProcessObject::IsIndexedOutputName(const DataObjectIdentifierType & name) const
{
  if (name == m_IndexedOutputs[0]->first)
  {
    return true;
  }
  for (const auto & indexedOutput : m_IndexedOutputs)
  {
    if (indexedOutput->first == name)
    {
      return true;
    }
  }
  return false;
}


void
ProcessObject::UpdateProgress(float progress)
{
  // value is clamped between 0 and 1.
  m_Progress = progressFloatToFixed(progress);

  this->InvokeEvent(ProgressEvent());
}


void
ProcessObject::IncrementProgress(float increment)
{
  // Clamp the value to be between 0 and 1.
  uint32_t integerIncrement = progressFloatToFixed(increment);

  uint32_t oldProgress = m_Progress.fetch_add(integerIncrement);

  uint32_t updatedProgress = m_Progress;

  // check if progress overflowed
  if (oldProgress > updatedProgress)
  {
    m_Progress = std::numeric_limits<uint32_t>::max();
  }

  if (std::this_thread::get_id() == this->m_UpdateThreadID)
  {
    this->InvokeEvent(ProgressEvent());
  }
}


bool
ProcessObject::GetReleaseDataFlag() const
{
  if (this->GetPrimaryOutput())
  {
    return this->GetPrimaryOutput()->GetReleaseDataFlag();
  }
  return false;
}


void
ProcessObject::SetReleaseDataFlag(bool val)
{
  for (auto & output : m_Outputs)
  {
    if (output.second)
    {
      output.second->SetReleaseDataFlag(val);
    }
  }
}


void
ProcessObject::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  Indent indent2 = indent.GetNextIndent();
  if (!m_Inputs.empty())
  {
    os << indent << "Inputs: " << std::endl;
    for (const auto & input : m_Inputs)
    {
      std::string req = "";
      if (this->IsRequiredInputName(input.first))
      {
        req = " *";
      }
      os << indent2 << input.first << ": (" << input.second.GetPointer() << ")" << req << std::endl;
    }
  }
  else
  {
    os << indent << "No Inputs\n";
  }

  os << indent << "Indexed Inputs: " << std::endl;
  unsigned int idx = 0;
  for (auto it = m_IndexedInputs.begin(); it != m_IndexedInputs.end(); ++it, ++idx)
  {
    os << indent2 << idx << ": " << (*it)->first << " (" << (*it)->second.GetPointer() << ")" << std::endl;
  }

  if (!m_RequiredInputNames.empty())
  {
    os << indent << "Required Input Names: ";
    for (auto it = m_RequiredInputNames.begin(); it != m_RequiredInputNames.end(); ++it)
    {
      if (it != m_RequiredInputNames.begin())
      {
        os << ", ";
      }
      os << *it;
    }
    os << std::endl;
  }
  else
  {
    os << indent << "No Required Input Names" << std::endl;
  }
  os << indent << "NumberOfRequiredInputs: " << m_NumberOfRequiredInputs << std::endl;

  if (!m_Outputs.empty())
  {
    os << indent << "Outputs: " << std::endl;
    for (const auto & output : m_Outputs)
    {
      os << indent2 << output.first << ": (" << output.second.GetPointer() << ")" << std::endl;
    }
  }
  else
  {
    os << indent << "No Outputs\n";
  }
  os << indent << "Indexed Outputs: " << std::endl;
  idx = 0;
  for (auto it = m_IndexedOutputs.begin(); it != m_IndexedOutputs.end(); ++it, ++idx)
  {
    os << indent2 << idx << ": " << (*it)->first << " (" << (*it)->second.GetPointer() << ")" << std::endl;
  }

  os << indent << "NumberOfRequiredOutputs: " << m_NumberOfRequiredOutputs << std::endl;
  os << indent << "Number Of Work Units: " << m_NumberOfWorkUnits << std::endl;
  os << indent << "ReleaseDataFlag: " << (this->GetReleaseDataFlag() ? "On" : "Off") << std::endl;
  os << indent << "ReleaseDataBeforeUpdateFlag: " << (m_ReleaseDataBeforeUpdateFlag ? "On" : "Off") << std::endl;
  os << indent << "AbortGenerateData: " << (m_AbortGenerateData ? "On" : "Off") << std::endl;
  os << indent << "Progress: " << progressFixedToFloat(m_Progress) << std::endl;
  os << indent << "Multithreader: " << std::endl;
  m_MultiThreader->PrintSelf(os, indent.GetNextIndent());
}


void
ProcessObject::Update()
{
  if (this->GetPrimaryOutput())
  {
    this->GetPrimaryOutput()->Update();
  }
}


void
ProcessObject::ResetPipeline()
{
  if (this->GetPrimaryOutput())
  {
    this->GetPrimaryOutput()->ResetPipeline();
  }
  else
  {
    // important to make it work on process objects without outputs
    this->PropagateResetPipeline();
  }
}


void
ProcessObject::PropagateResetPipeline()
{
  //
  // Reset this object.
  //
  // Clear the updating flag.
  m_Updating = false;

  m_UpdateThreadID = std::thread::id();
  //
  // Loop through the inputs
  //
  for (auto & input : m_Inputs)
  {
    if (input.second)
    {
      input.second->PropagateResetPipeline();
    }
  }
}


void
ProcessObject::VerifyPreconditions() ITKv5_CONST
{
  /**
   * Make sure that all the required named inputs are there and non null
   */
  for (const auto & requiredInputName : this->m_RequiredInputNames)
  {
    if (this->GetInput(requiredInputName) == nullptr)
    {
      itkExceptionMacro(<< "Input " << requiredInputName << " is required but not set.");
    }
  }

  /**
   * Verify the require named inputs.
   */
  auto i = m_RequiredInputNames.begin();
  while (i != m_RequiredInputNames.end())
  {
    if (this->GetInput(*i) == nullptr)
    {
      itkExceptionMacro(<< "Required Input " << *i << "is not specified!"
                        << " The required inputs are expected to be the first inputs.");
    }
    ++i;
  }

  /**
   * Count the number of required indexed inputs which have been assigned
   */
  const DataObjectPointerArraySizeType validIndexedInputs = this->GetNumberOfValidRequiredInputs();

  if (validIndexedInputs < this->m_NumberOfRequiredInputs)
  {
    itkExceptionMacro(<< "At least " << this->m_NumberOfRequiredInputs << " of the first "
                      << this->m_NumberOfRequiredInputs << " indexed inputs are required but only "
                      << validIndexedInputs << " are specified."
                      << " The required inputs are expected to be the first inputs.");
  }
}


void
ProcessObject::VerifyInputInformation() ITKv5_CONST
{}


void
ProcessObject::UpdateOutputInformation()
{
  /**
   * Watch out for loops in the pipeline
   */
  if (m_Updating)
  {
    /**
     * Since we are in a loop, we will want to update. But if
     * we don't modify this filter, then we will not execute
     * because our OutputInformationMTime will be more recent than
     * the MTime of our output.
     */
    this->Modified();
    return;
  }

  /**
   * Verify that the process object has been configured correctly,
   * that all required inputs are set, and needed parameters are set
   * appropriately before we continue the pipeline, i.e. is the filter
   * in a state that it can be run.
   */
  this->VerifyPreconditions();

  /**
   * We now wish to set the PipelineMTime of each output DataObject to
   * the largest of this ProcessObject's MTime, all input DataObject's
   * PipelineMTime, and all input's MTime.  We begin with the MTime of
   * this ProcessObject.
   */
  ModifiedTimeType t1 = this->GetMTime();

  /**
   * Loop through the inputs
   */
  for (auto & input : m_Inputs)
  {
    if (input.second)
    {
      DataObject * inputDO = input.second;

      /**
       * Propagate the UpdateOutputInformation call
       */
      m_Updating = true;
      inputDO->UpdateOutputInformation();
      m_Updating = false;

      /**
       * What is the PipelineMTime of this input? Compare this against
       * our current computation to find the largest one.
       */
      ModifiedTimeType t2 = inputDO->GetPipelineMTime();

      if (t2 > t1)
      {
        t1 = t2;
      }

      /**
       * Pipeline MTime of the input does not include the MTime of the
       * data object itself. Factor these mtimes into the next PipelineMTime
       */
      t2 = inputDO->GetMTime();
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
  if (t1 > m_OutputInformationMTime.GetMTime())
  {
    for (auto & output : m_Outputs)
    {
      DataObject * outputDO = output.second;
      if (outputDO)
      {
        outputDO->SetPipelineMTime(t1);
      }
    }

    /**
     * Verify that all the inputs are consistent with the
     * requirements of the filter. For example, subclasses might want
     * to ensure all the inputs are in the same coordinate frame.
     */
    this->VerifyInputInformation();

    /**
     * Finally, generate the output information.
     */
    this->GenerateOutputInformation();

    /**
     * Keep track of the last time GenerateOutputInformation() was called
     */
    m_OutputInformationMTime.Modified();
  }
}


void
ProcessObject::PropagateRequestedRegion(DataObject * output)
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
  for (auto & input : m_Inputs)
  {
    if (input.second)
    {
      input.second->PropagateRequestedRegion();
    }
  }
  m_Updating = false;
}


void
ProcessObject::GenerateInputRequestedRegion()
{
  /*
   * By default we require all the input to produce the output. This is
   * overridden in the subclasses since we can often produce the output with
   * just a portion of the input data.
   */
  for (auto & input : m_Inputs)
  {
    if (input.second)
    {
      input.second->SetRequestedRegionToLargestPossibleRegion();
    }
  }
}


void
ProcessObject::GenerateOutputRequestedRegion(DataObject * output)
{
  /**
   * By default we set all the output requested regions to be the same.
   */

  for (auto & o : m_Outputs)
  {
    if (o.second && o.second != output)
    {
      o.second->SetRequestedRegion(output);
    }
  }
}


void
ProcessObject::SetMultiThreader(MultiThreaderType * threader)
{
  if (this->m_MultiThreader != threader)
  {
    if (this->m_MultiThreader.IsNull())
    {
      this->m_MultiThreader = threader;
      m_NumberOfWorkUnits = m_MultiThreader->GetNumberOfWorkUnits();
    }
    else
    {
      ThreadIdType oldDefaultNumber = m_MultiThreader->GetNumberOfWorkUnits();
      this->m_MultiThreader = threader;
      ThreadIdType newDefaultNumber = m_MultiThreader->GetNumberOfWorkUnits();
      if (m_NumberOfWorkUnits == oldDefaultNumber)
      {
        m_NumberOfWorkUnits = newDefaultNumber;
      }
      else // clamp to new default
      {
        m_NumberOfWorkUnits = std::min(m_NumberOfWorkUnits, newDefaultNumber);
      }
    }
    this->Modified();
  }
}


void
ProcessObject::PrepareOutputs()
{
  if (this->GetReleaseDataBeforeUpdateFlag())
  {
    for (auto & output : m_Outputs)
    {
      if (output.second)
      {
        output.second->PrepareForNewData();
      }
    }
  }
}


void
ProcessObject::ReleaseInputs()
{
  for (auto & input : m_Inputs)
  {
    if (input.second)
    {
      if (input.second->ShouldIReleaseData())
      {
        input.second->ReleaseData();
      }
    }
  }
}


void
ProcessObject::UpdateOutputData(DataObject * itkNotUsed(output))
{
  /**
   * prevent chasing our tail
   */
  if (m_Updating)
  {
    return;
  }


  /**
   * Prepare all the outputs. This may deallocate previous bulk data.
   */
  this->PrepareOutputs();

  /**
   * Propagate the update call - make sure everything we
   * might rely on is up-to-date
   * Must call PropagateRequestedRegion before UpdateOutputData if multiple
   * inputs since they may lead back to the same data object.
   */
  m_Updating = true;
  m_UpdateThreadID = std::this_thread::get_id();

  if (m_Inputs.size() == 1)
  {
    if (this->GetPrimaryInput())
    {
      this->GetPrimaryInput()->UpdateOutputData();
    }
  }
  else
  {
    for (auto & input : m_Inputs)
    {
      if (input.second)
      {
        input.second->PropagateRequestedRegion();
        input.second->UpdateOutputData();
      }
    }
  }

  /**
   * Cache the state of any ReleaseDataFlag's on the inputs. While the
   * filter is executing, we need to set the ReleaseDataFlag's on the
   * inputs to false in case the current filter is implemented using a
   * mini-pipeline (which will try to release the inputs).  After the
   * filter finishes, we restore the state of the ReleaseDataFlag's
   * before the call to ReleaseInputs().
   */
  this->CacheInputReleaseDataFlags();

  /**
   * Tell all Observers that the filter is starting
   */
  this->InvokeEvent(StartEvent());

  /**
   * GenerateData this object - we have not aborted yet, and our progress
   * before we start to execute is 0.0.
   */
  m_AbortGenerateData = false;
  m_Progress = 0u;

  try
  {
    this->GenerateData();
  }
  catch (const ProcessAborted &)
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

  /**
   * If we ended due to aborting, push the progress up to 1.0 (since
   * it probably didn't end there)
   *
   */
  if (m_AbortGenerateData)
  {
    this->UpdateProgress(1.0f);
  }

  /**
   * Notify end event observers
   */
  this->InvokeEvent(EndEvent());

  /**
   * Now we have to mark the data as up to date.
   */
  for (auto & output : m_Outputs)
  {
    if (output.second)
    {
      output.second->DataHasBeenGenerated();
    }
  }

  /**
   * Restore the state of any input ReleaseDataFlags
   */
  this->RestoreInputReleaseDataFlags();

  /**
   * Release any inputs if marked for release
   */
  this->ReleaseInputs();

  // Mark that we are no longer updating the data in this filter
  m_Updating = false;
}


void
ProcessObject::CacheInputReleaseDataFlags()
{
  m_CachedInputReleaseDataFlags.clear();
  for (auto & input : m_Inputs)
  {
    if (input.second)
    {
      m_CachedInputReleaseDataFlags[input.first] = input.second->GetReleaseDataFlag();
      input.second->ReleaseDataFlagOff();
    }
    else
    {
      m_CachedInputReleaseDataFlags[input.first] = false;
    }
  }
}


void
ProcessObject::RestoreInputReleaseDataFlags()
{
  for (auto & input : m_Inputs)
  {
    if (input.second)
    {
      input.second->SetReleaseDataFlag(m_CachedInputReleaseDataFlags[input.first]);
    }
  }
  m_CachedInputReleaseDataFlags.clear();
}

void
ProcessObject::SetThreaderUpdateProgress(bool arg)
{
  this->m_ThreaderUpdateProgress = arg;
}

void
ProcessObject::GenerateOutputInformation()
{
  /*
   * Default implementation - copy information from first input to all outputs
   */

  DataObject * primaryInput = this->GetPrimaryInput();

  if (primaryInput)
  {
    for (auto & output : m_Outputs)
    {
      if (output.second)
      {
        output.second->CopyInformation(primaryInput);
      }
    }
  }
}


void
ProcessObject::UpdateLargestPossibleRegion()
{
  this->UpdateOutputInformation();

  if (this->GetPrimaryOutput())
  {
    this->GetPrimaryOutput()->SetRequestedRegionToLargestPossibleRegion();
    this->GetPrimaryOutput()->Update();
  }
}


void
ProcessObject::SetNumberOfRequiredInputs(DataObjectPointerArraySizeType nb)
{
  if (m_NumberOfRequiredInputs != nb)
  {
    m_NumberOfRequiredInputs = nb;
    this->Modified();
    if (m_NumberOfRequiredInputs > 0)
    {
      this->AddRequiredInputName(m_IndexedInputs[0]->first);
    }
    if (m_NumberOfRequiredInputs == 0)
    {
      this->RemoveRequiredInputName(m_IndexedInputs[0]->first);
    }
  }
}

} // end namespace itk
