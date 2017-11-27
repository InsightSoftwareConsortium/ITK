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
#include "itkMutexLockHolder.h"

#include <stdio.h>
#include <sstream>
#include <algorithm>

namespace itk
{


namespace
{ // local namespace for managing globals
const size_t ITK_GLOBAL_INDEX_NAMES_NUMBER = 100;
const size_t ITK_GLOBAL_INDEX_NAMES_LENGTH = 4;
const char globalIndexNames[ITK_GLOBAL_INDEX_NAMES_NUMBER][ITK_GLOBAL_INDEX_NAMES_LENGTH] =
{ "_0", "_1", "_2", "_3", "_4", "_5", "_6", "_7", "_8", "_9",
  "_10", "_11", "_12", "_13", "_14", "_15", "_16", "_17", "_18", "_19",
  "_20", "_21", "_22", "_23", "_24", "_25", "_26", "_27", "_28", "_29",
  "_30", "_31", "_32", "_33", "_34", "_35", "_36", "_37", "_38", "_39",
  "_40", "_41", "_42", "_43", "_44", "_45", "_46", "_47", "_48", "_49",
  "_50", "_51", "_52", "_53", "_54", "_55", "_56", "_57", "_58", "_59",
  "_60", "_61", "_62", "_63", "_64", "_65", "_66", "_67", "_68", "_69",
  "_70", "_71", "_72", "_73", "_74", "_75", "_76", "_77", "_78", "_79",
  "_80", "_81", "_82", "_83", "_84", "_85", "_86", "_87", "_88", "_89",
  "_90", "_91", "_92", "_93", "_94", "_95", "_96", "_97", "_98", "_99"
};

}


ProcessObject
::ProcessObject() :
  m_Inputs(),
  m_Outputs(),
  m_CachedInputReleaseDataFlags(),
  m_RequiredInputNames()
{
/*
 * Instantiate object with no start, end, or progress methods.
 */

  m_NumberOfRequiredInputs = 0;
  m_NumberOfRequiredOutputs = 0;

  m_AbortGenerateData = false;
  m_Progress = 0.0f;
  m_Updating = false;

  DataObjectPointerMap::value_type p("Primary", DataObjectPointer() );
  m_IndexedInputs.push_back( m_Inputs.insert(p).first );
  m_IndexedOutputs.push_back( m_Outputs.insert(p).first );

  m_Threader = MultiThreaderType::New();
  m_NumberOfThreads = m_Threader->GetNumberOfThreads();

  m_ReleaseDataBeforeUpdateFlag = true;

}


DataObject::Pointer
ProcessObject
::MakeOutput( const DataObjectIdentifierType & name )
{
/*
 * This is a default implementation to make sure we have something.
 * Once all the subclasses of ProcessObject provide an appopriate
 * MakeOutput(), then ProcessObject::MakeOutput() can be made pure
 * virtual.
 */

  itkDebugMacro("MakeOutput(" << name << ")");
  if( this->IsIndexedOutputName(name) )
    {
    return this->MakeOutput( this->MakeIndexFromOutputName(name) );
    }
  return static_cast<DataObject *>(DataObject::New().GetPointer());
}


DataObject::Pointer
ProcessObject
::MakeOutput(DataObjectPointerArraySizeType)
{
  return static_cast<DataObject *>(DataObject::New().GetPointer());
}


ProcessObject
::~ProcessObject()
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
  for ( DataObjectPointerMap::iterator it = m_Outputs.begin(); it != m_Outputs.end(); ++it )
    {
    if ( it->second )
      {
      // let the output know we no longer want to associate with the object
      it->second->DisconnectSource(this, it->first);
      // let go of our reference to the data object
      it->second = ITK_NULLPTR;
      }
    }
}


void
ProcessObject
::SetNumberOfIndexedInputs(DataObjectPointerArraySizeType num)
{
/*
 * Called by constructor to set up input array.
 */

  if( this->GetNumberOfIndexedInputs() != num )
    {
    if ( num < this->GetNumberOfIndexedInputs() )
      {
      // NB: The primary input must never be removed from the map, or
      // the indexed inputs array!

      // remove the extra inputs
      for( DataObjectPointerArraySizeType i=std::max<DataObjectPointerArraySizeType>(num, 1);
           i<this->GetNumberOfIndexedInputs(); ++i )
        {
        m_Inputs.erase( m_IndexedInputs[i]->first );
        }
      m_IndexedInputs.resize(std::max<DataObjectPointerArraySizeType>(num, 1) );


      if (num < 1 )
        {
        m_IndexedInputs[0]->second = ITK_NULLPTR;
        }
      }
    else
      {
      for ( DataObjectPointerArraySizeType i = m_IndexedInputs.size(); i < num; ++i)
        {
        DataObjectPointerMap::value_type p(this->MakeNameFromInputIndex( i ), DataObjectPointer() );
        // note: insert will not change value if it's already there.
        m_IndexedInputs.push_back ( m_Inputs.insert(p).first );
        }
      }

    this->Modified();
    }
}


ProcessObject::DataObjectPointerArraySizeType
ProcessObject
::GetNumberOfValidRequiredInputs() const
{
/*
 * Get the number of specified inputs
 */
  DataObjectPointerArraySizeType count = 0;
  for( DataObjectPointerArraySizeType i = 0; i < m_NumberOfRequiredInputs; i++ )
    {
    if( this->GetInput( i ) )
      {
      ++count;
      }
    }
  return count;
}


void
ProcessObject
::AddInput(DataObject *input)
{
/*
 * Adds an input to the first null position in the input list.
 * Expands the list memory if necessary
 */
  for ( unsigned idx = 0; idx < this->GetNumberOfIndexedInputs(); ++idx )
    {
    if ( !this->GetInput( idx ) )
      {
      this->SetNthInput( idx, input );
      return;
      }
    }
  this->SetNthInput( this->GetNumberOfIndexedInputs(), input );
}


void
ProcessObject
::RemoveInput(const DataObjectIdentifierType & key)
{
/*
 * Remove an input.
 */

  // if primary or required set to null
  if ( key ==  m_IndexedInputs[0]->first || this->IsRequiredInputName(key) )
    {
    this->SetInput(key, ITK_NULLPTR);
    return;
    }

  // set indexed input to null, remove if last
  for ( DataObjectPointerArraySizeType i = 1; i <  m_IndexedInputs.size(); ++i )
    {
    if ( m_IndexedInputs[i]->first == key )
      {
      this->SetNthInput(i, ITK_NULLPTR);
      if ( i == m_IndexedInputs.size() - 1 )
        {
        // remove the last indexed input
        this->SetNumberOfIndexedInputs( this->GetNumberOfIndexedInputs() -1 );
        }
      return;
      }
    }

  DataObjectPointerMap::iterator it = m_Inputs.find(key);

  if ( it != m_Inputs.end() )
    {
    m_Inputs.erase( it );
    this->Modified();
    }
}


void
ProcessObject
::RemoveInput(DataObjectPointerArraySizeType idx)
{
  if( idx < this->GetNumberOfIndexedInputs() )
    {
    this->RemoveInput( m_IndexedInputs[idx]->first );
    }
  else
    {
    this->RemoveInput( this->MakeNameFromInputIndex( idx ) );
    }
}


void
ProcessObject
::SetInput(const DataObjectIdentifierType & key, DataObject * input)
{
  if( key.empty() )
    {
    itkExceptionMacro("An empty string can't be used as an input identifier");
    }
  DataObjectPointerMap::iterator it = m_Inputs.find(key);
  if( it == m_Inputs.end() )
    {
    // this is a whole new entry
    m_Inputs[key] = input;
    this->Modified();
    }
  else if( it->second.GetPointer() != input )
    {
    // there is already an entry, but not with the right object
    it->second = input;
    this->Modified();
    }
  // the entry is already there - there is nothing to do
}


void
ProcessObject
::SetNthInput(DataObjectPointerArraySizeType idx, DataObject *input)
{
/* Set an Input of this filter. This method
 * does Register()/UnRegister() manually to
 * deal with the fact that smart pointers aren't
 * around to do the reference counting.
 */
  if ( idx >= this->GetNumberOfIndexedInputs() )
    {
    this->SetNumberOfIndexedInputs(idx + 1);
    }
  if ( m_IndexedInputs[idx]->second != input )
    {
    m_IndexedInputs[idx]->second = input;
    this->Modified();
    }
}


void
ProcessObject
::PushBackInput(const DataObject *input)
{
/*
 * Model a queue on the input list by providing a push back
 */

  this->SetNthInput( this->GetNumberOfIndexedInputs(), const_cast< DataObject * >( input ) );
}


void
ProcessObject
::PopBackInput()
{
/*
 * Model a stack on the input list by providing a pop back
 */

  if ( this->GetNumberOfIndexedInputs() > 0 )
    {
    this->SetNumberOfIndexedInputs( this->GetNumberOfIndexedInputs() - 1 );
    }
}


void
ProcessObject
::PushFrontInput(const DataObject *input)
{
  const DataObjectPointerArraySizeType nb = this->GetNumberOfIndexedInputs();
  for( DataObjectPointerArraySizeType i = nb; i > 0; i-- )
    {
    this->SetNthInput( i, this->GetInput(i-1) );
    }
  this->SetNthInput( 0, const_cast< DataObject * >( input ) );
}


void
ProcessObject
::PopFrontInput()
{
  DataObjectPointerArraySizeType nb = this->GetNumberOfIndexedInputs();
  if( nb > 0 )
    {
    for( DataObjectPointerArraySizeType i = 1; i < nb; i++ )
      {
      this->SetNthInput( i-1, this->GetInput( i ) );
      }
    this->SetNumberOfIndexedInputs( nb - 1 );
    }
}


void
ProcessObject
::RemoveOutput(const DataObjectIdentifierType & key)
{
  // if primary or required set to null
  if ( key == m_IndexedOutputs[0]->first )
    {
    this->SetOutput( key, ITK_NULLPTR );
    return;
    }

  // set indexed output to null, remove if last
  for ( DataObjectPointerArraySizeType i = 1; i <  m_IndexedOutputs.size(); ++i )
    {
    if ( m_IndexedOutputs[i]->first == key )
      {
      this->SetNthOutput(i, ITK_NULLPTR);
      if ( i == m_IndexedOutputs.size() - 1 )
        {
        // remove the last indexed input
        this->SetNumberOfIndexedOutputs( this->GetNumberOfIndexedOutputs() -1 );
        }
      return;
      }
    }


  DataObjectPointerMap::iterator it = m_Outputs.find(key);

  if ( it != m_Outputs.end() )
    {
    // let the output know we no longer want to associate with the object
    it->second->DisconnectSource( this, it->first );
    m_Outputs.erase( it );
    // let go of our reference to the data object
    this->Modified();
    }
}


void
ProcessObject
::RemoveOutput(DataObjectPointerArraySizeType idx)
{
  if( idx == this->GetNumberOfIndexedOutputs() - 1 )
    {
    // just remove the last indexed output
    this->SetNumberOfIndexedOutputs( this->GetNumberOfIndexedOutputs() -1 );
    }
  else
    {
    this->RemoveOutput( this->MakeNameFromOutputIndex( idx ) );
    }
}


void
ProcessObject
::SetOutput(const DataObjectIdentifierType & name, DataObject * output)
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

  if( key.empty() )
    {
    itkExceptionMacro("An empty string can't be used as an output identifier");
    }

  // does this change anything?
  DataObjectPointerMap::const_iterator it = m_Outputs.find(key);
  if ( it != m_Outputs.end() && it->second.GetPointer() == output)
    {
    return;
    }

  // Keep a handle to the original output and disconnect the old output from
  // the pipeline
  DataObjectPointer oldOutput;
  if ( m_Outputs[key] )
    {
    oldOutput = m_Outputs[key];
    m_Outputs[key]->DisconnectSource(this, key);
    }

  if ( output )
    {
    output->ConnectSource(this, key);
    }
  // save the current reference (which releases the previous reference)
  m_Outputs[key] = output;

  // if we are clearing an output, we need to create a new blank output
  // so we are prepared for the next Update(). this copies the requested
  // region ivar
  if ( !m_Outputs[key] )
    {
    itkDebugMacro(" creating new output object.");
    DataObjectPointer newOutput = this->MakeOutput(key);
    this->SetOutput(key, newOutput);

    // If we had an output object before, copy the requested region
    // ivars and release data flag to the the new output
    if ( oldOutput )
      {
      newOutput->SetRequestedRegion(oldOutput);
      newOutput->SetReleaseDataFlag( oldOutput->GetReleaseDataFlag() );
      }
    }

  this->Modified();
}


void
ProcessObject
::SetNthOutput(DataObjectPointerArraySizeType idx, DataObject *output)
{
  if ( idx >= this->GetNumberOfIndexedOutputs() )
    {
    this->SetNumberOfIndexedOutputs(idx + 1);
    }
  this->SetOutput( m_IndexedOutputs[idx]->first, output );
}


void
ProcessObject
::AddOutput(DataObject *output)
{
/*
 * Adds an output to the first null position in the output list.
 * Expands the list memory if necessary
 */

  for ( DataObjectPointerArraySizeType idx = 0; idx < this->GetNumberOfIndexedOutputs(); ++idx )
    {
    if ( !this->GetOutput( idx ) )
      {
      this->SetNthOutput( idx, output );
      return;
      }
    }
  this->SetNthOutput( this->GetNumberOfIndexedOutputs(), output );
}


void
ProcessObject
::SetNumberOfIndexedOutputs(DataObjectPointerArraySizeType num)
{
/*
 * Called by constructor to set up output array.
 */

  if( this->GetNumberOfIndexedOutputs() != num )
    {
    if ( num < this->GetNumberOfIndexedOutputs() )
      {
      // NB: The primary output must never be removed from the map, or
      // the indexed inputs array!

      // remove the extra outputs
      for( DataObjectPointerArraySizeType i=std::max<DataObjectPointerArraySizeType>(num, 1);
           i<this->GetNumberOfIndexedOutputs(); ++i )
        {
        // an output should never be ITK_NULLPTR
        itkAssertInDebugAndIgnoreInReleaseMacro( m_IndexedOutputs[i]->second );

        // let the output know we no longer want to associate with the
        // object
        m_IndexedOutputs[i]->second->DisconnectSource( this,  m_IndexedOutputs[i]->first );

        m_Outputs.erase( m_IndexedOutputs[i]->first );
        }

      m_IndexedOutputs.resize( std::max<DataObjectPointerArraySizeType>(num, 1) );

      if (num < 1 )
        {
        m_IndexedOutputs[0]->second = ITK_NULLPTR;
        }
      }
    else
      {
      for ( DataObjectPointerArraySizeType i = m_IndexedOutputs.size(); i < num; ++i)
        {
        DataObjectPointerMap::value_type p(this->MakeNameFromOutputIndex( i ), DataObjectPointer() );
        // note: insert will not change value if it's already there.
        m_IndexedOutputs.push_back ( m_Outputs.insert(p).first );
        }
      }

    this->Modified();
    }

}


DataObject *
ProcessObject
::GetOutput(const DataObjectIdentifierType & key)
{
  DataObjectPointerMap::iterator it = m_Outputs.find(key);
  if ( it == m_Outputs.end() )
    {
    return ITK_NULLPTR;
    }
  return it->second.GetPointer();
}


const DataObject *
ProcessObject
::GetOutput(const DataObjectIdentifierType & key) const
{
  DataObjectPointerMap::const_iterator it = m_Outputs.find(key);
  if ( it == m_Outputs.end() )
    {
    return ITK_NULLPTR;
    }
  return it->second.GetPointer();
}


DataObject *
ProcessObject
::GetOutput(DataObjectPointerArraySizeType i)
{
  return m_IndexedOutputs[i]->second;
}


const DataObject *
ProcessObject
::GetOutput(DataObjectPointerArraySizeType i) const
{
  return m_IndexedOutputs[i]->second;
}


void
ProcessObject
::SetPrimaryOutput(DataObject * object)
{
  this->SetOutput(m_IndexedOutputs[0]->first, object);
}


void
ProcessObject
::SetPrimaryOutputName(const DataObjectIdentifierType & key)
{
  if( key != this->m_IndexedOutputs[0]->first )
    {

    DataObjectPointerMap::value_type p(key, DataObjectPointer() );

    // note: insert will not change value if it's already there.
    DataObjectPointerMap::iterator it = this->m_Outputs.insert( p ).first;

    if ( it->second.IsNull() )
      {
      it->second = this->m_IndexedOutputs[0]->second;
      m_Outputs.erase(this->m_IndexedOutputs[0]);
      }
    this->m_IndexedOutputs[0] = it;

    this->Modified();
    }
}


bool
ProcessObject
::HasOutput( const DataObjectIdentifierType & key ) const
{
  DataObjectPointerMap::const_iterator it = m_Outputs.find(key);
  return it != m_Outputs.end();
}


ProcessObject::NameArray
ProcessObject
::GetOutputNames() const
{
  NameArray res;
  res.reserve(m_Outputs.size());
  for ( DataObjectPointerMap::const_iterator it = m_Outputs.begin(); it != m_Outputs.end(); ++it )
    {
    // only include the primary if it's required or set
    if ( it->first != m_IndexedOutputs[0]->first
         || it->second.IsNotNull() )
      {
      res.push_back( it->first );
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
ProcessObject
::GetOutputs()
{
  DataObjectPointerArray res;
  res.reserve(m_Outputs.size());
  for ( DataObjectPointerMap::iterator it = m_Outputs.begin(); it != m_Outputs.end(); ++it )
    {
    // only include the primary if it's required or set
    if ( it->first != m_IndexedOutputs[0]->first
         || it->second.IsNotNull() )
      {
      res.push_back( it->second.GetPointer() );
      }
    }
  return res;
}


ProcessObject::DataObjectPointerArraySizeType
ProcessObject
::GetNumberOfOutputs() const
{
  // only include the primary if it's required or set
  if ( m_IndexedOutputs[0]->second.IsNotNull() )
    {
    return m_Outputs.size();
    }
  return m_Outputs.size() - 1;
}


ProcessObject::DataObjectPointerArraySizeType
ProcessObject
::GetNumberOfIndexedOutputs() const
{
  // this first element should always contain the primary output's
  // name, if this is not true there is an internal logic error.
  itkAssertInDebugAndIgnoreInReleaseMacro( m_IndexedOutputs.size() >= 1 );

  if( m_IndexedOutputs.size() > 1 )
    {
    return m_IndexedOutputs.size();
    }
  return this->GetPrimaryOutput() != ITK_NULLPTR;
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
ProcessObject
::GetIndexedOutputs()
{
  DataObjectPointerArray res(this->GetNumberOfIndexedOutputs());
  for ( DataObjectPointerArraySizeType i = 0; i < this->GetNumberOfIndexedOutputs(); i++ )
    {
    res[i] = this->GetOutput(i);
    }
  return res;
}


DataObject *
ProcessObject
::GetInput(const DataObjectIdentifierType & key)
{
  DataObjectPointerMap::iterator it = m_Inputs.find(key);
  if ( it == m_Inputs.end() )
    {
    return ITK_NULLPTR;
    }
  return it->second.GetPointer();
}


const DataObject *
ProcessObject
::GetInput(const DataObjectIdentifierType & key) const
{
  DataObjectPointerMap::const_iterator it = m_Inputs.find(key);
  if ( it == m_Inputs.end() )
    {
    return ITK_NULLPTR;
    }
  return it->second.GetPointer();
}


void
ProcessObject
::SetPrimaryInput(DataObject * object)
{
  if ( m_IndexedInputs[0]->second != object )
    {
    m_IndexedInputs[0]->second = object;
    this->Modified();
    }
}


void
ProcessObject
::SetPrimaryInputName(const DataObjectIdentifierType & key)
{
  this->RemoveRequiredInputName( m_IndexedInputs[0]->first );
  this->AddRequiredInputName( key, 0 );
}


bool
ProcessObject
::HasInput( const DataObjectIdentifierType & key ) const
{
  DataObjectPointerMap::const_iterator it = m_Inputs.find(key);
  return it != m_Inputs.end();
}


ProcessObject::NameArray
ProcessObject
::GetInputNames() const
{
  NameArray res;
  res.reserve(m_Inputs.size());
  for ( DataObjectPointerMap::const_iterator it = m_Inputs.begin(); it != m_Inputs.end(); ++it )
    {
    // only include the primary if it's required or set
    if ( it->first != m_IndexedInputs[0]->first
         || it->second.IsNotNull()
         || this->IsRequiredInputName(it->first) )
      {
      res.push_back( it->first );
      }
    }
  return res;
}


bool
ProcessObject
::AddRequiredInputName( const DataObjectIdentifierType & name )
{
  if( name.empty() )
    {
    itkExceptionMacro("An empty string can't be used as an input identifier");
    }

  if( !m_RequiredInputNames.insert( name ).second )
    {
    return false;
    }


  this->AddOptionalInputName(name);

  if( name == m_IndexedInputs[0]->first && m_NumberOfRequiredInputs == 0 )
    {
    m_NumberOfRequiredInputs = 1;
    }

  return true;
}

void
ProcessObject
::AddOptionalInputName( const DataObjectIdentifierType & name )
{

  if( name.empty() )
    {
    itkExceptionMacro("An empty string can't be used as an input identifier");
    }

  // note: insert will not change value if it's already there.
  m_Inputs.insert( DataObjectPointerMap::value_type(name, DataObjectPointer() ) );

  this->Modified();
}


bool
ProcessObject
::AddRequiredInputName( const DataObjectIdentifierType & name,
                        DataObjectPointerArraySizeType idx )
{

  if( name.empty() )
    {
    itkExceptionMacro("An empty string can't be used as an input identifier");
    }

  if( !m_RequiredInputNames.insert( name ).second )
    {
    itkWarningMacro(<< "Input already \"" << name << "\" already required!");
    // Input already required, but it is not added as indexed input?
    return false;
    }

  this->AddOptionalInputName(name, idx);

  if( name == m_IndexedInputs[0]->first && m_NumberOfRequiredInputs == 0 )
    {
    m_NumberOfRequiredInputs = 1;
    }

  return true;
}

void
ProcessObject
::AddOptionalInputName( const DataObjectIdentifierType & name,
                        DataObjectPointerArraySizeType idx )
{

  if( name.empty() )
    {
    itkExceptionMacro("An empty string can't be used as an input identifier");
    }

  DataObjectPointerMap::value_type p(name, DataObjectPointer() );
  // note: insert will not change value if it's already in named inputs.
  DataObjectPointerMap::iterator it = m_Inputs.insert(p).first;

  if ( idx >= this->GetNumberOfIndexedInputs() )
    {
    this->SetNumberOfIndexedInputs(idx + 1);
    }
  else if( !it->second )
    {
    // if the old index had a data object move that to the new name
    it->second = this->GetInput( m_IndexedInputs[idx]->first );
    }

  // remove name of the old input ( may be new default index name
  // i.e. _1 )
  m_Inputs.erase( m_IndexedInputs[idx]->first );

  m_IndexedInputs[idx] = it;

  this->Modified();
}


bool
ProcessObject
::RemoveRequiredInputName( const DataObjectIdentifierType & name )
{
  if( m_RequiredInputNames.erase( name ) )
    {
    if( name == m_IndexedInputs[0]->first && m_NumberOfRequiredInputs == 1 )
      {
      m_NumberOfRequiredInputs = 0;
      }
    this->Modified();
    return true;
    }
  return false;
}


bool
ProcessObject
::IsRequiredInputName( const DataObjectIdentifierType & name ) const
{
  return m_RequiredInputNames.find( name ) != m_RequiredInputNames.end();
}


void
ProcessObject
::SetRequiredInputNames( const NameArray & names )
{
  m_RequiredInputNames.clear();
  for ( NameArray::const_iterator it = names.begin(); it != names.end(); ++it )
    {
    this->AddRequiredInputName( *it );
    }
  this->Modified();
}


ProcessObject::NameArray
ProcessObject
::GetRequiredInputNames() const
{
  NameArray res;
  res.reserve(m_RequiredInputNames.size());
  for ( NameSet::const_iterator it = m_RequiredInputNames.begin(); it != m_RequiredInputNames.end(); ++it )
    {
    res.push_back( *it );
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
ProcessObject
::GetInputs()
{
  DataObjectPointerArray res;
  res.reserve(m_Inputs.size());
  for ( DataObjectPointerMap::iterator it = m_Inputs.begin(); it != m_Inputs.end(); ++it )
    {
    // only include the primary if it's required or set
    if ( it->first != m_IndexedInputs[0]->first
         || it->second.IsNotNull()
         || this->IsRequiredInputName(it->first) )
      {
      res.push_back( it->second.GetPointer() );
      }
    }
  return res;
}


ProcessObject::DataObjectPointerArraySizeType
ProcessObject
::GetNumberOfInputs() const
{
  // only include the primary if it's required or set
  if ( m_IndexedInputs[0]->second.IsNotNull()
       || this->IsRequiredInputName(m_IndexedInputs[0]->first) )
    {
    return m_Inputs.size();
    }
  return m_Inputs.size() - 1;
}


ProcessObject::DataObjectPointerArraySizeType
ProcessObject
::GetNumberOfIndexedInputs() const
{
  // this first element should always contain the primary input's
  // name, if this is not true there is an internal logic error.
  itkAssertInDebugAndIgnoreInReleaseMacro(  m_IndexedInputs.size() >= 1 );

  if (  m_IndexedInputs.size() > 1 )
    {
    return m_IndexedInputs.size();
    }
  return static_cast<ProcessObject::DataObjectPointerArraySizeType>(this->GetPrimaryInput() != ITK_NULLPTR);
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
ProcessObject
::GetIndexedInputs()
{
  DataObjectPointerArray res(this->GetNumberOfIndexedInputs());
  for ( DataObjectPointerArraySizeType i = 0; i < this->GetNumberOfIndexedInputs(); i++ )
    {
    res[i] = this->GetInput(i);
    }
  return res;
}


ProcessObject::DataObjectIdentifierType
ProcessObject
::MakeNameFromInputIndex(DataObjectPointerArraySizeType idx) const
{
  if( idx == 0 )
    {
    return m_IndexedInputs[0]->first;
    }
  return this->MakeNameFromIndex(idx);
}


ProcessObject::DataObjectIdentifierType
ProcessObject
::MakeNameFromOutputIndex(DataObjectPointerArraySizeType idx) const
{
  if( idx == 0 )
    {
    return this->m_IndexedOutputs[0]->first;
    }
  return this->MakeNameFromIndex(idx);
}


ProcessObject::DataObjectIdentifierType
ProcessObject
::MakeNameFromIndex(DataObjectPointerArraySizeType idx) const
{
  if ( idx < ITK_GLOBAL_INDEX_NAMES_NUMBER )
    {
    return ProcessObject::DataObjectIdentifierType( globalIndexNames[idx] );
    }
  else
    {
    char buf[2+21]; // a 64-bit integer is ~20 decimal places max
    sprintf(buf, "_%u", static_cast<unsigned int>(idx));
    return buf;
    }
}


ProcessObject::DataObjectPointerArraySizeType
ProcessObject
::MakeIndexFromInputName(const DataObjectIdentifierType & name) const
{
  if( name == m_IndexedInputs[0]->first )
    {
    itkDebugMacro("MakeIndexFromName("<<name<<") -> 0");
    return 0;
    }
  return this->MakeIndexFromName( name );
}


ProcessObject::DataObjectPointerArraySizeType
ProcessObject
::MakeIndexFromOutputName(const DataObjectIdentifierType & name) const
{
  if( name == this->m_IndexedOutputs[0]->first )
    {
    itkDebugMacro("MakeIndexFromName("<<name<<") -> 0");
    return 0;
    }
  return this->MakeIndexFromName( name );
}


ProcessObject::DataObjectPointerArraySizeType
ProcessObject
::MakeIndexFromName(const DataObjectIdentifierType & name) const
{
  DataObjectIdentifierType baseName = "_";
  DataObjectPointerArraySizeType baseSize = baseName.size();
  if( name.size() <= baseSize || name.substr(0, baseSize) != baseName )
    {
    itkDebugMacro("MakeIndexFromName("<<name<<") -> exception bad base name");
    itkExceptionMacro(<<"Not an indexed data object: " << name);
    }
  DataObjectIdentifierType idxStr = name.substr(baseSize);
  DataObjectPointerArraySizeType idx;
  if( !(std::istringstream(idxStr) >> idx) )
    {
    itkDebugMacro("MakeIndexFromName("<<name<<") -> exception not an index");
    itkExceptionMacro(<<"Not an indexed data object: " << name);
    }
  itkDebugMacro("MakeIndexFromName("<<name<<") -> "<< idx);
  return idx;
}


bool
ProcessObject
::IsIndexedInputName(const DataObjectIdentifierType & name) const
{
  if( name == m_IndexedInputs[0]->first )
    {
    return true;
    }
  for (  DataObjectPointerArraySizeType i = 0; i < m_IndexedInputs.size(); ++i)
    {
    if ( m_IndexedInputs[i]->first == name )
      {
      return true;
      }
    }
  return false;
}


bool
ProcessObject
::IsIndexedOutputName(const DataObjectIdentifierType & name) const
{
  if( name == m_IndexedOutputs[0]->first )
    {
    return true;
    }
  for (  DataObjectPointerArraySizeType i = 0; i < m_IndexedOutputs.size(); ++i)
    {
    if ( m_IndexedOutputs[i]->first == name )
      {
      return true;
      }
    }
  return false;
}


void
ProcessObject
::UpdateProgress(float progress)
{
/*
 * Update the progress of the process object. If a ProgressMethod exists,
 * execute it. Then set the Progress ivar to amount. The parameter amount
 * should range between (0,1).
 */

  // Clamp the value to be between 0 and 1.
  m_Progress = std::max(progress, 0.0f);
  m_Progress = std::min(m_Progress, 1.0f);

  this->InvokeEvent( ProgressEvent() );
}


bool
ProcessObject
::GetReleaseDataFlag() const
{
  if ( this->GetPrimaryOutput() )
    {
    return this->GetPrimaryOutput()->GetReleaseDataFlag();
    }
  itkWarningMacro(<< "Output doesn't exist!");
  return false;
}


void
ProcessObject
::SetReleaseDataFlag(bool val)
{
  for ( DataObjectPointerMap::iterator it=m_Outputs.begin(); it != m_Outputs.end(); ++it )
    {
    if ( it->second )
      {
      it->second->SetReleaseDataFlag(val);
      }
    }
}


void
ProcessObject
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  Indent indent2 = indent.GetNextIndent();
  if ( !m_Inputs.empty() )
    {
    os << indent << "Inputs: " << std::endl;
    for ( DataObjectPointerMap::const_iterator it = m_Inputs.begin(); it != m_Inputs.end(); ++it )
      {
      std::string req = "";
      if( this->IsRequiredInputName( it->first ) )
        {
        req = " *";
        }
      os << indent2 << it->first<< ": (" << it->second.GetPointer() << ")" << req  << std::endl;
      }
    }
  else
    {
    os << indent << "No Inputs\n";
    }

  os << indent << "Indexed Inputs: " << std::endl;
  unsigned int idx = 0;
  for ( std::vector< DataObjectPointerMap::iterator >::const_iterator it = m_IndexedInputs.begin();
        it != m_IndexedInputs.end();
        ++it, ++idx)
    {
    os << indent2 << idx << ": " << (*it)->first  << " (" << (*it)->second.GetPointer() << ")"<< std::endl;
    }

  if( !m_RequiredInputNames.empty() )
    {
    os << indent << "Required Input Names: ";
    for( NameSet::const_iterator it = m_RequiredInputNames.begin(); it != m_RequiredInputNames.end(); ++it )
      {
      if( it != m_RequiredInputNames.begin() )
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
  os << indent << "NumberOfRequiredInputs: "
     << m_NumberOfRequiredInputs << std::endl;

  if ( !m_Outputs.empty() )
    {
    os << indent << "Outputs: " << std::endl;
    for ( DataObjectPointerMap::const_iterator it = m_Outputs.begin(); it != m_Outputs.end(); ++it )
      {
      os << indent2 << it->first << ": (" << it->second.GetPointer() << ")" << std::endl;
      }
    }
  else
    {
    os << indent << "No Outputs\n";
    }
  os << indent << "Indexed Outputs: " << std::endl;
  idx = 0;
  for ( std::vector< DataObjectPointerMap::iterator >::const_iterator it = m_IndexedOutputs.begin();
        it != m_IndexedOutputs.end();
        ++it, ++idx)
    {
    os << indent2 << idx << ": " << (*it)->first  << " (" << (*it)->second.GetPointer() << ")"<< std::endl;
    }

  os << indent << "NumberOfRequiredOutputs: "
     << m_NumberOfRequiredOutputs << std::endl;

  os << indent << "Number Of Threads: "
     << m_NumberOfThreads << std::endl;

  os << indent << "ReleaseDataFlag: "
     << ( this->GetReleaseDataFlag() ? "On" : "Off" ) << std::endl;

  os << indent << "ReleaseDataBeforeUpdateFlag: "
     << ( m_ReleaseDataBeforeUpdateFlag ? "On" : "Off" ) << std::endl;

  os << indent << "AbortGenerateData: " << ( m_AbortGenerateData ? "On" : "Off" ) << std::endl;
  os << indent << "Progress: " << m_Progress << std::endl;

  os << indent << "Multithreader: " << std::endl;
  m_Threader->PrintSelf( os, indent.GetNextIndent() );
}


void
ProcessObject
::Update()
{
  if ( this->GetPrimaryOutput() )
    {
    this->GetPrimaryOutput()->Update();
    }
}


void
ProcessObject
::ResetPipeline()
{
  if ( this->GetPrimaryOutput() )
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
  for ( DataObjectPointerMap::iterator it=m_Inputs.begin(); it != m_Inputs.end(); ++it )
    {
    if ( it->second )
      {
      it->second->PropagateResetPipeline();
      }
    }
}


void
ProcessObject
::VerifyPreconditions()
{
  /**
   * Make sure that all the required named inputs are there and non null
   */
  for( NameSet::const_iterator it = this->m_RequiredInputNames.begin(); it != this->m_RequiredInputNames.end(); ++it )
    {
    if ( this->GetInput( *it ) == ITK_NULLPTR )
      {
      itkExceptionMacro(<< "Input " << *it << " is required but not set.");
      }
    }

  /**
   * Verify the require named inputs.
   */
  NameSet::const_iterator i = m_RequiredInputNames.begin();
  while (i != m_RequiredInputNames.end())
    {
    if ( this->GetInput(*i) == ITK_NULLPTR )
      {
      itkExceptionMacro( << "Required Input " << *i << "is not specified!"
                         << " The required inputs are expected to be the first inputs.");

      }
    ++i;
    }

  /**
    * Count the number of required indexed inputs which have been assigned
    */
  const DataObjectPointerArraySizeType validIndexedInputs = this->GetNumberOfValidRequiredInputs();

  if ( validIndexedInputs < this->m_NumberOfRequiredInputs )
    {
    itkExceptionMacro(<< "At least " << this->m_NumberOfRequiredInputs
                      << " of the first " << this->m_NumberOfRequiredInputs
                      << " indexed inputs are required but only " << validIndexedInputs
                      << " are specified."
                      << " The required inputs are expected to be the first inputs.");
    }
}


void
ProcessObject
::VerifyInputInformation()
{
}


void
ProcessObject
::UpdateOutputInformation()
{
  ModifiedTimeType               t1, t2;
  DataObject *                   input;
  DataObject *                   output;

  /**
   * Watch out for loops in the pipeline
   */
  if ( m_Updating )
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
  t1 = this->GetMTime();

  /**
   * Loop through the inputs
   */
  for ( DataObjectPointerMap::iterator it=m_Inputs.begin(); it != m_Inputs.end(); ++it )
    {
    if ( it->second )
      {
      input = it->second;

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

      if ( t2 > t1 )
        {
        t1 = t2;
        }

      /**
       * Pipeline MTime of the input does not include the MTime of the
       * data object itself. Factor these mtimes into the next PipelineMTime
       */
      t2 = input->GetMTime();
      if ( t2 > t1 )
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
  if ( t1 > m_OutputInformationMTime.GetMTime() )
    {
    for ( DataObjectPointerMap::iterator it=m_Outputs.begin(); it != m_Outputs.end(); ++it )
      {
      output = it->second;
      if ( output )
        {
        output->SetPipelineMTime(t1);
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
ProcessObject
::PropagateRequestedRegion(DataObject *output)
{
  /**
   * check flag to avoid executing forever if there is a loop
   */
  if ( m_Updating )
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
  for ( DataObjectPointerMap::iterator it=m_Inputs.begin(); it != m_Inputs.end(); ++it )
    {
    if ( it->second )
      {
      it->second->PropagateRequestedRegion();
      }
    }
  m_Updating = false;
}


void
ProcessObject
::GenerateInputRequestedRegion()
{
/*
 * By default we require all the input to produce the output. This is
 * overridden in the subclasses since we can often produce the output with
 * just a portion of the input data.
 */
  for ( DataObjectPointerMap::iterator it=m_Inputs.begin(); it != m_Inputs.end(); ++it )
    {
    if ( it->second )
      {
      it->second->SetRequestedRegionToLargestPossibleRegion();
      }
    }
}


void
ProcessObject
::GenerateOutputRequestedRegion(DataObject *output)
{
/**
 * By default we set all the output requested regions to be the same.
 */

  for ( DataObjectPointerMap::iterator it=m_Outputs.begin(); it != m_Outputs.end(); ++it )
    {
    if ( it->second && it->second != output )
      {
      it->second->SetRequestedRegion(output);
      }
    }
}


void
ProcessObject
::PrepareOutputs()
{
  if ( this->GetReleaseDataBeforeUpdateFlag() )
    {
    for ( DataObjectPointerMap::iterator it=m_Outputs.begin(); it != m_Outputs.end(); ++it )
      {
      if ( it->second )
        {
        it->second->PrepareForNewData();
        }
      }
    }
}


void
ProcessObject
::ReleaseInputs()
{
  for ( DataObjectPointerMap::iterator it=m_Inputs.begin(); it != m_Inputs.end(); ++it )
    {
    if ( it->second )
      {
      if ( it->second->ShouldIReleaseData() )
        {
        it->second->ReleaseData();
        }
      }
    }
}


void
ProcessObject
::UpdateOutputData( DataObject * itkNotUsed(output) )
{
  /**
   * prevent chasing our tail
   */
  if ( m_Updating )
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
  if ( m_Inputs.size() == 1 )
    {
    if ( this->GetPrimaryInput() )
      {
      this->GetPrimaryInput()->UpdateOutputData();
      }
    }
  else
    {
    for ( DataObjectPointerMap::iterator it=m_Inputs.begin(); it != m_Inputs.end(); ++it )
      {
      if ( it->second )
        {
        it->second->PropagateRequestedRegion();
        it->second->UpdateOutputData();
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
  this->InvokeEvent( StartEvent() );

  /**
   * GenerateData this object - we have not aborted yet, and our progress
   * before we start to execute is 0.0.
   */
  m_AbortGenerateData = false;
  m_Progress = 0.0f;

  try
    {
    this->GenerateData();
    }
  catch ( ProcessAborted & )
    {
    this->InvokeEvent( AbortEvent() );
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
  if ( m_AbortGenerateData )
    {
    this->UpdateProgress(1.0f);
    }

  /**
   * Notify end event observers
   */
  this->InvokeEvent( EndEvent() );

  /**
   * Now we have to mark the data as up to date.
   */
  for ( DataObjectPointerMap::iterator it=m_Outputs.begin(); it != m_Outputs.end(); ++it )
    {
    if ( it->second )
      {
      it->second->DataHasBeenGenerated();
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
ProcessObject
::CacheInputReleaseDataFlags()
{
  m_CachedInputReleaseDataFlags.clear();
  for ( DataObjectPointerMap::iterator it = m_Inputs.begin(); it != m_Inputs.end(); ++it )
    {
    if ( it->second )
      {
      m_CachedInputReleaseDataFlags[it->first] = it->second->GetReleaseDataFlag();
      it->second->ReleaseDataFlagOff();
      }
    else
      {
      m_CachedInputReleaseDataFlags[it->first] = false;
      }
    }
}


void
ProcessObject
::RestoreInputReleaseDataFlags()
{
  for ( DataObjectPointerMap::iterator it = m_Inputs.begin(); it != m_Inputs.end(); ++it )
    {
    if ( it->second )
      {
      it->second->SetReleaseDataFlag(m_CachedInputReleaseDataFlags[it->first]);
      }
    }
  m_CachedInputReleaseDataFlags.clear();
}


void
ProcessObject
::GenerateOutputInformation()
{
/*
 * Default implementation - copy information from first input to all outputs
 */

  DataObject * input = this->GetPrimaryInput();

  if ( input )
    {
    for ( DataObjectPointerMap::iterator it=m_Outputs.begin(); it != m_Outputs.end(); ++it )
      {
      if ( it->second )
        {
        it->second->CopyInformation(input);
        }
      }
    }
}


void
ProcessObject
::UpdateLargestPossibleRegion()
{
  this->UpdateOutputInformation();

  if ( this->GetPrimaryOutput() )
    {
    this->GetPrimaryOutput()->SetRequestedRegionToLargestPossibleRegion();
    this->GetPrimaryOutput()->Update();
    }
}


template< typename TDomainPartitioner, typename TAssociate >
ProcessObject::ProcessObjectDomainThreader< TDomainPartitioner, TAssociate >
::ProcessObjectDomainThreader()
{
}


template< typename TDomainPartitioner, typename TAssociate >
ProcessObject::ProcessObjectDomainThreader< TDomainPartitioner, TAssociate >
::~ProcessObjectDomainThreader()
{
}


template< typename TDomainPartitioner, typename TAssociate >
void
ProcessObject::ProcessObjectDomainThreader< TDomainPartitioner, TAssociate >
::DetermineNumberOfThreadsUsed()
{
  MultiThreader * multiThreader = this->m_Associate->GetMultiThreader();
  this->SetMultiThreader( multiThreader );
  multiThreader->SetNumberOfThreads( this->m_Associate->GetNumberOfThreads() );

  Superclass::DetermineNumberOfThreadsUsed();
}


void
ProcessObject
::SetNumberOfRequiredInputs( DataObjectPointerArraySizeType nb )
{
  if( m_NumberOfRequiredInputs != nb )
    {
    m_NumberOfRequiredInputs = nb;
    this->Modified();
    if( m_NumberOfRequiredInputs > 0 )
      {
      this->AddRequiredInputName( m_IndexedInputs[0]->first );
      }
    if( m_NumberOfRequiredInputs == 0 )
      {
      this->RemoveRequiredInputName( m_IndexedInputs[0]->first );
      }
    }
}


#if !defined(ITK_LEGACY_REMOVE)
void
ProcessObject
::SetNumberOfInputs(DataObjectPointerArraySizeType num)
{
  this->SetNumberOfIndexedInputs(num);
}


void
ProcessObject
::SetNumberOfOutputs(DataObjectPointerArraySizeType num)
{
  this->SetNumberOfIndexedOutputs(num);
}


void
ProcessObject
::RemoveInput(DataObject * input)
{
  if( !input )
    {
    return;
    }
  DataObjectPointerArraySizeType nb = this->GetNumberOfIndexedInputs();
  for( DataObjectPointerArraySizeType i = 0; i < nb; i++ )
    {
    if( this->GetInput(i) == input )
      {
      this->RemoveInput( i );
      return;
      }
    }
  itkDebugMacro("tried to remove an input that was not in the list");
}


void
ProcessObject
::RemoveOutput(DataObject * output)
{
  if( !output )
    {
    return;
    }
  DataObjectPointerArraySizeType nb = this->GetNumberOfIndexedOutputs();
  for( DataObjectPointerArraySizeType i = 0; i < nb; i++ )
    {
    if( this->GetOutput(i) == output )
      {
      this->RemoveOutput( i );
      return;
      }
    }
  itkDebugMacro("tried to remove an output that was not in the list");
}
#endif

} // end namespace itk
