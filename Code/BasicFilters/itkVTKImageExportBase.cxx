/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVTKImageExportBase.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#include "itkVTKImageExportBase.h"
#include "itkCommand.h"

namespace itk
{

VTKImageExportBase::VTKImageExportBase()
{
  m_LastPipelineMTime = 0;
}

void VTKImageExportBase::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  
  os << indent << "Last Pipeline MTime: "
     << m_LastPipelineMTime<< std::endl;  
}

//----------------------------------------------------------------------------
void* VTKImageExportBase::GetCallbackUserData()
{
  return this;
}

VTKImageExportBase::UpdateInformationCallbackType
VTKImageExportBase::GetUpdateInformationCallback() const
{
  return &VTKImageExportBase::UpdateInformationCallbackFunction;
}

VTKImageExportBase::PipelineModifiedCallbackType
VTKImageExportBase::GetPipelineModifiedCallback() const
{
  return &VTKImageExportBase::PipelineModifiedCallbackFunction;
}

VTKImageExportBase::WholeExtentCallbackType
VTKImageExportBase::GetWholeExtentCallback() const
{
  return &VTKImageExportBase::WholeExtentCallbackFunction;
}

VTKImageExportBase::SpacingCallbackType
VTKImageExportBase::GetSpacingCallback() const
{
  return &VTKImageExportBase::SpacingCallbackFunction;
}

VTKImageExportBase::OriginCallbackType
VTKImageExportBase::GetOriginCallback() const
{
  return &VTKImageExportBase::OriginCallbackFunction;
}

VTKImageExportBase::ScalarTypeCallbackType
VTKImageExportBase::GetScalarTypeCallback() const
{
  return &VTKImageExportBase::ScalarTypeCallbackFunction;
}

VTKImageExportBase::NumberOfComponentsCallbackType
VTKImageExportBase::GetNumberOfComponentsCallback() const
{
  return &VTKImageExportBase::NumberOfComponentsCallbackFunction;
}

VTKImageExportBase::PropagateUpdateExtentCallbackType
VTKImageExportBase::GetPropagateUpdateExtentCallback() const
{
  return &VTKImageExportBase::PropagateUpdateExtentCallbackFunction;
}

VTKImageExportBase::UpdateDataCallbackType
VTKImageExportBase::GetUpdateDataCallback() const
{
  return &VTKImageExportBase::UpdateDataCallbackFunction;
}

VTKImageExportBase::DataExtentCallbackType
VTKImageExportBase::GetDataExtentCallback() const
{
  return &VTKImageExportBase::DataExtentCallbackFunction;
}

VTKImageExportBase::BufferPointerCallbackType
VTKImageExportBase::GetBufferPointerCallback() const
{
  return &VTKImageExportBase::BufferPointerCallbackFunction;
}


//----------------------------------------------------------------------------
void VTKImageExportBase::UpdateInformationCallback()
{
  this->UpdateOutputInformation();
}

int VTKImageExportBase::PipelineModifiedCallback()
{
  DataObjectPointer input = this->GetInput(0);
  unsigned long pipelineMTime = input->GetPipelineMTime();
  if(pipelineMTime > m_LastPipelineMTime)
    {
    m_LastPipelineMTime = pipelineMTime;
    return 1;
    }
  else
    {
    return 0;
    }
}

void VTKImageExportBase::UpdateDataCallback()
{
  // Get the input.
  DataObjectPointer input = this->GetInput(0);
  
  // Notify start event observers
  this->InvokeEvent(Command::StartEvent);

  // Make sure input is up to date.
  input->Update();
  
  // Notify end event observers
  this->InvokeEvent(Command::EndEvent);
}

//----------------------------------------------------------------------------
void VTKImageExportBase::UpdateInformationCallbackFunction(void* userData)
{
  static_cast<VTKImageExportBase*>(userData)->
    UpdateInformationCallback();
}

int VTKImageExportBase::PipelineModifiedCallbackFunction(void* userData)
{
  return static_cast<VTKImageExportBase*>(userData)->
    PipelineModifiedCallback();
}

int* VTKImageExportBase::WholeExtentCallbackFunction(void* userData)
{
  return static_cast<VTKImageExportBase*>(userData)->
    WholeExtentCallback();
}

float* VTKImageExportBase::SpacingCallbackFunction(void* userData)
{
  return static_cast<VTKImageExportBase*>(userData)->
    SpacingCallback();
}

float* VTKImageExportBase::OriginCallbackFunction(void* userData)
{
  return static_cast<VTKImageExportBase*>(userData)->
    OriginCallback();
}

const char* VTKImageExportBase::ScalarTypeCallbackFunction(void* userData)
{
  return static_cast<VTKImageExportBase*>(userData)->
    ScalarTypeCallback();
}
 
int VTKImageExportBase::NumberOfComponentsCallbackFunction(void* userData)
{
  return static_cast<VTKImageExportBase*>(userData)->
    NumberOfComponentsCallback();
}

void VTKImageExportBase::PropagateUpdateExtentCallbackFunction(void* userData,
                                                               int* extent)
{
  static_cast<VTKImageExportBase*>(userData)->
    PropagateUpdateExtentCallback(extent);
}

void VTKImageExportBase::UpdateDataCallbackFunction(void* userData)
{
  static_cast<VTKImageExportBase*>(userData)->
    UpdateDataCallback();
}

int* VTKImageExportBase::DataExtentCallbackFunction(void* userData)
{
  return static_cast<VTKImageExportBase*>(userData)->
    DataExtentCallback();
}

void* VTKImageExportBase::BufferPointerCallbackFunction(void* userData)
{
  return static_cast<VTKImageExportBase*>(userData)->
    BufferPointerCallback();
}


} // end namespace itk
