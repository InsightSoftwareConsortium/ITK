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
#include "itkVTKImageExportBase.h"

namespace itk
{
/**
 * Constructor sets up information for the image-type indepenedent
 * callbacks implemented in this superclass.
 */
VTKImageExportBase::VTKImageExportBase()
{
  m_LastPipelineMTime = 0;
}

void VTKImageExportBase::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Last Pipeline MTime: "
     << m_LastPipelineMTime << std::endl;
}

//----------------------------------------------------------------------------
void * VTKImageExportBase::GetCallbackUserData()
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

VTKImageExportBase::CallbackTypeProxy
VTKImageExportBase::GetOriginCallback() const
{
  return CallbackTypeProxy(&VTKImageExportBase::OriginCallbackFunction,
                           &VTKImageExportBase::FloatOriginCallbackFunction);
}

VTKImageExportBase::CallbackTypeProxy
VTKImageExportBase::GetSpacingCallback() const
{
  return CallbackTypeProxy(&VTKImageExportBase::SpacingCallbackFunction,
                           &VTKImageExportBase::FloatSpacingCallbackFunction);
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

/**
 * Implements the UpdateInformationCallback.  This sends
 * UpdateOutputInformation() through the ITK pipeline, which is the
 * equivalent to VTK's UpdateInformation().
 */
void VTKImageExportBase::UpdateInformationCallback()
{
  this->UpdateOutputInformation();
}

/**
 * Implements the PipelineModifiedCallback.  This returns 1 if the
 * ITK pipeline has been modified since the last time this callback was
 * invoked.  If the pipeline has not been modified, this returns 0.
 */
int VTKImageExportBase::PipelineModifiedCallback()
{
  DataObjectPointer input = this->GetInput(0);

  if ( !input )
    {
    itkExceptionMacro(<< "Need to set an input");
    }

  ModifiedTimeType pipelineMTime = input->GetPipelineMTime();

  if ( this->GetMTime() > pipelineMTime )
    {
    pipelineMTime = this->GetMTime();
    }

  // Pipeline MTime of the input does not include the MTime of the
  // data object itself. Factor these mtimes into the next PipelineMTime
  if( input->GetMTime() > pipelineMTime)
    {
    pipelineMTime = input->GetMTime();
    }

  if ( pipelineMTime > m_LastPipelineMTime )
    {
    m_LastPipelineMTime = pipelineMTime;
    return 1;
    }
  else
    {
    return 0;
    }
}

/**
 * Implements the UpdateDataCallback.  This sends and Update() call
 * through the ITK pipeline.
 */
void VTKImageExportBase::UpdateDataCallback()
{
  // Get the input.
  DataObjectPointer input = this->GetInput(0);

  if ( !input )
    {
    itkExceptionMacro(<< "Need to set an input");
    }

  // Notify start event observers
  this->InvokeEvent( StartEvent() );

  // Make sure input is up to date.
  input->Update();

  // Notify end event observers
  this->InvokeEvent( EndEvent() );
}

//----------------------------------------------------------------------------
void VTKImageExportBase::UpdateInformationCallbackFunction(void *userData)
{
  static_cast< VTKImageExportBase * >
  ( userData )->UpdateInformationCallback();
}

int VTKImageExportBase::PipelineModifiedCallbackFunction(void *userData)
{
  return static_cast< VTKImageExportBase * >
         ( userData )->PipelineModifiedCallback();
}

int * VTKImageExportBase::WholeExtentCallbackFunction(void *userData)
{
  return static_cast< VTKImageExportBase * >
         ( userData )->WholeExtentCallback();
}

double * VTKImageExportBase::OriginCallbackFunction(void *userData)
{
  return static_cast< VTKImageExportBase * >
         ( userData )->OriginCallback();
}

float * VTKImageExportBase::FloatOriginCallbackFunction(void *userData)
{
  return static_cast< VTKImageExportBase * >
         ( userData )->FloatOriginCallback();
}

double * VTKImageExportBase::SpacingCallbackFunction(void *userData)
{
  return static_cast< VTKImageExportBase * >
         ( userData )->SpacingCallback();
}

float * VTKImageExportBase::FloatSpacingCallbackFunction(void *userData)
{
  return static_cast< VTKImageExportBase * >
         ( userData )->FloatSpacingCallback();
}

const char * VTKImageExportBase::ScalarTypeCallbackFunction(void *userData)
{
  return static_cast< VTKImageExportBase * >
         ( userData )->ScalarTypeCallback();
}

int VTKImageExportBase::NumberOfComponentsCallbackFunction(void *userData)
{
  return static_cast< VTKImageExportBase * >
         ( userData )->NumberOfComponentsCallback();
}

void VTKImageExportBase::PropagateUpdateExtentCallbackFunction(void *userData,
                                                               int *extent)
{
  static_cast< VTKImageExportBase * >
  ( userData )->PropagateUpdateExtentCallback(extent);
}

int * VTKImageExportBase::DataExtentCallbackFunction(void *userData)
{
  return static_cast< VTKImageExportBase * >
         ( userData )->DataExtentCallback();
}

void * VTKImageExportBase::BufferPointerCallbackFunction(void *userData)
{
  return static_cast< VTKImageExportBase * >
         ( userData )->BufferPointerCallback();
}

void VTKImageExportBase::UpdateDataCallbackFunction(void *userData)
{
  static_cast< VTKImageExportBase * >( userData )->UpdateDataCallback();
}
} // end namespace itk
