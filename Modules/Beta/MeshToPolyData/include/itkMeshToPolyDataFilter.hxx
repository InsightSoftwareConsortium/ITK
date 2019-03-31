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
#ifndef itkMeshToPolyDataFilter_hxx
#define itkMeshToPolyDataFilter_hxx

#include "itkMeshToPolyDataFilter.h"

namespace itk
{

template< typename TInputMesh >
MeshToPolyDataFilter< TInputMesh >
::MeshToPolyDataFilter()
{
  // Modify superclass default values, can be overridden by subclasses
  this->SetNumberOfRequiredInputs(1);

  typename PolyDataType::Pointer output =
    static_cast< PolyDataType * >( this->MakeOutput(0).GetPointer() );
  this->ProcessObject::SetNumberOfRequiredOutputs(1);
  this->ProcessObject::SetNthOutput( 0, output.GetPointer() );
}


template< typename TInputMesh >
void
MeshToPolyDataFilter< TInputMesh >
::PrintSelf( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf( os, indent );
}


template< typename TInputMesh >
void
MeshToPolyDataFilter< TInputMesh >
::SetInput(const TInputMesh *input)
{
  // Process object is not const-correct so the const_cast is required here
  this->ProcessObject::SetNthInput( 0, const_cast< TInputMesh * >( input ) );
}


template< typename TInputMesh >
const typename MeshToPolyDataFilter< TInputMesh >::InputMeshType *
MeshToPolyDataFilter< TInputMesh >
::GetInput() const
{
  return itkDynamicCastInDebugMode< const TInputMesh * >( this->GetPrimaryInput() );
}


template< typename TInputMesh >
const typename MeshToPolyDataFilter< TInputMesh >::InputMeshType *
MeshToPolyDataFilter< TInputMesh >
::GetInput(unsigned int idx) const
{
  return dynamic_cast< const TInputMesh * > ( this->ProcessObject::GetInput(idx) );
}


template< typename TInputMesh >
ProcessObject::DataObjectPointer
MeshToPolyDataFilter< TInputMesh >
::MakeOutput(ProcessObject::DataObjectPointerArraySizeType)
{
  return PolyDataType::New().GetPointer();
}


template< typename TInputMesh >
ProcessObject::DataObjectPointer
MeshToPolyDataFilter< TInputMesh >
::MakeOutput(const ProcessObject::DataObjectIdentifierType &)
{
  return PolyDataType::New().GetPointer();
}


template< typename TInputMesh >
typename MeshToPolyDataFilter< TInputMesh >::PolyDataType *
MeshToPolyDataFilter< TInputMesh >
::GetOutput()
{
  // we assume that the first output is of the templated type
  return itkDynamicCastInDebugMode< PolyDataType * >( this->GetPrimaryOutput() );
}


template< typename TInputMesh >
const typename MeshToPolyDataFilter< TInputMesh >::PolyDataType *
MeshToPolyDataFilter< TInputMesh >
::GetOutput() const
{
  // we assume that the first output is of the templated type
  return itkDynamicCastInDebugMode< const PolyDataType * >( this->GetPrimaryOutput() );
}


template< typename TInputMesh >
typename MeshToPolyDataFilter< TInputMesh >::PolyDataType *
MeshToPolyDataFilter< TInputMesh >
::GetOutput(unsigned int idx)
{
  auto * out = dynamic_cast< PolyDataType * > ( this->ProcessObject::GetOutput(idx) );

  if ( out == nullptr && this->ProcessObject::GetOutput(idx) != nullptr )
    {
    itkWarningMacro (<< "Unable to convert output number " << idx << " to type " <<  typeid( PolyDataType ).name () );
    }
  return out;
}


template< typename TInputMesh >
void
MeshToPolyDataFilter< TInputMesh >
::GenerateData()
{
  const InputMeshType * inputMesh   =  this->GetInput();
}

} // end namespace itk

#endif // itkMeshToPolyDataFilter_hxx
