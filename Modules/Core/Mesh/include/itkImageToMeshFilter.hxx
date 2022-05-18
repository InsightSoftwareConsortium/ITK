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
#ifndef itkImageToMeshFilter_hxx
#define itkImageToMeshFilter_hxx

namespace itk
{
/**
 *
 */
template <typename TInputImage, typename TOutputMesh>
ImageToMeshFilter<TInputImage, TOutputMesh>::ImageToMeshFilter()
{
  this->ProcessObject::SetNumberOfRequiredInputs(1);

  OutputMeshPointer output = dynamic_cast<OutputMeshType *>(this->MakeOutput(0).GetPointer());

  this->ProcessObject::SetNumberOfRequiredOutputs(1);
  this->ProcessObject::SetNthOutput(0, output.GetPointer());
}

/**
 *   Make Output
 */
template <typename TInputImage, typename TOutputMesh>
DataObject::Pointer ImageToMeshFilter<TInputImage, TOutputMesh>::MakeOutput(DataObjectPointerArraySizeType)
{
  OutputMeshPointer outputMesh = OutputMeshType::New();

  return dynamic_cast<DataObject *>(outputMesh.GetPointer());
}

/**
 *
 */
template <typename TInputImage, typename TOutputMesh>
void
ImageToMeshFilter<TInputImage, TOutputMesh>::SetInput(unsigned int idx, const InputImageType * input)
{
  // process object is not const-correct, the const_cast
  // is required here.
  this->ProcessObject::SetNthInput(idx, const_cast<InputImageType *>(input));
}

/**
 *
 */
template <typename TInputImage, typename TOutputMesh>
auto
ImageToMeshFilter<TInputImage, TOutputMesh>::GetInput(unsigned int idx) -> const InputImageType *
{
  return dynamic_cast<const InputImageType *>(this->ProcessObject::GetInput(idx));
}

/**
 *
 */
template <typename TInputImage, typename TOutputMesh>
auto
ImageToMeshFilter<TInputImage, TOutputMesh>::GetOutput() -> OutputMeshType *
{
  return dynamic_cast<OutputMeshType *>(this->ProcessObject::GetOutput(0));
}

/**
 * copy information from first input to all outputs
 * This is a void implementation to prevent the
 * ProcessObject version to be called
 */
template <typename TInputImage, typename TOutputMesh>
void
ImageToMeshFilter<TInputImage, TOutputMesh>::GenerateOutputInformation()
{}
} // end namespace itk

#endif
