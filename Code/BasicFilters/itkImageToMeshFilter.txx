/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageToMeshFilter.txx
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
#ifndef _itkImageToMeshFilter_txx
#define _itkImageToMeshFilter_txx
#include "itkImageToMeshFilter.h"


namespace itk
{

/**
 *
 */
template <class TInputImage, class TOutputMesh>
ImageToMeshFilter<TInputImage,TOutputMesh>
::ImageToMeshFilter()
{
  this->ProcessObject::SetNumberOfRequiredInputs(1);

  OutputMeshPointer output
    = dynamic_cast<OutputMeshType*>(this->MakeOutput(0).GetPointer()); 

  this->ProcessObject::SetNumberOfRequiredOutputs(1);
  this->ProcessObject::SetNthOutput(0, output.GetPointer());

}

/**
 *
 */
template <class TInputImage, class TOutputMesh>
ImageToMeshFilter<TInputImage,TOutputMesh>
::~ImageToMeshFilter()
{
}
  

/**
 *   Make Ouput
 */
template <class TInputImage, class TOutputMesh>
DataObject::Pointer
ImageToMeshFilter<TInputImage,TOutputMesh>
::MakeOutput(unsigned int idx)
{
  OutputMeshPointer  outputMesh = OutputMeshType::New();
  return dynamic_cast< DataObject *>( outputMesh.GetPointer() );
}




/**
 *
 */
template <class TInputImage, class TOutputMesh>
void 
ImageToMeshFilter<TInputImage,TOutputMesh>
::SetInput(unsigned int idx, InputImageType *input)
{
  this->ProcessObject::SetNthInput(idx, input);
}


  
/**
 *
 */
template <class TInputImage, class TOutputMesh>
ImageToMeshFilter<TInputImage,TOutputMesh>::InputImagePointer
ImageToMeshFilter<TInputImage,TOutputMesh>
::GetInput(unsigned int idx) 
{
  return dynamic_cast<InputImageType*>
                     (this->ProcessObject::GetInput(idx).GetPointer());
}

 
/**
 *
 */
template <class TInputImage, class TOutputMesh>
ImageToMeshFilter<TInputImage,TOutputMesh>::OutputMeshPointer
ImageToMeshFilter<TInputImage,TOutputMesh>
::GetOutput(void) 
{
  return dynamic_cast<OutputMeshType*>
                     (this->ProcessObject::GetOutput(0).GetPointer());
}


/**
 *
 */
template <class TInputImage, class TOutputMesh>
void 
ImageToMeshFilter<TInputImage,TOutputMesh>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
}



/**
 * copy information from first input to all outputs
 * This is a void implementation to prevent the 
 * ProcessObject version to be called
 */
template <class TInputImage, class TOutputMesh>
void 
ImageToMeshFilter<TInputImage,TOutputMesh>
::GenerateOutputInformation()
{
}


} // end namespace itk

#endif
