/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMeshSource.txx
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
#ifndef _itkMeshSource_txx
#define _itkMeshSource_txx

#include "itkMeshSource.h"

namespace itk
{

/**
 *
 */
template<class TOutputMesh>
MeshSource<TOutputMesh>
::MeshSource()
{
  // Create the output. We use static_cast<> here because we know the default
  // output must be of type TOutputMesh
  typename TOutputMesh::Pointer output
    = static_cast<TOutputMesh*>(this->MakeOutput(0).GetPointer()); 

  this->ProcessObject::SetNumberOfRequiredOutputs(1);
  this->ProcessObject::SetNthOutput(0, output.GetPointer());

  m_GenerateDataRegion = 0;
  m_GenerateDataNumberOfRegions = 0;
}

/**
 *
 */
template<class TOutputMesh>
MeshSource<TOutputMesh>::DataObjectPointer
MeshSource<TOutputMesh>
::MakeOutput(unsigned int idx)
{
  return static_cast<DataObject*>(TOutputMesh::New().GetPointer());
}

/**
 *
 */
template<class TOutputMesh>
MeshSource<TOutputMesh>::OutputMeshPointer
MeshSource<TOutputMesh>
::GetOutput()
{
  if (this->GetNumberOfOutputs() < 1)
    {
    return 0;
    }
  
  return static_cast<TOutputMesh*>
                     (this->ProcessObject::GetOutput(0).GetPointer());
}

  
/**
 *
 */
template<class TOutputMesh>
MeshSource<TOutputMesh>::OutputMeshPointer
MeshSource<TOutputMesh>
::GetOutput(unsigned int idx)
{
  return static_cast<TOutputMesh*>
                     (this->ProcessObject::GetOutput(idx).GetPointer());
}


/**
 *
 */
template<class TOutputMesh>
void 
MeshSource<TOutputMesh>
::SetOutput(TOutputMesh *output)
{
  itkWarningMacro(<< "SetOutput(): This method is slated to be removed from ITK.  Please use GraftOutput() in possible combination with DisconnectPipeline() instead." );
  this->ProcessObject::SetNthOutput(0, output);
}


/**
 *
 */
template<class TOutputMesh>
void 
MeshSource<TOutputMesh>
::GenerateInputRequestedRegion()
{
  Superclass::GenerateInputRequestedRegion();
}


/**
 * 
 */
template<class TOutputMesh>
void
MeshSource<TOutputMesh>
::GraftOutput(TOutputMesh *graft)
{
  OutputMeshPointer output = this->GetOutput();

  if (output && graft)
    {
    // grab a handle to the bulk data of the specified data object
    // output->SetPixelContainer( graft->GetPixelContainer() );
    
    // copy the region ivars of the specified data object
    // output->SetRequestedRegion( graft->GetRequestedRegion() );
    // output->SetLargestPossibleRegion( graft->GetLargestPossibleRegion() );
    // output->SetBufferedRegion( graft->GetBufferedRegion() );

    // copy the meta-information
    output->CopyInformation( graft );
    }
}


/**
 *
 */
template<class TOutputMesh>
void 
MeshSource<TOutputMesh>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
}

} // end namespace itk

#endif
