/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVTKImageImport.txx
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
#ifndef __itkVTKImageImport_txx
#define __itkVTKImageImport_txx

#include "itkVTKImageImport.h"

namespace itk
{


template <typename TOutputImage>
VTKImageImport<TOutputImage>
::VTKImageImport()
{
  typedef typename OutputImageType::PixelType ScalarType;

  if(typeid(ScalarType) == typeid(double))
    {
    m_ScalarTypeName = "double";
    }
  else if(typeid(ScalarType) == typeid(float))
    {
    m_ScalarTypeName = "float";
    }
  else if(typeid(ScalarType) == typeid(long))
    {
    m_ScalarTypeName = "long";
    }
  else if(typeid(ScalarType) == typeid(unsigned long))
    {
    m_ScalarTypeName = "unsigned long";
    }
  else if(typeid(ScalarType) == typeid(int))
    {
    m_ScalarTypeName = "int";
    }
  else if(typeid(ScalarType) == typeid(unsigned int))
    {
    m_ScalarTypeName = "unsigned int";
    }
  else if(typeid(ScalarType) == typeid(short))
    {
    m_ScalarTypeName = "short";
    }
  else if(typeid(ScalarType) == typeid(unsigned short))
    {
    m_ScalarTypeName = "unsigned short";
    }
  else if(typeid(ScalarType) == typeid(char))
    {
    m_ScalarTypeName = "char";
    }
  else if(typeid(ScalarType) == typeid(unsigned char))
    {
    m_ScalarTypeName = "unsigned char";
    }
  else
    {
    itkErrorMacro(<<"Type currently not supported");
    }
}

/**
 *
 */
template <typename TOutputImage>
void 
VTKImageImport<TOutputImage>
::PropagateRequestedRegion(DataObject* outputPtr)
{
  OutputImageType* output = dynamic_cast<OutputImageType*>(outputPtr);
  if(!output)
    {
    itkErrorMacro(<<"Downcast from DataObject to my Image type failed.");
    return;
    }
  Superclass::PropagateRequestedRegion(output);
  if(m_PropagateUpdateExtentCallback)
    {
    OutputRegionType region = output->GetRequestedRegion();
    OutputSizeType size = region.GetSize();
    OutputIndexType index = region.GetIndex();
    
    int updateExtent[6];
    unsigned int i=0;
    for(;i < OutputImageDimension;++i)
      {
      updateExtent[i*2] = int(index[i]);
      updateExtent[i*2+1] = int(index[i]+size[i])-1;
      }
    for(;i < 3; ++i)
      {
      updateExtent[i*2] = 0;
      updateExtent[i*2+1] = 0;
      }
    (m_PropagateUpdateExtentCallback)(m_CallbackUserData, updateExtent);
    }
}

template <typename TOutputImage>
void 
VTKImageImport<TOutputImage>
::UpdateOutputInformation()
{
  // If set, use the callbacks to propagate pipeline update information.
  if(m_UpdateInformationCallback)
    {
    (m_UpdateInformationCallback)(m_CallbackUserData);
    }
  if(m_PipelineModifiedCallback)
    {
    if((m_PipelineModifiedCallback)(m_CallbackUserData))
      {
      this->Modified();
      }
    }  
  
  Superclass::UpdateOutputInformation();
}

/** 
 *
 */
template <typename TOutputImage>
void 
VTKImageImport<TOutputImage>
::GenerateOutputInformation()
{
  // call the superclass' implementation of this method
  Superclass::GenerateOutputInformation();
  
  // get pointer to the output
  OutputImagePointer output = this->GetOutput();

  if (m_WholeExtentCallback)
    {
    int* extent = (m_WholeExtentCallback)(m_CallbackUserData);
    OutputIndexType index;
    OutputSizeType size;
    
    for(unsigned int i=0;i < OutputImageDimension;++i)
      {
      index[i] = extent[i*2];
      size[i] = (extent[i*2+1]-extent[i*2])+1;
      }
    
    OutputRegionType region;
    region.SetIndex(index);
    region.SetSize(size);
    output->SetLargestPossibleRegion(region);
    }
  if (m_SpacingCallback)
    {
    float* inSpacing = (m_SpacingCallback)(m_CallbackUserData);
    double outSpacing[OutputImageDimension];
    for(unsigned int i=0;i < OutputImageDimension;++i)
      {
      outSpacing[i] = inSpacing[i];
      }
    output->SetSpacing(outSpacing);
    }
  if (m_OriginCallback)
    {
    float* inOrigin = (m_OriginCallback)(m_CallbackUserData);
    double outOrigin[OutputImageDimension];
    for(unsigned int i=0;i < OutputImageDimension;++i)
      {
      outOrigin[i] = inOrigin[i];
      }
    output->SetOrigin(outOrigin);
    }
  if (m_NumberOfComponentsCallback)
    {
    unsigned int components =
      (m_NumberOfComponentsCallback)(m_CallbackUserData);
    if(components != 1)
      {
      itkErrorMacro(<< "Input number of components is " << components
                    << " but should be 1.");
      }
    }
  if (m_ScalarTypeCallback)
    {
    typedef typename OutputImageType::PixelType ScalarType;
    const char* scalarName = (m_ScalarTypeCallback)(m_CallbackUserData);
    if(scalarName != m_ScalarTypeName)
      {
      itkErrorMacro(<< "Input scalar type is " << scalarName
                    << " but should be " << m_ScalarTypeName.c_str());
      }
    }
}


/**
 *
 */
template <typename TOutputImage>
void 
VTKImageImport<TOutputImage>
::GenerateData()
{
  // Normally, GenerateData() allocates memory.  However, the application
  // provides the memory for this filter via the SetImportPointer() method.
  // Therefore, this filter does not call outputPtr->Allocate().
  
  if (m_UpdateDataCallback)
    {
    (m_UpdateDataCallback)(m_CallbackUserData);
    }
  if (m_DataExtentCallback && m_BufferPointerCallback)
    {
    // get pointer to the output
    OutputImagePointer output = this->GetOutput();
    
    int* extent = (m_DataExtentCallback)(m_CallbackUserData);
    OutputIndexType index;
    OutputSizeType size;
    
    unsigned long importSize=1;
    for(unsigned int i=0;i < OutputImageDimension;++i)
      {
      index[i] = extent[i*2];
      size[i] = (extent[i*2+1]-extent[i*2])+1;
      importSize *= size[i];
      }
    
    OutputRegionType region;
    region.SetIndex(index);
    region.SetSize(size);
    output->SetBufferedRegion(region);
    output->Allocate();
    
    void* data = (m_BufferPointerCallback)(m_CallbackUserData);
    OutputPixelType* importPointer = reinterpret_cast<OutputPixelType*>(data);
    OutputPixelType* bufferPointer =
      output->GetPixelContainer()->GetBufferPointer();
    unsigned long bufferSize = output->GetPixelContainer()->Size();
    unsigned long dataSize = bufferSize;
    if(importSize < bufferSize)
      {
      itkErrorMacro(<<"Import and Buffer sizes do not match: "
                    << importSize << " v. " << bufferSize);
      dataSize = importSize;
      }
    else if(importSize > bufferSize)
      {
      itkErrorMacro(<<"Import and Buffer sizes do not match: "
                    << importSize << " v. " << bufferSize);
      }
    memcpy(bufferPointer, importPointer,
	   dataSize*sizeof(OutputPixelType));
    }
}

} // namespace itk

#endif
