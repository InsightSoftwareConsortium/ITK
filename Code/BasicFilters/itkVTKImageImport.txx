/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVTKImageImport.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

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
    itkExceptionMacro(<<"Type currently not supported");
    }
  m_DataExtentCallback = 0;
  m_WholeExtentCallback = 0;
  m_BufferPointerCallback = 0;
  m_UpdateDataCallback = 0;
  m_PipelineModifiedCallback = 0;
  m_NumberOfComponentsCallback = 0;
  m_SpacingCallback = 0;
  m_OriginCallback = 0;
  m_UpdateInformationCallback = 0;
  m_ScalarTypeCallback = 0;
  m_DataExtentCallback = 0;
  m_PropagateUpdateExtentCallback = 0;
  m_CallbackUserData = 0;
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
    itkExceptionMacro(<<"Downcast from DataObject to my Image type failed.");
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
      itkExceptionMacro(<< "Input number of components is " << components
                        << " but should be 1.");
      }
    }
  if (m_ScalarTypeCallback)
    {
    typedef typename OutputImageType::PixelType ScalarType;
    const char* scalarName = (m_ScalarTypeCallback)(m_CallbackUserData);
    if(scalarName != m_ScalarTypeName)
      {
      itkExceptionMacro(<< "Input scalar type is " << scalarName
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
      itkExceptionMacro(<<"Import and Buffer sizes do not match: "
                        << importSize << " v. " << bufferSize);
      dataSize = importSize;
      }
    else if(importSize > bufferSize)
      {
      itkExceptionMacro(<<"Import and Buffer sizes do not match: "
                        << importSize << " v. " << bufferSize);
      }
    memcpy(bufferPointer, importPointer,
           dataSize*sizeof(OutputPixelType));
    }
}

template <typename TOutputImage>
void 
VTKImageImport<TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  if (m_DataExtentCallback)
    {
    os << "DataExtentCallback: " << m_DataExtentCallback << std::endl;
    }
  if (m_WholeExtentCallback)
    {
    os << "WholeExtentCallback: " << m_WholeExtentCallback << std::endl;
    }
  if (m_BufferPointerCallback)
    {
    os << "BufferPointerCallback: " << m_BufferPointerCallback << std::endl;
    }
  if (m_UpdateDataCallback)
    {
    os << "UpdateDataCallback: " << m_UpdateDataCallback << std::endl;
    }
  if (m_PipelineModifiedCallback)
    {
    os << "PipelineModifiedCallback: " << m_PipelineModifiedCallback << std::endl;
    }
  if (m_NumberOfComponentsCallback)
    {
    os << "NumberOfComponentsCallback: " << m_NumberOfComponentsCallback << std::endl;
    }
  if (m_SpacingCallback)
    {
    os << "SpacingCallback: " << m_SpacingCallback << std::endl;
    }
  if (m_OriginCallback)
    {
    os << "OriginCallback: " << m_OriginCallback << std::endl;
    }
  if (m_UpdateInformationCallback)
    {
    os << "UpdateInformationCallback: " << m_UpdateInformationCallback << std::endl;
    }
  if (m_ScalarTypeCallback)
    {
    os << "ScalarTypeCallback: " << m_ScalarTypeCallback << std::endl;
    }
  if (m_PropagateUpdateExtentCallback)
    {
    os << "PropagateUpdateExtentCallback: " << m_PropagateUpdateExtentCallback << std::endl;
    }
  if (m_CallbackUserData)
    {
    os << "CallbackUserData: " << m_CallbackUserData << std::endl;
    }
}

} // namespace itk

#endif
