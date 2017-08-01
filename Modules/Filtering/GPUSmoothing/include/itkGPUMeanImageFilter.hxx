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
#ifndef itkGPUMeanImageFilter_hxx
#define itkGPUMeanImageFilter_hxx

#include "itkGPUMeanImageFilter.h"

namespace itk
{

template< typename TInputImage, typename TOutputImage >
GPUMeanImageFilter< TInputImage, TOutputImage >::GPUMeanImageFilter()
{
  std::ostringstream defines;

  if(TInputImage::ImageDimension > 3)
    {
    itkExceptionMacro("GPUMeanImageFilter supports 1/2/3D image.");
    }

  defines << "#define DIM_" << TInputImage::ImageDimension << "\n";
  defines << "#define PIXELTYPE ";
  GetTypenameInString( typeid ( typename TInputImage::PixelType ), defines );

  const char* GPUSource = GPUMeanImageFilter::GetOpenCLSource();

  // load and build program
  this->m_GPUKernelManager->LoadProgramFromString( GPUSource, defines.str().c_str() );

  // create kernel
  m_MeanFilterGPUKernelHandle = this->m_GPUKernelManager->CreateKernel("MeanFilter");
}

template< typename TInputImage, typename TOutputImage >
GPUMeanImageFilter< TInputImage, TOutputImage >::~GPUMeanImageFilter()
{

}

template< typename TInputImage, typename TOutputImage >
void
GPUMeanImageFilter< TInputImage, TOutputImage >::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

template< typename TInputImage, typename TOutputImage >
void
GPUMeanImageFilter< TInputImage, TOutputImage >::GPUGenerateData()
{
  typedef typename itk::GPUTraits< TInputImage >::Type  GPUInputImage;
  typedef typename itk::GPUTraits< TOutputImage >::Type GPUOutputImage;

  typename GPUInputImage::Pointer  inPtr =  dynamic_cast< GPUInputImage * >( this->ProcessObject::GetInput(0) );
  typename GPUOutputImage::Pointer otPtr =  dynamic_cast< GPUOutputImage * >( this->ProcessObject::GetOutput(0) );

  typename GPUOutputImage::SizeType outSize = otPtr->GetLargestPossibleRegion().GetSize();

  int radius[3];
  int imgSize[3];

  radius[0] = radius[1] = radius[2] = 0;
  imgSize[0] = imgSize[1] = imgSize[2] = 1;

  int ImageDim = (int)TInputImage::ImageDimension;

  for(int i=0; i<ImageDim; i++)
    {
    radius[i]  = (this->GetRadius() )[i];
    imgSize[i] = outSize[i];
    }

  size_t localSize[3], globalSize[3];
  localSize[0] = localSize[1] = localSize[2] = OpenCLGetLocalBlockSize(ImageDim);
  for(int i=0; i<ImageDim; i++)
    {
    globalSize[i] = localSize[i]*(unsigned int)ceil( (float)outSize[i]/(float)localSize[i]); //
                                                                                             // total
                                                                                             // #
                                                                                             // of
                                                                                             // threads
    }


  // arguments set up
  int argidx = 0;
  this->m_GPUKernelManager->SetKernelArgWithImage(m_MeanFilterGPUKernelHandle, argidx++, inPtr->GetGPUDataManager() );
  this->m_GPUKernelManager->SetKernelArgWithImage(m_MeanFilterGPUKernelHandle, argidx++, otPtr->GetGPUDataManager() );

  for(int i=0; i<ImageDim; i++)
    {
    this->m_GPUKernelManager->SetKernelArg(m_MeanFilterGPUKernelHandle, argidx++, sizeof(int), &(radius[i]) );
    }

  for(int i=0; i<ImageDim; i++)
    {
    this->m_GPUKernelManager->SetKernelArg(m_MeanFilterGPUKernelHandle, argidx++, sizeof(int), &(imgSize[i]) );
    }

  // launch kernel
  this->m_GPUKernelManager->LaunchKernel( m_MeanFilterGPUKernelHandle, (int)TInputImage::ImageDimension, globalSize,
                                          localSize );
}

} // end namespace itk

#endif
