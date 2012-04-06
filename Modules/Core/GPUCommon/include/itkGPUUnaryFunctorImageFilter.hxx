#ifndef __itkGPUUnaryFunctorImageFilter_hxx
#define __itkGPUUnaryFunctorImageFilter_hxx

#include "itkGPUUnaryFunctorImageFilter.h"

namespace itk
{

template< class TInputImage, class TOutputImage, class TFunction, class TParentImageFilter >
void
GPUUnaryFunctorImageFilter< TInputImage, TOutputImage, TFunction, TParentImageFilter >
::GenerateOutputInformation()
{
  CPUSuperclass::GenerateOutputInformation();
}

template< class TInputImage, class TOutputImage, class TFunction, class TParentImageFilter >
void
GPUUnaryFunctorImageFilter< TInputImage, TOutputImage, TFunction, TParentImageFilter >
::GPUGenerateData()
{
  // Applying functor using GPU kernel
  typedef typename itk::GPUTraits< TInputImage >::Type  GPUInputImage;
  typedef typename itk::GPUTraits< TOutputImage >::Type GPUOutputImage;

  typename GPUInputImage::Pointer  inPtr =  dynamic_cast< GPUInputImage * >( this->ProcessObject::GetInput(0) );
  typename GPUOutputImage::Pointer otPtr =  dynamic_cast< GPUOutputImage * >( this->ProcessObject::GetOutput(0) );

  typename GPUOutputImage::SizeType outSize = otPtr->GetLargestPossibleRegion().GetSize();

  int imgSize[3];
  imgSize[0] = imgSize[1] = imgSize[2] = 1;

  int ImageDim = (int)TInputImage::ImageDimension;

  for(int i=0; i<ImageDim; i++)
    {
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

  // arguments set up using Functor
  int argidx = (this->GetFunctor() ).SetGPUKernelArguments(this->m_GPUKernelManager,
                                                           m_UnaryFunctorImageFilterGPUKernelHandle);
  // arguments set up
  this->m_GPUKernelManager->SetKernelArgWithImage(m_UnaryFunctorImageFilterGPUKernelHandle, argidx++,
                                                  inPtr->GetGPUDataManager() );
  this->m_GPUKernelManager->SetKernelArgWithImage(m_UnaryFunctorImageFilterGPUKernelHandle, argidx++,
                                                  otPtr->GetGPUDataManager() );
  for(int i=0; i<(int)TInputImage::ImageDimension; i++)
    {
    this->m_GPUKernelManager->SetKernelArg(m_UnaryFunctorImageFilterGPUKernelHandle, argidx++, sizeof(int),
                                           &(imgSize[i]) );
    }

  // launch kernel
  this->m_GPUKernelManager->LaunchKernel(m_UnaryFunctorImageFilterGPUKernelHandle, ImageDim, globalSize, localSize );
}

} // end of namespace itk

#endif
