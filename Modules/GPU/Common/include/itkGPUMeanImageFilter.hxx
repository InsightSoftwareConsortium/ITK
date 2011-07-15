#ifndef __itkGPUMeanImageFilter_hxx
#define __itkGPUMeanImageFilter_hxx

#include "itkGPUMeanImageFilter.h"

namespace itk
{

  template< class TInputImage, class TOutputImage >
  GPUMeanImageFilter< TInputImage, TOutputImage >::GPUMeanImageFilter()
  {
    std::ostringstream defines;

    if(TInputImage::ImageDimension > 3)
      {
      itkExceptionMacro("GPUMeanImageFilter supports 1/2/3D image.");
      }

    defines << "#define DIM_" << TInputImage::ImageDimension << "\n";

    if ( typeid ( typename TInputImage::PixelType ) == typeid ( unsigned char ) )
      {
      defines << "#define PIXELTYPE unsigned char\n";
      }
    else if ( typeid ( typename TInputImage::PixelType ) == typeid ( short ) )
      {
      defines << "#define PIXELTYPE short\n";
      }
    else if ( typeid ( typename TInputImage::PixelType ) == typeid ( int ) )
      {
      defines << "#define PIXELTYPE int\n";
      }
    else if ( typeid ( typename TInputImage::PixelType ) == typeid ( float ) )
      {
      defines << "#define PIXELTYPE float\n";
      }
    else
      {
      itkExceptionMacro("GPUMeanImageFilter supports unsigned char, short, int and float images.");
      }

    std::string oclSrcPath = std::string ( itk_root_path ) + "/Code/GPU/GPUMeanImageFilter.cl";

    std::cout << "Defines: " << defines.str() << "\nSource code path: " << oclSrcPath << std::endl;

    // load and build program
    this->m_KernelManager->LoadProgramFromFile( oclSrcPath.c_str(), defines.str().c_str() );

    // create kernel
    this->m_KernelHandle = this->m_KernelManager->CreateKernel("MeanFilter");
  }

  template< class TInputImage, class TOutputImage >
GPUMeanImageFilter< TInputImage, TOutputImage >::~GPUMeanImageFilter()
{

}

template< class TInputImage, class TOutputImage >
void
GPUMeanImageFilter< TInputImage, TOutputImage >::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  //os << indent << "GPU: " << ( m_GPUEnabled ? "Enabled" : "Disabled" ) << std::endl;
}


template< class TInputImage, class TOutputImage >
void
GPUMeanImageFilter< TInputImage, TOutputImage >::GPUGenerateData()
{
  typedef typename itk::GPUTraits< TInputImage >::Type    GPUInputImage;
  typedef typename itk::GPUTraits< TOutputImage >::Type   GPUOutputImage;

  typename GPUInputImage::Pointer  inPtr =  dynamic_cast< GPUInputImage * >( this->ProcessObject::GetInput(0) );
  typename GPUOutputImage::Pointer otPtr =  dynamic_cast< GPUOutputImage * >( this->ProcessObject::GetOutput(0) );

  typename GPUOutputImage::SizeType outSize = otPtr->GetLargestPossibleRegion().GetSize();

  int radius[3];
  int imgSize[3];

  radius[0] = radius[1] = radius[2] = 0;
  imgSize[0] = imgSize[1] = imgSize[2] = 1;

  for(int i=0; i<(int)TInputImage::ImageDimension; i++)
    {
    radius[i]  = (this->GetRadius())[i];
    imgSize[i] = outSize[i];
    }

  size_t localSize[2], globalSize[2];
  localSize[0] = localSize[1] = 16;
  globalSize[0] = localSize[0]*(unsigned int)ceil((float)outSize[0]/(float)localSize[0]); // total # of threads
  globalSize[1] = localSize[1]*(unsigned int)ceil((float)outSize[1]/(float)localSize[1]);

  // arguments set up
  int argidx = 0;
  this->m_KernelManager->SetKernelArgWithImage(this->m_KernelHandle, argidx++, inPtr->GetGPUDataManager());
  this->m_KernelManager->SetKernelArgWithImage(this->m_KernelHandle, argidx++, otPtr->GetGPUDataManager());

  for(int i=0; i<(int)TInputImage::ImageDimension; i++)
    {
    this->m_KernelManager->SetKernelArg(this->m_KernelHandle, argidx++, sizeof(int), &(radius[i]));
    }

  for(int i=0; i<(int)TInputImage::ImageDimension; i++)
    {
    this->m_KernelManager->SetKernelArg(this->m_KernelHandle, argidx++, sizeof(int), &(imgSize[i]));
    }

  // launch kernel
  this->m_KernelManager->LaunchKernel( this->m_KernelHandle, (int)TInputImage::ImageDimension, globalSize, localSize );
}

} // end namespace itk

#endif
