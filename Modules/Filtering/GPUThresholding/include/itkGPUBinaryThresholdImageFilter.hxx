#ifndef __itkGPUBinaryThresholdImageFilter_hxx
#define __itkGPUBinaryThresholdImageFilter_hxx

#include "itkGPUBinaryThresholdImageFilter.h"

namespace itk
{
/**
 * Constructor
 */
template< class TInputImage, class TOutputImage >
GPUBinaryThresholdImageFilter< TInputImage, TOutputImage >
::GPUBinaryThresholdImageFilter()
{
  std::ostringstream defines;

  if(TInputImage::ImageDimension > 3)
    {
    itkExceptionMacro("GPUBinaryThresholdImageFilter supports 1/2/3D image.");
    }

  std::vector<std::string> validTypes;
  validTypes.push_back( "uint8_t" );
  validTypes.push_back( "uint16_t" );
  validTypes.push_back( "char" );
  validTypes.push_back( "int" );
  validTypes.push_back( "unsigned int" );
  validTypes.push_back( "float" );
  validTypes.push_back( "double" );

  defines << "#define DIM_" << TInputImage::ImageDimension << "\n";

  std::string validTypeName;
  bool isValid = GetValidTypename(typeid( typename TInputImage::PixelType ), validTypes, validTypeName);
  if (isValid)
    {
      defines << "#define InPixelType " << validTypeName << "\n";
      defines << "#define OutPixelType " << validTypeName << "\n";
#ifdef __APPLE__
      // This is to work around a bug in the OpenCL compiler on Mac OS 10.6 and 10.7 with NVidia drivers
      // where the compiler was not handling uint8_t arguments correctly.
      // be sure to define the kernel arguments as InArgType and OutArgType in the kernel source
      // Using uint16_t instead of uint8_t in the kernel definition
      // is a known workaround to this problem.
      if (validTypeName == "uint8_t")
      {
        defines << "#define InArgType uint16_t\n";
        defines << "#define OutArgType uint16_t\n";
      }
      else
      {
        defines << "#define InArgType " << validTypeName << "\n";
        defines << "#define OutArgType " << validTypeName << "\n";
      }
#else
      defines << "#define InArgType " << validTypeName << "\n";
      defines << "#define OutArgType " << validTypeName << "\n";
#endif
    }
  else
    {
      std::ostringstream excpMsg;
      excpMsg << "GPUBinaryThresholdImageFilter supports";
      unsigned int sz = validTypes.size();
      for (unsigned int ii = 0; ii < sz; ++ii)
        {
        if (ii < sz-1)
          {
            excpMsg << " " << validTypes[ii] << ",";
          }
        else
          {
            excpMsg << " and " << validTypes[ii] << " input and output images.";
          }
        }
      itkExceptionMacro(<< excpMsg.str().c_str());
    }

  std::cout << "Defines: " << defines.str() << std::endl;
  const char* GPUSource = GPUBinaryThresholdImageFilter::GetOpenCLSource();

  // load and build program
  this->m_GPUKernelManager->LoadProgramFromString( GPUSource, defines.str().c_str() );

  // create kernel
  this->m_UnaryFunctorImageFilterGPUKernelHandle = this->m_GPUKernelManager->CreateKernel("BinaryThresholdFilter");
}

template< class TInputImage, class TOutputImage >
void
GPUBinaryThresholdImageFilter< TInputImage, TOutputImage >
::GPUGenerateData()
{
  // set up the functor values
  typename InputPixelObjectType::Pointer lowerThreshold = this->GetLowerThresholdInput();
  typename InputPixelObjectType::Pointer upperThreshold = this->GetUpperThresholdInput();

  if ( lowerThreshold->Get() > upperThreshold->Get() )
    {
    itkExceptionMacro(<< "Lower threshold cannot be greater than upper threshold.");
    }

  // Setup up the functor
  this->GetFunctor().SetLowerThreshold( lowerThreshold->Get() );
  this->GetFunctor().SetUpperThreshold( upperThreshold->Get() );

  this->GetFunctor().SetInsideValue( this->GetInsideValue() );
  this->GetFunctor().SetOutsideValue( this->GetOutsideValue() );

  GPUSuperclass::GPUGenerateData();
}

} // end of namespace itk

#endif
