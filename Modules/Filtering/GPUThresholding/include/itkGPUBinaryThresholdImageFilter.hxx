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
#ifndef itkGPUBinaryThresholdImageFilter_hxx
#define itkGPUBinaryThresholdImageFilter_hxx

#include "itkGPUBinaryThresholdImageFilter.h"

namespace itk
{
/**
 * Constructor
 */
template< typename TInputImage, typename TOutputImage >
GPUBinaryThresholdImageFilter< TInputImage, TOutputImage >
::GPUBinaryThresholdImageFilter()
{
  std::ostringstream defines;

  if(TInputImage::ImageDimension > 3)
    {
    itkExceptionMacro("GPUBinaryThresholdImageFilter supports 1/2/3D image.");
    }

  std::vector<std::string> validTypes;
  validTypes.push_back( "unsigned char" );
  validTypes.push_back( "unsigned short" );
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
      // where the compiler was not handling unsigned char arguments correctly.
      // be sure to define the kernel arguments as InArgType and OutArgType in the kernel source
      // Using unsigned short instead of unsigned char in the kernel definition
      // is a known workaround to this problem.
      if (validTypeName == "unsigned char")
      {
        defines << "#define InArgType unsigned short\n";
        defines << "#define OutArgType unsigned short\n";
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

template< typename TInputImage, typename TOutputImage >
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
