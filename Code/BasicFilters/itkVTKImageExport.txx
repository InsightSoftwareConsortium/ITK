/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVTKImageExport.txx
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
#ifndef __itkVTKImageExport_txx
#define __itkVTKImageExport_txx

namespace itk
{

/**
 * The constructor records the name of the pixel's scalar type for the
 * image to be sent to vtkImageImport's ScalarTypeCallback.
 */
template <class TInputImage>
VTKImageExport<TInputImage>::VTKImageExport()
{
  typedef typename InputImageType::PixelType ScalarType;

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

template <class TInputImage>
void VTKImageExport<TInputImage>::PrintSelf(std::ostream& os,
                                            Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}


/**
 * Set the input image for this filter.
 */
template <class TInputImage>
void VTKImageExport<TInputImage>::SetInput(InputImageType* input)
{
  this->ProcessObject::SetNthInput(0, input);
}


/**
 * Get the current input image.
 */
template <class TInputImage>
typename VTKImageExport<TInputImage>::InputImagePointer
VTKImageExport<TInputImage>::GetInput()
{
  return static_cast<TInputImage*>(
    this->ProcessObject::GetInput(0).GetPointer());
}


/**
 * Implements the WholeExtentCallback.  This returns a pointer to an
 * array of six integers describing the VTK-style extent of the image.
 * This corresponds to the ITK image's LargestPossibleRegion.
 */
template <class TInputImage>
int* VTKImageExport<TInputImage>::WholeExtentCallback()
{
  InputImagePointer input = this->GetInput();
  InputRegionType region = input->GetLargestPossibleRegion();
  InputSizeType size = region.GetSize();
  InputIndexType index = region.GetIndex();

  unsigned int i=0;
  // Fill in the known portion of the extent.
  for(;i < InputImageDimension;++i)
    {
    m_WholeExtent[i*2] = int(index[i]);
    m_WholeExtent[i*2+1] = int(index[i]+size[i])-1;
    }
  // Fill in the extent for dimensions up to three.
  for(;i < 3; ++i)
    {
    m_WholeExtent[i*2] = 0;
    m_WholeExtent[i*2+1] = 0;
    }
  return m_WholeExtent;
}


/**
 * Implements the SpacingCallback.  This returns a pointer to an array
 * of three floating point values describing the spacing of the image.
 */
template <class TInputImage>
float* VTKImageExport<TInputImage>::SpacingCallback()
{
  InputImagePointer input = this->GetInput();
  const double* spacing = input->GetSpacing();
  
  unsigned int i=0;
  // Fill in the known portion of the spacing.
  for(;i < InputImageDimension;++i)
    {
    m_DataSpacing[i] = float(spacing[i]);
    }
  // Fill up the spacing with defaults up to three dimensions.
  for(;i < 3;++i)
    {
    m_DataSpacing[i] = 1;
    }
  return m_DataSpacing;
}


/**
 * Implements the OriginCallback.  This returns a pointer to an array
 * of three floating point values describing the origin of the image.
 */
template <class TInputImage>
float* VTKImageExport<TInputImage>::OriginCallback()
{
  InputImagePointer input = this->GetInput();
  const double* origin = input->GetOrigin();

  unsigned int i=0;
  // Fill in the known portion of the origin.
  for(;i < InputImageDimension;++i)
    {
    m_DataOrigin[i] = float(origin[i]);
    }
  // Fill up the origin with defaults up to three dimensions.
  for(;i < 3;++i)
    {
    m_DataOrigin[i] = 0;
    }
  return m_DataOrigin;
}


/**
 * Implements the ScalarTypeCallback.  This returns the name of the
 * scalar value type of the image.
 */
template <class TInputImage>
const char* VTKImageExport<TInputImage>::ScalarTypeCallback()
{
  return m_ScalarTypeName.c_str();
}


/**
 * Implements the NumberOfComponentsCallback.  This returns the number of
 * components per pixel for the image.
 *
 * Currently, only one pixel component is supported by this class.
 */
template <class TInputImage>
int VTKImageExport<TInputImage>::NumberOfComponentsCallback()
{
  return 1;
}


/**
 * Implements the PropagateUpdateExtentCallback.  This takes the
 * update extent from VTK and translates it into a RequestedRegion for
 * the input image.  This requested region is then propagated through
 * the ITK pipeline.
 */
template <class TInputImage>
void VTKImageExport<TInputImage>::PropagateUpdateExtentCallback(int* extent)
{  
  InputSizeType size;
  InputIndexType index;

  for(unsigned int i=0;i < InputImageDimension;++i)
    {
    index[i] = extent[i*2];
    size[i] = (extent[i*2+1]-extent[i*2])+1;
    }
  
  InputRegionType region;
  region.SetSize(size);
  region.SetIndex(index);
  
  InputImagePointer input = this->GetInput();
  input->SetRequestedRegion(region);
}


/**
 * Implements the DataExtentCallback.  This returns a pointer to an
 * array of six integers describing the VTK-style extent of the
 * buffered portion of the image.  This corresponds to the ITK image's
 * BufferedRegion.
 */
template <class TInputImage>
int* VTKImageExport<TInputImage>::DataExtentCallback()
{
  InputImagePointer input = this->GetInput();
  InputRegionType region = input->GetBufferedRegion();
  InputSizeType size = region.GetSize();
  InputIndexType index = region.GetIndex();

  unsigned int i=0;
  for(;i < InputImageDimension;++i)
    {
    m_DataExtent[i*2] = int(index[i]);
    m_DataExtent[i*2+1] = int(index[i]+size[i])-1;
    }
  for(;i < 3; ++i)
    {
    m_DataExtent[i*2] = 0;
    m_DataExtent[i*2+1] = 0;
    }
  return m_DataExtent;
}


/**
 * Implements the BufferPointerCallback.  This returns a pointer to
 * the image's memory buffer.
 */
template <class TInputImage>
void* VTKImageExport<TInputImage>::BufferPointerCallback()
{
  InputImagePointer input = this->GetInput();
  return input->GetBufferPointer();
}

} // end namespace itk

#endif
