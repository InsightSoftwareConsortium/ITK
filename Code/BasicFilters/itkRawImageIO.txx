/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRawImageIO.txx
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
#include "itkRawImageIO.h"

namespace itk
{

template <class TPixel>
RawImageIOFactory<TPixel>::RawImageIOFactory()
{
  myProductType::Pointer m_MyProduct = myProductType::New();
  RawImageIO::FileExtensionsListType& extensionsList =
    m_MyProduct->GetSupportedFileExtensions();

  for (int i = 0; i < extensionsList.size(); i++)
    {
    RegisterOverride(m_MyProduct->GetSupportedFileExtensions()[i].c_str(),
                     "RawImageIO", "Create RawImageIO", true,
                     CreateObjectFunction<RawImageIO>::New());
    }
}

template <class TPixel>
const char* RawImageIOFactory<TPixel>::GetITKSourceVersion()
{
  return ITK_SOURCE_VERSION;
}

template <class TPixel>
const char* RawImageIOFactory<TPixel>::GetDescription() const
{
  return "RawImageIOFactory - Object factory with registry";
}

template <class TPixel>
RawImageIO<TPixel>::RawImageIO() 
: ImageIO()
{
  m_FilePrefix = "";
  m_FilePattern = "%s.%d";
  
  this->SetNumberOfComponents(1);
  
  for (int idx = 0; idx < 3; ++idx)
    {
    m_ImageExtent[idx*2] = m_ImageExtent[idx*2 + 1] = 0;
    m_ImageVOI[idx*2] = m_ImageVOI[idx*2 + 1] = 0;
    m_ImageSpacing[idx] = 1.0;
    m_ImageOrigin[idx] = 0.0;
    }
  
  m_HeaderSize = 0;
  m_ManualHeaderSize = false;
  
  // Left over from short reader
  m_ImageMask = 0xffff;
  m_ImageByteOrder = Superclass::BigEndian;
  m_FileDimensionality = 2;
}

template <class TPixel>
RawImageIO<TPixel>::~RawImageIO()
{
}

template <class TPixel>
void RawImageIO<TPixel>::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

template <class TPixel>
unsigned long RawImageIO<TPixel>::GetHeaderSize()
{
  return this->GetHeaderSize(m_ImageExtent[4]); //first z-slice
}

template <class TPixel>
unsigned long RawImageIO<TPixel>::GetHeaderSize(unsigned long idx)
{
  if ( m_FileName == "" && m_FilePattern == "" )
    {
    itkErrorMacro(<<"Either a FileName or FilePattern must be specified.");
    return 0;
    }

  if ( ! m_ManualHeaderSize )
    {
    m_PixelType = ITK_SHORT;
    this->ComputeStrides();

    // make sure we figure out a filename to open
    this->ComputeInternalFileName(idx);
    this->OpenFile();
    
    // Get the size of the header from the size of the image
    m_File.seekg(0,std::ios::end);
    
    return (unsigned long)((unsigned long)m_File.tellg() - 
      (unsigned long)m_Strides[this->GetFileDimensionality()]);
    }

  return m_HeaderSize;
}

template <class TPixel>
void RawImageIO<TPixel>::ComputeInternalFileName(unsigned long slice)
{
  char buf[2048];

  if ( m_FileName == "" && m_FilePattern == "" )
    {
    itkErrorMacro(<<"Either a FileName or FilePattern must be specified.");
    return;
    }

  // make sure we figure out a filename to open
  if ( m_FileName != "" )
    {
    sprintf(buf, "%s", m_FileName.c_str());
    m_InternalFileName = buf;
    }
  else 
    {
    if ( m_FilePrefix != "" )
      {
      sprintf (buf, m_FilePattern.c_str(), m_FilePrefix.c_str(), slice);
      }
    else
      {
      sprintf (buf, m_FilePattern.c_str(), slice);
      }
    m_InternalFileName = buf;
    }
}

template <class TPixel>
void RawImageIO<TPixel>::OpenFile()
{
  if ( m_FileName == "" && m_FilePattern == "")
    {
    itkErrorMacro(<<"Either a FileName or FilePattern must be specified.");
    return;
    }

  // Close file from any previous image
  if ( m_File.is_open() )
    {
    m_File.close();
    }
  
  // Open the new file
  itkDebugMacro(<< "Initialize: opening file " << m_InternalFileName);
#ifdef _WIN32
  m_File.open(m_InternalFileName.c_str(), std::ios::in | std::ios::binary);
#else
  m_File.open(m_InternalFileName.c_str(), std::ios::in);
#endif
  if ( m_File.fail() )
    {
    itkErrorMacro(<< "Could not open file: " << m_InternalFileName);
    return;
    }
}

template <class TPixel>
void RawImageIO<TPixel>::SetHeaderSize(unsigned long size)
{
  if ( size != m_HeaderSize)
    {
    m_HeaderSize = size;
    this->Modified();
    }
  m_ManualHeaderSize = true;
}

template <class TPixel>
void RawImageIO<TPixel>::Load()
{
  this->OpenFile();
  
  //Grab the first slice
  m_FileData = (void*)0;
}

template <class TPixel>
void RawImageIO<TPixel>::Load2D(const std::string fileName)
{
}

template <class TPixel>
void RawImageIO<TPixel>::Load2DSlice(const std::string fileName,
                                     const unsigned int sliceNum,
                                     const unsigned int offset)
{
}

template <class TPixel>
void RawImageIO<TPixel>::Save(const std::string headerFile, 
                              const std::string dataFile)
{
}

template <class TPixel>
void RawImageIO<TPixel>::Save3D(const std::string headerFile, 
                                const std::string dataFile)
{
}

template <class TPixel>
RawImageIO<TPixel>::FileExtensionsListType& 
RawImageIO<TPixel>::GetSupportedFileExtensions() const
{
  static FileExtensionsListType fileExtensionsList;

  // This means this is the first call to us
  if (fileExtensionsList.size() == 0)
    {
    }

  return fileExtensionsList;
}


} // namespace itk
