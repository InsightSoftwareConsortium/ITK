/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRawImageIO.cxx
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

RawImageIOFactory::RawImageIOFactory()
{
  myProductType::Pointer m_MyProduct = RawImageIO::New();
  RawImageIO::FileExtensionsListType& extensionsList =
    m_MyProduct->GetSupportedFileExtensions();
  int i;

  for (i = 0; i < extensionsList.size(); i++)
    {
    RegisterOverride(m_MyProduct->GetSupportedFileExtensions()[i].c_str(),
                     "RawImageIO", "Create RawImageIO", true,
                     CreateObjectFunction<RawImageIO>::New());
    }
}

const char* RawImageIOFactory::GetITKSourceVersion()
{
  return ITK_SOURCE_VERSION;
}

const char* RawImageIOFactory::GetDescription() const
{
  return "RawImageIOFactory - Object factory with registry";
}

RawImageIO::RawImageIO() : ImageIO()
{
}

RawImageIO::~RawImageIO()
{
}

void RawImageIO::Load()
{
}

void RawImageIO::Load2D(const std::string fileName)
{
}

void RawImageIO::Load2DSlice(const std::string fileName,
                                  const unsigned int sliceNum,
                                  const unsigned int offset)
{
}

void RawImageIO::Save(const std::string headerFile, const std::string dataFile)
{
}

void RawImageIO::Save3D(const std::string headerFile, 
                       const std::string dataFile)
{
}

RawImageIO::FileExtensionsListType& 
RawImageIO::GetSupportedFileExtensions() const
{
  static FileExtensionsListType fileExtensionsList;

  // This means this is the first call to us
  if (fileExtensionsList.size() == 0)
    {
    }

  return fileExtensionsList;
}


} // namespace itk
