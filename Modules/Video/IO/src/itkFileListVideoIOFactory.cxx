/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#include "itkFileListVideoIOFactory.h"
#include "itkFileListVideoIO.h"
#include "itkVersion.h"

namespace itk
{
FileListVideoIOFactory::FileListVideoIOFactory()
{
  this->RegisterOverride(
    "itkVideoIOBase", "itkFileListVideoIO", "FileList Video IO", true, CreateObjectFunction<FileListVideoIO>::New());
}

FileListVideoIOFactory::~FileListVideoIOFactory() = default;

const char *
FileListVideoIOFactory::GetITKSourceVersion() const
{
  return ITK_SOURCE_VERSION;
}

const char *
FileListVideoIOFactory::GetDescription() const
{
  return "FileList VideoIO Factory, allows the loading of a list of image files as a videos into Insight";
}

// Undocumented API used to register during static initialization.
// DO NOT CALL DIRECTLY.
void
FileListVideoIOFactoryRegister__Private()
{
  ObjectFactoryBase::RegisterInternalFactoryOnce<FileListVideoIOFactory>();
}

} // end namespace itk
