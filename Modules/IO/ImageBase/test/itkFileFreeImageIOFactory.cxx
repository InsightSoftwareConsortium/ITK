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
#include "itkFileFreeImageIOFactory.h"
#include "itkFileFreeImageIO.h"
#include "itkCreateObjectFunction.h"
#include "itkVersion.h"

#define SPECIFIC_IMAGEIO_MODULE_TEST

/**
 * Routine that is called when the shared library is loaded by
 * itk::ObjectFactoryBase::LoadDynamicFactories().
 *
 * itkLoad() is C (not C++) function.
 */
extern "C" {
  ITKIOImageBase_EXPORT itk::ObjectFactoryBase* itkLoad();
}


itk::ObjectFactoryBase* itkLoad()
{
  static itk::FileFreeImageIOFactory::Pointer f
    = itk::FileFreeImageIOFactory::New();
  return f;
}

namespace itk
{

FileFreeImageIOFactory::FileFreeImageIOFactory()
{
  this->RegisterOverride("itkImageIOBase",
                         "itkFileFreeImageIO",
                         "ImageIO that creates an in-memory file from a text description",
                         1,
                         CreateObjectFunction<FileFreeImageIO>::New());
}

FileFreeImageIOFactory::~FileFreeImageIOFactory()
{
}

const char*
FileFreeImageIOFactory::GetITKSourceVersion(void) const
{
  return ITK_SOURCE_VERSION;
}

const char*
FileFreeImageIOFactory::GetDescription() const
{
  return "ImageIO that creates an in-memory file from a text description";
}

} // end namespace itk
