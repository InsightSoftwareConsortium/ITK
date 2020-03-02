/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#include "itkAnalyzeObjectLabelMapImageIOFactory.h"
#include "itkCreateObjectFunction.h"
#include "itkAnalyzeObjectLabelMapImageIO.h"
#include "itkVersion.h"

namespace itk
{
void
AnalyzeObjectLabelMapImageIOFactory::PrintSelf(std::ostream &, Indent) const
{}

AnalyzeObjectLabelMapImageIOFactory::AnalyzeObjectLabelMapImageIOFactory()
{
  this->RegisterOverride("itkImageIOBase",
                         "itkAnalyzeObjectLabelMapImageIO",
                         "Anaylze Object Label Map IO",
                         true,
                         CreateObjectFunction<AnalyzeObjectLabelMapImageIO>::New());
}

AnalyzeObjectLabelMapImageIOFactory::~AnalyzeObjectLabelMapImageIOFactory() = default;

const char *
AnalyzeObjectLabelMapImageIOFactory::GetITKSourceVersion() const
{
  return ITK_SOURCE_VERSION;
}

const char *
AnalyzeObjectLabelMapImageIOFactory::GetDescription() const
{
  return "Anaylyze Object Map ImageIO Factory, allows the loading of Object Maps images into insight";
}

// Undocumented API used to register during static initialization.
// DO NOT CALL DIRECTLY.

static bool AnalyzeObjectLabelMapImageIOFactoryHasBeenRegistered{ false };

void AnalyzeObjectLabelMap_EXPORT
AnalyzeObjectLabelMapImageIOFactoryRegister__Private()
{
  if (!AnalyzeObjectLabelMapImageIOFactoryHasBeenRegistered)
  {
    AnalyzeObjectLabelMapImageIOFactoryHasBeenRegistered = true;
    AnalyzeObjectLabelMapImageIOFactory::RegisterOneFactory();
  }
}

} // end namespace itk
