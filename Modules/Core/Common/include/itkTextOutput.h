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
#ifndef itkTextOutput_h
#define itkTextOutput_h

#include "itkOutputWindow.h"
#include "itkObjectFactory.h"

namespace itk
{
// this class is used to send output to stdout and not the itk window
class ITKCommon_EXPORT TextOutput : public OutputWindow
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(TextOutput);

  using Self = TextOutput;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods).   */
  itkTypeMacro(TextOutput, OutputWindow);

  /** New macro for creation of through a Smart Pointer   */
  itkNewMacro(TextOutput);

  void
  DisplayText(const char * s) override
  {
    std::cout << s << std::endl;
  }

protected:
  TextOutput();
  ~TextOutput() override;
};
} // namespace itk

#endif
