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

#ifndef vtkCaptureScreen_h
#define vtkCaptureScreen_h

#include <string>
#include "vtkSmartPointer.h"
#include "vtkWindowToImageFilter.h"
#include "vtkRenderWindow.h"

template <typename TImageWriter>
class vtkCaptureScreen
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(vtkCaptureScreen);

  using ImageWriterType = TImageWriter;

  vtkCaptureScreen(vtkRenderWindow * iRenderer)
    : m_Renderer(iRenderer)
  {}

  vtkCaptureScreen() = default;
  ~vtkCaptureScreen() = default;

  void
  operator()(const std::string & iFileName) const
  {
    Capture(m_Renderer, iFileName);
  }

  void
  operator()(vtkRenderWindow * iRenderer, const std::string & iFileName)
  {
    m_Renderer = iRenderer;
    Capture(m_Renderer, iFileName);
  }

private:
  vtkRenderWindow * m_Renderer{ nullptr };

  void
  Capture(vtkRenderWindow * iRenderer, const std::string & iFileName) const
  {
    if (iRenderer)
    {
      vtkSmartPointer<vtkWindowToImageFilter> Dumper = vtkSmartPointer<vtkWindowToImageFilter>::New();
      Dumper->SetInput(iRenderer);
      Dumper->Update();

      vtkSmartPointer<ImageWriterType> writer = vtkSmartPointer<ImageWriterType>::New();
      writer->SetFileName(iFileName.c_str());
      writer->SetInputConnection(Dumper->GetOutputPort());
      writer->Write();
    }
  }
};

#endif
