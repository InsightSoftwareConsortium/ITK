/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkThresholdImageFilterTest.cxx
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
#include <iostream>
#include <strstream>
#include "itkPhysicalImage.h"
#include "itkRandomImageSource.h"
#include "itkThresholdImageFilter.h"
#include "itkCommand.h"
#include "itkOutputWindow.h"


// this class is used to send output to stdout and not the itk window
class TextOutput : public itk::OutputWindow
{
public:
  virtual void DisplayText(const char* s)
    {
      std::cout << s << std::endl;
    }
};

int main()
{
  // Comment the following if you want to use the itk text output window
  itk::OutputWindow::SetInstance(new TextOutput);

  // Uncomment the following if you want to see each message independently
  // itk::OutputWindow::GetInstance()->PromptUserOn();

  typedef itk::PhysicalImage<float,2> FloatImage2DType;

  itk::RandomImageSource<FloatImage2DType>::Pointer random;
  random = itk::RandomImageSource<FloatImage2DType>::New();
  random->SetMin(0.0);
  random->SetMax(1000.0);

  float spacing[2] = {0.7, 2.1};
  random->SetSpacing( spacing );
  float origin[2] = {15, 400};
  random->SetOrigin( origin );
    
  std::ostrstream *os;

  // Test #1, filter goes out of scope
  itk::OutputWindow::GetInstance()->DisplayText( "Test #1: Filter goes out of scope -----------------" );
  {
  itk::ThresholdImageFilter<FloatImage2DType>::Pointer threshold;
  threshold = itk::ThresholdImageFilter<FloatImage2DType>::New();
  threshold->SetInput(random->GetOutput());
  threshold->Update();

  std::cout << "Input spacing: " << random->GetOutput()->GetSpacing()[0]
            << ", "
            << random->GetOutput()->GetSpacing()[1] << std::endl;
  std::cout << "Output spacing: " << threshold->GetOutput()->GetSpacing()[0]
            << ", "
            << threshold->GetOutput()->GetSpacing()[1] << std::endl;

  os = new std::ostrstream();
  *os << "Filter: " << threshold.GetPointer() << std::ends;
  itk::OutputWindow::GetInstance()->DisplayText( os->str() );
  os->rdbuf()->freeze(0);
  delete os;
  os = new std::ostrstream();
  *os << "Output #0: " << threshold->GetOutput(0).GetPointer() << std::ends;
  itk::OutputWindow::GetInstance()->DisplayText( os->str() );
  os->rdbuf()->freeze(0);
  delete os;
  os = new std::ostrstream();
  *os << "Output #1: " << threshold->GetOutput(1).GetPointer() << std::ends;
  itk::OutputWindow::GetInstance()->DisplayText( os->str() );
  os->rdbuf()->freeze(0);
  delete os;

  threshold->GetOutput(0)->DebugOn();
  threshold->GetOutput(1)->DebugOn();
  threshold->DebugOn();

  itk::OutputWindow::GetInstance()->DisplayText( "Ending Test #1: filter goes out of scope" );
  itk::OutputWindow::GetInstance()->DisplayText( "End of Test #1 -----------------------------------" );
  }
  
  // Test #2, user keeps an extra handle to an output
  itk::OutputWindow::GetInstance()->DisplayText( "Test #2: User keeps an extra hold on an output  -----------------" );
  {
  FloatImage2DType::Pointer keep;

  itk::ThresholdImageFilter<FloatImage2DType>::Pointer threshold;
  threshold = itk::ThresholdImageFilter<FloatImage2DType>::New();
  threshold->SetInput(random->GetOutput());
  threshold->Update();

  os = new std::ostrstream();
  *os << "Filter: " << threshold.GetPointer() << std::ends;
  itk::OutputWindow::GetInstance()->DisplayText( os->str() );
  os->rdbuf()->freeze(0);
  delete os;
  os = new std::ostrstream();
  *os << "Output #0: " << threshold->GetOutput(0).GetPointer() << std::ends;
  itk::OutputWindow::GetInstance()->DisplayText( os->str() );
  os->rdbuf()->freeze(0);
  delete os;
  os = new std::ostrstream();
  *os << "Output #1: " << threshold->GetOutput(1).GetPointer() << std::ends;
  itk::OutputWindow::GetInstance()->DisplayText( os->str() );
  os->rdbuf()->freeze(0);
  delete os;

  threshold->GetOutput(0)->DebugOn();
  threshold->GetOutput(1)->DebugOn();
  threshold->DebugOn();

  keep = threshold->GetOutput(0);

  itk::OutputWindow::GetInstance()->DisplayText( "End of Test #2: last handle on output 0 should go out of scope");
  }
  itk::OutputWindow::GetInstance()->DisplayText( "End of Test #2 -----------------------------------");

  // Test #3, user disconnects a data object from the pipeline
  itk::OutputWindow::GetInstance()->DisplayText( "Test #3: application disconnects a data object from the pipeline  -----------------" );
  {
  FloatImage2DType::Pointer keep;

  itk::ThresholdImageFilter<FloatImage2DType>::Pointer threshold;
  threshold = itk::ThresholdImageFilter<FloatImage2DType>::New();
  threshold->SetInput(random->GetOutput());
  threshold->Update();

  os = new std::ostrstream();
  *os << "Filter: " << threshold.GetPointer() << std::ends;
  itk::OutputWindow::GetInstance()->DisplayText( os->str() );
  os->rdbuf()->freeze(0);
  delete os;
  os = new std::ostrstream();
  *os << "Output #0: " << threshold->GetOutput(0).GetPointer() << std::ends;
  itk::OutputWindow::GetInstance()->DisplayText( os->str() );
  os->rdbuf()->freeze(0);
  delete os;
  os = new std::ostrstream();
  *os << "Output #1: " << threshold->GetOutput(1).GetPointer() << std::ends;
  itk::OutputWindow::GetInstance()->DisplayText( os->str() );
  os->rdbuf()->freeze(0);
  delete os;

  threshold->GetOutput(0)->DebugOn();
  threshold->GetOutput(1)->DebugOn();
  threshold->DebugOn();

  keep = threshold->GetOutput(0);
  keep->DisconnectPipeline();

  itk::OutputWindow::GetInstance()->DisplayText( "End of Test #3: last handle on output 0 should go out of scope");
  }
  itk::OutputWindow::GetInstance()->DisplayText( "End of Test #3 -----------------------------------");
  
  return EXIT_SUCCESS;
}



