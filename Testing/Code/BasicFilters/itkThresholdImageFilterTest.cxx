/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkThresholdImageFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


Copyright (c) 2000 National Library of Medicine
All rights reserved.

See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include <iostream>
#include <strstream>
#include "itkPhysicalImage.h"
#include "itkScalar.h"
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

//  typedef itk::Image<itk::Scalar<float>,2> FloatImage2DType;
  typedef itk::PhysicalImage<float,2> FloatImage2DType;

  itk::RandomImageSource<FloatImage2DType>::Pointer random;
  random = itk::RandomImageSource<FloatImage2DType>::New();
  random->SetMin(0.0);
  random->SetMax(1000.0);

  std::ostrstream *os;

  // Test #1, filter goes out of scope
  itk::OutputWindow::GetInstance()->DisplayText( "Test #1: Filter goes out of scope -----------------" );
  {
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



