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

  itk::ThresholdImageFilter<FloatImage2DType>::Pointer threshold;
  threshold = itk::ThresholdImageFilter<FloatImage2DType>::New();
  threshold->SetInput(random->GetOutput());
  threshold->Update();

  std::cout << "threshold reference count = " << threshold->GetReferenceCount() << std::endl;
  
  return EXIT_SUCCESS;
}



