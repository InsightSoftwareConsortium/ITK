/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkBasicArchitectureTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


Copyright (c) 2000 National Library of Medicine
All rights reserved.

See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include <iostream>
#include "itkImage.h"
#include "itkScalar.h"
#include "itkVector.h"
#include "itkRandomImageSource.h"
#include "itkShrinkImage.h"
#include "itkWriteVTKImage.h"
#include "itkReadVTKImage.h"
#include "itkCommand.h"
#include "itkOutputWindow.h"

class ShowProgressObject
{
public:
  ShowProgressObject(itk::ProcessObject* o)
    {
      m_Process = o;
    }
  void ShowProgress()
    {
      std::cout << "Progress " << m_Process->GetProgress() << std::endl;
    }
  itk::ProcessObject::Pointer m_Process;
};

  
class StartEndEvent
{
public:
  void Start() 
    {
      std::cout << "start event" << std::endl;
    }
  void End()
    {
      std::cout << "end event " << std::endl;
    }
};


void main()
{
//  itk::OutputWindow::GetInstance()->PromptUserOn();
  // Test the creation of an image with native type
  itk::Image<float,2>::Pointer if2 = itk::Image<float,2>::New();

  std::cout << std::endl
            << "Image dimension is " << itk::Image<float,5>::ImageDimension
            << std::endl;
  std::cout << "Image dimension is " << itk::Image<short,1>::ImageDimension
            << std::endl;

  // Begin by creating a simple pipeline
  //
  // Create another source
  itk::ReadVTKImage< itk::Image<itk::Scalar<float>,2> >::Pointer reader;
  reader = itk::ReadVTKImage< itk::Image<itk::Scalar<float>,2> >::New();
  reader->SetFileName("junkInput.vtk");

  // Create a source
  itk::RandomImageSource< itk::Image<itk::Scalar<float>,2> >::Pointer random;
  random = itk::RandomImageSource< itk::Image<itk::Scalar<float>,2> >::New();

  
  // Create a filter
  itk::ShrinkImage< itk::Image<itk::Scalar<float>,2>, itk::Image<itk::Scalar<float>,2> >::Pointer shrink;
  shrink = itk::ShrinkImage< itk::Image<itk::Scalar<float>,2>, itk::Image<itk::Scalar<float>,2> >::New();
  // Create a ShowProgress object
  ShowProgressObject progressWatch(shrink);
  StartEndEvent startEndWatch;
  // Create a command that to call ShowProgress when event is fired
  itk::SimpleMemberCommand<ShowProgressObject>::Pointer command;
  command = itk::SimpleMemberCommand<ShowProgressObject>::New();
  command->SetCallbackFunction(&progressWatch,
                               ShowProgressObject::ShowProgress);
  // Add the command as an observer to the shrink object
  shrink->AddObserver(itk::Command::ProgressEvent, command);
  itk::SimpleMemberCommand<StartEndEvent>::Pointer start;
  start = itk::SimpleMemberCommand<StartEndEvent>::New();
  start->SetCallbackFunction(&startEndWatch,
                          StartEndEvent::Start);
  shrink->AddObserver(itk::Command::StartEvent, 
                      start);
  itk::SimpleMemberCommand<StartEndEvent>::Pointer end;
  end = itk::SimpleMemberCommand<StartEndEvent>::New();
  end->SetCallbackFunction(&startEndWatch,
                          StartEndEvent::End);
  shrink->AddObserver(itk::Command::EndEvent, 
                      end);
  shrink->SetInput(random->GetOutput());
  shrink->SetShrinkFactor(2);
  shrink->DebugOn();

  // Create a mapper
  itk::WriteVTKImage< itk::Image<itk::Scalar<float>,2> >::Pointer writer;
  writer = itk::WriteVTKImage< itk::Image<itk::Scalar<float>,2> >::New();
  writer->SetInput(shrink->GetOutput());
  writer->SetFileName("junkImage.vtk");
  writer->SetFileTypeToASCII();
  writer->DebugOn();
  writer->Write();
  exit(EXIT_SUCCESS);
}



