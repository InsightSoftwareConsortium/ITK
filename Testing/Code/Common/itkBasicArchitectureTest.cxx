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
#include "itkWriteRawImage.h"
#include "itkReadVTKImage.h"
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

// The following three classes are used to support callbacks
// on the shrink filter in the pipeline that follows later
class ShowProgressObject
{
public:
  ShowProgressObject(itk::ProcessObject* o)
    {m_Process = o;}
  void ShowProgress()
    {std::cout << "Progress " << m_Process->GetProgress() << std::endl;}
  itk::ProcessObject::Pointer m_Process;
};
  
class StartEndEvent
{
public:
  void Start() 
    {std::cout << "start event" << std::endl;}
  void End()
    {std::cout << "end event " << std::endl;}
};


class AllEvents
{
public:
  void WatchEvents(itk::LightObject *caller, unsigned long event)
    {
      const char* eventName = 0;
      switch(event)
        {
        case itk::Command::DeleteEvent:
          eventName = "DeleteEvent";
          break;
        case itk::Command::StartEvent:
          eventName = "StartEvent";
          break;
        case itk::Command::EndEvent:
          eventName = "EndEvent";
          break;
        case itk::Command::ProgressEvent:
          {
          itk::ProcessObject* obj = dynamic_cast<itk::ProcessObject*>(caller);
          std::cout << "AnyEvent Progress " << obj->GetProgress() << std::endl;
          eventName = "ProgressEvent";
          break;
          }
        case itk::Command::PickEvent:
          eventName = "PickEvent";
          break;
        case itk::Command::StartPickEvent:
          eventName = "StartPickEvent";
          break;
        case itk::Command::AbortCheckEvent:
          eventName = "AbortCheckEvent";
          break;
        case itk::Command::ExitEvent:
          eventName = "ExitEvent";
          break;
        default:
          eventName = "UserEvent";
        }
      std::cout << "Event name: " << eventName << " Id: " << event << std::endl;
    }
};


int main()
{
  // Comment the following if you want to use the itk text output window
  itk::OutputWindow::SetInstance(new TextOutput);
  // Uncomment the following if you want to see each message independently
  // itk::OutputWindow::GetInstance()->PromptUserOn();

  // Test the creation of an image with native type
  //
  itk::Image<float,2>::Pointer if2 = itk::Image<float,2>::New();

  std::cout << std::endl
            << "Image dimension is " << itk::Image<float,5>::ImageDimension
            << std::endl;
  std::cout << "Image dimension is " << itk::Image<short,1>::ImageDimension
            << std::endl;

  // Begin by creating a simple pipeline. Use the Scalar class as a pixel.
  //
  // Create a typedef to make the code more digestable
  typedef itk::Image<itk::Scalar<float>,2> FloatImage2DType;

  // Create a source object (in this case a reader)
  itk::ReadVTKImage<FloatImage2DType>::Pointer reader;
  reader = itk::ReadVTKImage<FloatImage2DType>::New();
  reader->SetFileName("junkInput.vtk");

  // Create another source object (in this case a random image generator).
  // The source object is templated on the output type.
  //
  itk::RandomImageSource<FloatImage2DType>::Pointer random;
  random = itk::RandomImageSource<FloatImage2DType>::New();
  random->SetMin(0.0);
  random->SetMax(1.0);

  // Create a filter...shrink the image by an integral amount. We also 
  // add some callbacks to the start, progress, and end filter execution
  // methods. The filter is templated on the input and output data types.
  //
  itk::ShrinkImage<FloatImage2DType,FloatImage2DType>::Pointer shrink;
  shrink = itk::ShrinkImage<FloatImage2DType,FloatImage2DType>::New();
  shrink->SetInput(random->GetOutput());
  shrink->SetShrinkFactor(2);
  shrink->DebugOn();
  
  // Create a command to call ShowProgress when progress event is triggered
  ShowProgressObject progressWatch(shrink);
  itk::SimpleMemberCommand<ShowProgressObject>::Pointer command;
  command = itk::SimpleMemberCommand<ShowProgressObject>::New();
  command->SetCallbackFunction(&progressWatch,
                               &ShowProgressObject::ShowProgress);
  shrink->AddObserver(itk::Command::ProgressEvent, command);
  
  // Create a command to call StartEndEvent when start event is triggered
  StartEndEvent startEndWatch;
  itk::SimpleMemberCommand<StartEndEvent>::Pointer start;
  start = itk::SimpleMemberCommand<StartEndEvent>::New();
  start->SetCallbackFunction(&startEndWatch, &StartEndEvent::Start);
  shrink->AddObserver(itk::Command::StartEvent, start);
  
  // Create a command to call StartEndEvent when end event is triggered
  itk::SimpleMemberCommand<StartEndEvent>::Pointer end;
  end = itk::SimpleMemberCommand<StartEndEvent>::New();
  end->SetCallbackFunction(&startEndWatch, &StartEndEvent::End);
  shrink->AddObserver(itk::Command::EndEvent, end);
  
  // Create a command that to call AnyEvent when event is fired
  AllEvents allWatch;
  itk::MemberCommand<AllEvents>::Pointer allEvents;
  allEvents = itk::MemberCommand<AllEvents>::New();
  allEvents->SetCallbackFunction(&allWatch,
                                 &AllEvents::WatchEvents);
  shrink->AddObserver(itk::Command::AnyEvent, allEvents);
  
  // Create a mapper (in this case a writer). A mapper
  // is templated on the input type.
  //
  itk::WriteVTKImage<FloatImage2DType>::Pointer writer;
  writer = itk::WriteVTKImage<FloatImage2DType>::New();
  writer->SetInput(shrink->GetOutput());
  writer->SetFileName("BasicArchitectureImage.vtk");
  writer->SetFileTypeToASCII();
  writer->DebugOn();
  writer->Write();

  itk::WriteRawImage<FloatImage2DType>::Pointer rawWriter;
  rawWriter = itk::WriteRawImage<FloatImage2DType>::New();
  rawWriter->SetInput(shrink->GetOutput());
  rawWriter->SetFileName("BasicArchitectureImage.dat");
  rawWriter->SetFileTypeToBinary();
  rawWriter->SetByteOrderToBigEndian();
  rawWriter->DebugOn();
  rawWriter->Write();

  return EXIT_SUCCESS;
}



