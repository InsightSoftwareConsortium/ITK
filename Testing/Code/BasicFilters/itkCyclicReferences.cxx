/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkCyclicReferences.cxx
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
#include "itkShrinkImageFilter.h"
#include "itkVTKImageWriter.h"
#include "itkVTKImageReader.h"
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

// The following class is used to support callbacks
// on the shrink filter in the pipeline that follows later.
class DeleteEvent
{
public:
  void Delete(const itk::LightObject *caller, unsigned long event) 
    {std::cout << "Deleting: " << caller->GetClassName() << std::endl;}
};

// Note about scoping: Lots of blocks are created here to force the order
// of deletion of objects to insure that the output is in the correct order.
// (Deletion of the smart pointers occurs automatically as scope is exited.)
//
int main()
{
  // Comment the following if you want to use the itk text output window
  itk::OutputWindow::SetInstance(new TextOutput);

  // Uncomment the following if you want to see each message independently
  // itk::OutputWindow::GetInstance()->PromptUserOn();

  // Begin by creating a simple pipeline. Use the Scalar class as a pixel.
  //
  // Create a typedef to make the code more digestable
  typedef itk::PhysicalImage<itk::Scalar<float>,2> FloatImage2DType;

  // Test the deletion of an image with native type.
  // (scope operators cause automagic smart pointer destruction)
  {//image
  itk::PhysicalImage<float,2>::Pointer if2 = itk::PhysicalImage<float,2>::New();
  DeleteEvent deleteEvent;
  itk::MemberCommand<DeleteEvent>::Pointer deleteCommand;
  deleteCommand = itk::MemberCommand<DeleteEvent>::New();
  deleteCommand->SetCallbackFunction(&deleteEvent, &DeleteEvent::Delete);
  if2->AddObserver(itk::Command::DeleteEvent, deleteCommand);

  //test unregister from vector of data objects
    {
    std::vector<itk::DataObject::Pointer> v;
    v.push_back(if2.GetPointer());
    }
  }//image

  // Create a source object (in this case a reader)
  {//Reader
  itk::VTKImageReader<FloatImage2DType>::Pointer reader;
  reader = itk::VTKImageReader<FloatImage2DType>::New();
  reader->SetFileName("junkInput.vtk");
  DeleteEvent deleteReader;
  itk::MemberCommand<DeleteEvent>::Pointer deleteReaderCommand;
  deleteReaderCommand = itk::MemberCommand<DeleteEvent>::New();
  deleteReaderCommand->SetCallbackFunction(&deleteReader, &DeleteEvent::Delete);
  reader->AddObserver(itk::Command::DeleteEvent, deleteReaderCommand);
  }//reader
  
  // Create another source object (in this case a random image generator).
  // The source object is templated on the output type.
  //
  {//random
  itk::RandomImageSource<FloatImage2DType>::Pointer random;
  random = itk::RandomImageSource<FloatImage2DType>::New();
  random->SetMin(0.0);
  random->SetMax(1.0);
  DeleteEvent deleteRandom;
  itk::MemberCommand<DeleteEvent>::Pointer deleteRandomCommand;
  deleteRandomCommand = itk::MemberCommand<DeleteEvent>::New();
  deleteRandomCommand->SetCallbackFunction(&deleteRandom, &DeleteEvent::Delete);
  random->AddObserver(itk::Command::DeleteEvent, deleteRandomCommand);

  // Create a filter...shrink the image by an integral amount. We also 
  // add some callbacks to the start, progress, and end filter execution
  // methods. The filter is templated on the input and output data types.
  //
  {//shrink
  itk::ShrinkImageFilter<FloatImage2DType,FloatImage2DType>::Pointer shrink;
  shrink = itk::ShrinkImageFilter<FloatImage2DType,FloatImage2DType>::New();
  shrink->SetInput(random->GetOutput());
  shrink->SetShrinkFactor(2);
  DeleteEvent deleteShrink;
  itk::MemberCommand<DeleteEvent>::Pointer deleteShrinkCommand;
  deleteShrinkCommand = itk::MemberCommand<DeleteEvent>::New();
  deleteShrinkCommand->SetCallbackFunction(&deleteShrink, &DeleteEvent::Delete);
  shrink->AddObserver(itk::Command::DeleteEvent, deleteShrinkCommand);
  
  // Create a mapper (in this case a writer). A mapper
  // is templated on the input type.
  //
  {//write
  itk::VTKImageWriter<FloatImage2DType>::Pointer writer;
  writer = itk::VTKImageWriter<FloatImage2DType>::New();
  writer->SetInput(shrink->GetOutput());
  writer->SetFileName("BasicArchitectureImage.vtk");
  writer->SetFileTypeToASCII();
  writer->Write();
  DeleteEvent deleteWriter;
  itk::MemberCommand<DeleteEvent>::Pointer deleteWriterCommand;
  deleteWriterCommand = itk::MemberCommand<DeleteEvent>::New();
  deleteWriterCommand->SetCallbackFunction(&deleteWriter, &DeleteEvent::Delete);
  writer->AddObserver(itk::Command::DeleteEvent, deleteWriterCommand);
  }//write
  }//shrink
  }//random
  
  return EXIT_SUCCESS;
}
