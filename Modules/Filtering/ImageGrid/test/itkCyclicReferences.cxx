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

#include <iostream>
#include "itkRandomImageSource.h"
#include "itkShrinkImageFilter.h"
#include "itkCommand.h"
#include "itkTextOutput.h"


// The following class is used to support callbacks
// on the shrink filter in the pipeline that follows later.
class DeleteEvent
{
public:
  void Delete(const itk::Object *caller, const itk::EventObject &)
    {std::cout << "Deleting: " << caller->GetNameOfClass() << std::endl;}
};

// Note about scoping: Lots of blocks are created here to force the order
// of deletion of objects to insure that the output is in the correct order.
// (Deletion of the smart pointers occurs automatically as scope is exited.)
//
int itkCyclicReferences(int, char* [] )
{
  // Comment the following if you want to use the itk text output window
  itk::OutputWindow::SetInstance( itk::TextOutput::New() );

  // Uncomment the following if you want to see each message independently
  // itk::OutputWindow::GetInstance()->PromptUserOn();

  // Begin by creating a simple pipeline. Use a scalar as a pixel.
  //
  // Create a typedef to make the code more digestable
  typedef itk::Image<float,2> FloatImage2DType;

  // Test the deletion of an image with native type.
  // (scope operators cause automatic smart pointer destruction)
  {//image
  itk::Image<float,2>::Pointer if2 = itk::Image<float,2>::New();
  DeleteEvent deleteEvent;
  itk::MemberCommand<DeleteEvent>::Pointer deleteCommand;
  deleteCommand = itk::MemberCommand<DeleteEvent>::New();
  deleteCommand->SetCallbackFunction(&deleteEvent, &DeleteEvent::Delete);
  if2->AddObserver(itk::DeleteEvent(), deleteCommand);

  //test unregister from vector of data objects
  {
  std::vector<itk::DataObject::Pointer> v;
  v.push_back(if2.GetPointer());
  }
  }//image

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
  random->AddObserver(itk::DeleteEvent(), deleteRandomCommand);

  // Create a filter...shrink the image by an integral amount. We also
  // add some callbacks to the start, progress, and end filter execution
  // methods. The filter is templated on the input and output data types.
  //
  {//shrink
  itk::ShrinkImageFilter<FloatImage2DType,FloatImage2DType>::Pointer shrink;
  shrink = itk::ShrinkImageFilter<FloatImage2DType,FloatImage2DType>::New();
  shrink->SetInput(random->GetOutput());
  shrink->SetShrinkFactors(2);
  DeleteEvent deleteShrink;
  itk::MemberCommand<DeleteEvent>::Pointer deleteShrinkCommand;
  deleteShrinkCommand = itk::MemberCommand<DeleteEvent>::New();
  deleteShrinkCommand->SetCallbackFunction(&deleteShrink, &DeleteEvent::Delete);
  shrink->AddObserver(itk::DeleteEvent(), deleteShrinkCommand);
  }//shrink

  }//random

  return EXIT_SUCCESS;
}
