/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include "itkExtractImageFilter.h"
#include "itkGTest.h"
#include "itkCommand.h"

#include <iostream>

//
// This test ensures the abort event occurs and the ProcessAborted
// exception occurs.
//

namespace
{

bool onAbortCalled = false;

void
onProgress(itk::Object * obj, const itk::EventObject &, void *)
{
  const itk::ProcessObject::Pointer p(dynamic_cast<itk::ProcessObject *>(obj));
  if (p.IsNull())
  {
    return;
  }
  if (p->GetProgress() > .1)
  {
    std::cout << "Setting AbortGenerateDataOn()" << std::endl;
    p->AbortGenerateDataOn();
  }
}

void
onAbort(itk::Object *, const itk::EventObject &, void *)
{
  std::cout << "Abort Event" << std::endl;
  onAbortCalled = true;
}

} // namespace

TEST(AbortProcessObject, AbortEventAndException)
{
  onAbortCalled = false;

  // type alias to simplify the syntax
  using ShortImage = itk::Image<short, 2>;
  auto img = ShortImage::New();

  // fill in an image
  constexpr ShortImage::SizeType size{ 100, 100 };
  const ShortImage::RegionType   region{ size };
  img->SetRegions(region);
  img->Allocate();

  itk::ImageRegionIterator<ShortImage> iterator(img, region);

  short i = 0;
  while (!iterator.IsAtEnd())
  {
    iterator.Set(i++);
    ++iterator;
  }

  // Create a filter
  const itk::ExtractImageFilter<ShortImage, ShortImage>::Pointer extract =
    itk::ExtractImageFilter<ShortImage, ShortImage>::New();
  extract->SetInput(img);

  // fill in an image
  constexpr ShortImage::SizeType extractSize{ 99, 99 };
  const ShortImage::RegionType   extractRegion{ extractSize };
  extract->SetExtractionRegion(extractRegion);

  const itk::CStyleCommand::Pointer progressCmd = itk::CStyleCommand::New();
  progressCmd->SetCallback(onProgress);
  progressCmd->SetObjectName("Progress Event");
  extract->AddObserver(itk::ProgressEvent(), progressCmd);

  const itk::CStyleCommand::Pointer abortCmd = itk::CStyleCommand::New();
  abortCmd->SetCallback(onAbort);
  abortCmd->SetObjectName("Abort Event");
  extract->AddObserver(itk::AbortEvent(), abortCmd);

  std::cout << extract << std::endl;

  bool exceptionCaught = false;
  try
  {
    extract->UpdateLargestPossibleRegion();
  }
  catch (const itk::ProcessAborted &)
  {
    exceptionCaught = true;
  }
  EXPECT_TRUE(exceptionCaught) << "Expected ProcessAborted exception to be thrown.";
  EXPECT_TRUE(onAbortCalled) << "Expected Abort Event callback to be called.";
}
