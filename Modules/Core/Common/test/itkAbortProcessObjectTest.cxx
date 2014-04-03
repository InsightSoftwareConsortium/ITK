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
#include "itkExtractImageFilter.h"
#include "itkTestingMacros.h"

#include "itkCommand.h"

//
// This test ensures the abort event occurs and the ProcessAborted
// exception occurs.
//

namespace
{

bool onAbortCalled = false;

void onProgress( itk::Object *obj, const itk::EventObject &, void *)
{
  itk::ProcessObject::Pointer p( dynamic_cast<itk::ProcessObject*>(obj) );
  if ( p.IsNull() )
    {
    return;
    }
  if ( p->GetProgress() > .1 )
    {
    std::cout << "Setting AbortGenerateDataOn()" << std::endl;
    p->AbortGenerateDataOn();
    }
}

void onAbort( itk::Object *, const itk::EventObject &, void *)
{
  std::cout << "Abort Event" << std::endl;
  onAbortCalled = true;
}

}

int itkAbortProcessObjectTest(int, char* [] )
{
  // typedefs to simplify the syntax
  typedef itk::Image<short, 2>   ShortImage;
  ShortImage::Pointer img = ShortImage::New();

  // fill in an image
  const ShortImage::IndexType  index = {{0, 0}};
  const ShortImage::SizeType   size = {{100, 100}};
  ShortImage::RegionType region;
  region.SetSize( size );
  region.SetIndex( index );
  img->SetRegions( region );
  img->Allocate();

  itk::ImageRegionIterator<ShortImage> iterator(img, region);

  short i=0;
  while( !iterator.IsAtEnd() )
    {
      iterator.Set( i++ );
      ++iterator;
    }

  // Create a filter
  itk::ExtractImageFilter< ShortImage, ShortImage >::Pointer extract;
  extract = itk::ExtractImageFilter< ShortImage, ShortImage >::New();
  extract->SetInput( img );

  // fill in an image
  ShortImage::IndexType  extractIndex = {{0, 0}};
  ShortImage::SizeType   extractSize = {{99, 99}};
  ShortImage::RegionType extractRegion;
  extractRegion.SetSize( extractSize );
  extractRegion.SetIndex( extractIndex );
  extract->SetExtractionRegion(extractRegion);

  itk::CStyleCommand::Pointer progressCmd = itk::CStyleCommand::New();
  progressCmd->SetCallback(onProgress);
  progressCmd->SetObjectName("Aborting Command");
  extract->AddObserver(itk::ProgressEvent(), progressCmd);

  itk::CStyleCommand::Pointer abortCmd = itk::CStyleCommand::New();
  abortCmd->SetCallback(onAbort);
  extract->AddObserver(itk::AbortEvent(), abortCmd);

  std::cout << extract << std::endl;
  try
    {
    extract->UpdateLargestPossibleRegion();
    }
  catch(itk::ProcessAborted &)
    {
    if (onAbortCalled)
      {
      return EXIT_SUCCESS;
      }
    std::cout << "Caught expected abort exception, but didn't get Abort Event!";
    }

  return EXIT_FAILURE;
}
