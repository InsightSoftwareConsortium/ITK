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

#include "itkWatershedImageFilter.h"
#include "itkWatershedEquivalenceRelabeler.h"
#include "itkWatershedBoundaryResolver.h"
#include "itkFilterWatcher.h"

inline void println(const char *s) { std::cout << s << std::endl; }

int itkWatershedImageFilterTest(int, char* [] )
{
  typedef itk::Image<float, 2>               ImageType2D;
  typedef itk::Image<itk::IdentifierType, 2> LongImageType2D;

  println("Creating some images");
  itk::ImageRegion<2> Region2D;

  itk::Size<2>  size2D;
   size2D[0] = 314;
   size2D[1] = 314;

  itk::Index<2> orig2D;
   orig2D[0] = 0;
   orig2D[1] = 0;

  Region2D.SetSize(size2D);
  Region2D.SetIndex(orig2D);

  ImageType2D::Pointer image2D = ImageType2D::New();
   image2D->SetLargestPossibleRegion(Region2D);
   image2D->SetBufferedRegion(Region2D);
   image2D->SetRequestedRegion(Region2D);
   image2D->Allocate();

  LongImageType2D::Pointer longimage2D = LongImageType2D::New();
   longimage2D->SetRegions(Region2D);
   longimage2D->Allocate(true); // initialize
                                                       // buffer to zero

 itk::ImageRegionIterator<ImageType2D>
     it2D(image2D, image2D->GetRequestedRegion());
  println("Initializing an image");
  float q = 0.00f;
  for (; !it2D.IsAtEnd(); ++it2D)
    {
    it2D.Value() = std::sin(q);
    q = q + 0.10f;
    }

  println("Testing various associated objects");
  println("Testing EquivalenceRelabeler");
  itk::EquivalencyTable::Pointer t= itk::EquivalencyTable::New();

  itk::watershed::EquivalenceRelabeler<long, 2>::Pointer eq
    = itk::watershed::EquivalenceRelabeler<long, 2>::New();
  eq->SetInputImage(longimage2D);
  eq->SetEquivalencyTable(t);
  eq->GetEquivalencyTable();
  eq->Update();

  println("Testing WatershedMiniPipelineProgressCommand.  Forcing the execution of the const Execute method which is not normally called.");
  itk::WatershedMiniPipelineProgressCommand::Pointer wmppc
    = itk::WatershedMiniPipelineProgressCommand::New();
  wmppc->SetCount(2);
  wmppc->GetCount();
  wmppc->SetNumberOfFilters(2);
  wmppc->GetNumberOfFilters();
  wmppc->SetFilter(eq);
  wmppc->GetFilter();
  const itk::ProcessObject *constp = eq.GetPointer();
  wmppc->Execute(constp, itk::ProgressEvent());
  wmppc->Execute(eq.GetPointer(), itk::ProgressEvent());

  println("Testing watershed::BoundaryResolver");
  itk::watershed::BoundaryResolver<float, 2>::Pointer br = itk::watershed::BoundaryResolver<float, 2>::New();
  if( br.IsNull() )
    {
    return EXIT_FAILURE;
    }
  itk::watershed::Boundary<float, 1>::Pointer boundaryA = itk::watershed::Boundary<float, 1>::New();
  if( boundaryA.IsNull() )
    {
    return EXIT_FAILURE;
    }
  itk::watershed::Boundary<float, 1>::Pointer boundaryB = itk::watershed::Boundary<float, 1>::New();
  if( boundaryB.IsNull() )
    {
    return EXIT_FAILURE;
    }

  println("Creating the watershed filter");
  itk::WatershedImageFilter<ImageType2D>::Pointer ws_filter =
                  itk::WatershedImageFilter<ImageType2D>::New();
  FilterWatcher watchIt(ws_filter);

  ws_filter->SetInput(image2D);
  ws_filter->SetThreshold(.05f);
  ws_filter->GetThreshold();
  ws_filter->SetLevel(1.0f);
  ws_filter->GetLevel();

  println("Executing the filter");
  try
    {
    ws_filter->Update();
    }
  catch (...)
    {
    std::cerr << "WatershedImageFilter exception thrown" << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
