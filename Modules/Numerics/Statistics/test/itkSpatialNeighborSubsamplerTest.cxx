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

#include "itkSpatialNeighborSubsampler.h"
#include "itkImageToNeighborhoodSampleAdaptor.h"
#include "itkImageRegionConstIteratorWithIndex.h"

int itkSpatialNeighborSubsamplerTest(int, char * [] )
{

// Given the following 25x25 image on the left
// with the region constraint marked by |---|
//                                      |   |
//                                      |---|
// Sample all points within a radius of 10 from the point at (2,6) marked
// by an X in the image.  Replace each of the sampled points with a value of 255
// The correct image should match the image on the right where a + indicates a
// value of 255

// input image with search index      |        output image with sampled
// and region constraint indicated    |        points indicated by +
// ooooooooooooooooooooooooo          |        ooooooooooooooooooooooooo
// ooooooooooooooooooooooooo          |        ooooooooooooooooooooooooo
// ooooooooooooooooooooooooo          |        ooooooooooooooooooooooooo
// ooooooooooooooooooooooooo          |        ooooooooooooooooooooooooo
// ooooooooooooooooooooooooo          |        ooooooooooooooooooooooooo
// |---------------------|oo          |        +++++++++++++oooooooooooo
// |oXooooooooooooooooooo|oo          |        +++++++++++++oooooooooooo
// |ooooooooooooooooooooo|oo          |        +++++++++++++oooooooooooo
// |ooooooooooooooooooooo|oo          |        +++++++++++++oooooooooooo
// |ooooooooooooooooooooo|oo          |        +++++++++++++oooooooooooo
// |ooooooooooooooooooooo|oo          |        +++++++++++++oooooooooooo
// |ooooooooooooooooooooo|oo          |        +++++++++++++oooooooooooo
// |ooooooooooooooooooooo|oo          |        +++++++++++++oooooooooooo
// |ooooooooooooooooooooo|oo          |        +++++++++++++oooooooooooo
// |---------------------|oo          |        +++++++++++++oooooooooooo
// ooooooooooooooooooooooooo          |        ooooooooooooooooooooooooo
// ooooooooooooooooooooooooo          |        ooooooooooooooooooooooooo
// ooooooooooooooooooooooooo          |        ooooooooooooooooooooooooo
// ooooooooooooooooooooooooo          |        ooooooooooooooooooooooooo
// ooooooooooooooooooooooooo          |        ooooooooooooooooooooooooo
// ooooooooooooooooooooooooo          |        ooooooooooooooooooooooooo
// ooooooooooooooooooooooooo          |        ooooooooooooooooooooooooo
// ooooooooooooooooooooooooo          |        ooooooooooooooooooooooooo
// ooooooooooooooooooooooooo          |        ooooooooooooooooooooooooo
// ooooooooooooooooooooooooo          |        ooooooooooooooooooooooooo

  std::cout << "SpatialNeighborSubsampler Test \n \n";

  typedef unsigned int               PixelType;
  typedef itk::Image< PixelType, 2 > ImageType;
  typedef ImageType::RegionType      RegionType;
  typedef ImageType::IndexType       IndexType;
  typedef ImageType::SizeType        SizeType;
  typedef itk::ZeroFluxNeumannBoundaryCondition< ImageType >
                                     BoundaryCondition;
  typedef itk::Statistics::ImageToNeighborhoodSampleAdaptor< ImageType, BoundaryCondition >
                                     AdaptorType;
  typedef itk::Statistics::SpatialNeighborSubsampler< AdaptorType, RegionType >
                                     SamplerType;
  typedef itk::ImageRegionConstIteratorWithIndex< ImageType >
                                     IteratorType;

  ImageType::Pointer inImage = ImageType::New();
  SizeType sz;
  sz.Fill(25);
  IndexType idx;
  idx.Fill(0);
  RegionType region;
  region.SetSize(sz);
  region.SetIndex(idx);

  inImage->SetRegions(region);
  inImage->Allocate(true); // initialize buffer
                                                  // to zero

  SizeType szConstraint;
  szConstraint[0] = 23;
  szConstraint[1] = 10;
  IndexType idxConstraint;
  idxConstraint[0] = 0;
  idxConstraint[1] = 5;
  RegionType regionConstraint;
  regionConstraint.SetSize(szConstraint);
  regionConstraint.SetIndex(idxConstraint);

  AdaptorType::Pointer sample = AdaptorType::New();
  sample->SetImage(inImage);

  SamplerType::Pointer sampler_orig = SamplerType::New();
  sampler_orig->SetSample(sample);
  sampler_orig->SetSampleRegion(region);
  sampler_orig->SetRegionConstraint(regionConstraint);

  sampler_orig->SetRadius(10);
  sampler_orig->CanSelectQueryOn();

  // test clone mechanism
  SamplerType::Pointer sampler = sampler_orig->Clone().GetPointer();
  if (sampler->GetSample() != sampler_orig->GetSample())
    {
    std::cerr << "Clone did not copy the sample correctly!" << std::endl;
    return EXIT_FAILURE;
    }
  if (sampler->GetSampleRegion() != sampler_orig->GetSampleRegion())
    {
    std::cerr << "Clone did not copy the region correctly!" << std::endl;
    return EXIT_FAILURE;
    }
  if (sampler->GetRegionConstraint() != sampler_orig->GetRegionConstraint())
    {
    std::cerr << "Clone did not copy the region constraint correctly!" << std::endl;
    return EXIT_FAILURE;
    }
  if (sampler->GetRadius() != sampler_orig->GetRadius())
    {
    std::cerr << "Clone did not copy the radius correctly!" << std::endl;
    return EXIT_FAILURE;
    }
  if (sampler->GetCanSelectQuery() != sampler_orig->GetCanSelectQuery())
    {
    std::cerr << "Clone did not copy CanSelectQuery correctly!" << std::endl;
    return EXIT_FAILURE;
    }

  SamplerType::SubsamplePointer subsample = SamplerType::SubsampleType::New();
  IndexType queryIdx;
  queryIdx[0] = 2;
  queryIdx[1] = 6;
  ImageType::OffsetValueType queryOffset = inImage->ComputeOffset(queryIdx);
  sampler->Search(queryOffset, subsample);

  IndexType index;

  for (SamplerType::SubsampleConstIterator sIt = subsample->Begin();
       sIt != subsample->End();
       ++sIt)
    {
    index = sIt.GetMeasurementVector()[0].GetIndex();
    inImage->SetPixel(index, 255);
    }

  // To validate, create a region that should contain only the points that
  // should now be = 255.  Iterate through the image and confirm that only
  // values inside the region are 255 and that ALL values within the region
  // are 255.
  SizeType validSz;
  validSz[0] = 13;
  validSz[1] = 10;
  IndexType validStart;
  validStart[0] = 0;
  validStart[1] = 5;
  RegionType validRegion;
  validRegion.SetSize(validSz);
  validRegion.SetIndex(validStart);

  IteratorType it( inImage, region);
  it.GoToBegin();
  while( !it.IsAtEnd() )
    {
    PixelType curValue = it.Get();
    IndexType curIdx = it.GetIndex();
    if (validRegion.IsInside(curIdx))
      {
      // inside the region, value must be 255
      if (curValue != 255)
        {
        std::cout << "Error! Pixel at " << curIdx
                  << " should be 255 but is " << curValue
                  << " instead!" << std::endl;
        return EXIT_FAILURE;
        }
      }
    else
      {
      // outside of the region, value must be 0
      if (curValue != 0)
        {
        std::cout << "Error! Pixel at " << curIdx
                  << " should be 0 but is " << curValue
                  << " instead!" << std::endl;
        return EXIT_FAILURE;
        }
      }
    ++it;
    } // end for each point in the image

  std::cout << "All pixels and only pixels within intersection of"
            << " the image region and constraint region are equal to 255."
            << std::endl;
  return EXIT_SUCCESS;
}
