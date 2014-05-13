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

#include "itkWin32Header.h"

#include <fstream>

#include "itkImageToNeighborhoodSampleAdaptor.h"
#include "itkSubsample.h"
#include "itkImageFileWriter.h"
#include "itkZeroFluxNeumannBoundaryCondition.h"

#include "itkUniformRandomSpatialNeighborSubsampler.h"

int itkUniformRandomSpatialNeighborSubsamplerTest(int argc, char* argv[] )
{
  std::cout << "UniformRandomSpatialNeighborSubsampler Test \n \n";

  std::string outFile = "";
  if (argc > 1)
    {
    outFile = argv[1];
    }

  typedef itk::Image< float, 2 > FloatImage;
  typedef FloatImage::RegionType RegionType;
  typedef FloatImage::IndexType  IndexType;
  typedef FloatImage::SizeType   SizeType;
  typedef itk::ZeroFluxNeumannBoundaryCondition< FloatImage >
                                 BoundaryCondition;
  typedef itk::Statistics::ImageToNeighborhoodSampleAdaptor< FloatImage, BoundaryCondition >
                                 AdaptorType;
  typedef itk::Statistics::UniformRandomSpatialNeighborSubsampler< AdaptorType, RegionType >
                                 SamplerType;
  typedef itk::ImageFileWriter< FloatImage >
                                 WriterType;

  FloatImage::Pointer inImage = FloatImage::New();
  SizeType sz;
  sz.Fill(35);
  IndexType idx;
  idx.Fill(0);
  RegionType region;
  region.SetSize(sz);
  region.SetIndex(idx);

  inImage->SetRegions(region);
  inImage->Allocate(true); // initialize buffer
                                                  // to zero

  AdaptorType::Pointer sample = AdaptorType::New();
  sample->SetImage(inImage);

  SamplerType::Pointer sampler_orig = SamplerType::New();
  sampler_orig->SetSample(sample);
  sampler_orig->SetSampleRegion(region);
  sampler_orig->SetRadius(20);
  sampler_orig->SetNumberOfResultsRequested(50);
  sampler_orig->SetSeed(100);
  sampler_orig->CanSelectQueryOff();

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
  if (sampler->GetRadius() != sampler_orig->GetRadius())
    {
    std::cerr << "Clone did not copy the radius correctly!" << std::endl;
    return EXIT_FAILURE;
    }
  if (sampler->GetNumberOfResultsRequested() != sampler_orig->GetNumberOfResultsRequested())
    {
    std::cerr << "Clone did not copy the number of results requested correctly!" << std::endl;
    return EXIT_FAILURE;
    }
  if (sampler->GetSeed() != sampler_orig->GetSeed())
    {
    std::cerr << "Clone did not copy the seed correctly!" << std::endl;
    return EXIT_FAILURE;
    }
  if (sampler->GetCanSelectQuery() != sampler_orig->GetCanSelectQuery())
    {
    std::cerr << "Clone did not copy CanSelectQuery correctly!" << std::endl;
    return EXIT_FAILURE;
    }

  SamplerType::SubsamplePointer subsample = SamplerType::SubsampleType::New();
  sampler->Search(612, subsample);

  for (SamplerType::SubsampleConstIterator sIt = subsample->Begin();
       sIt != subsample->End();
       ++sIt)
    {
    IndexType index;
    index = sIt.GetMeasurementVector()[0].GetIndex();
    inImage->SetPixel(index, 255);
    }

  if (outFile != "")
    {
    WriterType::Pointer writer = WriterType::New();
    writer->SetFileName( outFile );
    writer->SetInput( inImage );
    try
      {
      writer->Update();
      }
    catch( itk::ExceptionObject & excp )
      {
      std::cerr << excp << std::endl;
      }
    }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;
}
