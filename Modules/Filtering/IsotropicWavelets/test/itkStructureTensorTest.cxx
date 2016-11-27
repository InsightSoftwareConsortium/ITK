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
#include <string>
#include <cmath>
#include "itkStructureTensor.h"

#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkImageRegionIterator.h"
// Visualize for dev/debug purposes. Set in cmake file. Require VTK
#ifdef ITK_VISUALIZE_TESTS
#  include "itkViewImage.h"
#endif
using namespace std;
using namespace itk;

template <unsigned int N>
int
runStructureTensorTest()
{
  const unsigned int                         ImageDimension = N;
  typedef itk::Image<double, ImageDimension> ImageType;
  typedef itk::Index<ImageDimension>         IndexType;
  typedef itk::Size<ImageDimension>          SizeType;
  typedef itk::ImageRegion<ImageDimension>   RegionType;

  SizeType size;
  size.Fill(64);
  IndexType start;
  start.Fill(0);
  RegionType region;
  region.SetIndex(start);
  region.SetSize(size);
  typename ImageType::Pointer inputImage1 = ImageType::New();
  inputImage1->SetRegions(region);
  inputImage1->Allocate();
  inputImage1->FillBuffer(0);

  typename ImageType::Pointer inputImage2 = ImageType::New();
  inputImage2->SetRegions(region);
  inputImage2->Allocate();
  inputImage2->FillBuffer(0);

  // Fill half image with non-zero.
  SizeType sizeHalf;
  sizeHalf.Fill(32);
  IndexType startHalf;
  startHalf.Fill(32);
  RegionType regionHalfLeft;
  regionHalfLeft.SetIndex(start);
  regionHalfLeft.SetSize(sizeHalf);
  RegionType regionHalfRight;
  regionHalfRight.SetIndex(startHalf);
  regionHalfRight.SetSize(sizeHalf);

  typedef itk::ImageRegionIterator<ImageType> InputIteratorType;
  InputIteratorType                           inputIt1(inputImage1, regionHalfLeft);
  inputIt1.GoToBegin();
  while (!inputIt1.IsAtEnd())
  {
    inputIt1.Set(1.0);
    ++inputIt1;
  }
  InputIteratorType inputIt2(inputImage2, regionHalfRight);
  inputIt2.GoToBegin();
  while (!inputIt2.IsAtEnd())
  {
    inputIt2.Set(1.0);
    ++inputIt2;
  }

  // Structure Tensor
  typedef itk::StructureTensor<ImageType>  StructureTensorType;
  typename StructureTensorType::Pointer    tensor = StructureTensorType::New();
  std::vector<typename ImageType::Pointer> inputs;
  inputs.push_back(inputImage1);
  inputs.push_back(inputImage2);
  tensor->SetInputs(inputs);
  tensor->Update();
  typename StructureTensorType::OutputImageType::Pointer eigenImage = tensor->GetOutput();

  return EXIT_SUCCESS;
}

int
itkStructureTensorTest(int, char *[])
{
  const unsigned int dimension2D = 2;
  return runStructureTensorTest<dimension2D>();
  // const unsigned int dimension3D = 3;
}
