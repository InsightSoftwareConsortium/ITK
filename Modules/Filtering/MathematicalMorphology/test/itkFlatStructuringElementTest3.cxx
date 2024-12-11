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

#include "itkFlatStructuringElement.h"
#include "itkImageFileWriter.h"
#include "itkTestingMacros.h"

template <class TSEType>
void
SEToFile(const TSEType & e, const std::string & fname)
{

  const unsigned int Dimension = TSEType::NeighborhoodDimension;

  using ImageType = itk::Image<unsigned char, Dimension>;

  auto img = ImageType::New();

  const typename ImageType::IndexType start{};

  typename ImageType::SizeType size;
  for (unsigned int i = 0; i < Dimension; ++i)
  {
    size[i] = e.GetRadius()[i] * 2 + 1;
  }


  const typename ImageType::RegionType region(start, size);
  img->SetRegions(region);
  img->Allocate();
  img->FillBuffer(0);

  typename TSEType::ConstIterator     SEIt;
  itk::ImageRegionIterator<ImageType> it(img, region);

  for (SEIt = e.Begin(); SEIt != e.End(); ++SEIt, ++it)
  {
    it.Set(*SEIt);
  }

  using WriterType = itk::ImageFileWriter<ImageType>;
  auto writer = WriterType::New();
  writer->SetFileName(fname);
  writer->SetInput(img);
  writer->Update();
}

int
itkFlatStructuringElementTest3(int argc, char * argv[])
{
  // test polygon SEs
  if (argc < 4)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " OutputImage Radius Lines Dimension" << std::endl;
    return EXIT_FAILURE;
  }

  int               dimension = 2;
  const std::string outputImage = argv[1];
  const int         radius = std::stoi(argv[2]);
  const int         lines = std::stoi(argv[3]);
  if (argc > 4)
  {
    dimension = std::stoi(argv[4]);
  }

  if (dimension == 2)
  {
    using SE2Type = itk::FlatStructuringElement<2>;

    auto          r2 = itk::MakeFilled<SE2Type::RadiusType>(radius);
    const SE2Type P = SE2Type::Polygon(r2, lines);
    SEToFile(P, outputImage);
  }
  else if (dimension == 3)
  {
    using SE3Type = itk::FlatStructuringElement<3>;

    auto          r3 = itk::MakeFilled<SE3Type::RadiusType>(radius);
    const SE3Type P = SE3Type::Polygon(r3, lines);
    SEToFile(P, outputImage);
  }
  else
  {
    std::cerr << "Only 2 and 3 dimensions are supported." << std::endl;
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
