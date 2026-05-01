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

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkTestingMacros.h"
#include "itkVTIImageIO.h"
#include "itkVTIImageIOFactory.h"

#include <cmath>
#include <string>
#include <vector>

namespace
{
// Expected oblique Direction matrix baked into the VTI_oblique_direction.vti
// fixture: a 45-degree rotation about the Z axis.  Row-major 3x3.
constexpr double kExpectedDirection[3][3] = {
  { 0.70710678, -0.70710678, 0.0 },
  { 0.70710678, 0.70710678, 0.0 },
  { 0.0, 0.0, 1.0 },
};

constexpr double kDirectionTol = 1.0e-6;

bool
DirectionMatchesOblique(const itk::ImageIOBase * io, const char * where)
{
  const unsigned int nd = io->GetNumberOfDimensions();
  for (unsigned int axis = 0; axis < nd && axis < 3; ++axis)
  {
    const std::vector<double> & v = io->GetDirection(axis);
    for (unsigned int r = 0; r < nd && r < 3; ++r)
    {
      const double want = kExpectedDirection[r][axis];
      const double got = v[r];
      if (std::abs(got - want) > kDirectionTol)
      {
        std::cerr << where << ": Direction mismatch at axis " << axis << ", component " << r << ": got " << got
                  << ", expected " << want << std::endl;
        return false;
      }
    }
  }
  return true;
}
} // namespace

int
itkVTIImageIODirectionTest(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " <inputDir> <outputDir>" << std::endl;
    return EXIT_FAILURE;
  }
  const std::string inputDir = argv[1];
  const std::string outputDir = argv[2];
  const std::string fixture = inputDir + "/VTI_oblique_direction.vti";
  const std::string rtOutput = outputDir + "/VTI_oblique_direction_rt.vti";

  using ImageT = itk::Image<unsigned char, 3>;

  // --- Read the fixture and assert Direction was recovered. --------------
  auto reader = itk::ImageFileReader<ImageT>::New();
  reader->SetFileName(fixture);
  reader->SetImageIO(itk::VTIImageIO::New());
  ITK_TRY_EXPECT_NO_EXCEPTION(reader->Update());

  if (!DirectionMatchesOblique(reader->GetImageIO(), "initial read"))
  {
    return EXIT_FAILURE;
  }

  // --- Round-trip: write, re-read, assert Direction survived. -----------
  auto writer = itk::ImageFileWriter<ImageT>::New();
  writer->SetFileName(rtOutput);
  writer->SetInput(reader->GetOutput());
  writer->SetImageIO(itk::VTIImageIO::New());
  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());

  auto reader2 = itk::ImageFileReader<ImageT>::New();
  reader2->SetFileName(rtOutput);
  reader2->SetImageIO(itk::VTIImageIO::New());
  ITK_TRY_EXPECT_NO_EXCEPTION(reader2->Update());

  if (!DirectionMatchesOblique(reader2->GetImageIO(), "round-trip read"))
  {
    return EXIT_FAILURE;
  }

  std::cout << "Direction round-trip OK" << std::endl;
  return EXIT_SUCCESS;
}
