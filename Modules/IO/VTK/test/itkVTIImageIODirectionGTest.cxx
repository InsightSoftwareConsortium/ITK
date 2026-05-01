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
#include "itkVTIImageIO.h"
#include "itkVTIImageIOFactory.h"
#include "itkGTest.h"

#include <cmath>
#include <string>
#include <vector>

#ifndef VTI_TEST_INPUT_DIR
#  error "VTI_TEST_INPUT_DIR must be defined by the build system."
#endif

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

void
ExpectDirectionMatchesOblique(const itk::ImageIOBase * io)
{
  const unsigned int nd = io->GetNumberOfDimensions();
  for (unsigned int axis = 0; axis < nd && axis < 3; ++axis)
  {
    const std::vector<double> & v = io->GetDirection(axis);
    for (unsigned int r = 0; r < nd && r < 3; ++r)
    {
      EXPECT_NEAR(v[r], kExpectedDirection[r][axis], kDirectionTol)
        << "Direction mismatch at axis " << axis << ", component " << r;
    }
  }
}
} // namespace

TEST(VTIImageIODirection, RoundTrip)
{
  using ImageT = itk::Image<unsigned char, 3>;
  const std::string fixture = std::string(VTI_TEST_INPUT_DIR) + "/VTI_oblique_direction.vti";
  const std::string rtOutput = std::string(::testing::TempDir()) + "/VTI_oblique_direction_rt.vti";

  // --- Read the fixture and assert Direction was recovered. --------------
  auto reader = itk::ImageFileReader<ImageT>::New();
  reader->SetFileName(fixture);
  reader->SetImageIO(itk::VTIImageIO::New());
  ASSERT_NO_THROW(reader->Update());
  ExpectDirectionMatchesOblique(reader->GetImageIO());

  // --- Round-trip: write, re-read, assert Direction survived. -----------
  auto writer = itk::ImageFileWriter<ImageT>::New();
  writer->SetFileName(rtOutput);
  writer->SetInput(reader->GetOutput());
  writer->SetImageIO(itk::VTIImageIO::New());
  ASSERT_NO_THROW(writer->Update());

  auto reader2 = itk::ImageFileReader<ImageT>::New();
  reader2->SetFileName(rtOutput);
  reader2->SetImageIO(itk::VTIImageIO::New());
  ASSERT_NO_THROW(reader2->Update());
  ExpectDirectionMatchesOblique(reader2->GetImageIO());
}
