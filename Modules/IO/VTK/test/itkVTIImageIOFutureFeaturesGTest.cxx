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

// Guard tests for Phase-2-deferred VTIImageIO features.
//
// Each test case reads a fixture whose XML header intentionally triggers
// a specific F-NNN guard exception at InternalReadImageInformation
// time.  The test asserts the guard fires and that the diagnostic
// contains the expected F-NNN tag so future work items remain
// discoverable via `git grep F-NNN`.
//
// When a feature is implemented in the follow-up PR, each guard test
// flips from "expect exception" to "expect success + pixelwise
// comparison" in the same commit that removes the guard -- producing a
// visible red/green transition in git history.

#include "itkVTIImageIO.h"
#include "itkGTest.h"

#include <string>

#ifndef VTI_TEST_INPUT_DIR
#  error "VTI_TEST_INPUT_DIR must be defined by the build system."
#endif

namespace
{
// Attempt to ReadImageInformation() on `fname`; return the exception
// description if one was thrown, or an empty string on unexpected success.
std::string
ExpectException(const std::string & fname)
{
  auto io = itk::VTIImageIO::New();
  io->SetFileName(fname);
  try
  {
    io->ReadImageInformation();
  }
  catch (const itk::ExceptionObject & e)
  {
    return e.GetDescription();
  }
  return {};
}

void
CheckGuard(const std::string & fixture, const std::string & expectedTag)
{
  const std::string fname = std::string(VTI_TEST_INPUT_DIR) + "/" + fixture;
  const std::string message = ExpectException(fname);
  ASSERT_FALSE(message.empty()) << "no exception thrown for " << fname;
  EXPECT_NE(message.find(expectedTag), std::string::npos)
    << "exception did not contain '" << expectedTag << "' tag.  Message was:\n"
    << message;
}
} // namespace

// F-001: vtkLZ4DataCompressor should raise a tagged exception.
TEST(VTIImageIOFutureFeatures, F001_LZ4) { CheckGuard("VTI_guard_lz4.vti", "F-001 LZ4"); }

// F-002: vtkLZMADataCompressor should raise a tagged exception.
TEST(VTIImageIOFutureFeatures, F002_LZMA) { CheckGuard("VTI_guard_lzma.vti", "F-002 LZMA"); }

// F-010: unknown compressor string should raise the catch-all exception.
TEST(VTIImageIOFutureFeatures, F010_UnknownCompressor)
{
  CheckGuard("VTI_guard_unknown_compressor.vti", "F-010 Unknown VTK compressor");
}

// F-005: multi-Piece ImageData should raise a tagged exception.
TEST(VTIImageIOFutureFeatures, F005_MultiPiece) { CheckGuard("VTI_guard_multipiece.vti", "F-005 Multi-Piece"); }

// F-007 (binary symmetric-tensor write) is guarded in Write() and exercised
// in itkVTIImageIOTest's "Binary tensor write correctly rejected" block.
