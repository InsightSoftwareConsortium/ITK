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

#include "itkImageFileReader.h"
#include "itkTestingMacros.h"
#include "itkVTIImageIO.h"

#include <string>

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

bool
Contains(const std::string & haystack, const std::string & needle)
{
  return haystack.find(needle) != std::string::npos;
}

int
CheckGuard(const char * label, const std::string & fname, const std::string & expectedTag)
{
  const std::string message = ExpectException(fname);
  if (message.empty())
  {
    std::cerr << label << " FAILED: no exception thrown for " << fname << std::endl;
    return EXIT_FAILURE;
  }
  if (!Contains(message, expectedTag))
  {
    std::cerr << label << " FAILED: exception did not contain '" << expectedTag << "' tag.  Message was:\n"
              << message << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << label << " OK: guard fired with '" << expectedTag << "' tag" << std::endl;
  return EXIT_SUCCESS;
}
} // namespace

int
itkVTIImageIOFutureFeaturesTest(int argc, char * argv[])
{
  if (argc < 2)
  {
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " <inputDir>" << std::endl;
    return EXIT_FAILURE;
  }
  const std::string inputDir = argv[1];
  int               status = EXIT_SUCCESS;

  // F-001: vtkLZ4DataCompressor should raise a tagged exception.
  status |= CheckGuard("F-001 LZ4", inputDir + "/VTI_guard_lz4.vti", "F-001 LZ4");

  // F-002: vtkLZMADataCompressor should raise a tagged exception.
  status |= CheckGuard("F-002 LZMA", inputDir + "/VTI_guard_lzma.vti", "F-002 LZMA");

  // F-010: unknown compressor string should raise the catch-all exception.
  status |= CheckGuard(
    "F-010 unknown-compressor", inputDir + "/VTI_guard_unknown_compressor.vti", "F-010 Unknown VTK compressor");

  // F-005: multi-Piece ImageData should raise a tagged exception.
  status |= CheckGuard("F-005 multi-Piece", inputDir + "/VTI_guard_multipiece.vti", "F-005 Multi-Piece");

  // F-007: binary symmetric-tensor write path is guarded in Write().
  //   Tested in itkVTIImageIOTest (`Binary tensor write correctly rejected`),
  //   which exercises the writer directly.  Not repeated here.

  if (status == EXIT_SUCCESS)
  {
    std::cout << "itkVTIImageIOFutureFeaturesTest PASSED" << std::endl;
  }
  return status;
}
