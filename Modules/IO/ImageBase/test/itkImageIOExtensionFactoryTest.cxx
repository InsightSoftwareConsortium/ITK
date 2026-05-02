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

// Regression test for the public ImageIOBase::HasSupported{Read,Write}Extension
// API and the two-phase ImageIOFactory::CreateImageIO dispatch that depends
// on it. See PR #2511 (originally proposed by Marian Klymov, 2019-11-15)
// and the takeover that promoted these methods from `protected` to `public`.

#include "itkImageIOBase.h"
#include "itkImageIOFactory.h"
#include "itkMetaImageIO.h"
#include "itkTestingMacros.h"

#include <iostream>
#include <string>

namespace
{
int
Verify(const char * label, bool actual, bool expected)
{
  if (actual == expected)
  {
    std::cout << "  PASS: " << label << " -> " << (actual ? "true" : "false") << '\n';
    return 0;
  }
  std::cerr << "  FAIL: " << label << " -> " << (actual ? "true" : "false") << " (expected "
            << (expected ? "true" : "false") << ")\n";
  return 1;
}
} // namespace

int
itkImageIOExtensionFactoryTest(int, char *[])
{
  int errors = 0;

  // The MetaImageIO is always linked into ITKIOMeta; instantiate one
  // directly so the test does not depend on factory registration order.
  const auto io = itk::MetaImageIO::New();

  std::cout << "--- HasSupportedReadExtension ---" << '\n';
  errors += Verify("'image.mha' (default ignoreCase=true)", io->HasSupportedReadExtension("image.mha"), true);
  errors += Verify("'image.MHA' (default ignoreCase=true)", io->HasSupportedReadExtension("image.MHA"), true);
  errors += Verify("'image.mhd' (default ignoreCase=true)", io->HasSupportedReadExtension("image.mhd"), true);
  errors += Verify("'image.MHA' with ignoreCase=false", io->HasSupportedReadExtension("image.MHA", false), false);
  errors += Verify("'image.png' should be rejected", io->HasSupportedReadExtension("image.png"), false);
  errors += Verify("'no_extension' should be rejected", io->HasSupportedReadExtension("no_extension"), false);

  std::cout << "--- HasSupportedWriteExtension ---" << '\n';
  errors += Verify("'out.mha'", io->HasSupportedWriteExtension("out.mha"), true);
  errors += Verify("'out.MHA' default", io->HasSupportedWriteExtension("out.MHA"), true);
  errors += Verify("'out.MHA' ignoreCase=false", io->HasSupportedWriteExtension("out.MHA", false), false);
  errors += Verify("'out.png' should be rejected", io->HasSupportedWriteExtension("out.png"), false);

  std::cout << "--- ImageIOFactory two-phase dispatch ---" << '\n';
  // For a path with a recognized extension but no actual file on disk,
  // CanReadFile must fail, and the factory must return nullptr (not
  // crash, not return the wrong IO).
  const auto missingFileIO =
    itk::ImageIOFactory::CreateImageIO("/this/path/does/not/exist.mha", itk::IOFileModeEnum::ReadMode);
  errors += Verify("CreateImageIO returns nullptr for missing .mha (read)", missingFileIO == nullptr, true);

  // For a write path, the matching IO is allowed to claim it before any
  // file is touched: a non-null result indicates the extension-match
  // dispatch reached the right IO.
  const auto writeIO =
    itk::ImageIOFactory::CreateImageIO("/this/path/does/not/exist.mha", itk::IOFileModeEnum::WriteMode);
  if (writeIO == nullptr)
  {
    std::cerr << "  FAIL: CreateImageIO(.mha, WriteMode) returned nullptr; expected an IO\n";
    ++errors;
  }
  else
  {
    std::cout << "  PASS: CreateImageIO(.mha, WriteMode) -> " << writeIO->GetNameOfClass() << '\n';
  }

  std::cout << "Test finished. " << errors << " error(s).\n";
  return (errors == 0) ? EXIT_SUCCESS : EXIT_FAILURE;
}
