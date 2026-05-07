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

// Round-trip a small file at a path that contains non-ASCII (UTF-8)
// characters. On Windows, the Windows-utf8-codepage.manifest attached
// to every ITK test driver makes the narrow Win32 *A APIs treat byte
// strings as UTF-8 (Windows 10 1903 and later), so std::ofstream and
// std::ifstream constructed from a UTF-8 const char * "just work". On
// POSIX platforms the encoding is byte-transparent.

#include <cstdio>
#include <fstream>
#include <string>

#include "gtest/gtest.h"

namespace
{

// "speciäl-fil👍.txt" encoded as UTF-8 bytes.
const char *
SpecialBasename()
{
  return "speci\xC3\xA4l-fil\xF0\x9F\x91\x8D.txt";
}

std::string
TempDirFromEnv()
{
  for (const char * var : { "ITK_TEST_OUTPUT_DIR", "TEMP", "TMP", "TMPDIR" })
  {
    if (const char * envDir = std::getenv(var))
    {
      return envDir;
    }
  }
  return ".";
}

} // namespace

TEST(WindowsUtf8Codepage, RoundTripFileAtNonAsciiPath)
{
  const std::string path = TempDirFromEnv() + "/" + SpecialBasename();
  const std::string payload = "round-trip payload\n";

  {
    std::ofstream out(path.c_str());
    ASSERT_TRUE(out.is_open()) << "failed to open " << path << " for writing";
    out << payload;
  }

  std::ifstream     in(path.c_str());
  const bool        readOpened = in.is_open();
  const std::string actual =
    readOpened ? std::string((std::istreambuf_iterator<char>(in)), std::istreambuf_iterator<char>()) : std::string{};
  std::remove(path.c_str());

  ASSERT_TRUE(readOpened) << "failed to open " << path << " for reading";
  EXPECT_EQ(actual, payload);
}
