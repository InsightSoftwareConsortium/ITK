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

#include "itkStimulateImageIO.h"

#include <fstream>
#include <iostream>
#include <string>

namespace
{
bool
ExpectInvalidHeader(const std::string & fileName, const std::string & header)
{
  std::ofstream file(fileName);
  if (!file)
  {
    std::cerr << "Could not create " << fileName << std::endl;
    return false;
  }
  file << header;
  file.close();

  const itk::StimulateImageIO::Pointer io = itk::StimulateImageIO::New();
  io->SetFileName(fileName);
  try
  {
    io->ReadImageInformation();
  }
  catch (const itk::ExceptionObject &)
  {
    return true;
  }
  return false;
}
} // namespace

int
itkStimulateImageIOInvalidHeaderTest(int argc, char * argv[])
{
  if (argc != 2)
  {
    std::cerr << "Usage: " << argv[0] << " output-directory" << std::endl;
    return EXIT_FAILURE;
  }

  const std::string outputDirectory = argv[1];
  const std::string oversizedHeader = outputDirectory + "/stimulate-oversized-num-dim.spr";
  const std::string hugeHeader = outputDirectory + "/stimulate-huge-num-dim.spr";
  const std::string malformedHeader = outputDirectory + "/stimulate-malformed-num-dim.spr";
  const std::string missingDimensionsHeader = outputDirectory + "/stimulate-missing-dim.spr";

  if (!ExpectInvalidHeader(oversizedHeader, "numDim: 9\ndim: 4 4 2 2\ndataType: WORD\n") ||
      !ExpectInvalidHeader(hugeHeader, "numDim: 4294967295\ndataType: WORD\n") ||
      !ExpectInvalidHeader(malformedHeader, "numDim: malformed\ndim: 4 4\ndataType: WORD\n") ||
      !ExpectInvalidHeader(missingDimensionsHeader, "numDim: 2\nfov: 1 1\ndataType: WORD\n"))
  {
    return EXIT_FAILURE;
  }
  return EXIT_SUCCESS;
}
