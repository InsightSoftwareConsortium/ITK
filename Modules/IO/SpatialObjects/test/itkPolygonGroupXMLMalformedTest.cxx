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

// Verifies that malformed numeric content in a PolygonGroup XML file is
// reported as itk::ExceptionObject (not std::invalid_argument /
// std::out_of_range), so that downstream catchers that handle only ITK
// exceptions do not crash. Regression coverage for ITK issue #3213 and
// the request originally raised in Slicer PR #6200.

#include <fstream>
#include <string>

#include "itkPolygonGroupSpatialObjectXMLFile.h"
#include "itksys/SystemTools.hxx"
#include "itkTestingMacros.h"

namespace
{
int
runOne(const std::string & xmlPath, const std::string & xmlBody)
{
  {
    std::ofstream os(xmlPath.c_str());
    if (!os.good())
    {
      std::cerr << "Cannot write fixture: " << xmlPath << std::endl;
      return EXIT_FAILURE;
    }
    os << xmlBody;
  }

  const auto reader = itk::PolygonGroupSpatialObjectXMLFileReader::New();
  reader->SetFilename(xmlPath.c_str());

  bool sawItkException = false;
  try
  {
    reader->GenerateOutputInformation();
  }
  catch (const itk::ExceptionObject & e)
  {
    sawItkException = true;
    std::cout << "Caught expected itk::ExceptionObject: " << e.GetDescription() << std::endl;
  }
  catch (const std::exception & e)
  {
    std::cerr << "FAIL: low-level std::exception leaked out of reader: " << e.what() << std::endl;
    itksys::SystemTools::RemoveFile(xmlPath);
    return EXIT_FAILURE;
  }

  itksys::SystemTools::RemoveFile(xmlPath);

  if (!sawItkException)
  {
    std::cerr << "FAIL: malformed XML did not raise any exception" << std::endl;
    return EXIT_FAILURE;
  }
  return EXIT_SUCCESS;
}
} // namespace

int
itkPolygonGroupXMLMalformedTest(int argc, char * argv[])
{
  if (argc < 2)
  {
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " outputDir" << std::endl;
    return EXIT_FAILURE;
  }
  const std::string outDir(argv[1]);

  const std::string nonNumericSize = R"(<?xml version="1.0"?>
<POLYGONGROUP>
  <X-SIZE>not-an-int</X-SIZE>
</POLYGONGROUP>
)";

  const std::string nonNumericResolution = R"(<?xml version="1.0"?>
<POLYGONGROUP>
  <X-RESOLUTION>abc</X-RESOLUTION>
</POLYGONGROUP>
)";

  if (runOne(outDir + "/PolygonGroupXMLMalformed_Size.xml", nonNumericSize) != EXIT_SUCCESS)
  {
    return EXIT_FAILURE;
  }
  if (runOne(outDir + "/PolygonGroupXMLMalformed_Resolution.xml", nonNumericResolution) != EXIT_SUCCESS)
  {
    return EXIT_FAILURE;
  }
  return EXIT_SUCCESS;
}
