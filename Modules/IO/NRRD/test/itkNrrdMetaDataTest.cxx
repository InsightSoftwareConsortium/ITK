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
#include "itkMetaDataObject.h"
#include "itkNumberToString.h"
#include "itksys/SystemTools.hxx"
#include "itkNrrdImageIO.h"

#include <fstream>
#include <sstream>

/** This test was written in response to ITK task 2549
 * https://itk.icts.uiowa.edu/jira/browse/ITK-2549
 *
 * The complaint was that MetaData items were not getting
 * copied from an image to the NRRD file on disk.
 *
 * If this test succeeds, this bug has been fixed.
 */
int
itkNrrdMetaDataTest(int argc, char * argv[])
{

  if (argc < 2)
  {
    std::cerr << "Missing data directory argument" << std::endl;
    return EXIT_FAILURE;
  }

  // Image type
  using ImageType = itk::Image<unsigned char, 3>;
  // create dummy image
  auto                          image1 = ImageType::New();
  constexpr ImageType::SizeType size{ 2, 2, 2 };
  image1->SetRegions(size);
  image1->Allocate();
  image1->FillBuffer(1);
  const char * metaDataObjectName = "NrrdTest";
  const char * metaDataObjectValue = "123456";
  // add a metadataobject to the dictionary
  itk::MetaDataDictionary & dict = image1->GetMetaDataDictionary();
  itk::EncapsulateMetaData<std::string>(dict, metaDataObjectName, metaDataObjectValue);

  // write the file then read it back in.
  using ImageWriterType = itk::ImageFileWriter<ImageType>;
  using ImageReaderType = itk::ImageFileReader<ImageType>;

  // test uses 1st arg to specify where to drop data
  std::string fname = argv[1];
  fname += "/metadatatest.nrrd";
  // set up writer
  auto writer = ImageWriterType::New();
  writer->SetImageIO(itk::NrrdImageIO::New());
  writer->SetFileName(fname.c_str());
  writer->SetInput(image1);
  // set up reader
  auto reader = ImageReaderType::New();
  reader->SetFileName(fname.c_str());
  reader->SetImageIO(itk::NrrdImageIO::New());
  ImageType::Pointer image2;
  // write and then read
  try
  {
    writer->Update();
    reader->Update();
    image2 = reader->GetOutput();
  }
  catch (const itk::ExceptionObject & e)
  {
    std::cerr << "Exception in file reader or writer " << std::endl;
    std::cerr << e.GetDescription() << std::endl;
    std::cerr << e.GetLocation() << std::endl;
    return EXIT_FAILURE;
  }
  // see if the test metaDataObject was copied to the output and is
  // present in the image read in.
  dict = image2->GetMetaDataDictionary();
  std::string NrrdTest;
  // if it exists and the string matches what we put in on the image
  // to write, AOK.
  if (itk::ExposeMetaData<std::string>(dict, metaDataObjectName, NrrdTest) == false || NrrdTest != metaDataObjectValue)
  {
    return EXIT_FAILURE; // oops!
  }

  // Precision test: verify that native double and float metadata round-trips
  // without the 6-digit truncation from default stream precision.
  //
  // The NrrdImageIO write path serialises native double/float metadata via
  // _dump_metadata_to_stream<T>.  Without the ConvertNumberToString fix the
  // output is truncated to 6 significant digits.  The read-back string then
  // parses to a different bit-pattern.
  //
  // These checks FAIL against the unfixed NrrdImageIO and PASS with the fix.
  {
    constexpr double   hpDouble = 1.2345678901234568;
    constexpr float    hpFloat = 1.2345679f;
    const char * const hpDoubleKey = "high_precision_double";
    const char * const hpFloatKey = "high_precision_float";

    // Write a fresh image with high-precision double and float metadata.
    itk::MetaDataDictionary & dict2 = image1->GetMetaDataDictionary();
    itk::EncapsulateMetaData<double>(dict2, hpDoubleKey, hpDouble);
    itk::EncapsulateMetaData<float>(dict2, hpFloatKey, hpFloat);

    std::string precFname = argv[1];
    precFname += "/metadatatest_precision.nrrd";

    auto precWriter = ImageWriterType::New();
    precWriter->SetImageIO(itk::NrrdImageIO::New());
    precWriter->SetFileName(precFname.c_str());
    precWriter->SetInput(image1);

    auto precReader = ImageReaderType::New();
    precReader->SetFileName(precFname.c_str());
    precReader->SetImageIO(itk::NrrdImageIO::New());

    try
    {
      precWriter->Update();
      precReader->Update();
    }
    catch (const itk::ExceptionObject & ex)
    {
      std::cerr << "Precision test write/read failed: " << ex << '\n';
      return EXIT_FAILURE;
    }

    const itk::MetaDataDictionary & rdict = precReader->GetOutput()->GetMetaDataDictionary();

    std::string doubleStr;
    if (!itk::ExposeMetaData<std::string>(rdict, hpDoubleKey, doubleStr))
    {
      std::cerr << "Key " << hpDoubleKey << " not found after round-trip\n";
      return EXIT_FAILURE;
    }
    const double parsedDouble = std::stod(doubleStr);
    if (parsedDouble != hpDouble)
    {
      std::cerr << "Double precision loss: stored " << itk::ConvertNumberToString(hpDouble) << " but NRRD string '"
                << doubleStr << "' parses to " << itk::ConvertNumberToString(parsedDouble) << '\n';
      return EXIT_FAILURE;
    }

    std::string floatStr;
    if (!itk::ExposeMetaData<std::string>(rdict, hpFloatKey, floatStr))
    {
      std::cerr << "Key " << hpFloatKey << " not found after round-trip\n";
      return EXIT_FAILURE;
    }
    const float parsedFloat = std::stof(floatStr);
    if (parsedFloat != hpFloat)
    {
      std::cerr << "Float precision loss: stored " << itk::ConvertNumberToString(hpFloat) << " but NRRD string '"
                << floatStr << "' parses to " << itk::ConvertNumberToString(parsedFloat) << '\n';
      return EXIT_FAILURE;
    }

    itksys::SystemTools::RemoveFile(precFname);
  }

  // Helper: read the ASCII header of a NRRD file (lines up to but not
  // including the first blank line, which separates header from data).
  // Open in binary mode so that trailing CR characters on Windows-written
  // files are visible to us (text mode would strip them) and always strip
  // them explicitly, giving consistent cross-platform behavior.
  const auto readNrrdHeader = [](const std::string & path) {
    std::ifstream      in(path, std::ios::binary);
    std::ostringstream out;
    std::string        line;
    while (std::getline(in, line))
    {
      if (!line.empty() && line.back() == '\r')
      {
        line.pop_back();
      }
      if (line.empty())
      {
        break;
      }
      out << line << '\n';
    }
    return out.str();
  };

  // Test for https://discourse.itk.org/t/6666 (NrrdImageIO and NRRD version 5):
  //   When the user sets a v5-only field (measurement frame) via the
  //   documented "NRRD_<field>" dictionary convention, the on-disk magic
  //   must be bumped from NRRD0004 to NRRD0005.  teem's nrrdSave auto-
  //   selects the minimum-version magic based on the interesting fields
  //   set on the Nrrd struct (see nrrd__FormatNRRD_whichVersion in
  //   NrrdIO's formatNRRD.c), so the correctness guarantee here is that
  //   the writer actually transfers the measurement frame onto the Nrrd
  //   struct when the metadata is supplied in the documented form.
  {
    auto v5Image = ImageType::New();
    v5Image->SetRegions(size);
    v5Image->Allocate();
    v5Image->FillBuffer(0);
    itk::MetaDataDictionary &        v5Dict = v5Image->GetMetaDataDictionary();
    std::vector<std::vector<double>> mframe = { { 1.0, 0.0, 0.0 }, { 0.0, 1.0, 0.0 }, { 0.0, 0.0, 1.0 } };
    itk::EncapsulateMetaData<std::vector<std::vector<double>>>(v5Dict, "NRRD_measurement frame", mframe);

    std::string v5Fname = argv[1];
    v5Fname += "/metadatatest_nrrd5.nrrd";

    auto v5Writer = ImageWriterType::New();
    v5Writer->SetImageIO(itk::NrrdImageIO::New());
    v5Writer->SetFileName(v5Fname.c_str());
    v5Writer->SetInput(v5Image);
    try
    {
      v5Writer->Update();
    }
    catch (const itk::ExceptionObject & ex)
    {
      std::cerr << "V5 auto-bump write failed: " << ex << '\n';
      return EXIT_FAILURE;
    }

    std::string magic;
    {
      std::ifstream in(v5Fname, std::ios::binary);
      std::getline(in, magic);
    } // close the handle before RemoveFile below (matters on Windows)
    if (magic.rfind("NRRD0005", 0) != 0)
    {
      std::cerr << "Expected file to begin with NRRD0005 (measurement frame is a "
                << "v5-only field); got '" << magic << "'\n";
      return EXIT_FAILURE;
    }
    const std::string header = readNrrdHeader(v5Fname);
    if (header.find("\nmeasurement frame:") == std::string::npos ||
        header.find("measurement frame:=") != std::string::npos)
    {
      std::cerr << "Expected standard 'measurement frame:' field. Header:\n" << header;
      return EXIT_FAILURE;
    }

    itksys::SystemTools::RemoveFile(v5Fname);
  }

  // Regression test for the pre-existing 666666 sentinel in
  // WriteMeasurementFrame:
  //   When the user supplies a measurement frame whose dimensions do not
  //   match the image's space dimension, the writer must throw rather
  //   than silently write a frame populated with the sentinel value
  //   666666 (which teem faithfully emits to the on-disk header,
  //   corrupting the orientation matrix).
  {
    auto badImage = ImageType::New(); // 3-D image, spaceDim = 3
    badImage->SetRegions(size);
    badImage->Allocate();
    badImage->FillBuffer(0);
    itk::MetaDataDictionary & badDict = badImage->GetMetaDataDictionary();
    // 2x2 frame provided for a 3-D image -- malformed.
    std::vector<std::vector<double>> badFrame = { { 1.0, 0.0 }, { 0.0, 1.0 } };
    itk::EncapsulateMetaData<std::vector<std::vector<double>>>(badDict, "NRRD_measurement frame", badFrame);

    std::string badFname = argv[1];
    badFname += "/metadatatest_badframe.nrrd";

    auto badWriter = ImageWriterType::New();
    badWriter->SetImageIO(itk::NrrdImageIO::New());
    badWriter->SetFileName(badFname.c_str());
    badWriter->SetInput(badImage);
    bool threw = false;
    try
    {
      badWriter->Update();
    }
    catch (const itk::ExceptionObject &)
    {
      threw = true;
    }
    if (!threw)
    {
      std::cerr << "Writing a malformed measurement frame must throw; it did not.\n";
      return EXIT_FAILURE;
    }
    // If a partial file was produced before the throw, verify the
    // 666666 sentinel is not present.
    if (itksys::SystemTools::FileExists(badFname.c_str()))
    {
      const std::string header = readNrrdHeader(badFname);
      if (header.find("666666") != std::string::npos)
      {
        std::cerr << "666666 sentinel leaked into the header (regression). Header:\n" << header;
        return EXIT_FAILURE;
      }
      itksys::SystemTools::RemoveFile(badFname);
    }
  }

  return EXIT_SUCCESS;
}
