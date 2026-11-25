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

// Ground truth: NrrdImageIO, not MINCImageIO, converts a RAS-space NRRD with the MINC file's geometry to ITK LPS.

#include "gtest/gtest.h"
#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkMINCImageIO.h"
#include "itkMINCImageIOFactory.h"
#include "itkMetaDataObject.h"
#include "itkNrrdImageIOFactory.h"
#include "itkMINCImageIOConfigurePrivate.h"
#include "itksys/SystemTools.hxx"

#include <cstdio>
#include <fstream>
#include <iomanip>
#include <optional>
#include <string>
#include <vector>

#define _STRING(s) #s
#define TOSTRING(s) std::string(_STRING(s))

namespace
{
using ImageType = itk::Image<unsigned char, 3>;

constexpr char environmentVariable[] = "ITK_MINC_IO_RAS_TO_LPS";

// World geometry (RAS) of t1_z+_byte_cor.mnc, read from its xspace, yspace and zspace HDF5 attributes with h5py.
constexpr unsigned int sizes[3] = { 30, 40, 10 };
constexpr double       starts[3] = { -18.0, 60.0, -10.0 };
constexpr double       steps[3] = { 1.0, 1.0, 1.2 };
constexpr double       cosines[3][3] = { { 0.9980212170406957, 0.0523040113746069, -0.034899007589522905 },
                                         { -0.05172001539071517, 0.9985092971339445, 0.01744200519034907 },
                                         { 0.03575998606925314, -0.015601993922049426, 0.9992386107341845 } };

std::string
OutputPath(const std::string & name)
{
  return TOSTRING(ITK_TEST_OUTPUT_DIR) + "/" + name;
}

// Sets or unsets ITK_MINC_IO_RAS_TO_LPS for one scope, so other tests in the driver see the original environment.
class ScopedRAStoLPSEnvironment
{
public:
  explicit ScopedRAStoLPSEnvironment(const std::optional<std::string> & value)
  {
    std::string saved;
    if (itksys::SystemTools::GetEnv(environmentVariable, saved))
    {
      m_Saved = saved;
    }
    Apply(value);
  }

  ~ScopedRAStoLPSEnvironment() { Apply(m_Saved); }

  ScopedRAStoLPSEnvironment(const ScopedRAStoLPSEnvironment &) = delete;
  ScopedRAStoLPSEnvironment &
  operator=(const ScopedRAStoLPSEnvironment &) = delete;

private:
  static void
  Apply(const std::optional<std::string> & value)
  {
    if (value)
    {
      itksys::SystemTools::PutEnv(std::string(environmentVariable) + "=" + *value);
    }
    else
    {
      itksys::SystemTools::UnPutEnv(environmentVariable);
    }
  }

  std::optional<std::string> m_Saved;
};

// Writes a NRRD file in right-anterior-superior space with the geometry of t1_z+_byte_cor.mnc and zero voxels.
void
WriteRASNrrd(const std::string & fileName)
{
  std::ofstream nrrd(fileName, std::ios::binary);
  nrrd << std::setprecision(17);
  nrrd << "NRRD0004\n"
       << "type: unsigned char\n"
       << "dimension: 3\n"
       << "space: right-anterior-superior\n"
       << "sizes: " << sizes[0] << ' ' << sizes[1] << ' ' << sizes[2] << '\n'
       << "space directions:";
  for (unsigned int axis = 0; axis < 3; ++axis)
  {
    nrrd << " (" << steps[axis] * cosines[axis][0] << ',' << steps[axis] * cosines[axis][1] << ','
         << steps[axis] * cosines[axis][2] << ')';
  }
  double origin[3] = { 0.0, 0.0, 0.0 };
  for (unsigned int axis = 0; axis < 3; ++axis)
  {
    for (unsigned int component = 0; component < 3; ++component)
    {
      origin[component] += starts[axis] * cosines[axis][component];
    }
  }
  nrrd << "\nkinds: domain domain domain\n"
       << "encoding: raw\n"
       << "space origin: (" << origin[0] << ',' << origin[1] << ',' << origin[2] << ")\n\n";
  const std::vector<char> voxels(sizes[0] * sizes[1] * sizes[2], 0);
  nrrd.write(voxels.data(), static_cast<std::streamsize>(voxels.size()));
}

ImageType::ConstPointer
ReadWithFactory(const std::string & fileName)
{
  auto reader = itk::ImageFileReader<ImageType>::New();
  reader->SetFileName(fileName);
  reader->Update();
  return reader->GetOutput();
}

// The NRRD reader normalizes space directions, so allow rounding; observed differences are below 1e-14.
void
ExpectSameGeometry(const ImageType * mincImage, const ImageType * expected)
{
  constexpr double tolerance = 1e-12;
  EXPECT_EQ(mincImage->GetLargestPossibleRegion().GetSize(), expected->GetLargestPossibleRegion().GetSize());
  for (unsigned int i = 0; i < 3; ++i)
  {
    EXPECT_NEAR(mincImage->GetOrigin()[i], expected->GetOrigin()[i], tolerance) << "origin[" << i << ']';
    EXPECT_NEAR(mincImage->GetSpacing()[i], expected->GetSpacing()[i], tolerance) << "spacing[" << i << ']';
    for (unsigned int j = 0; j < 3; ++j)
    {
      EXPECT_NEAR(mincImage->GetDirection()[i][j], expected->GetDirection()[i][j], tolerance)
        << "direction[" << i << "][" << j << ']';
    }
  }
}

// Returns a copy of the image geometry with x and y mirrored, as MINCImageIO reports it without RAS to LPS conversion.
ImageType::Pointer
MirrorXY(const ImageType * image)
{
  auto mirrored = ImageType::New();
  mirrored->CopyInformation(image);
  auto origin = image->GetOrigin();
  auto direction = image->GetDirection();
  for (unsigned int row = 0; row < 2; ++row)
  {
    origin[row] = -origin[row];
    for (unsigned int column = 0; column < 3; ++column)
    {
      direction[row][column] = -direction[row][column];
    }
  }
  mirrored->SetOrigin(origin);
  mirrored->SetDirection(direction);
  return mirrored;
}

class MINCImageIOLPSGeometry : public ::testing::Test
{
protected:
  // The GTest driver does not register IO factories, unlike the legacy test driver.
  static void
  SetUpTestSuite()
  {
    itk::MINCImageIOFactory::RegisterOneFactory();
    itk::NrrdImageIOFactory::RegisterOneFactory();
  }

  void
  SetUp() override
  {
    // Per test, because CTest runs the discovered tests in parallel.
    const std::string nrrdFile =
      OutputPath(std::string("itkMINCImageIOLPSGeometryGTest_") +
                 ::testing::UnitTest::GetInstance()->current_test_info()->name() + "_ras.nrrd");
    WriteRASNrrd(nrrdFile);
    m_LPSTruth = ReadWithFactory(nrrdFile);
    m_RASValues = MirrorXY(m_LPSTruth);
    std::remove(nrrdFile.c_str());
  }

  const std::string       m_MINCFile{ ITK_MINC_T1_Z_PLUS_BYTE_COR };
  ImageType::ConstPointer m_LPSTruth;
  ImageType::Pointer      m_RASValues;
};
} // namespace

// The default read, as ImageFileReader without SetImageIO, Python and SimpleITK see it.
TEST_F(MINCImageIOLPSGeometry, DefaultFactoryReadMatchesNrrdGeometry)
{
  const bool defaultRAStoLPS = itk::MINCImageIO::GetDefaultRAStoLPS();
  SCOPED_TRACE(std::string("GetDefaultRAStoLPS() = ") + (defaultRAStoLPS ? "true" : "false"));
  ExpectSameGeometry(ReadWithFactory(m_MINCFile), defaultRAStoLPS ? m_LPSTruth.GetPointer() : m_RASValues.GetPointer());
}

TEST_F(MINCImageIOLPSGeometry, DefaultFollowsCMakeOptionWhenEnvironmentUnset)
{
  const ScopedRAStoLPSEnvironment environment(std::nullopt);
  EXPECT_EQ(itk::MINCImageIO::GetDefaultRAStoLPS(), static_cast<bool>(ITK_MINC_IO_RAS_TO_LPS));
}

TEST_F(MINCImageIOLPSGeometry, EnvironmentVariableSwitchesFactoryReads)
{
  {
    const ScopedRAStoLPSEnvironment environment(std::string("OFF"));
    SCOPED_TRACE("ITK_MINC_IO_RAS_TO_LPS=OFF");
    ExpectSameGeometry(ReadWithFactory(m_MINCFile), m_RASValues);
  }
  {
    const ScopedRAStoLPSEnvironment environment(std::string("ON"));
    SCOPED_TRACE("ITK_MINC_IO_RAS_TO_LPS=ON");
    ExpectSameGeometry(ReadWithFactory(m_MINCFile), m_LPSTruth);
  }
}

// Files record whether their writer converted; the migration guide tells users to check this key.
TEST_F(MINCImageIOLPSGeometry, WriterRecordsRAStoLPSAttribute)
{
  const ImageType::ConstPointer image = ReadWithFactory(m_MINCFile);
  for (const bool writerRAStoLPS : { false, true })
  {
    const std::string writtenFile =
      OutputPath(std::string("itkMINCImageIOLPSGeometryGTest_") + (writerRAStoLPS ? "lps" : "ras") + ".mnc");
    auto writerIO = itk::MINCImageIO::New();
    writerIO->SetRAStoLPS(writerRAStoLPS);
    auto writer = itk::ImageFileWriter<ImageType>::New();
    writer->SetImageIO(writerIO);
    writer->SetInput(image);
    writer->SetFileName(writtenFile);
    ASSERT_NO_THROW(writer->Update());

    int storedRAStoLPS = -1;
    EXPECT_TRUE(
      itk::ExposeMetaData<int>(ReadWithFactory(writtenFile)->GetMetaDataDictionary(), "itk:RAStoLPS", storedRAStoLPS))
      << writtenFile;
    EXPECT_EQ(storedRAStoLPS, static_cast<int>(writerRAStoLPS)) << writtenFile;
    std::remove(writtenFile.c_str());
  }
}
