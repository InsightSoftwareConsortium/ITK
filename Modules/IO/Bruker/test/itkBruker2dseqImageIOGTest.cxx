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

#include "itkBruker2dseqImageIO.h"
#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkMetaDataObject.h"
#include "itksys/SystemTools.hxx"

#include <gtest/gtest.h>

#include <fstream>
#include <string>
#include <vector>

#define _STRING(s) #s
#define TOSTRING(s) std::string(_STRING(s))

namespace
{
// Writes a minimal ParaVision reconstruction and returns the path of its 2dseq
std::string
WriteDataset(const std::string & name, const std::string & visu, int frameCount)
{
  const std::string dir = TOSTRING(ITK_TEST_OUTPUT_DIR) + "/" + name + "/pdata/1";
  if (!itksys::SystemTools::MakeDirectory(dir))
  {
    return std::string();
  }
  {
    std::ofstream visuStream(dir + "/visu_pars");
    visuStream << visu;
  }
  {
    // int16 little-endian pixels of 4x2 frames, values counting up from zero
    std::ofstream dataStream(dir + "/2dseq", std::ios::binary);
    for (int i = 0; i < 8 * frameCount; ++i)
    {
      const char bytes[2] = { static_cast<char>(i & 0xff), static_cast<char>((i >> 8) & 0xff) };
      dataStream.write(bytes, 2);
    }
  }
  return dir + "/2dseq";
}

// A minimal ParaVision 360 style visu_pars: run-length encoded arrays, enum
// arrays, a value block interrupted by a $$ comment, a string wrapped across
// lines (trailing space kept), a comma inside a string, and a trailer after
// ##END=. 4x2 pixels, 3 slices, 4 movie frames, movie varying fastest, slice
// positions stored against the orientation's third axis.
std::string
MakeVisuPars()
{
  std::string visu;
  visu += "##TITLE=Parameter List, ParaVision 360 V3.6\n";
  visu += "##JCAMPDX=4.24\n";
  visu += "##DATATYPE=Parameter Values\n";
  visu += "##ORIGIN=Bruker BioSpin GmbH & Co. KG\n";
  visu += "##OWNER=nmrsu\n";
  visu += "$$ Write Options: Symbolic Enums, RLE encoded arrays, symbol visibility\n";
  visu += "$$ 2024-07-25 09:18:04.415 +0200  nmrsu@host\n";
  visu += "$$ /opt/nmrdata/PV-360.3.6/data/test/visu_pars\n";
  visu += "$$ process parxserver\n";
  visu += "##$VisuVersion=8\n";
  visu += "##$VisuCreationDate=<2024-07-25T09:18:04,238+0200>\n";
  visu += "##$VisuCoreFrameCount=12\n";
  visu += "##$VisuCoreDim=2\n";
  visu += "##$VisuCoreSize=( 2 )\n";
  visu += "4 2\n";
  visu += "##$VisuCoreDimDesc=( 2 )\n";
  visu += "spatial spatial\n";
  visu += "##$VisuCoreExtent=( 2 )\n";
  visu += "4 2\n";
  visu += "##$VisuCoreFrameThickness=( 1 )\n";
  visu += "1.5\n";
  visu += "##$VisuCoreUnits=( 2, 65 )\n";
  visu += "<mm> <mm>\n";
  visu += "##$VisuCoreOrientation=( 3, 9 )\n";
  visu += "1 0 0 0 1 0 0 0 1 \n";
  visu += "1 0 0 0 1 0 0 0 1 \n";
  visu += "1 0 0 0 1 0 0 0 1\n";
  visu += "##$VisuCorePosition=( 3, 3 )\n";
  visu += "0 0 0 \n";
  visu += "$$ @vis= VisuCoreOrientation VisuCorePosition\n";
  visu += "0 0 -1.5 \n";
  visu += "0 0 -3\n";
  visu += "##$VisuCoreSlicePacksDef=(1, 1)\n";
  visu += "##$VisuCoreDataOffs=( 1 )\n";
  visu += "10\n";
  visu += "##$VisuCoreDataSlope=( 12 )\n";
  visu += "@12*(2)\n";
  visu += "##$VisuCoreFrameType=( 1 )\n";
  visu += "MAGNITUDE_IMAGE\n";
  visu += "##$VisuCoreWordType=_16BIT_SGN_INT\n";
  visu += "##$VisuCoreByteOrder=littleEndian\n";
  visu += "##$VisuCoreDiskSliceOrder=( 1 )\n";
  visu += "disk_normal_slice_order\n";
  visu += "##$VisuFGOrderDescDim=2\n";
  visu += "##$VisuFGOrderDesc=( 2 )\n";
  visu += "(4, <FG_MOVIE>, <maps, \n";
  visu += "fitted>, 0, 0) (3, <FG_SLICE>, <>, 0, 1)\n";
  visu += "##$VisuGroupDepVals=( 1 )\n";
  visu += "(<VisuCorePosition>, 0)\n";
  visu += "##$VisuAcqRepetitionTime=( 1 )\n";
  visu += "2000\n";
  visu += "##END=\n";
  visu += "$$ File finished by PARX at 2024-07-25 09:18:04.417 +0200\n";
  return visu;
}

// Replaces the run-length encoded slope array of the ParaVision 360 dataset
std::string
VisuParsWithSlope(const std::string & slopeRecord)
{
  std::string                  visu = MakeVisuPars();
  const std::string::size_type slopePos = visu.find("@12*(2)");
  visu.replace(slopePos, 7, slopeRecord);
  return visu;
}

void
ExpectReadThrows(const std::string & name, const std::string & visu)
{
  const std::string path = WriteDataset(name, visu, 12);
  ASSERT_FALSE(path.empty());
  auto io = itk::Bruker2dseqImageIO::New();
  auto reader = itk::ImageFileReader<itk::Image<float, 4>>::New();
  reader->SetImageIO(io);
  reader->SetFileName(path);
  EXPECT_THROW(reader->Update(), itk::ExceptionObject);
}
} // namespace

TEST(Bruker2dseqImageIO, ReadParaVision360Dataset)
{
  const std::string path = WriteDataset("bruker2dseq_pv360", MakeVisuPars(), 12);
  ASSERT_FALSE(path.empty());

  using ImageType = itk::Image<float, 4>;
  auto io = itk::Bruker2dseqImageIO::New();
  auto reader = itk::ImageFileReader<ImageType>::New();
  reader->SetImageIO(io);
  reader->SetFileName(path);
  ASSERT_NO_THROW(reader->Update());

  const ImageType::Pointer image = reader->GetOutput();
  const auto               size = image->GetLargestPossibleRegion().GetSize();
  EXPECT_EQ(size[0], 4u);
  EXPECT_EQ(size[1], 2u);
  EXPECT_EQ(size[2], 3u);
  EXPECT_EQ(size[3], 4u);

  const ImageType::SpacingType spacing = image->GetSpacing();
  EXPECT_DOUBLE_EQ(spacing[0], 1.0);
  EXPECT_DOUBLE_EQ(spacing[1], 1.0);
  EXPECT_DOUBLE_EQ(spacing[2], 1.5);
  EXPECT_DOUBLE_EQ(spacing[3], 2.0);

  const ImageType::PointType origin = image->GetOrigin();
  EXPECT_DOUBLE_EQ(origin[0], 0.5);
  EXPECT_DOUBLE_EQ(origin[1], 0.5);
  EXPECT_DOUBLE_EQ(origin[2], 0.0);

  // Slice positions run along -z while the orientation's third row is +z
  EXPECT_DOUBLE_EQ(image->GetDirection()(2, 2), -1.0);

  // Stored frame order is movie-fastest; the reader reorders to slice-fastest,
  // then applies the RLE slope (2) and the broadcast single offset (10)
  for (itk::IndexValueType t = 0; t < 4; ++t)
  {
    for (itk::IndexValueType z = 0; z < 3; ++z)
    {
      for (itk::IndexValueType y = 0; y < 2; ++y)
      {
        for (itk::IndexValueType x = 0; x < 4; ++x)
        {
          const itk::Index<4> index = { { x, y, z, t } };
          const float         expected = 2.0f * static_cast<float>((t + 4 * z) * 8 + y * 4 + x) + 10.0f;
          EXPECT_FLOAT_EQ(image->GetPixel(index), expected) << "at " << index;
        }
      }
    }
  }

  const itk::MetaDataDictionary & dict = io->GetMetaDataDictionary();
  std::vector<double>             slopes;
  ASSERT_TRUE(itk::ExposeMetaData(dict, "VisuCoreDataSlope", slopes));
  EXPECT_EQ(slopes.size(), 12u);
  std::string diskSliceOrder;
  ASSERT_TRUE(itk::ExposeMetaData(dict, "VisuCoreDiskSliceOrder", diskSliceOrder));
  EXPECT_EQ(diskSliceOrder, "disk_normal_slice_order");
  std::vector<std::vector<std::string>> frameGroups;
  ASSERT_TRUE(itk::ExposeMetaData(dict, "VisuFGOrderDesc", frameGroups));
  ASSERT_EQ(frameGroups.size(), 2u);
  ASSERT_EQ(frameGroups[0].size(), 5u);
  EXPECT_EQ(frameGroups[0][2], "<maps, fitted>");
  std::vector<std::vector<double>> slicePacksDef;
  ASSERT_TRUE(itk::ExposeMetaData(dict, "VisuCoreSlicePacksDef", slicePacksDef));
  ASSERT_EQ(slicePacksDef.size(), 1u);
  EXPECT_EQ(slicePacksDef[0], std::vector<double>({ 1.0, 1.0 }));
}

TEST(Bruker2dseqImageIO, RejectOversizedRLEExpansion)
{
  ExpectReadThrows("bruker2dseq_rle_bytes", VisuParsWithSlope("@999999999*(2)"));
  ExpectReadThrows("bruker2dseq_rle_digits", VisuParsWithSlope("@1000000000*(2)"));
}

TEST(Bruker2dseqImageIO, RejectScalingArrayOfUnexpectedLength)
{
  ExpectReadThrows("bruker2dseq_slope_count", VisuParsWithSlope("@3*(2)"));
}

TEST(Bruker2dseqImageIO, IgnoreSliceStepSmallerThanThickness)
{
  std::string visu = MakeVisuPars();
  // Slice positions a ten-thousandth of the 1.5 mm thickness apart carry no direction
  const std::string::size_type positionPos = visu.find("0 0 -1.5 \n");
  ASSERT_NE(positionPos, std::string::npos);
  visu.replace(positionPos, 10, "0 0 -0.0001 \n");
  const std::string::size_type lastPos = visu.find("0 0 -3\n");
  ASSERT_NE(lastPos, std::string::npos);
  visu.replace(lastPos, 7, "0 0 -0.0002\n");

  const std::string path = WriteDataset("bruker2dseq_flat_slices", visu, 12);
  ASSERT_FALSE(path.empty());
  auto io = itk::Bruker2dseqImageIO::New();
  auto reader = itk::ImageFileReader<itk::Image<float, 4>>::New();
  reader->SetImageIO(io);
  reader->SetFileName(path);
  ASSERT_NO_THROW(reader->Update());

  EXPECT_DOUBLE_EQ(reader->GetOutput()->GetSpacing()[2], 1.5);
  EXPECT_DOUBLE_EQ(reader->GetOutput()->GetDirection()(2, 2), 1.0);
}
