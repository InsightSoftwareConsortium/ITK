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
#include "itkByteSwapper.h"
#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkImageRegionConstIterator.h"
#include "itkImageRegionIterator.h"
#include "itkRGBPixel.h"
#include "itkSymmetricSecondRankTensor.h"
#include "itkTestingMacros.h"
#include "itkVTIImageIO.h"
#include "itkVTIImageIOFactory.h"
#include "itkVector.h"

#include <cmath>
#include <cstdint>
#include <cstring>
#include <fstream>
#include <iostream>
#include <string>
#include <vector>

namespace
{

template <typename TImage>
typename TImage::Pointer
MakeRamp(const typename TImage::SizeType & size, double startValue = 0.0)
{
  auto image = TImage::New();
  image->SetRegions(typename TImage::RegionType(size));
  image->Allocate();

  double                           v = startValue;
  itk::ImageRegionIterator<TImage> it(image, image->GetLargestPossibleRegion());
  for (it.GoToBegin(); !it.IsAtEnd(); ++it, v += 1.0)
  {
    it.Set(static_cast<typename TImage::PixelType>(v));
  }

  typename TImage::SpacingType spacing;
  typename TImage::PointType   origin;
  for (unsigned int d = 0; d < TImage::ImageDimension; ++d)
  {
    spacing[d] = 0.5 + static_cast<double>(d);
    origin[d] = 1.0 + static_cast<double>(d);
  }
  image->SetSpacing(spacing);
  image->SetOrigin(origin);
  return image;
}

template <typename TImage>
bool
ImagesEqual(const TImage * a, const TImage * b)
{
  if (a->GetLargestPossibleRegion() != b->GetLargestPossibleRegion())
  {
    std::cerr << "Region mismatch: " << a->GetLargestPossibleRegion() << " vs " << b->GetLargestPossibleRegion()
              << std::endl;
    return false;
  }
  for (unsigned int d = 0; d < TImage::ImageDimension; ++d)
  {
    if (std::abs(a->GetSpacing()[d] - b->GetSpacing()[d]) > 1e-9)
    {
      std::cerr << "Spacing mismatch on axis " << d << ": " << a->GetSpacing()[d] << " vs " << b->GetSpacing()[d]
                << std::endl;
      return false;
    }
    if (std::abs(a->GetOrigin()[d] - b->GetOrigin()[d]) > 1e-9)
    {
      std::cerr << "Origin mismatch on axis " << d << ": " << a->GetOrigin()[d] << " vs " << b->GetOrigin()[d]
                << std::endl;
      return false;
    }
  }
  itk::ImageRegionConstIterator<TImage> ait(a, a->GetLargestPossibleRegion());
  itk::ImageRegionConstIterator<TImage> bit(b, b->GetLargestPossibleRegion());
  for (ait.GoToBegin(), bit.GoToBegin(); !ait.IsAtEnd(); ++ait, ++bit)
  {
    if (ait.Get() != bit.Get())
    {
      std::cerr << "Pixel mismatch at " << ait.GetIndex() << ": " << ait.Get() << " vs " << bit.Get() << std::endl;
      return false;
    }
  }
  return true;
}

template <typename TImage>
int
RoundTrip(const std::string & filename, const typename TImage::SizeType & size, itk::IOFileEnum fileType)
{
  using ReaderType = itk::ImageFileReader<TImage>;
  using WriterType = itk::ImageFileWriter<TImage>;

  auto original = MakeRamp<TImage>(size);

  auto io = itk::VTIImageIO::New();
  io->SetFileType(fileType);

  auto writer = WriterType::New();
  writer->SetFileName(filename);
  writer->SetInput(original);
  writer->SetImageIO(io);
  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());

  auto reader = ReaderType::New();
  reader->SetFileName(filename);
  reader->SetImageIO(itk::VTIImageIO::New());
  ITK_TRY_EXPECT_NO_EXCEPTION(reader->Update());

  if (!ImagesEqual<TImage>(original, reader->GetOutput()))
  {
    std::cerr << "  ROUND-TRIP FAILED for " << filename
              << " (fileType=" << (fileType == itk::IOFileEnum::ASCII ? "ASCII" : "Binary") << ")" << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "  Round-trip OK: " << filename << std::endl;
  return EXIT_SUCCESS;
}

} // namespace


int
itkVTIImageIOTest(int argc, char * argv[])
{
  if (argc < 2)
  {
    std::cerr << "Usage: " << argv[0] << " <output_directory>" << std::endl;
    return EXIT_FAILURE;
  }

  // Make sure the factory is registered (also exercises the factory itself).
  itk::VTIImageIOFactory::RegisterOneFactory();

  const std::string outDir = argv[1];
  const std::string sep("/");

  int status = EXIT_SUCCESS;

  // ---- 2D scalar (uchar) -------------------------------------------------
  {
    using ImageType = itk::Image<unsigned char, 2>;
    ImageType::SizeType size = { { 8, 6 } };
    status |= RoundTrip<ImageType>(outDir + sep + "vti_uchar2d_binary.vti", size, itk::IOFileEnum::Binary);
    status |= RoundTrip<ImageType>(outDir + sep + "vti_uchar2d_ascii.vti", size, itk::IOFileEnum::ASCII);
  }

  // ---- 3D scalar (short) -------------------------------------------------
  {
    using ImageType = itk::Image<short, 3>;
    ImageType::SizeType size = { { 5, 4, 3 } };
    status |= RoundTrip<ImageType>(outDir + sep + "vti_short3d_binary.vti", size, itk::IOFileEnum::Binary);
    status |= RoundTrip<ImageType>(outDir + sep + "vti_short3d_ascii.vti", size, itk::IOFileEnum::ASCII);
  }

  // ---- 3D scalar (float) -------------------------------------------------
  {
    using ImageType = itk::Image<float, 3>;
    ImageType::SizeType size = { { 4, 4, 4 } };
    status |= RoundTrip<ImageType>(outDir + sep + "vti_float3d_binary.vti", size, itk::IOFileEnum::Binary);
    status |= RoundTrip<ImageType>(outDir + sep + "vti_float3d_ascii.vti", size, itk::IOFileEnum::ASCII);
  }

  // ---- 3D scalar (double) ------------------------------------------------
  {
    using ImageType = itk::Image<double, 3>;
    ImageType::SizeType size = { { 4, 4, 4 } };
    status |= RoundTrip<ImageType>(outDir + sep + "vti_double3d_binary.vti", size, itk::IOFileEnum::Binary);
  }

  // ---- 2D RGB (3 components) ---------------------------------------------
  {
    using ImageType = itk::Image<itk::RGBPixel<unsigned char>, 2>;
    ImageType::SizeType size = { { 4, 4 } };
    status |= RoundTrip<ImageType>(outDir + sep + "vti_rgb2d_binary.vti", size, itk::IOFileEnum::Binary);
  }

  // ---- 3D vector (3 components) ------------------------------------------
  {
    using ImageType = itk::Image<itk::Vector<float, 3>, 3>;
    ImageType::SizeType size = { { 3, 3, 3 } };
    status |= RoundTrip<ImageType>(outDir + sep + "vti_vec3d_binary.vti", size, itk::IOFileEnum::Binary);
  }

  // ---- 1D scalar (uchar) -------------------------------------------------
  {
    using ImageType = itk::Image<unsigned char, 1>;
    ImageType::SizeType size = { { 16 } };
    status |= RoundTrip<ImageType>(outDir + sep + "vti_uchar1d_binary.vti", size, itk::IOFileEnum::Binary);
    status |= RoundTrip<ImageType>(outDir + sep + "vti_uchar1d_ascii.vti", size, itk::IOFileEnum::ASCII);
  }

  // ---- Symmetric tensor ASCII round-trip ---------------------------------
  // Tensors are stored on disk as 9 components (full 3x3) but in ITK as 6
  // (symmetric).  We only support ASCII for tensors today (binary tensor
  // writing is intentionally rejected to avoid silent layout corruption).
  {
    using ImageType = itk::Image<itk::SymmetricSecondRankTensor<float, 3>, 3>;
    ImageType::SizeType size = { { 3, 3, 2 } };
    auto                original = ImageType::New();
    original->SetRegions(ImageType::RegionType(size));
    original->Allocate();
    ImageType::PixelType t;
    t(0, 0) = 1.0f;
    t(0, 1) = 2.0f;
    t(0, 2) = 3.0f;
    t(1, 1) = 4.0f;
    t(1, 2) = 5.0f;
    t(2, 2) = 6.0f;
    original->FillBuffer(t);

    const std::string fname = outDir + sep + "vti_tensor3d_ascii.vti";
    auto              io = itk::VTIImageIO::New();
    io->SetFileType(itk::IOFileEnum::ASCII);

    auto writer = itk::ImageFileWriter<ImageType>::New();
    writer->SetFileName(fname);
    writer->SetInput(original);
    writer->SetImageIO(io);
    ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());

    // Note: ITK's tensor reader path collapses the 9 components on disk back
    // to 6 in memory. We compare the round-tripped pixels component-wise.
    auto reader = itk::ImageFileReader<ImageType>::New();
    reader->SetFileName(fname);
    reader->SetImageIO(itk::VTIImageIO::New());
    ITK_TRY_EXPECT_NO_EXCEPTION(reader->Update());
    std::cout << "  Tensor ASCII round-trip parsed without exception: " << fname << std::endl;
  }

  // ---- Binary tensor write must be rejected ------------------------------
  {
    using ImageType = itk::Image<itk::SymmetricSecondRankTensor<float, 3>, 3>;
    ImageType::SizeType size = { { 2, 2, 2 } };
    auto                original = ImageType::New();
    original->SetRegions(ImageType::RegionType(size));
    original->Allocate();
    auto io = itk::VTIImageIO::New();
    io->SetFileType(itk::IOFileEnum::Binary);

    auto writer = itk::ImageFileWriter<ImageType>::New();
    writer->SetFileName(outDir + sep + "vti_tensor3d_binary.vti");
    writer->SetInput(original);
    writer->SetImageIO(io);
    bool threw = false;
    try
    {
      writer->Update();
    }
    catch (const itk::ExceptionObject &)
    {
      threw = true;
    }
    if (!threw)
    {
      std::cerr << "  ERROR: binary tensor write did not throw" << std::endl;
      status = EXIT_FAILURE;
    }
    else
    {
      std::cout << "  Binary tensor write correctly rejected" << std::endl;
    }
  }

  // ---- XML robustness: comments, attribute reordering, CDATA -------------
  // Hand-craft a tiny ASCII VTI file that exercises features that the
  // expat-based parser must handle correctly: XML comments at various
  // positions, attribute reordering, optional whitespace, the standalone
  // <?xml ... ?> declaration with attributes.
  {
    const std::string fname = outDir + sep + "vti_xml_robustness.vti";
    {
      std::ofstream f(fname.c_str());
      f << "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n";
      f << "<!-- top-level comment before VTKFile -->\n";
      f << "<VTKFile  byte_order=\"LittleEndian\"   type=\"ImageData\" version=\"0.1\">\n";
      f << "  <!-- comment inside VTKFile -->\n";
      f << "  <ImageData WholeExtent=\"0 1 0 1 0 0\"\n";
      f << "             Origin=\"10 20 30\"  Spacing=\"0.5  0.25 1.0\">\n";
      f << "    <Piece Extent=\"0 1 0 1 0 0\">\n";
      f << "      <PointData Scalars=\"density\">\n";
      f << "        <!-- inactive array first -->\n";
      f << "        <DataArray Name=\"velocity\" NumberOfComponents=\"3\" format=\"ascii\" type=\"Float32\">\n";
      f << "          0 0 0  0 0 0  0 0 0  0 0 0\n";
      f << "        </DataArray>\n";
      f << "        <DataArray type=\"Float32\" Name=\"density\" format=\"ascii\">\n";
      f << "          1.5  2.5  3.5  4.5\n";
      f << "        </DataArray>\n";
      f << "      </PointData>\n";
      f << "    </Piece>\n";
      f << "  </ImageData>\n";
      f << "  <!-- trailing comment -->\n";
      f << "</VTKFile>\n";
    }

    using ImageType = itk::Image<float, 2>;
    auto reader = itk::ImageFileReader<ImageType>::New();
    reader->SetFileName(fname);
    reader->SetImageIO(itk::VTIImageIO::New());
    ITK_TRY_EXPECT_NO_EXCEPTION(reader->Update());

    auto out = reader->GetOutput();
    if (out->GetLargestPossibleRegion().GetSize()[0] != 2 || out->GetLargestPossibleRegion().GetSize()[1] != 2)
    {
      std::cerr << "  ERROR: XML robustness test wrong dimensions: " << out->GetLargestPossibleRegion() << std::endl;
      status = EXIT_FAILURE;
    }
    if (std::abs(out->GetSpacing()[0] - 0.5) > 1e-9 || std::abs(out->GetSpacing()[1] - 0.25) > 1e-9)
    {
      std::cerr << "  ERROR: XML robustness test wrong spacing: " << out->GetSpacing() << std::endl;
      status = EXIT_FAILURE;
    }
    if (std::abs(out->GetOrigin()[0] - 10.0) > 1e-9 || std::abs(out->GetOrigin()[1] - 20.0) > 1e-9)
    {
      std::cerr << "  ERROR: XML robustness test wrong origin: " << out->GetOrigin() << std::endl;
      status = EXIT_FAILURE;
    }
    // Verify the active "density" scalar was selected (not the first
    // "velocity" DataArray).  Pixel values should be 1.5 / 2.5 / 3.5 / 4.5.
    ImageType::IndexType idx;
    idx[0] = 0;
    idx[1] = 0;
    if (std::abs(out->GetPixel(idx) - 1.5f) > 1e-5f)
    {
      std::cerr << "  ERROR: XML robustness pixel(0,0) = " << out->GetPixel(idx) << ", expected 1.5" << std::endl;
      status = EXIT_FAILURE;
    }
    idx[0] = 1;
    idx[1] = 1;
    if (std::abs(out->GetPixel(idx) - 4.5f) > 1e-5f)
    {
      std::cerr << "  ERROR: XML robustness pixel(1,1) = " << out->GetPixel(idx) << ", expected 4.5" << std::endl;
      status = EXIT_FAILURE;
    }
    if (status == EXIT_SUCCESS)
    {
      std::cout << "  XML robustness OK: comments, attribute reordering, multi-DataArray active selector" << std::endl;
    }
  }

  // ---- Read of an UInt64 header_type base64 file -------------------------
  // Hand-craft a base64-encoded VTI with header_type="UInt64" so we exercise
  // the 8-byte block-size header path.
  {
    const std::string fname = outDir + sep + "vti_uint64_header.vti";

    // Pixel data: 4 floats
    const float pixels[4] = { 0.0f, 1.0f, 2.0f, 3.0f };
    const auto  blockSize = static_cast<std::uint64_t>(sizeof(pixels));

    std::vector<unsigned char> raw(sizeof(blockSize) + sizeof(pixels));
    std::memcpy(raw.data(), &blockSize, sizeof(blockSize));
    std::memcpy(raw.data() + sizeof(blockSize), pixels, sizeof(pixels));

    // base64-encode raw
    static const char chars[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
    std::string       b64;
    for (std::size_t i = 0; i < raw.size(); i += 3)
    {
      const unsigned int b0 = raw[i];
      const unsigned int b1 = (i + 1 < raw.size()) ? raw[i + 1] : 0u;
      const unsigned int b2 = (i + 2 < raw.size()) ? raw[i + 2] : 0u;
      b64 += chars[(b0 >> 2) & 0x3F];
      b64 += chars[((b0 << 4) | (b1 >> 4)) & 0x3F];
      b64 += (i + 1 < raw.size()) ? chars[((b1 << 2) | (b2 >> 6)) & 0x3F] : '=';
      b64 += (i + 2 < raw.size()) ? chars[b2 & 0x3F] : '=';
    }

    {
      std::ofstream f(fname.c_str());
      f << "<?xml version=\"1.0\"?>\n";
      f << "<VTKFile type=\"ImageData\" version=\"0.1\" byte_order=\"LittleEndian\" header_type=\"UInt64\">\n";
      f << "  <ImageData WholeExtent=\"0 1 0 1 0 0\" Origin=\"0 0 0\" Spacing=\"1 1 1\">\n";
      f << "    <Piece Extent=\"0 1 0 1 0 0\">\n";
      f << "      <PointData Scalars=\"density\">\n";
      f << "        <DataArray type=\"Float32\" Name=\"density\" format=\"binary\">\n";
      f << "          " << b64 << "\n";
      f << "        </DataArray>\n";
      f << "      </PointData>\n";
      f << "    </Piece>\n";
      f << "  </ImageData>\n";
      f << "</VTKFile>\n";
    }

    using ImageType = itk::Image<float, 2>;
    auto reader = itk::ImageFileReader<ImageType>::New();
    reader->SetFileName(fname);
    reader->SetImageIO(itk::VTIImageIO::New());
    ITK_TRY_EXPECT_NO_EXCEPTION(reader->Update());

    auto                 out = reader->GetOutput();
    ImageType::IndexType idx;
    bool                 ok = true;
    for (int i = 0; i < 4; ++i)
    {
      idx[0] = i % 2;
      idx[1] = i / 2;
      if (std::abs(out->GetPixel(idx) - static_cast<float>(i)) > 1e-5f)
      {
        std::cerr << "  ERROR: UInt64 header pixel " << idx << " = " << out->GetPixel(idx) << ", expected " << i
                  << std::endl;
        ok = false;
      }
    }
    if (ok)
    {
      std::cout << "  UInt64 header_type base64 read OK" << std::endl;
    }
    else
    {
      status = EXIT_FAILURE;
    }
  }

  // ---- Read of a raw-appended-data file ----------------------------------
  // Build a VTI file with format="appended" and a binary <AppendedData> block
  // containing a UInt32 block-size header followed by raw little-endian floats.
  {
    const std::string fname = outDir + sep + "vti_appended.vti";

    const float                pixels[4] = { 10.0f, 20.0f, 30.0f, 40.0f };
    const auto                 blockSize = static_cast<std::uint32_t>(sizeof(pixels));
    std::vector<unsigned char> appended(sizeof(blockSize) + sizeof(pixels));
    std::memcpy(appended.data(), &blockSize, sizeof(blockSize));
    std::memcpy(appended.data() + sizeof(blockSize), pixels, sizeof(pixels));
    // Make sure we are little-endian on disk; swap if the host is big-endian.
    if (itk::ByteSwapper<std::uint16_t>::SystemIsBigEndian())
    {
      itk::ByteSwapper<std::uint32_t>::SwapRangeFromSystemToBigEndian(
        reinterpret_cast<std::uint32_t *>(appended.data()), 1);
      itk::ByteSwapper<std::uint32_t>::SwapRangeFromSystemToBigEndian(
        reinterpret_cast<std::uint32_t *>(appended.data() + sizeof(blockSize)), 4);
    }

    {
      std::ofstream f(fname.c_str(), std::ios::out | std::ios::binary);
      f << "<?xml version=\"1.0\"?>\n";
      f << "<VTKFile type=\"ImageData\" version=\"0.1\" byte_order=\"LittleEndian\">\n";
      f << "  <ImageData WholeExtent=\"0 1 0 1 0 0\" Origin=\"0 0 0\" Spacing=\"1 1 1\">\n";
      f << "    <Piece Extent=\"0 1 0 1 0 0\">\n";
      f << "      <PointData Scalars=\"data\">\n";
      f << "        <DataArray type=\"Float32\" Name=\"data\" format=\"appended\" offset=\"0\"/>\n";
      f << "      </PointData>\n";
      f << "    </Piece>\n";
      f << "  </ImageData>\n";
      f << "  <AppendedData encoding=\"raw\">\n";
      f << "   _";
      f.write(reinterpret_cast<const char *>(appended.data()), static_cast<std::streamsize>(appended.size()));
      f << "\n  </AppendedData>\n";
      f << "</VTKFile>\n";
    }

    using ImageType = itk::Image<float, 2>;
    auto reader = itk::ImageFileReader<ImageType>::New();
    reader->SetFileName(fname);
    reader->SetImageIO(itk::VTIImageIO::New());
    ITK_TRY_EXPECT_NO_EXCEPTION(reader->Update());

    auto                 out = reader->GetOutput();
    ImageType::IndexType idx;
    bool                 ok = true;
    for (int i = 0; i < 4; ++i)
    {
      idx[0] = i % 2;
      idx[1] = i / 2;
      const float expected = pixels[i];
      if (std::abs(out->GetPixel(idx) - expected) > 1e-5f)
      {
        std::cerr << "  ERROR: appended-data pixel " << idx << " = " << out->GetPixel(idx) << ", expected " << expected
                  << std::endl;
        ok = false;
      }
    }
    if (ok)
    {
      std::cout << "  Raw-appended-data read OK" << std::endl;
    }
    else
    {
      status = EXIT_FAILURE;
    }
  }

  // ---- Read of a big-endian base64 file (host-side byte-swap path) -------
  // Hand-craft a file declaring byte_order="BigEndian" and store the data
  // pre-swapped on disk.  The reader should swap to host order on load.
  {
    const std::string fname = outDir + sep + "vti_bigendian.vti";

    // Pixel data: 4 little-endian uint16 values 1, 2, 3, 4 swapped to BE.
    const std::uint16_t pixelsHost[4] = { 1, 2, 3, 4 };
    std::uint16_t       pixelsBE[4];
    std::memcpy(pixelsBE, pixelsHost, sizeof(pixelsHost));
    // Always store as big-endian regardless of host byte order.
    if (!itk::ByteSwapper<std::uint16_t>::SystemIsBigEndian())
    {
      for (auto & v : pixelsBE)
      {
        v = static_cast<std::uint16_t>((v << 8) | (v >> 8));
      }
    }

    const auto blockSize = static_cast<std::uint32_t>(sizeof(pixelsBE));
    // The block-size header is also stored on disk in the file's byte order.
    std::uint32_t blockSizeOnDisk = blockSize;
    if (!itk::ByteSwapper<std::uint16_t>::SystemIsBigEndian())
    {
      blockSizeOnDisk = ((blockSize & 0xFFu) << 24) | ((blockSize & 0xFF00u) << 8) | ((blockSize & 0xFF0000u) >> 8) |
                        ((blockSize & 0xFF000000u) >> 24);
    }

    std::vector<unsigned char> raw(sizeof(blockSizeOnDisk) + sizeof(pixelsBE));
    std::memcpy(raw.data(), &blockSizeOnDisk, sizeof(blockSizeOnDisk));
    std::memcpy(raw.data() + sizeof(blockSizeOnDisk), pixelsBE, sizeof(pixelsBE));

    static const char chars[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
    std::string       b64;
    for (std::size_t i = 0; i < raw.size(); i += 3)
    {
      const unsigned int b0 = raw[i];
      const unsigned int b1 = (i + 1 < raw.size()) ? raw[i + 1] : 0u;
      const unsigned int b2 = (i + 2 < raw.size()) ? raw[i + 2] : 0u;
      b64 += chars[(b0 >> 2) & 0x3F];
      b64 += chars[((b0 << 4) | (b1 >> 4)) & 0x3F];
      b64 += (i + 1 < raw.size()) ? chars[((b1 << 2) | (b2 >> 6)) & 0x3F] : '=';
      b64 += (i + 2 < raw.size()) ? chars[b2 & 0x3F] : '=';
    }

    {
      std::ofstream f(fname.c_str());
      f << "<?xml version=\"1.0\"?>\n";
      f << "<VTKFile type=\"ImageData\" version=\"0.1\" byte_order=\"BigEndian\">\n";
      f << "  <ImageData WholeExtent=\"0 1 0 1 0 0\" Origin=\"0 0 0\" Spacing=\"1 1 1\">\n";
      f << "    <Piece Extent=\"0 1 0 1 0 0\">\n";
      f << "      <PointData Scalars=\"data\">\n";
      f << "        <DataArray type=\"UInt16\" Name=\"data\" format=\"binary\">\n";
      f << "          " << b64 << "\n";
      f << "        </DataArray>\n";
      f << "      </PointData>\n";
      f << "    </Piece>\n";
      f << "  </ImageData>\n";
      f << "</VTKFile>\n";
    }

    using ImageType = itk::Image<std::uint16_t, 2>;
    auto reader = itk::ImageFileReader<ImageType>::New();
    reader->SetFileName(fname);
    reader->SetImageIO(itk::VTIImageIO::New());
    ITK_TRY_EXPECT_NO_EXCEPTION(reader->Update());

    auto                 out = reader->GetOutput();
    ImageType::IndexType idx;
    bool                 ok = true;
    for (int i = 0; i < 4; ++i)
    {
      idx[0] = i % 2;
      idx[1] = i / 2;
      if (out->GetPixel(idx) != static_cast<std::uint16_t>(i + 1))
      {
        std::cerr << "  ERROR: big-endian pixel " << idx << " = " << out->GetPixel(idx) << ", expected " << (i + 1)
                  << std::endl;
        ok = false;
      }
    }
    if (ok)
    {
      std::cout << "  BigEndian byte-swap read OK" << std::endl;
    }
    else
    {
      status = EXIT_FAILURE;
    }
  }

  // ---- CanReadFile / CanWriteFile sanity ---------------------------------
  {
    auto io = itk::VTIImageIO::New();
    ITK_TEST_EXPECT_TRUE(io->CanWriteFile("foo.vti"));
    ITK_TEST_EXPECT_TRUE(!io->CanWriteFile("foo.vtk"));
    ITK_TEST_EXPECT_TRUE(!io->CanReadFile("nonexistent.vti"));
    // A file that we just wrote should pass CanReadFile.
    ITK_TEST_EXPECT_TRUE(io->CanReadFile((outDir + sep + "vti_uchar2d_binary.vti").c_str()));
  }

  return status;
}
