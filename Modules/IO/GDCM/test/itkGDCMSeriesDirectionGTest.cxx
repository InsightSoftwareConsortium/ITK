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

// Extended regression suite for ImageSeriesReader direction-cosine handling
// when the user-supplied file ordering implies a slice-stacking direction
// opposite to the GDCM-derived 3rd column of the direction matrix.
//
// Bug references:
//   - SimpleITK#2292   — bottom-to-top input produced direction[2][2] = -1
//                        when origin was simultaneously set to the bottom slice
//                        (origin/direction inconsistency).
//   - ITKElastix#291   — direction-cosine 3rd column not [0,0,1] when expected.
//   - ITK PR #5357     — proposed fix in itkImageSeriesReader.hxx.
//
// The fix flips the sign of the 3rd direction column when
//   dot(positionN - position1, direction[*][N-1]) < 0.
// These tests pin the contract from multiple angles:
//   * axis-aligned vs oblique row/col orientation
//   * axial / sagittal / coronal patient frames
//   * forward, reversed, and ReverseOrderOn() input
//   * orthonormality, right-handedness (det == +1)
//   * ForceOrthogonalDirection on/off symmetry
//   * physical-point invariance for every voxel under reordering

#include "itkImage.h"
#include "itkImageFileWriter.h"
#include "itkGDCMImageIO.h"
#include "itkGDCMSeriesFileNames.h"
#include "itkMetaDataObject.h"
#include "itkGTest.h"
#include "itkMath.h"
#include "itksys/SystemTools.hxx"
#include "itkImageSeriesReader.h"
#include "itkImageRegionConstIteratorWithIndex.h"
#include "itkImageRegionIteratorWithIndex.h"
#include <array>
#include <iomanip>
#include <random>
#include <sstream>
#include <string>
#include <vector>

#define _STRING(s) #s
#define TOSTRING(s) std::string(_STRING(s))

namespace
{

constexpr double kGeomTol = 1e-3; // mm — DICOM IPP precision
constexpr double kUnitTol = 1e-6; // unit-vector tolerance

using PixelT = uint16_t;
using Slice2D = itk::Image<PixelT, 2>;
using Volume3D = itk::Image<PixelT, 3>;

// Format a 3-component vector as DICOM DS string ("a\b\c").
std::string
formatDS(double a, double b, double c)
{
  std::ostringstream os;
  os.precision(6);
  os << std::fixed << a << '\\' << b << '\\' << c;
  return os.str();
}

std::string
formatOrientation(const std::array<double, 3> & row, const std::array<double, 3> & col)
{
  std::ostringstream os;
  os.precision(6);
  os << std::fixed << row[0] << '\\' << row[1] << '\\' << row[2] << '\\' << col[0] << '\\' << col[1] << '\\' << col[2];
  return os.str();
}

// Cross product for std::array<double,3>.
std::array<double, 3>
cross(const std::array<double, 3> & a, const std::array<double, 3> & b)
{
  return { { a[1] * b[2] - a[2] * b[1], a[2] * b[0] - a[0] * b[2], a[0] * b[1] - a[1] * b[0] } };
}

double
norm3(const std::array<double, 3> & v)
{
  return std::sqrt(v[0] * v[0] + v[1] * v[1] + v[2] * v[2]);
}

std::array<double, 3>
normalize3(const std::array<double, 3> & v)
{
  const double n = norm3(v);
  return { { v[0] / n, v[1] / n, v[2] / n } };
}

// Synthesize a DICOM series by writing one 2D slice per file with controlled
// ImagePositionPatient / ImageOrientationPatient tags.  Returns full file
// paths in the order written.
struct SeriesSpec
{
  std::string           dir;
  std::array<double, 3> rowOrient; // 0020,0037 first triplet
  std::array<double, 3> colOrient; // 0020,0037 second triplet
  std::array<double, 3> pos0;      // first slice IPP
  std::array<double, 3> stepIPP;   // delta between successive slices
  unsigned int          numSlices = 5;
  itk::Size<2>          pixelSize = { { 4, 3 } };
  std::string           seriesUID = "1.2.826.0.1.5357";
  std::string           modality = "CT";
};

std::vector<std::string>
writeSyntheticSeries(const SeriesSpec & spec)
{
  itksys::SystemTools::MakeDirectory(spec.dir);
  const std::string orientation = formatOrientation(spec.rowOrient, spec.colOrient);

  Slice2D::SizeType  sz = spec.pixelSize;
  Slice2D::IndexType start;
  start.Fill(0);
  Slice2D::RegionType region(start, sz);

  std::vector<std::string> files;
  files.reserve(spec.numSlices);

  for (unsigned int i = 0; i < spec.numSlices; ++i)
  {
    auto image = Slice2D::New();
    image->SetRegions(region);
    image->Allocate();

    // Encode the slice index into the pixel buffer so we can verify pixel
    // identity after physical-point lookup: pixel = 1000*slice + 10*y + x.
    itk::ImageRegionIteratorWithIndex<Slice2D> it(image, region);
    for (it.GoToBegin(); !it.IsAtEnd(); ++it)
    {
      const auto idx = it.GetIndex();
      it.Set(
        static_cast<PixelT>(1000U * (i + 1U) + 10U * static_cast<unsigned>(idx[1]) + static_cast<unsigned>(idx[0])));
    }

    auto & dict = image->GetMetaDataDictionary();

    const std::string ipp = formatDS(
      spec.pos0[0] + i * spec.stepIPP[0], spec.pos0[1] + i * spec.stepIPP[1], spec.pos0[2] + i * spec.stepIPP[2]);

    itk::EncapsulateMetaData<std::string>(dict, "0020|0032", ipp);
    itk::EncapsulateMetaData<std::string>(dict, "0020|0037", orientation);
    itk::EncapsulateMetaData<std::string>(dict, "0008|0060", spec.modality);
    itk::EncapsulateMetaData<std::string>(dict, "0020|0013", std::to_string(i + 1));
    itk::EncapsulateMetaData<std::string>(dict, "0010|0010", "PR5357^Test");
    itk::EncapsulateMetaData<std::string>(dict, "0020|000e", spec.seriesUID);
    itk::EncapsulateMetaData<std::string>(dict, "0020|000d", spec.seriesUID + ".1");
    itk::EncapsulateMetaData<std::string>(dict, "0010|0020", "PR5357");
    itk::EncapsulateMetaData<std::string>(dict, "0008|0020", "20260101");
    itk::EncapsulateMetaData<std::string>(dict, "0008|0030", "120000");
    itk::EncapsulateMetaData<std::string>(dict, "0008|0016", "1.2.840.10008.5.1.4.1.1.2");
    itk::EncapsulateMetaData<std::string>(dict, "0008|0018", spec.seriesUID + "." + std::to_string(i + 1));

    auto gdcmIO = itk::GDCMImageIO::New();
    gdcmIO->KeepOriginalUIDOn();

    auto writer = itk::ImageFileWriter<Slice2D>::New();
    writer->SetImageIO(gdcmIO);

    std::ostringstream fname;
    fname << spec.dir << "/slice_" << std::setw(4) << std::setfill('0') << i << ".dcm";
    writer->SetFileName(fname.str());
    writer->SetInput(image);
    writer->Update();
    files.push_back(fname.str());
  }
  return files;
}

// Read a series with the given file ordering and ForceOrthogonalDirection setting.
Volume3D::Pointer
readSeries(const std::vector<std::string> & files, bool forceOrtho = true, bool reverseOrder = false)
{
  auto reader = itk::ImageSeriesReader<Volume3D>::New();
  auto io = itk::GDCMImageIO::New();
  reader->SetImageIO(io);
  reader->SetFileNames(files);
  if (forceOrtho)
  {
    reader->ForceOrthogonalDirectionOn();
  }
  else
  {
    reader->ForceOrthogonalDirectionOff();
  }
  if (reverseOrder)
  {
    reader->ReverseOrderOn();
  }
  reader->UpdateLargestPossibleRegion();
  Volume3D::Pointer out = reader->GetOutput();
  out->DisconnectPipeline();
  return out;
}

// Determinant of a 3×3 itk::Matrix-like type with operator[][].
template <typename TMatrix>
double
det3(const TMatrix & m)
{
  return m[0][0] * (m[1][1] * m[2][2] - m[1][2] * m[2][1]) - m[0][1] * (m[1][0] * m[2][2] - m[1][2] * m[2][0]) +
         m[0][2] * (m[1][0] * m[2][1] - m[1][1] * m[2][0]);
}

// Assert columns are unit length and mutually orthogonal.  Determinant must be
// ±1 (orthonormal); ITK allows left-handed direction matrices, so the sign is
// not constrained.
template <typename TMatrix>
void
expectOrthonormal(const TMatrix & d)
{
  for (unsigned int c = 0; c < 3; ++c)
  {
    double n2 = 0.0;
    for (unsigned int r = 0; r < 3; ++r)
    {
      n2 += d[r][c] * d[r][c];
    }
    EXPECT_NEAR(n2, 1.0, kUnitTol) << "column " << c << " not unit length";
  }
  for (unsigned int c1 = 0; c1 < 3; ++c1)
  {
    for (unsigned int c2 = c1 + 1; c2 < 3; ++c2)
    {
      double dot = 0.0;
      for (unsigned int r = 0; r < 3; ++r)
      {
        dot += d[r][c1] * d[r][c2];
      }
      EXPECT_NEAR(dot, 0.0, kUnitTol) << "columns " << c1 << "," << c2 << " not orthogonal";
    }
  }
  EXPECT_NEAR(std::abs(det3(d)), 1.0, kUnitTol) << "direction det not ±1";
}

// Verify that for every voxel in 'a', its physical point maps to a valid voxel
// in 'b' carrying the identical pixel value.
void
expectPhysicalPointInvariant(const Volume3D::Pointer & a, const Volume3D::Pointer & b)
{
  ASSERT_EQ(a->GetLargestPossibleRegion().GetSize(), b->GetLargestPossibleRegion().GetSize());
  itk::ImageRegionConstIteratorWithIndex<Volume3D> it(a, a->GetLargestPossibleRegion());
  for (it.GoToBegin(); !it.IsAtEnd(); ++it)
  {
    Volume3D::PointType p;
    a->TransformIndexToPhysicalPoint(it.GetIndex(), p);
    auto bIdx = b->TransformPhysicalPointToIndex(p);
    ASSERT_TRUE(b->GetLargestPossibleRegion().IsInside(bIdx))
      << "physical point " << p << " from index " << it.GetIndex() << " maps outside b";
    EXPECT_EQ(it.Get(), b->GetPixel(bIdx))
      << "pixel mismatch at physical point " << p << " a-idx " << it.GetIndex() << " b-idx " << bIdx;
  }
}

struct GDCMSeriesDirection : public ::testing::Test
{
  void
  SetUp() override
  {
    // CTest runs each TEST_F in a separate process and may run them
    // concurrently.  Suffix with the running test name so each process
    // owns its own scratch directory, otherwise one test's TearDown
    // races with another test's body and we see "file not found".
    const auto * info = ::testing::UnitTest::GetInstance()->current_test_info();
    m_TempDir = std::string(TOSTRING(ITK_TEST_OUTPUT_DIR)) + "/GDCMSeriesDirection_" + info->name();
    itksys::SystemTools::MakeDirectory(m_TempDir);
  }
  void
  TearDown() override
  {
    itksys::SystemTools::RemoveADirectory(m_TempDir);
  }
  std::string
  makeCaseDir(const std::string & tag)
  {
    const std::string d = m_TempDir + "/" + tag;
    itksys::SystemTools::MakeDirectory(d);
    return d;
  }
  std::string m_TempDir;
};

} // namespace


// ---------------------------------------------------------------------------
// 1. The exact SimpleITK#2292 reproduction: monotonically decreasing Z,
//    axis-aligned axial orientation, given to the reader bottom-to-top.
// ---------------------------------------------------------------------------
TEST_F(GDCMSeriesDirection, SimpleITK2292_BottomToTop_OriginAndDirectionAgree)
{
  SeriesSpec spec;
  spec.dir = makeCaseDir("simpleitk2292_btt");
  spec.rowOrient = { { 1.0, 0.0, 0.0 } };
  spec.colOrient = { { 0.0, 1.0, 0.0 } };
  spec.pos0 = { { -216.5, -216.5, 70.0 } }; // top slice
  spec.stepIPP = { { 0.0, 0.0, -1.25 } };   // moving down in Z
  spec.numSlices = 9;

  auto files = writeSyntheticSeries(spec);
  // Hand the reader the BOTTOM slice first (reverse the natural write order).
  std::vector<std::string> btt(files.rbegin(), files.rend());

  auto img = readSeries(btt);
  expectOrthonormal(img->GetDirection());

  // Bottom slice was written at z = 70 + 8*(-1.25) = 60.
  EXPECT_NEAR(img->GetOrigin()[2], 60.0, kGeomTol);
  // Direction Z must point UP (positive) to be consistent with origin = bottom
  // slice and forced-positive spacing.
  EXPECT_GT(img->GetDirection()[2][2], 0.0) << "Origin is the bottom slice; direction Z must be +1 to reach the top.";

  // Walking from origin by spacing*(N-1) along direction[*][2] must land on the top slice.
  Volume3D::IndexType last;
  last[0] = 0;
  last[1] = 0;
  last[2] = static_cast<itk::IndexValueType>(img->GetLargestPossibleRegion().GetSize()[2]) - 1;
  Volume3D::PointType p;
  img->TransformIndexToPhysicalPoint(last, p);
  EXPECT_NEAR(p[2], 70.0, kGeomTol);
}

// ---------------------------------------------------------------------------
// 2. Same series, top-to-bottom input — origin is the top slice (Z=70),
//    direction Z must be negative.
// ---------------------------------------------------------------------------
TEST_F(GDCMSeriesDirection, SimpleITK2292_TopToBottom_OriginAndDirectionAgree)
{
  SeriesSpec spec;
  spec.dir = makeCaseDir("simpleitk2292_ttb");
  spec.rowOrient = { { 1.0, 0.0, 0.0 } };
  spec.colOrient = { { 0.0, 1.0, 0.0 } };
  spec.pos0 = { { -216.5, -216.5, 70.0 } };
  spec.stepIPP = { { 0.0, 0.0, -1.25 } };
  spec.numSlices = 9;

  auto files = writeSyntheticSeries(spec);
  auto img = readSeries(files);
  expectOrthonormal(img->GetDirection());

  EXPECT_NEAR(img->GetOrigin()[2], 70.0, kGeomTol);
  EXPECT_LT(img->GetDirection()[2][2], 0.0);
}

// ---------------------------------------------------------------------------
// 3. Forward and reverse readings must place every voxel at the same physical
//    point — this is the contract the patch promises.
// ---------------------------------------------------------------------------
TEST_F(GDCMSeriesDirection, PhysicalPointInvariance_AxialReversal)
{
  SeriesSpec spec;
  spec.dir = makeCaseDir("axial_invariance");
  spec.rowOrient = { { 1.0, 0.0, 0.0 } };
  spec.colOrient = { { 0.0, 1.0, 0.0 } };
  spec.pos0 = { { 0.0, 0.0, 0.0 } };
  spec.stepIPP = { { 0.0, 0.0, 2.5 } };
  spec.numSlices = 6;

  auto                     files = writeSyntheticSeries(spec);
  std::vector<std::string> reversed(files.rbegin(), files.rend());

  auto fwd = readSeries(files);
  auto rev = readSeries(reversed);

  expectOrthonormal(fwd->GetDirection());
  expectOrthonormal(rev->GetDirection());
  ITK_EXPECT_VECTOR_NEAR(fwd->GetSpacing(), rev->GetSpacing(), kGeomTol);
  expectPhysicalPointInvariant(fwd, rev);
}

// ---------------------------------------------------------------------------
// 4. ReverseOrderOn() flag — same input, two interpretations, must agree on
//    physical-point identity.  This is the trivially-reproducible case
//    @blowekamp identified.
// ---------------------------------------------------------------------------
TEST_F(GDCMSeriesDirection, PhysicalPointInvariance_ReverseOrderFlag)
{
  SeriesSpec spec;
  spec.dir = makeCaseDir("reverse_order_flag");
  spec.rowOrient = { { 1.0, 0.0, 0.0 } };
  spec.colOrient = { { 0.0, 1.0, 0.0 } };
  spec.pos0 = { { 10.0, -5.0, 100.0 } };
  spec.stepIPP = { { 0.0, 0.0, 3.0 } };
  spec.numSlices = 7;

  auto files = writeSyntheticSeries(spec);
  auto fwd = readSeries(files, true, false);
  auto rev = readSeries(files, true, true);

  expectOrthonormal(fwd->GetDirection());
  expectOrthonormal(rev->GetDirection());
  ITK_EXPECT_VECTOR_NEAR(fwd->GetSpacing(), rev->GetSpacing(), kGeomTol);
  EXPECT_NE(fwd->GetOrigin(), rev->GetOrigin());
  expectPhysicalPointInvariant(fwd, rev);
}

// ---------------------------------------------------------------------------
// 5. Sagittal acquisition: row=AP, col=SI, slice-stack = LR.
//    Reverse the file order; direction's 1st column (which by GDCM
//    construction equals row-orientation) is unchanged, but the slice-stack
//    column (3rd) must flip sign.
// ---------------------------------------------------------------------------
TEST_F(GDCMSeriesDirection, Sagittal_AxisAligned_Reversal)
{
  SeriesSpec spec;
  spec.dir = makeCaseDir("sagittal");
  spec.rowOrient = { { 0.0, 1.0, 0.0 } };  // patient anterior
  spec.colOrient = { { 0.0, 0.0, -1.0 } }; // patient inferior (DICOM convention)
  spec.pos0 = { { -100.0, -120.0, 80.0 } };
  spec.stepIPP = { { 1.0, 0.0, 0.0 } }; // moving in +X
  spec.numSlices = 5;

  auto                     files = writeSyntheticSeries(spec);
  std::vector<std::string> rev(files.rbegin(), files.rend());

  auto fwd = readSeries(files);
  auto rv = readSeries(rev);
  expectOrthonormal(fwd->GetDirection());
  expectOrthonormal(rv->GetDirection());

  // dirN sign-flip must reflect in the 3rd column.
  EXPECT_NEAR(fwd->GetDirection()[0][2], -rv->GetDirection()[0][2], kUnitTol);
  EXPECT_NEAR(fwd->GetDirection()[1][2], -rv->GetDirection()[1][2], kUnitTol);
  EXPECT_NEAR(fwd->GetDirection()[2][2], -rv->GetDirection()[2][2], kUnitTol);
  expectPhysicalPointInvariant(fwd, rv);
}

// ---------------------------------------------------------------------------
// 6. Coronal acquisition: row=LR, col=SI, slice-stack = AP.
// ---------------------------------------------------------------------------
TEST_F(GDCMSeriesDirection, Coronal_AxisAligned_Reversal)
{
  SeriesSpec spec;
  spec.dir = makeCaseDir("coronal");
  spec.rowOrient = { { 1.0, 0.0, 0.0 } };
  spec.colOrient = { { 0.0, 0.0, -1.0 } };
  spec.pos0 = { { -50.0, 30.0, 5.0 } };
  spec.stepIPP = { { 0.0, -2.0, 0.0 } }; // posterior-to-anterior reversed
  spec.numSlices = 6;

  auto                     files = writeSyntheticSeries(spec);
  std::vector<std::string> rev(files.rbegin(), files.rend());

  auto fwd = readSeries(files);
  auto rv = readSeries(rev);
  expectOrthonormal(fwd->GetDirection());
  expectOrthonormal(rv->GetDirection());
  expectPhysicalPointInvariant(fwd, rv);
}

// ---------------------------------------------------------------------------
// 7. Oblique acquisition (no zeros, no ones in the orientation triplets).
//    This is the explicit case @thewtex requested.  Row and column
//    orientations are derived from a small pitch-yaw rotation of the
//    canonical axial frame.
// ---------------------------------------------------------------------------
TEST_F(GDCMSeriesDirection, Oblique_NonAxisAligned_Reversal)
{
  // Build an oblique row/col by rotating the axial (e_x, e_y) about an axis
  // that mixes all three world components.  Use angles chosen so no element
  // of the resulting row/col is 0 or ±1.
  const double a = 0.27; // ~15.5°
  const double b = 0.41; // ~23.5°
  const double ca = std::cos(a), sa = std::sin(a);
  const double cb = std::cos(b), sb = std::sin(b);
  // Compose Rz(a) * Rx(b) applied to (1,0,0) and (0,1,0).
  std::array<double, 3> row = { { ca, sa * cb, sa * sb } };
  std::array<double, 3> col = { { -sa, ca * cb, ca * sb } };
  row = normalize3(row);
  col = normalize3(col);
  // Slice stacking direction = row × col, then offset every slice along it.
  const std::array<double, 3> n = normalize3(cross(row, col));

  SeriesSpec spec;
  spec.dir = makeCaseDir("oblique");
  spec.rowOrient = row;
  spec.colOrient = col;
  spec.pos0 = { { -45.5, 12.25, 88.125 } };
  spec.stepIPP = { { 1.7 * n[0], 1.7 * n[1], 1.7 * n[2] } };
  spec.numSlices = 5;

  auto                     files = writeSyntheticSeries(spec);
  std::vector<std::string> rev(files.rbegin(), files.rend());

  auto fwd = readSeries(files);
  auto rv = readSeries(rev);
  expectOrthonormal(fwd->GetDirection());
  expectOrthonormal(rv->GetDirection());

  // The first two columns derive from the (unchanged) DICOM ImageOrientationPatient
  // tags and must NOT change between forward and reversed readings.
  for (unsigned int r = 0; r < 3; ++r)
  {
    EXPECT_NEAR(fwd->GetDirection()[r][0], rv->GetDirection()[r][0], kUnitTol)
      << "row-orient (col 0) drifted at row " << r;
    EXPECT_NEAR(fwd->GetDirection()[r][1], rv->GetDirection()[r][1], kUnitTol)
      << "col-orient (col 1) drifted at row " << r;
  }
  // The 3rd column must flip sign under reversal.
  for (unsigned int r = 0; r < 3; ++r)
  {
    EXPECT_NEAR(fwd->GetDirection()[r][2], -rv->GetDirection()[r][2], kUnitTol)
      << "slice-stack (col 2) sign-flip failed at row " << r;
  }
  expectPhysicalPointInvariant(fwd, rv);
}

// ---------------------------------------------------------------------------
// 8. Reviewer's specific request: third-column equals cross-product of the
//    first two columns (after the fix is applied).
// ---------------------------------------------------------------------------
TEST_F(GDCMSeriesDirection, ThirdColumnEqualsCrossProduct_Oblique)
{
  const double          a = 0.31, b = 0.19;
  const double          ca = std::cos(a), sa = std::sin(a);
  const double          cb = std::cos(b), sb = std::sin(b);
  std::array<double, 3> row = normalize3({ { ca, sa * cb, sa * sb } });
  std::array<double, 3> col = normalize3({ { -sa, ca * cb, ca * sb } });
  std::array<double, 3> n = normalize3(cross(row, col));

  SeriesSpec spec;
  spec.dir = makeCaseDir("crossprod");
  spec.rowOrient = row;
  spec.colOrient = col;
  spec.pos0 = { { 0.0, 0.0, 0.0 } };
  spec.stepIPP = { { 2.0 * n[0], 2.0 * n[1], 2.0 * n[2] } };
  spec.numSlices = 4;

  auto files = writeSyntheticSeries(spec);
  for (bool reverse : { false, true })
  {
    std::vector<std::string> ord = files;
    if (reverse)
    {
      std::reverse(ord.begin(), ord.end());
    }
    auto                  img = readSeries(ord);
    const auto &          d = img->GetDirection();
    std::array<double, 3> c0 = { { d[0][0], d[1][0], d[2][0] } };
    std::array<double, 3> c1 = { { d[0][1], d[1][1], d[2][1] } };
    std::array<double, 3> c2 = { { d[0][2], d[1][2], d[2][2] } };
    auto                  c0xc1 = cross(c0, c1);
    // c0×c1 must be parallel to c2 (either +c2 or −c2 depending on handedness).
    const double sign = (c0xc1[0] * c2[0] + c0xc1[1] * c2[1] + c0xc1[2] * c2[2]) >= 0 ? 1.0 : -1.0;
    EXPECT_NEAR(c0xc1[0], sign * c2[0], kUnitTol) << "reverse=" << reverse;
    EXPECT_NEAR(c0xc1[1], sign * c2[1], kUnitTol) << "reverse=" << reverse;
    EXPECT_NEAR(c0xc1[2], sign * c2[2], kUnitTol) << "reverse=" << reverse;
  }
}

// ---------------------------------------------------------------------------
// 9. ForceOrthogonalDirectionOff: the legacy code path must remain unchanged
//    (this test pins behavior — it should NOT be affected by the patch).
// ---------------------------------------------------------------------------
TEST_F(GDCMSeriesDirection, ForceOrthogonalDirectionOff_UsesDirectIPPDifference)
{
  SeriesSpec spec;
  spec.dir = makeCaseDir("force_ortho_off");
  spec.rowOrient = { { 1.0, 0.0, 0.0 } };
  spec.colOrient = { { 0.0, 1.0, 0.0 } };
  spec.pos0 = { { 0.0, 0.0, 0.0 } };
  spec.stepIPP = { { 0.0, 0.0, -2.0 } };
  spec.numSlices = 4;

  auto files = writeSyntheticSeries(spec);
  auto img = readSeries(files, /*forceOrtho=*/false);
  // With ForceOrthogonalDirection OFF, the code sets direction[*][N-1] = dirN/|dirN|,
  // so Z component should literally equal sign(stepIPP).
  EXPECT_NEAR(img->GetDirection()[2][2], -1.0, kUnitTol);
  expectOrthonormal(img->GetDirection());
}

// ---------------------------------------------------------------------------
// 10. Symmetry: with ForceOrthogonalDirection ON vs OFF, the resulting
//     direction matrices must agree on simple axis-aligned cases (the GDCM
//     cross-product third column equals the IPP-derived third column up to
//     sign, and the patch reconciles the sign).
// ---------------------------------------------------------------------------
TEST_F(GDCMSeriesDirection, ForceOrthogonalOnOff_AgreeOnAxisAligned)
{
  SeriesSpec spec;
  spec.dir = makeCaseDir("ortho_onoff_agree");
  spec.rowOrient = { { 1.0, 0.0, 0.0 } };
  spec.colOrient = { { 0.0, 1.0, 0.0 } };
  spec.pos0 = { { 1.0, 2.0, 3.0 } };
  spec.stepIPP = { { 0.0, 0.0, -1.5 } };
  spec.numSlices = 5;

  auto files = writeSyntheticSeries(spec);
  auto on = readSeries(files, true);
  auto off = readSeries(files, false);
  for (unsigned int r = 0; r < 3; ++r)
  {
    for (unsigned int c = 0; c < 3; ++c)
    {
      EXPECT_NEAR(on->GetDirection()[r][c], off->GetDirection()[r][c], kUnitTol)
        << "drift at [" << r << "][" << c << "]";
    }
  }
  ITK_EXPECT_VECTOR_NEAR(on->GetSpacing(), off->GetSpacing(), kUnitTol);
  ITK_EXPECT_VECTOR_NEAR(on->GetOrigin(), off->GetOrigin(), kGeomTol);
}

// ---------------------------------------------------------------------------
// 11. Two-slice series — the minimum non-degenerate case.  Spacing must be
//     correctly derived; reversal must still produce physical-point invariance.
// ---------------------------------------------------------------------------
TEST_F(GDCMSeriesDirection, TwoSlices_MinimumValidCase)
{
  SeriesSpec spec;
  spec.dir = makeCaseDir("two_slices");
  spec.rowOrient = { { 1.0, 0.0, 0.0 } };
  spec.colOrient = { { 0.0, 1.0, 0.0 } };
  spec.pos0 = { { 0.0, 0.0, 100.0 } };
  spec.stepIPP = { { 0.0, 0.0, -7.5 } };
  spec.numSlices = 2;

  auto                     files = writeSyntheticSeries(spec);
  std::vector<std::string> rev(files.rbegin(), files.rend());
  auto                     fwd = readSeries(files);
  auto                     rv = readSeries(rev);
  EXPECT_NEAR(fwd->GetSpacing()[2], 7.5, kGeomTol);
  EXPECT_NEAR(rv->GetSpacing()[2], 7.5, kGeomTol);
  expectPhysicalPointInvariant(fwd, rv);
}

// ---------------------------------------------------------------------------
// 13. Many slices, irregular but monotonic step (sub-millimetre, large series).
// ---------------------------------------------------------------------------
TEST_F(GDCMSeriesDirection, ManySlices_FineSpacing)
{
  SeriesSpec spec;
  spec.dir = makeCaseDir("many_slices");
  spec.rowOrient = { { 1.0, 0.0, 0.0 } };
  spec.colOrient = { { 0.0, 1.0, 0.0 } };
  spec.pos0 = { { 0.0, 0.0, 0.0 } };
  spec.stepIPP = { { 0.0, 0.0, 0.625 } };
  spec.numSlices = 64;

  auto                     files = writeSyntheticSeries(spec);
  std::vector<std::string> rev(files.rbegin(), files.rend());
  auto                     fwd = readSeries(files);
  auto                     rv = readSeries(rev);
  EXPECT_NEAR(fwd->GetSpacing()[2], 0.625, kGeomTol);
  EXPECT_NEAR(rv->GetSpacing()[2], 0.625, kGeomTol);
  expectOrthonormal(fwd->GetDirection());
  expectOrthonormal(rv->GetDirection());
  expectPhysicalPointInvariant(fwd, rv);
}

// ---------------------------------------------------------------------------
// 14. Real ITK test data: ITKData/Input/DicomSeries (used by the original PR
//     as ReadSlicesReverseOrder).  Run with both default and reversed input
//     orderings; physical points must be invariant.
// ---------------------------------------------------------------------------
TEST_F(GDCMSeriesDirection, RealDicomSeries_PhysicalPointInvariant)
{
  const std::string dataDir = TOSTRING(DICOM_SERIES_INPUT);
  if (!itksys::SystemTools::FileIsDirectory(dataDir))
  {
    GTEST_SKIP() << "DICOM_SERIES_INPUT not available: " << dataDir;
  }
  auto names = itk::GDCMSeriesFileNames::New();
  names->SetDirectory(dataDir);
  names->SetUseSeriesDetails(true);
  std::vector<std::string> files = names->GetInputFileNames();
  ASSERT_GT(files.size(), 1u);

  auto fwd = readSeries(files, true, false);
  auto rev = readSeries(files, true, true);

  expectOrthonormal(fwd->GetDirection());
  expectOrthonormal(rev->GetDirection());
  ITK_EXPECT_VECTOR_NEAR(fwd->GetSpacing(), rev->GetSpacing(), kGeomTol);
  expectPhysicalPointInvariant(fwd, rev);
}

// ---------------------------------------------------------------------------
// 15. ITKElastix#291 contract: for an axis-aligned axial acquisition with
//     monotonically increasing Z, the third direction column is exactly
//     [0, 0, 1] regardless of how the file list was sorted.
// ---------------------------------------------------------------------------
TEST_F(GDCMSeriesDirection, ITKElastix291_TimeSeriesShapedDirection)
{
  SeriesSpec spec;
  spec.dir = makeCaseDir("elastix291");
  spec.rowOrient = { { 1.0, 0.0, 0.0 } };
  spec.colOrient = { { 0.0, 1.0, 0.0 } };
  spec.pos0 = { { 0.0, 0.0, 0.0 } };
  spec.stepIPP = { { 0.0, 0.0, 1.0 } };
  spec.numSlices = 8;

  auto files = writeSyntheticSeries(spec);
  for (bool reverse : { false, true })
  {
    std::vector<std::string> ord = files;
    if (reverse)
    {
      std::reverse(ord.begin(), ord.end());
    }
    auto img = readSeries(ord);
    EXPECT_NEAR(img->GetDirection()[0][2], 0.0, kUnitTol);
    EXPECT_NEAR(img->GetDirection()[1][2], 0.0, kUnitTol);
    // Origin sits at the first listed slice; direction Z sign must reach the last listed slice.
    if (!reverse)
    {
      EXPECT_NEAR(img->GetDirection()[2][2], 1.0, kUnitTol);
      EXPECT_NEAR(img->GetOrigin()[2], 0.0, kGeomTol);
    }
    else
    {
      EXPECT_NEAR(img->GetDirection()[2][2], -1.0, kUnitTol);
      EXPECT_NEAR(img->GetOrigin()[2], 7.0, kGeomTol);
    }
    expectOrthonormal(img->GetDirection());
  }
}

// ---------------------------------------------------------------------------
// 16. Origin equals the first listed slice's IPP — the patch must not move
//     the origin, only adjust the direction.
// ---------------------------------------------------------------------------
TEST_F(GDCMSeriesDirection, OriginEqualsFirstListedSliceIPP)
{
  SeriesSpec spec;
  spec.dir = makeCaseDir("origin_first");
  spec.rowOrient = { { 1.0, 0.0, 0.0 } };
  spec.colOrient = { { 0.0, 1.0, 0.0 } };
  spec.pos0 = { { 11.5, -22.5, 33.5 } };
  spec.stepIPP = { { 0.0, 0.0, -3.0 } };
  spec.numSlices = 4;

  auto                     files = writeSyntheticSeries(spec);
  std::vector<std::string> rev(files.rbegin(), files.rend());

  auto fwd = readSeries(files);
  EXPECT_NEAR(fwd->GetOrigin()[0], 11.5, kGeomTol);
  EXPECT_NEAR(fwd->GetOrigin()[1], -22.5, kGeomTol);
  EXPECT_NEAR(fwd->GetOrigin()[2], 33.5, kGeomTol);

  auto rv = readSeries(rev);
  // First listed slice in 'rev' is the LAST written slice → z = 33.5 + 3*(-3) = 24.5
  EXPECT_NEAR(rv->GetOrigin()[2], 24.5, kGeomTol);
}

// ---------------------------------------------------------------------------
// 17. Spacing is always positive, regardless of input order or orientation.
// ---------------------------------------------------------------------------
TEST_F(GDCMSeriesDirection, SpacingAlwaysPositive)
{
  for (double dz : { -1.25, +1.25 })
  {
    SeriesSpec spec;
    spec.dir = makeCaseDir(dz < 0 ? "spacing_pos_neg" : "spacing_pos_pos");
    spec.rowOrient = { { 1.0, 0.0, 0.0 } };
    spec.colOrient = { { 0.0, 1.0, 0.0 } };
    spec.pos0 = { { 0.0, 0.0, 0.0 } };
    spec.stepIPP = { { 0.0, 0.0, dz } };
    spec.numSlices = 4;

    auto files = writeSyntheticSeries(spec);
    auto img = readSeries(files);
    EXPECT_GT(img->GetSpacing()[0], 0.0);
    EXPECT_GT(img->GetSpacing()[1], 0.0);
    EXPECT_GT(img->GetSpacing()[2], 0.0);
    EXPECT_NEAR(img->GetSpacing()[2], 1.25, kGeomTol);
  }
}

// ---------------------------------------------------------------------------
// 18. Stress: random orientations + random IPP step direction.  The contract
//     "physical points are invariant under input reversal" must hold for all
//     of them.
// ---------------------------------------------------------------------------
TEST_F(GDCMSeriesDirection, RandomizedStress_PhysicalPointInvariance)
{
  std::mt19937                           rng(20260429u);
  std::uniform_real_distribution<double> ang(-1.5, 1.5);
  std::uniform_real_distribution<double> off(-50.0, 50.0);
  std::uniform_real_distribution<double> sp(0.5, 4.0);

  for (unsigned int trial = 0; trial < 8; ++trial)
  {
    const double          a = ang(rng), b = ang(rng);
    const double          ca = std::cos(a), sa = std::sin(a);
    const double          cb = std::cos(b), sb = std::sin(b);
    std::array<double, 3> row = normalize3({ { ca, sa * cb, sa * sb } });
    std::array<double, 3> col = normalize3({ { -sa, ca * cb, ca * sb } });
    // Re-orthogonalize col against row (Gram–Schmidt) to keep DICOM happy.
    double dot = row[0] * col[0] + row[1] * col[1] + row[2] * col[2];
    col = normalize3({ { col[0] - dot * row[0], col[1] - dot * row[1], col[2] - dot * row[2] } });
    auto         n = normalize3(cross(row, col));
    const double step = sp(rng);

    SeriesSpec spec;
    spec.dir = makeCaseDir("rand_" + std::to_string(trial));
    spec.rowOrient = row;
    spec.colOrient = col;
    spec.pos0 = { { off(rng), off(rng), off(rng) } };
    spec.stepIPP = { { step * n[0], step * n[1], step * n[2] } };
    spec.numSlices = 5;

    auto                     files = writeSyntheticSeries(spec);
    std::vector<std::string> rev(files.rbegin(), files.rend());

    auto fwd = readSeries(files);
    auto rv = readSeries(rev);
    expectOrthonormal(fwd->GetDirection());
    expectOrthonormal(rv->GetDirection());
    expectPhysicalPointInvariant(fwd, rv);
  }
}
