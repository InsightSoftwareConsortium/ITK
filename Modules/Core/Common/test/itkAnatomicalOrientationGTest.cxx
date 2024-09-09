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

#include <gtest/gtest.h>

#include "itkGTest.h"
#ifndef ITK_FUTURE_LEGACY_REMOVE
#  define ITK_LEGACY_SILENT
#endif
#include "itkAnatomicalOrientation.h"
#include "itkImage.h"
#include <sstream>


TEST(AnatomicalOrientation, ConstructionAndValues)
{
  using itk::AnatomicalOrientation;
  using OE = AnatomicalOrientation::ToEnum;
  using CE = AnatomicalOrientation::CoordinateEnum;
  using ImageType = itk::Image<float, 3>;

  ImageType::DirectionType d;

  AnatomicalOrientation do1(OE::LPS);

  EXPECT_EQ("LPS", do1.GetAsToStringEncoding());
  EXPECT_EQ(CE::RightToLeft, do1.GetPrimaryTerm());
  EXPECT_EQ(CE::AnteriorToPosterior, do1.GetSecondaryTerm());
  EXPECT_EQ(CE::InferiorToSuperior, do1.GetTertiaryTerm());
  EXPECT_EQ(OE::LPS, do1.GetAsOrientation());

  d.SetIdentity();
  EXPECT_EQ(d, do1.GetAsDirection());


  do1 = AnatomicalOrientation::CreateFromToStringEncoding("RAS");

  EXPECT_EQ("RAS", do1.GetAsToStringEncoding());
  EXPECT_EQ(CE::LeftToRight, do1.GetPrimaryTerm());
  EXPECT_EQ(CE::PosteriorToAnterior, do1.GetSecondaryTerm());
  EXPECT_EQ(CE::InferiorToSuperior, do1.GetTertiaryTerm());
  EXPECT_EQ(OE::RAS, do1.GetAsOrientation());

  d.Fill(0.0);
  d(0, 0) = -1.0;
  d(1, 1) = -1.0;
  d(2, 2) = 1.0;
  EXPECT_EQ(d, do1.GetAsDirection());


  do1 = AnatomicalOrientation::CreateFromToStringEncoding("rai");

  EXPECT_EQ("RAI", do1.GetAsToStringEncoding());
  EXPECT_EQ(CE::LeftToRight, do1.GetPrimaryTerm());
  EXPECT_EQ(CE::PosteriorToAnterior, do1.GetSecondaryTerm());
  EXPECT_EQ(CE::SuperiorToInferior, do1.GetTertiaryTerm());
  EXPECT_EQ(OE::RAI, do1.GetAsOrientation());


  do1 = AnatomicalOrientation(OE::PIR);

  EXPECT_EQ("PIR", do1.GetAsToStringEncoding());
  EXPECT_EQ(CE::AnteriorToPosterior, do1.GetPrimaryTerm());
  EXPECT_EQ(CE::SuperiorToInferior, do1.GetSecondaryTerm());
  EXPECT_EQ(CE::LeftToRight, do1.GetTertiaryTerm());
  EXPECT_EQ(OE::PIR, do1.GetAsOrientation());

  d.Fill(0.0);
  d(1, 0) = 1.0;
  d(2, 1) = -1.0;
  d(0, 2) = -1.0;
  EXPECT_EQ(d, do1.GetAsDirection());

  AnatomicalOrientation do2(d);

  EXPECT_EQ("PIR", do2.GetAsToStringEncoding());
  EXPECT_EQ(CE::AnteriorToPosterior, do2.GetPrimaryTerm());
  EXPECT_EQ(CE::SuperiorToInferior, do2.GetSecondaryTerm());
  EXPECT_EQ(CE::LeftToRight, do2.GetTertiaryTerm());
  EXPECT_EQ(OE::PIR, do2.GetAsOrientation());

  EXPECT_EQ(d, do2.GetAsDirection());

  AnatomicalOrientation do3 = AnatomicalOrientation::CreateFromToStringEncoding("something invalid");
  EXPECT_EQ("INVALID", do3.GetAsToStringEncoding());
  EXPECT_EQ(CE::UNKNOWN, do3.GetPrimaryTerm());
  EXPECT_EQ(CE::UNKNOWN, do3.GetSecondaryTerm());
  EXPECT_EQ(CE::UNKNOWN, do3.GetTertiaryTerm());
  EXPECT_EQ(OE::INVALID, do3.GetAsOrientation());
}


TEST(AnatomicalOrientation, DirectionCosinesToOrientation)
{
  using itk::AnatomicalOrientation;
  using OE = AnatomicalOrientation::ToEnum;
  using ImageType = itk::Image<float, 3>;
  ImageType::DirectionType d;
  d.SetIdentity();

  EXPECT_EQ(OE::LPS, AnatomicalOrientation::DirectionCosinesToOrientation(d));

  d.Fill(0.0);
  d(0, 0) = -1.0;
  d(1, 1) = -1.0;
  d(2, 2) = -1.0;
  EXPECT_EQ(OE::RAI, AnatomicalOrientation::DirectionCosinesToOrientation(d));

  d.Fill(0.0);
  d(2, 0) = 1;
  d(0, 1) = 1;
  d(1, 2) = 1;
  EXPECT_EQ(OE::SLP, AnatomicalOrientation::DirectionCosinesToOrientation(d));

  d.Fill(0.0);
  d(1, 0) = 1;
  d(2, 1) = 1;
  d(0, 2) = 1;
  EXPECT_EQ(OE::PSL, AnatomicalOrientation::DirectionCosinesToOrientation(d));

  d.Fill(0.0);
  d(0, 0) = 1;
  d(2, 1) = 1;
  d(1, 2) = 1;
  EXPECT_EQ(OE::LSP, AnatomicalOrientation::DirectionCosinesToOrientation(d));

  d.Fill(0.0);
  d(1, 0) = 1;
  d(0, 1) = 1;
  d(2, 2) = 1;
  EXPECT_EQ(OE::PLS, AnatomicalOrientation::DirectionCosinesToOrientation(d));

  d.Fill(0.0);
  d(2, 0) = 1;
  d(1, 1) = 1;
  d(0, 2) = 1;
  EXPECT_EQ(OE::SPL, AnatomicalOrientation::DirectionCosinesToOrientation(d));

  const double                                 data[] = { 0.5986634407395047, 0.22716302314740483, -0.768113953548866,
                          0.5627936241740271, 0.563067040943212,   0.6051601804419384,
                          0.5699696670095713, -0.794576911518317,  0.20924175102261847 };
  ImageType::DirectionType::InternalMatrixType m{ data };
  d.GetVnlMatrix() = m;
  EXPECT_EQ(OE::PIR, AnatomicalOrientation::DirectionCosinesToOrientation(d));
}

TEST(AnatomicalOrientation, OrientationToDirectionCosines)
{
  using itk::AnatomicalOrientation;
  using ImageType = itk::Image<float, 3>;
  using OE = AnatomicalOrientation::ToEnum;

  ImageType::DirectionType d;
  d.SetIdentity();

  EXPECT_EQ(d, AnatomicalOrientation::OrientationToDirectionCosines(OE::LPS));

  d.Fill(0.0);
  d(0, 0) = -1.0;
  d(1, 1) = -1.0;
  d(2, 2) = -1.0;
  EXPECT_EQ(d, AnatomicalOrientation::OrientationToDirectionCosines(OE::RAI));

  d.Fill(0.0);
  d(2, 0) = 1;
  d(0, 1) = 1;
  d(1, 2) = 1;
  EXPECT_EQ(d, AnatomicalOrientation::OrientationToDirectionCosines(OE::SLP));

  d.Fill(0.0);
  d(1, 0) = 1;
  d(2, 1) = 1;
  d(0, 2) = 1;
  EXPECT_EQ(d, AnatomicalOrientation::OrientationToDirectionCosines(OE::PSL));

  d.Fill(0.0);
  d(0, 0) = 1;
  d(2, 1) = 1;
  d(1, 2) = 1;
  EXPECT_EQ(d, AnatomicalOrientation::OrientationToDirectionCosines(OE::LSP));

  d.Fill(0.0);
  d(1, 0) = 1;
  d(0, 1) = 1;
  d(2, 2) = 1;
  EXPECT_EQ(d, AnatomicalOrientation::OrientationToDirectionCosines(OE::PLS));

  d.Fill(0.0);
  d(2, 0) = 1;
  d(1, 1) = 1;
  d(0, 2) = 1;
  EXPECT_EQ(d, AnatomicalOrientation::OrientationToDirectionCosines(OE::SPL));
}

TEST(AntomicalOrientation, ToFromEnumInteroperability)
{

  using OE = itk::AnatomicalOrientation::ToEnum;
  using FromOE = itk::AnatomicalOrientation::FromEnum;
  using CE = itk::AnatomicalOrientation::CoordinateEnum;

  static_assert(int(OE::RAI) == int(FromOE::LPS));
  static_assert(int(OE::LPS) == int(FromOE::RAI));
  static_assert(int(OE::RAS) == int(FromOE::LPI));
  static_assert(int(OE::LPI) == int(FromOE::RAS));
  static_assert(int(OE::PIR) == int(FromOE::ASL));
  static_assert(int(OE::ASL) == int(FromOE::PIR));

  itk::AnatomicalOrientation itk_rai(FromOE::RAI);

  EXPECT_EQ(itk_rai, itk::AnatomicalOrientation(OE::LPS));
  EXPECT_EQ(itk_rai.GetAsOrientation(), OE::LPS);
  EXPECT_EQ(itk_rai.GetAsToStringEncoding(), "LPS");
  const std::array<CE, 3> expected_terms = { { CE::RightToLeft, CE::AnteriorToPosterior, CE::InferiorToSuperior } };
  EXPECT_EQ(itk_rai.GetTerms(), expected_terms);
}

#ifndef ITK_FUTURE_LEGACY_REMOVE
#  include "itkSpatialOrientation.h"
TEST(AnatomicalOrientation, LegacyInteroperability)
{

  using OE = itk::AnatomicalOrientation::ToEnum;
  using SOE = itk::SpatialOrientationEnums::ValidCoordinateOrientations;

  // byte for byte compatibility, may assist with migration of bindings when types are not strictly enforced.
  static_assert(int(SOE::ITK_COORDINATE_ORIENTATION_RAI) == int(OE::LPS));
  static_assert(int(SOE::ITK_COORDINATE_ORIENTATION_LPS) == int(OE::RAI));
  static_assert(int(SOE::ITK_COORDINATE_ORIENTATION_RSA) == int(OE::LIP));
  static_assert(int(SOE::ITK_COORDINATE_ORIENTATION_ASL) == int(OE::PIR));

  itk::AnatomicalOrientation itk_rai(SOE::ITK_COORDINATE_ORIENTATION_RAI);
  EXPECT_EQ(itk_rai, OE::LPS);
  EXPECT_EQ(itk_rai.GetAsOrientation(), OE::LPS);
  EXPECT_EQ(itk_rai.GetAsToStringEncoding(), "LPS");
}
#endif
