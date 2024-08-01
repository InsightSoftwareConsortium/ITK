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
#include "itkDICOMOrientation.h"
#include "itkImage.h"
#include <sstream>


TEST(DICOMOrientation, ConstructionAndValues)
{
  using itk::DICOMOrientation;
  using OE = DICOMOrientation::OrientationEnum;
  using CE = DICOMOrientation::CoordinateEnum;
  using ImageType = itk::Image<float, 3>;

  ImageType::DirectionType d;

  DICOMOrientation do1(OE::LPS);

  EXPECT_EQ("LPS", do1.GetAsString());
  EXPECT_EQ(CE::Left, do1.GetPrimaryTerm());
  EXPECT_EQ(CE::Posterior, do1.GetSecondaryTerm());
  EXPECT_EQ(CE::Superior, do1.GetTertiaryTerm());
  EXPECT_EQ(OE::LPS, do1.GetAsOrientation());

  d.SetIdentity();
  EXPECT_EQ(d, do1.GetAsDirection());


  do1 = DICOMOrientation("RAS");

  EXPECT_EQ("RAS", do1.GetAsString());
  EXPECT_EQ(CE::Right, do1.GetPrimaryTerm());
  EXPECT_EQ(CE::Anterior, do1.GetSecondaryTerm());
  EXPECT_EQ(CE::Superior, do1.GetTertiaryTerm());
  EXPECT_EQ(OE::RAS, do1.GetAsOrientation());

  d.Fill(0.0);
  d(0, 0) = -1.0;
  d(1, 1) = -1.0;
  d(2, 2) = 1.0;
  EXPECT_EQ(d, do1.GetAsDirection());


  do1 = DICOMOrientation("rai");

  EXPECT_EQ("RAI", do1.GetAsString());
  EXPECT_EQ(CE::Right, do1.GetPrimaryTerm());
  EXPECT_EQ(CE::Anterior, do1.GetSecondaryTerm());
  EXPECT_EQ(CE::Inferior, do1.GetTertiaryTerm());
  EXPECT_EQ(OE::RAI, do1.GetAsOrientation());


  do1 = DICOMOrientation(OE::PIR);

  EXPECT_EQ("PIR", do1.GetAsString());
  EXPECT_EQ(CE::Posterior, do1.GetPrimaryTerm());
  EXPECT_EQ(CE::Inferior, do1.GetSecondaryTerm());
  EXPECT_EQ(CE::Right, do1.GetTertiaryTerm());
  EXPECT_EQ(OE::PIR, do1.GetAsOrientation());

  d.Fill(0.0);
  d(1, 0) = 1.0;
  d(2, 1) = -1.0;
  d(0, 2) = -1.0;
  EXPECT_EQ(d, do1.GetAsDirection());

  DICOMOrientation do2(d);

  EXPECT_EQ("PIR", do2.GetAsString());
  EXPECT_EQ(CE::Posterior, do2.GetPrimaryTerm());
  EXPECT_EQ(CE::Inferior, do2.GetSecondaryTerm());
  EXPECT_EQ(CE::Right, do2.GetTertiaryTerm());
  EXPECT_EQ(OE::PIR, do2.GetAsOrientation());

  EXPECT_EQ(d, do2.GetAsDirection());

  DICOMOrientation do3("something invalid");
  EXPECT_EQ("INVALID", do3.GetAsString());
  EXPECT_EQ(CE::UNKNOWN, do3.GetPrimaryTerm());
  EXPECT_EQ(CE::UNKNOWN, do3.GetSecondaryTerm());
  EXPECT_EQ(CE::UNKNOWN, do3.GetTertiaryTerm());
  EXPECT_EQ(OE::INVALID, do3.GetAsOrientation());
}


TEST(DICOMOrientation, DirectionCosinesToOrientation)
{
  using itk::DICOMOrientation;
  using OE = DICOMOrientation::OrientationEnum;
  using ImageType = itk::Image<float, 3>;
  ImageType::DirectionType d;
  d.SetIdentity();

  EXPECT_EQ(OE::LPS, DICOMOrientation::DirectionCosinesToOrientation(d));

  d.Fill(0.0);
  d(0, 0) = -1.0;
  d(1, 1) = -1.0;
  d(2, 2) = -1.0;
  EXPECT_EQ(OE::RAI, DICOMOrientation::DirectionCosinesToOrientation(d));

  d.Fill(0.0);
  d(2, 0) = 1;
  d(0, 1) = 1;
  d(1, 2) = 1;
  EXPECT_EQ(OE::SLP, DICOMOrientation::DirectionCosinesToOrientation(d));

  d.Fill(0.0);
  d(1, 0) = 1;
  d(2, 1) = 1;
  d(0, 2) = 1;
  EXPECT_EQ(OE::PSL, DICOMOrientation::DirectionCosinesToOrientation(d));

  d.Fill(0.0);
  d(0, 0) = 1;
  d(2, 1) = 1;
  d(1, 2) = 1;
  EXPECT_EQ(OE::LSP, DICOMOrientation::DirectionCosinesToOrientation(d));

  d.Fill(0.0);
  d(1, 0) = 1;
  d(0, 1) = 1;
  d(2, 2) = 1;
  EXPECT_EQ(OE::PLS, DICOMOrientation::DirectionCosinesToOrientation(d));

  d.Fill(0.0);
  d(2, 0) = 1;
  d(1, 1) = 1;
  d(0, 2) = 1;
  EXPECT_EQ(OE::SPL, DICOMOrientation::DirectionCosinesToOrientation(d));

  const double data[] = {0.5986634407395047, 0.22716302314740483, -0.768113953548866,
                         0.5627936241740271, 0.563067040943212, 0.6051601804419384,
                         0.5699696670095713, -0.794576911518317, 0.20924175102261847};
  ImageType::DirectionType::InternalMatrixType m{data};
  d.GetVnlMatrix() = m;
  EXPECT_EQ(OE::PIR, DICOMOrientation::DirectionCosinesToOrientation(d));

}

TEST(DICOMOrientation, OrientationToDirectionCosines)
{
  using itk::DICOMOrientation;
  using ImageType = itk::Image<float, 3>;
  using OE = DICOMOrientation::OrientationEnum;

  ImageType::DirectionType d;
  d.SetIdentity();

  EXPECT_EQ(d, DICOMOrientation::OrientationToDirectionCosines(OE::LPS));

  d.Fill(0.0);
  d(0, 0) = -1.0;
  d(1, 1) = -1.0;
  d(2, 2) = -1.0;
  EXPECT_EQ(d, DICOMOrientation::OrientationToDirectionCosines(OE::RAI));

  d.Fill(0.0);
  d(2, 0) = 1;
  d(0, 1) = 1;
  d(1, 2) = 1;
  EXPECT_EQ(d, DICOMOrientation::OrientationToDirectionCosines(OE::SLP));

  d.Fill(0.0);
  d(1, 0) = 1;
  d(2, 1) = 1;
  d(0, 2) = 1;
  EXPECT_EQ(d, DICOMOrientation::OrientationToDirectionCosines(OE::PSL));

  d.Fill(0.0);
  d(0, 0) = 1;
  d(2, 1) = 1;
  d(1, 2) = 1;
  EXPECT_EQ(d, DICOMOrientation::OrientationToDirectionCosines(OE::LSP));

  d.Fill(0.0);
  d(1, 0) = 1;
  d(0, 1) = 1;
  d(2, 2) = 1;
  EXPECT_EQ(d, DICOMOrientation::OrientationToDirectionCosines(OE::PLS));

  d.Fill(0.0);
  d(2, 0) = 1;
  d(1, 1) = 1;
  d(0, 2) = 1;
  EXPECT_EQ(d, DICOMOrientation::OrientationToDirectionCosines(OE::SPL));
}

#ifndef ITK_FUTURE_LEGACY_REMOVE
#include "itkSpatialOrientation.h"
TEST(DICOMOrientation, LegacyInteroperability)
{

  using OE = itk::DICOMOrientation::OrientationEnum;
  using SOE = itk::SpatialOrientationEnums::ValidCoordinateOrientations;

  // byte for byte compatibility, may assist with migration of bindings when types are not strictly enforced.
  static_assert( int(SOE::ITK_COORDINATE_ORIENTATION_RAI) == int(OE::LPS) );
  static_assert( int(SOE::ITK_COORDINATE_ORIENTATION_LPS) == int(OE::RAI) );
  static_assert( int(SOE::ITK_COORDINATE_ORIENTATION_RSA) == int(OE::LIP) );
  static_assert( int(SOE::ITK_COORDINATE_ORIENTATION_ASL) == int(OE::PIR) );

  itk::DICOMOrientation itk_rai(SOE::ITK_COORDINATE_ORIENTATION_RAI);
  EXPECT_EQ( itk_rai, OE::LPS);
  EXPECT_EQ( itk_rai.GetAsOrientation(), OE::LPS);
  EXPECT_EQ( itk_rai.GetAsString(), "LPS");

}
#endif
