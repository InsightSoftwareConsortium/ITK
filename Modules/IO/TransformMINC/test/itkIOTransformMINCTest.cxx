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

#include <iostream>
#include <fstream>
#include "itkMINCTransformIOFactory.h"
#include "itkMINCImageIOFactory.h"
#include "itkMINCTransformIO.h"
#include "itkMINCImageIO.h"
#include "itkTransformFileWriter.h"
#include "itkTransformFileReader.h"
#include "itkAffineTransform.h"
#include "itkTransformFactory.h"
#include "itksys/SystemTools.hxx"
#include "itkDisplacementFieldTransform.h"
#include "itkCompositeTransform.h"
#include "itkIOTestHelper.h"
#include "itkMath.h"
#include "itkTestingMacros.h"

static constexpr int point_counter = 1000;
using TransformFileReader = itk::TransformFileReaderTemplate<double>;
using TransformFileWriter = itk::TransformFileWriterTemplate<double>;
using MINCTransformIO = itk::MINCTransformIO;

template <typename T>
void
RandomPix(vnl_random & randgen, itk::Vector<T, 3> & pix, double _max = itk::NumericTraits<T>::max())
{
  for (unsigned int i = 0; i < 3; ++i)
  {
    pix[i] = randgen.drand64(_max);
  }
}

template <typename T>
void
RandomPoint(vnl_random & randgen, itk::Point<T, 3> & pix, double _max = itk::NumericTraits<T>::max())
{
  for (unsigned int i = 0; i < 3; ++i)
  {
    pix[i] = randgen.drand64(_max);
  }
}


static int
check_linear(const char * linear_transform, bool ras_to_lps)
{
  std::cout << "check_linear, ras_to_lps=" << ras_to_lps << std::endl << std::endl;

  int testStatus = EXIT_SUCCESS;

  using AffineTransformType = itk::AffineTransform<double, 3>;
  constexpr double tolerance = 1e-5;

  auto affine = AffineTransformType::New();

  itk::ObjectFactoryBase::RegisterFactory(itk::MINCTransformIOFactory::New());

  // Set its parameters
  AffineTransformType::OutputVectorType rot_axis;
  rot_axis[0] = 0.0;
  rot_axis[1] = 1.0;
  rot_axis[2] = 0.0;
  // Set its parameters
  affine->Rotate3D(rot_axis, itk::Math::pi / 6);

  AffineTransformType::OutputVectorType offset;

  offset[0] = 0.0;
  offset[1] = 0.0;
  offset[2] = 10.0;

  affine->Translate(offset);

  affine->Scale(4.0);

  TransformFileWriter::Pointer writer;
  TransformFileReader::Pointer reader;
  MINCTransformIO::Pointer     mincIO = MINCTransformIO::New();
  mincIO->SetRAStoLPS(ras_to_lps);

  reader = TransformFileReader::New();
  writer = TransformFileWriter::New();
  writer->SetTransformIO(mincIO);
  reader->SetTransformIO(mincIO);

  writer->AddTransform(affine);

  writer->SetFileName(linear_transform);
  reader->SetFileName(linear_transform);

  // affine->Print(std::cout);

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());

  std::cout << "Testing read : " << std::endl;

  ITK_TRY_EXPECT_NO_EXCEPTION(reader->Update());

  TransformFileReader::TransformListType list = *reader->GetTransformList();
  ITK_TEST_EXPECT_EQUAL("AffineTransform_double_3_3", list.front()->GetTransformTypeAsString());

  AffineTransformType::Pointer affine2 = static_cast<AffineTransformType *>(list.front().GetPointer());

  std::cout << "Read transform : " << std::endl;
  // affine2->Print(std::cout);

  vnl_random randgen(12345678);

  AffineTransformType::InputPointType pnt, pnt2;

  std::cout << "Testing that transformations are the same ..." << std::endl;
  for (int i = 0; i < point_counter; ++i)
  {
    AffineTransformType::OutputPointType v1;
    AffineTransformType::OutputPointType v2;

    RandomPoint<double>(randgen, pnt, 100);
    pnt2 = pnt;
    v1 = affine->TransformPoint(pnt);
    v2 = affine2->TransformPoint(pnt2);

    double expectedDiff = 0.0;
    if (!itk::Math::FloatAlmostEqual(expectedDiff, (v1 - v2).GetSquaredNorm(), 10, tolerance))
    {
      std::cerr << "Test failed!"
                << "  In " __FILE__ ", line " << __LINE__ << std::endl;
      std::cerr << "Error in pixel value at point ( " << pnt << ")" << std::endl;
      std::cerr << "Expected value " << v1;
      std::cerr << " differs from " << v2;
      std::cerr << " by more than " << tolerance << std::endl;
      testStatus = EXIT_FAILURE;
    }
  }
  std::cout << " Done !" << std::endl;

  return testStatus;
}

static int
check_nonlinear_double(const char * nonlinear_transform, bool ras_to_lps)
{
  std::cout << "check_nonlinear_double, ras_to_lps=" << ras_to_lps << std::endl << std::endl;
  int testStatus = EXIT_SUCCESS;

  constexpr double tolerance = 1e-5;

  using DisplacementFieldTransform = itk::DisplacementFieldTransform<double, 3>;
  using DisplacementFieldType = DisplacementFieldTransform::DisplacementFieldType;

  auto disp = DisplacementFieldTransform::New();
  auto field = DisplacementFieldType::New();

  // create zero displacement field
  DisplacementFieldType::SizeType  imageSize3D = { { 10, 10, 10 } };
  DisplacementFieldType::IndexType startIndex3D = { { 0, 0, 0 } };

  double                            spacing[] = { 2.0, 2.0, 2.0 };
  double                            origin[] = { -10.0, -10.0, -10.0 };
  DisplacementFieldType::RegionType region;

  region.SetSize(imageSize3D);
  region.SetIndex(startIndex3D);

  field->SetRegions(region);

  field->SetSpacing(spacing);
  field->SetOrigin(origin);
  field->Allocate();

  DisplacementFieldType::PixelType zeroDisplacement{};
  field->FillBuffer(zeroDisplacement);

  vnl_random                                      randgen(12345678);
  itk::ImageRegionIterator<DisplacementFieldType> it(field, field->GetLargestPossibleRegion());

  for (it.GoToBegin(); !it.IsAtEnd(); ++it)
  {
    DisplacementFieldType::PixelType pix;
    if (tolerance > 0.0)
    {
      RandomPix<double>(randgen, pix, 100);
    }
    else
    {
      RandomPix<double>(randgen, pix);
    }
    it.Set(pix);
  }

  disp->SetDisplacementField(field);

  // disp->Print(std::cout);

  TransformFileWriter::Pointer nlwriter;
  TransformFileReader::Pointer nlreader;
  MINCTransformIO::Pointer     mincIO = MINCTransformIO::New();
  mincIO->SetRAStoLPS(ras_to_lps);

  nlreader = TransformFileReader::New();
  nlwriter = TransformFileWriter::New();
  nlreader->SetTransformIO(mincIO);
  nlwriter->SetTransformIO(mincIO);

  nlwriter->AddTransform(disp);
  nlwriter->SetFileName(nonlinear_transform);
  nlreader->SetFileName(nonlinear_transform);

  // Testing writing
  std::cout << "Testing write of non linear transform (double) : " << std::endl;

  ITK_TRY_EXPECT_NO_EXCEPTION(nlwriter->Update());


  // Testing writing
  std::cout << "Testing read of non linear transform (double): " << std::endl;

  ITK_TRY_EXPECT_NO_EXCEPTION(nlreader->Update());

  std::cout << "[PASSED]" << std::endl;

  std::cout << "Comparing of non linear transform (double) : " << std::endl;

  TransformFileReader::TransformListType list = *nlreader->GetTransformList();
  std::cout << "Read :" << list.size() << " transformations" << std::endl;

  ITK_TEST_EXPECT_EQUAL("DisplacementFieldTransform_double_3_3", list.front()->GetTransformTypeAsString());

  DisplacementFieldTransform::Pointer disp2 = static_cast<DisplacementFieldTransform *>(list.front().GetPointer());
  DisplacementFieldType::ConstPointer field2 = disp2->GetDisplacementField();

  itk::ImageRegionConstIterator<DisplacementFieldType> it2(field2, field2->GetLargestPossibleRegion());
  if (tolerance == 0.0)
  {
    for (it.GoToBegin(), it2.GoToBegin(); !it.IsAtEnd() && !it2.IsAtEnd(); ++it, ++it2)
    {
      if (it.Value() != it2.Value())
      {
        std::cerr << "Test failed!"
                  << "  In " __FILE__ ", line " << __LINE__ << std::endl;
        std::cerr << "Error in pixel value" << std::endl;
        std::cerr << "Expected value " << it.Value();
        std::cerr << " differs from " << it2.Value() << std::endl;
        testStatus = EXIT_FAILURE;
      }
    }
  }
  else
  { // account for rounding errors
    for (it.GoToBegin(), it2.GoToBegin(); !it.IsAtEnd() && !it2.IsAtEnd(); ++it, ++it2)
    {
      double expectedDiff = 0.0;
      if (!itk::Math::FloatAlmostEqual(expectedDiff, (it.Value() - it2.Value()).GetSquaredNorm(), 10, tolerance))
      {
        std::cerr << "Test failed!"
                  << "  In " __FILE__ ", line " << __LINE__ << std::endl;
        std::cerr << "Error in pixel value" << std::endl;
        std::cerr << "Expected value " << it.Value();
        std::cerr << " differs from " << it2.Value();
        std::cerr << " by more than " << tolerance << std::endl;
        testStatus = EXIT_FAILURE;
      }
    }
  }

  return testStatus;
}


static int
check_nonlinear_float(const char * nonlinear_transform, bool ras_to_lps)
{

  std::cout << "check_nonlinear_float, ras_to_lps=" << ras_to_lps << std::endl << std::endl;

  int testStatus = EXIT_SUCCESS;

  double tolerance = 1e-5;

  using TransformFileWriterFloat = itk::TransformFileWriterTemplate<float>;
  using TransformFileReaderFloat = itk::TransformFileReaderTemplate<float>;
  using MINCTransformIOFloat = itk::MINCTransformIOTemplate<float>;

  using DisplacementFieldTransform = itk::DisplacementFieldTransform<float, 3>;
  using DisplacementFieldType = DisplacementFieldTransform::DisplacementFieldType;

  auto disp = DisplacementFieldTransform::New();
  auto field = DisplacementFieldType::New();

  // create zero displacement field
  DisplacementFieldType::SizeType  imageSize3D = { { 10, 10, 10 } };
  DisplacementFieldType::IndexType startIndex3D = { { 0, 0, 0 } };

  double                            spacing[] = { 2.0, 2.0, 2.0 };
  double                            origin[] = { -10.0, -10.0, -10.0 };
  DisplacementFieldType::RegionType region;

  region.SetSize(imageSize3D);
  region.SetIndex(startIndex3D);

  field->SetRegions(region);

  field->SetSpacing(spacing);
  field->SetOrigin(origin);
  field->Allocate();

  DisplacementFieldType::PixelType zeroDisplacement{};
  field->FillBuffer(zeroDisplacement);

  vnl_random                                      randgen(12345678);
  itk::ImageRegionIterator<DisplacementFieldType> it(field, field->GetLargestPossibleRegion());

  for (it.GoToBegin(); !it.IsAtEnd(); ++it)
  {
    DisplacementFieldType::PixelType pix;
    if (tolerance > 0.0)
    {
      RandomPix<float>(randgen, pix, 100);
    }
    else
    {
      RandomPix<float>(randgen, pix);
    }
    it.Set(pix);
  }

  disp->SetDisplacementField(field);

  // disp->Print(std::cout);

  TransformFileWriterFloat::Pointer nlwriter;
  TransformFileReaderFloat::Pointer nlreader;

  nlreader = TransformFileReaderFloat::New();
  nlwriter = TransformFileWriterFloat::New();

  MINCTransformIOFloat::Pointer mincIO = MINCTransformIOFloat::New();
  mincIO->SetRAStoLPS(ras_to_lps);
  nlreader->SetTransformIO(mincIO);
  nlwriter->SetTransformIO(mincIO);

  nlwriter->AddTransform(disp);
  nlwriter->SetFileName(nonlinear_transform);
  nlreader->SetFileName(nonlinear_transform);

  // Testing writing
  std::cout << "Testing write of non linear transform (float): " << std::endl;

  ITK_TRY_EXPECT_NO_EXCEPTION(nlwriter->Update());


  // Testing writing
  std::cout << "Testing read of non linear transform (float) : " << std::endl;

  ITK_TRY_EXPECT_NO_EXCEPTION(nlreader->Update());


  std::cout << "[PASSED]" << std::endl;

  std::cout << "Comparing of non linear transform : " << std::endl;

  TransformFileReaderFloat::TransformListType list = *nlreader->GetTransformList();
  std::cout << "Read :" << list.size() << " transformations" << std::endl;

  ITK_TEST_EXPECT_EQUAL("DisplacementFieldTransform_float_3_3", list.front()->GetTransformTypeAsString());

  DisplacementFieldTransform::Pointer disp2 = static_cast<DisplacementFieldTransform *>(list.front().GetPointer());
  DisplacementFieldType::ConstPointer field2 = disp2->GetDisplacementField();

  itk::ImageRegionConstIterator<DisplacementFieldType> it2(field2, field2->GetLargestPossibleRegion());
  if (tolerance == 0.0)
  {
    for (it.GoToBegin(), it2.GoToBegin(); !it.IsAtEnd() && !it2.IsAtEnd(); ++it, ++it2)
    {
      if (it.Value() != it2.Value())
      {
        std::cerr << "Test failed!"
                  << "  In " __FILE__ ", line " << __LINE__ << std::endl;
        std::cerr << "Error in pixel value" << std::endl;
        std::cerr << "Expected value " << it.Value();
        std::cerr << " differs from " << it2.Value() << std::endl;
        testStatus = EXIT_FAILURE;
      }
    }
  }
  else
  { // account for rounding errors
    for (it.GoToBegin(), it2.GoToBegin(); !it.IsAtEnd() && !it2.IsAtEnd(); ++it, ++it2)
    {
      double expectedDiff = 0.0;
      if (!itk::Math::FloatAlmostEqual(expectedDiff, (it.Value() - it2.Value()).GetSquaredNorm(), 10, tolerance))
      {
        std::cerr << "Test failed!"
                  << "  In " __FILE__ ", line " << __LINE__ << std::endl;
        std::cerr << "Error in pixel value" << std::endl;
        std::cerr << "Expected value " << it.Value();
        std::cerr << " differs from " << it2.Value();
        std::cerr << " by more than " << tolerance << std::endl;
        testStatus = EXIT_FAILURE;
      }
    }
  }

  return testStatus;
}

static int
secondTest(bool ras_to_lps)
{
  std::cout << "secondTest, ras_to_lps=" << ras_to_lps << std::endl << std::endl;

  // rotation of 30 degrees around X axis
  std::filebuf fb;
  fb.open("Rotation.xfm", std::ios::out);
  std::ostream os(&fb);
  os << "MNI Transform File" << std::endl;
  os << "Transform_Type = Linear;" << std::endl;
  os << "Linear_Transform =" << std::endl;
  os << "1 0 0 0" << std::endl;
  os << "0 0.866025447845459 -0.5 0" << std::endl;
  os << "0 0.5 0.866025447845459 0;" << std::endl;
  fb.close();

  TransformFileReader::Pointer reader;
  reader = TransformFileReader::New();
  MINCTransformIO::Pointer mincIO = MINCTransformIO::New();
  mincIO->SetRAStoLPS(ras_to_lps);
  reader->SetTransformIO(mincIO);
  reader->SetFileName("Rotation.xfm");
  reader->Update();

  const TransformFileReader::TransformListType * list = reader->GetTransformList();

  ITK_TEST_EXPECT_EQUAL(1, list->size());

  return EXIT_SUCCESS;
}


static int
check_composite(const char * transform_file, bool ras_to_lps)
{
  std::cout << "check_composite, ras_to_lps=" << ras_to_lps << std::endl << std::endl;

  int testStatus = EXIT_SUCCESS;

  using AffineTransformType = itk::AffineTransform<double, 3>;
  using CompositeTransformType = itk::CompositeTransform<double, 3>;

  constexpr double tolerance = 1e-5;

  auto affine1 = AffineTransformType::New();
  auto affine2 = AffineTransformType::New();
  auto compositeTransform = CompositeTransformType::New();

  itk::ObjectFactoryBase::RegisterFactory(itk::MINCTransformIOFactory::New());

  // Set its parameters
  AffineTransformType::OutputVectorType rot_axis;
  rot_axis[0] = 0.0;
  rot_axis[1] = 1.0;
  rot_axis[2] = 0.0;
  // Set its parameters
  affine1->Rotate3D(rot_axis, itk::Math::pi / 6);

  AffineTransformType::OutputVectorType offset;

  offset[0] = 0.0;
  offset[1] = 0.0;
  offset[2] = 10.0;

  affine2->Translate(offset);

  compositeTransform->AddTransform(affine1);
  compositeTransform->AddTransform(affine2);


  TransformFileWriter::Pointer writer;
  TransformFileReader::Pointer reader;

  MINCTransformIO::Pointer mincIO = MINCTransformIO::New();
  mincIO->SetRAStoLPS(ras_to_lps);

  reader = TransformFileReader::New();
  writer = TransformFileWriter::New();
  reader->SetTransformIO(mincIO);
  writer->SetTransformIO(mincIO);

  writer->AddTransform(compositeTransform);

  writer->SetFileName(transform_file);
  reader->SetFileName(transform_file);

  // compositeTransform->Print(std::cout);

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());

  std::cout << "Testing read : " << std::endl;

  ITK_TRY_EXPECT_NO_EXCEPTION(reader->Update());

  TransformFileReader::TransformListType list = *reader->GetTransformList();

  // MINC XFM internally collapeses two concatenated linear transforms into one
  ITK_TEST_EXPECT_EQUAL("AffineTransform_double_3_3", list.front()->GetTransformTypeAsString());

  AffineTransformType::Pointer affine_xfm = static_cast<AffineTransformType *>(list.front().GetPointer());

  std::cout << "Read transform : " << std::endl;
  // affine_xfm->(std::cout);

  vnl_random randgen(12345678);

  AffineTransformType::InputPointType pnt, pnt2;

  std::cout << "Testing that transformations are the same ..." << std::endl;
  for (int i = 0; i < point_counter; ++i)
  {
    AffineTransformType::OutputPointType v1;
    AffineTransformType::OutputPointType v2;

    RandomPoint<double>(randgen, pnt, 100);
    pnt2 = pnt;
    v1 = compositeTransform->TransformPoint(pnt);
    v2 = affine_xfm->TransformPoint(pnt2);

    double expectedDiff = 0.0;
    if (!itk::Math::FloatAlmostEqual(expectedDiff, (v1 - v2).GetSquaredNorm(), 10, tolerance))
    {
      std::cerr << "Test failed!"
                << "  In " __FILE__ ", line " << __LINE__ << std::endl;
      std::cerr << "Error in pixel value at point ( " << pnt << ")" << std::endl;
      std::cerr << "Expected value " << v1;
      std::cerr << " differs from " << v2;
      std::cerr << " by more than " << tolerance << std::endl;
      testStatus = EXIT_FAILURE;
    }
  }
  std::cout << " Done !" << std::endl;

  return testStatus;
}

static int
check_composite2(const char * transform_file, const char * transform_grid_file, bool ras_to_lps)
{
  std::cout << "check_composite2, ras_to_lps=" << ras_to_lps << std::endl << std::endl;

  int testStatus = EXIT_SUCCESS;

  constexpr double tolerance = 1e-5;

  std::filebuf fb;
  fb.open(transform_file, std::ios::out);
  std::ostream os(&fb);
  std::cout << "Testing reading of composite transform, ras_to_lps=" << ras_to_lps << std::endl
            << std::endl
            << std::endl
            << std::endl;

  // create concatenation of two transforms:
  // nonlinear grid with shift by 1
  // rotation by 45 deg
  os << "MNI Transform File" << std::endl;
  os << std::endl;
  os << "Transform_Type = Grid_Transform;" << std::endl;
  os << "Displacement_Volume = " << transform_grid_file << ';' << std::endl;
  os << "Transform_Type = Linear;" << std::endl;

  os << "Linear_Transform =" << std::endl;
  os << " 0.70710676908493 -0.70710676908493 0 0" << std::endl;
  os << " 0.70710676908493 0.70710676908493 0 0" << std::endl;
  os << " 0 0 1 0;" << std::endl;

  fb.close();

  // generate field
  using DisplacementFieldTransform = itk::DisplacementFieldTransform<double, 3>;
  using DisplacementFieldType = DisplacementFieldTransform::DisplacementFieldType;

  auto field = DisplacementFieldType::New();

  // create displacement field in MINC space (always)
  DisplacementFieldType::SizeType  imageSize3D = { { 10, 10, 10 } };
  DisplacementFieldType::IndexType startIndex3D = { { 0, 0, 0 } };

  double                            spacing[] = { 2.0, 2.0, 2.0 };
  double                            origin[] = { -10.0, -10.0, -10.0 };
  DisplacementFieldType::RegionType region;

  region.SetSize(imageSize3D);
  region.SetIndex(startIndex3D);

  field->SetRegions(region);

  field->SetSpacing(spacing);
  field->SetOrigin(origin);
  field->Allocate();

  DisplacementFieldType::PixelType displacement{};
  displacement[0] = 1.0;
  field->FillBuffer(displacement);

  using MincWriterType = itk::ImageFileWriter<DisplacementFieldType>;

  auto writer = MincWriterType::New();
  auto mincImageIO = itk::MINCImageIO::New();
  mincImageIO->SetRAStoLPS(false);
  writer->SetImageIO(mincImageIO);


  // expecting .mnc here
  writer->SetFileName(transform_grid_file);
  writer->SetInput(field);
  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  // NOW, read back with RAS to LPS conversion

  TransformFileReader::Pointer reader;
  using CompositeTransformType = itk::CompositeTransform<double, 3>;

  reader = TransformFileReader::New();

  MINCTransformIO::Pointer mincIO = MINCTransformIO::New();
  mincIO->SetRAStoLPS(ras_to_lps);
  reader->SetTransformIO(mincIO);
  reader->SetFileName(transform_file);

  ITK_TRY_EXPECT_NO_EXCEPTION(reader->Update());

  const TransformFileReader::TransformListType * list = reader->GetTransformList();
  ITK_TEST_EXPECT_EQUAL(2, list->size());

  std::cout << "Reading back transforms : " << list->size() << std::endl;

  using CompositeTransformType = itk::CompositeTransform<double, 3>;
  using TransformType = itk::Transform<double, 3>;

  auto _xfm = CompositeTransformType::New();
  for (const auto & it : *list)
  {
    _xfm->AddTransform(static_cast<TransformType *>(it.GetPointer()));
  }
  std::cout << std::endl;
  std::cout << std::endl;
  std::cout << std::endl;

  std::cout << "Testing that transformations is expected ..." << std::endl;

  CompositeTransformType::InputPointType  pnt;
  CompositeTransformType::OutputPointType v, v2;

  if (ras_to_lps)
  {
    pnt[0] = -1.0;
  }
  else
  {
    pnt[0] = 1.0;
  }
  pnt[1] = pnt[2] = 0.0;
  // expected transform: shift by 1 , rotate by 45 deg
  if (ras_to_lps)
  {
    v2[0] = v2[1] = -sqrt(2);
  }
  else
  {
    v2[0] = v2[1] = sqrt(2);
  }
  v2[2] = 0.0;

  v = _xfm->TransformPoint(pnt);
  std::cout << "In:" << pnt << " Out:" << v << std::endl;

  double expectedDiff = 0.0;
  if (!itk::Math::FloatAlmostEqual(expectedDiff, (v - v2).GetSquaredNorm(), 10, tolerance))
  {
    std::cerr << "Test failed!"
              << "  In " __FILE__ ", line " << __LINE__ << std::endl;
    std::cerr << "Error in pixel value at point ( " << pnt << ")" << std::endl;
    std::cerr << "Expected value " << v2;
    std::cerr << " differs from " << v;
    std::cerr << " by more than " << tolerance << std::endl;
    testStatus = EXIT_FAILURE;
  }

  std::cout << " Done !" << std::endl;

  return testStatus;
}


int
itkIOTransformMINCTest(int argc, char * argv[])
{
  itk::ObjectFactoryBase::RegisterFactory(itk::MINCImageIOFactory::New(),
                                          itk::ObjectFactoryBase::InsertionPositionEnum::INSERT_AT_FRONT);
  if (argc > 1)
  {
    itksys::SystemTools::ChangeDirectory(argv[1]);
  }
  itk::TransformFactory<itk::DisplacementFieldTransform<double, 3>>::RegisterTransform();
  itk::TransformFactory<itk::DisplacementFieldTransform<float, 3>>::RegisterTransform();

  bool result = true;
  for (bool ras_to_lps : { false, true })
  {
    std::cout << "RAS to LPS=" << ras_to_lps << std::endl << std::endl << std::endl;

    bool result1 = check_linear("itkIOTransformMINCTestTransformLinear.xfm", ras_to_lps) == EXIT_SUCCESS;
    bool result2 = check_nonlinear_double("itkIOTransformMINCTestTransformNonLinear.xfm", ras_to_lps) == EXIT_SUCCESS;
    bool result3 =
      check_nonlinear_float("itkIOTransformMINCTestTransformNonLinear_float.xfm", ras_to_lps) == EXIT_SUCCESS;
    bool result4 = secondTest(ras_to_lps) == EXIT_SUCCESS;
    bool result5 = check_composite("itkIOTransformMINCTestTransformComposite.xfm", ras_to_lps) == EXIT_SUCCESS;
    bool result6 = check_composite2("itkIOTransformMINCTestTransformComposite2.xfm",
                                    "itkIOTransformMINCTestTransformComposite2_grid_0.mnc",
                                    ras_to_lps) == EXIT_SUCCESS;

    result = result && result1 && result2 && result3 && result4 && result5 && result6;
  }

  return result ? EXIT_SUCCESS : EXIT_FAILURE;
}
