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
#include "itkTransformFileWriter.h"
#include "itkTransformFileReader.h"
#include "itkAffineTransform.h"
#include "itkTransformFactory.h"
#include "itksys/SystemTools.hxx"
#include "itkDisplacementFieldTransform.h"
#include "itkIOTestHelper.h"
#include "itkMINCTransformAdapter.h"
#include "itkMINCTransformIO.h"
#include "itkMath.h"
#include "itkTestingMacros.h"


static constexpr double tolerance = 1e-5;
static constexpr int    point_counter = 1000;


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
compare_linear(const char * linear_transform)
{
  itk::ObjectFactoryBase::RegisterFactory(itk::MINCTransformIOFactory::New());

  using AffineTransformType = itk::AffineTransform<double, 3>;
  auto affine = AffineTransformType::New();

  AffineTransformType::OutputVectorType rot_axis;
  rot_axis[0] = 1.0;
  rot_axis[1] = 1.0;
  rot_axis[2] = 0.0;
  // Set its parameters
  affine->Rotate3D(rot_axis, itk::Math::pi / 12);

  AffineTransformType::OutputVectorType offset;

  offset[0] = 0.0;
  offset[1] = 0.0;
  offset[2] = 10.0;

  affine->Translate(offset);

  affine->Scale(1.2);

  itk::TransformFileWriter::Pointer writer;
  itk::MINCTransformIO::Pointer     mincIO = itk::MINCTransformIO::New();
  // MINC standard is always LPS
  mincIO->SetRAStoLPS(false);

  writer = itk::TransformFileWriter::New();
  writer->SetTransformIO(mincIO);
  writer->AddTransform(affine);
  writer->SetFileName(linear_transform);

  try
  {
    writer->Update();

    std::cout << "Comparing of linear transforms ITK vs MINC: " << affine << std::endl;
    itk::MINCTransformAdapter<double, 3, 3>::Pointer xfm = itk::MINCTransformAdapter<double, 3, 3>::New();

    xfm->OpenXfm(linear_transform);

    vnl_random randgen(12345678);

    AffineTransformType::InputPointType pnt, pnt2;

    for (int i = 0; i < point_counter; ++i)
    {
      AffineTransformType::OutputPointType v1;
      AffineTransformType::OutputPointType v2;

      RandomPoint<double>(randgen, pnt, 100);
      pnt2 = pnt;
      v1 = affine->TransformPoint(pnt);
      v2 = xfm->TransformPoint(pnt2);

      if ((v1 - v2).GetSquaredNorm() > tolerance)
      {
        std::cout << "Original Coordinate (" << v1 << ") doesn't match read-in Coordinate (" << v2 << " ) "
                  << " in " << linear_transform << " at " << pnt << std::endl;
        return EXIT_FAILURE;
      }
    }
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << "Error while reading the transforms" << std::endl;
    std::cerr << excp << std::endl;
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}

static int
compare_nonlinear_double(const char * nonlinear_transform)
{

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

  vnl_random                                               randgen(12345678);
  itk::ImageRegionIteratorWithIndex<DisplacementFieldType> it(field, field->GetLargestPossibleRegion());

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

  itk::TransformFileWriter::Pointer nlwriter;
  itk::MINCTransformIO::Pointer     mincIO = itk::MINCTransformIO::New();
  // MINC standard is always LPS
  mincIO->SetRAStoLPS(false);

  nlwriter = itk::TransformFileWriter::New();
  nlwriter->SetTransformIO(mincIO);
  nlwriter->AddTransform(disp);
  nlwriter->SetFileName(nonlinear_transform);

  try
  {
    nlwriter->Update();
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << "Error while saving the transforms" << std::endl;
    std::cerr << excp << std::endl;
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
  }

  std::cout << "Comparing of non linear transforms ITK vs MINC " << std::endl;
  itk::MINCTransformAdapter<double, 3, 3>::Pointer xfm = itk::MINCTransformAdapter<double, 3, 3>::New();
  xfm->OpenXfm(nonlinear_transform);

  for (it.GoToBegin(); !it.IsAtEnd(); ++it)
  {
    DisplacementFieldTransform::OutputPointType v1;
    DisplacementFieldTransform::OutputPointType v2;
    DisplacementFieldType::PointType            pnt;

    field->TransformIndexToPhysicalPoint(it.GetIndex(), pnt);

    v1 = disp->TransformPoint(pnt);
    v2 = xfm->TransformPoint(pnt);

    if ((v1 - v2).GetSquaredNorm() > tolerance)
    {
      std::cout << "Original Pixel (" << v1 << ") doesn't match read-in Pixel (" << v2 << " ) "
                << " in " << nonlinear_transform << std::endl;
      return EXIT_FAILURE;
    }
  }


  return EXIT_SUCCESS;
}

int
itkMINCTransformAdapterTest(int argc, char * argv[])
{
  if (argc > 1)
  {
    itksys::SystemTools::ChangeDirectory(argv[1]);
  }


  constexpr unsigned int InputDimension = 3;
  constexpr unsigned int OutputDimension = 3;

  using ParametersValueType = double;

  using TransformAdapterType = itk::MINCTransformAdapter<ParametersValueType, InputDimension, OutputDimension>;

  TransformAdapterType::Pointer xfm = TransformAdapterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(xfm, MINCTransformAdapter, Transform);

  // Test exceptions
  TransformAdapterType::InputVectorType vector{};
  TransformAdapterType::InputPointType  point{};
  ITK_TRY_EXPECT_EXCEPTION(xfm->TransformVector(vector, point));

  TransformAdapterType::InputVnlVectorType vnlVector{};
  ITK_TRY_EXPECT_EXCEPTION(xfm->TransformVector(vnlVector, point));

  TransformAdapterType::InputVectorPixelType vectorPixel{};
  ITK_TRY_EXPECT_EXCEPTION(xfm->TransformVector(vectorPixel, point));

  TransformAdapterType::InputCovariantVectorType covariantVector{};
  ITK_TRY_EXPECT_EXCEPTION(xfm->TransformCovariantVector(covariantVector, point));

  ITK_TRY_EXPECT_EXCEPTION(xfm->TransformCovariantVector(vectorPixel, point));

  TransformAdapterType::FixedParametersType fixedParams{};
  ITK_TRY_EXPECT_EXCEPTION(xfm->SetFixedParameters(fixedParams));

  TransformAdapterType::JacobianType jacobian{};
  ITK_TRY_EXPECT_EXCEPTION(xfm->ComputeJacobianWithRespectToParameters(point, jacobian));

  ITK_TRY_EXPECT_EXCEPTION(xfm->GetNumberOfParameters());

  TransformAdapterType::ParametersType params{};
  ITK_TRY_EXPECT_EXCEPTION(xfm->SetParameters(params));

  ITK_TRY_EXPECT_EXCEPTION(xfm->GetParameters());

  constexpr unsigned int InputDimensionExcp1 = 1;
  constexpr unsigned int OutputDimensionExcp1 = 1;

  using TransformAdapterTypeExcp11 =
    itk::MINCTransformAdapter<ParametersValueType, InputDimensionExcp1, OutputDimensionExcp1>;

  ITK_TRY_EXPECT_EXCEPTION(TransformAdapterTypeExcp11::New());

  constexpr unsigned int InputDimensionExcp2 = 2;
  constexpr unsigned int OutputDimensionExcp2 = 2;

  using TransformAdapterTypeExcp22 =
    itk::MINCTransformAdapter<ParametersValueType, InputDimensionExcp2, OutputDimensionExcp2>;

  ITK_TRY_EXPECT_EXCEPTION(TransformAdapterTypeExcp22::New());

  constexpr unsigned int InputDimensionExcp4 = 4;
  constexpr unsigned int OutputDimensionExcp4 = 4;

  using TransformAdapterTypeExcp44 =
    itk::MINCTransformAdapter<ParametersValueType, InputDimensionExcp4, OutputDimensionExcp4>;

  ITK_TRY_EXPECT_EXCEPTION(TransformAdapterTypeExcp44::New());


  itk::TransformFactory<itk::DisplacementFieldTransform<double, 3>>::RegisterTransform();
  itk::ObjectFactoryBase::RegisterFactory(itk::MINCTransformIOFactory::New());

  int result1 = compare_linear("itkMINCTransformAdapterTestTransformLinear.xfm");
  int result2 = compare_nonlinear_double("itkMINCTransformAdapterTestTransformNonLinear.xfm");

  return !(result1 == EXIT_SUCCESS && result2 == EXIT_SUCCESS);
}
