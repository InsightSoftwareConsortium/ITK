/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
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
#include "itkTransformFileWriter.h"
#include "itkTransformFileReader.h"
#include "itkAffineTransform.h"
#include "itkTransformFactory.h"
#include "itksys/SystemTools.hxx"
#include "itkDisplacementFieldTransform.h"
#include "itkCompositeTransform.h"
#include "itkIOTestHelper.h"
#include "itkMath.h"

static constexpr int point_counter = 1000;
using TransformFileReader = itk::TransformFileReaderTemplate<double>;
using TransformFileWriter = itk::TransformFileWriterTemplate<double>;

template <typename T>
void
RandomPix(vnl_random & randgen, itk::Vector<T, 3> & pix, double _max = itk::NumericTraits<T>::max())
{
  for (unsigned int i = 0; i < 3; i++)
  {
    pix[i] = randgen.drand64(_max);
  }
}

template <typename T>
void
RandomPoint(vnl_random & randgen, itk::Point<T, 3> & pix, double _max = itk::NumericTraits<T>::max())
{
  for (unsigned int i = 0; i < 3; i++)
  {
    pix[i] = randgen.drand64(_max);
  }
}


static int
check_linear(const char * linear_transform)
{
  using AffineTransformType = itk::AffineTransform<double, 3>;
  const double tolerance = 1e-5;

  AffineTransformType::Pointer affine = AffineTransformType::New();

  itk::ObjectFactoryBase::RegisterFactory(itk::MINCTransformIOFactory::New());

  // Set it's parameters
  AffineTransformType::OutputVectorType rot_axis;
  rot_axis[0] = 0.0;
  rot_axis[1] = 1.0;
  rot_axis[2] = 0.0;
  // Set it's parameters
  affine->Rotate3D(rot_axis, itk::Math::pi / 6);

  AffineTransformType::OutputVectorType offset;

  offset[0] = 0.0;
  offset[1] = 0.0;
  offset[2] = 10.0;

  affine->Translate(offset);

  affine->Scale(4.0);

  TransformFileWriter::Pointer writer;
  TransformFileReader::Pointer reader;

  reader = TransformFileReader::New();
  writer = TransformFileWriter::New();
  writer->AddTransform(affine);

  writer->SetFileName(linear_transform);
  reader->SetFileName(linear_transform);

  // Testing writing std::cout << "Testing write : ";
  affine->Print(std::cout);
  try
  {
    writer->Update();
    std::cout << std::endl;
    std::cout << "Testing read : " << std::endl;
    reader->Update();
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << "Error while saving the transforms" << std::endl;
    std::cerr << excp << std::endl;
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
  }

  try
  {
    TransformFileReader::TransformListType list = *reader->GetTransformList();

    if (list.front()->GetTransformTypeAsString() != "AffineTransform_double_3_3")
    {
      std::cerr << "Read back transform of type:" << list.front()->GetTransformTypeAsString() << std::endl;
      return EXIT_FAILURE;
    }
    AffineTransformType::Pointer affine2 = static_cast<AffineTransformType *>(list.front().GetPointer());

    std::cout << "Read transform : " << std::endl;
    affine2->Print(std::cout);

    vnl_random randgen(12345678);

    AffineTransformType::InputPointType pnt, pnt2;

    std::cout << "Testing that transformations are the same ..." << std::endl;
    for (int i = 0; i < point_counter; i++)
    {
      AffineTransformType::OutputPointType v1;
      AffineTransformType::OutputPointType v2;

      RandomPoint<double>(randgen, pnt, 100);
      pnt2 = pnt;
      v1 = affine->TransformPoint(pnt);
      v2 = affine2->TransformPoint(pnt2);

      if ((v1 - v2).GetSquaredNorm() > tolerance)
      {
        std::cerr << "Original Pixel (" << v1 << ") doesn't match read-in Pixel (" << v2 << " ) "
                  << " in " << linear_transform << " at " << pnt << std::endl;
        return EXIT_FAILURE;
      }
    }
    std::cout << " Done !" << std::endl;
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
check_nonlinear_double(const char * nonlinear_transform)
{
  const double tolerance = 1e-5;

  using DisplacementFieldTransform = itk::DisplacementFieldTransform<double, 3>;
  using DisplacementFieldType = DisplacementFieldTransform::DisplacementFieldType;

  DisplacementFieldTransform::Pointer disp = DisplacementFieldTransform::New();
  DisplacementFieldType::Pointer      field = DisplacementFieldType::New();

  // create zero displacement field
  DisplacementFieldType::SizeType  imageSize3D = { { 10, 10, 10 } };
  DisplacementFieldType::IndexType startIndex3D = { { 0, 0, 0 } };

  double                            spacing[] = { 2.0, 2.0, 2.0 };
  double                            origin[] = { -10.0, -10.0, -10.0 };
  DisplacementFieldType::RegionType region;

  region.SetSize(imageSize3D);
  region.SetIndex(startIndex3D);

  field->SetLargestPossibleRegion(region);
  field->SetBufferedRegion(region);
  field->SetRequestedRegion(region);

  field->SetSpacing(spacing);
  field->SetOrigin(origin);
  field->Allocate();

  DisplacementFieldType::PixelType zeroDisplacement;
  zeroDisplacement.Fill(0.0);
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

  disp->Print(std::cout);

  TransformFileWriter::Pointer nlwriter;
  TransformFileReader::Pointer nlreader;

  nlreader = TransformFileReader::New();
  nlwriter = TransformFileWriter::New();
  nlwriter->AddTransform(disp);
  nlwriter->SetFileName(nonlinear_transform);
  nlreader->SetFileName(nonlinear_transform);

  // Testing writing
  std::cout << "Testing write of non linear transform (double) : " << std::endl;

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

  // Testing writing
  std::cout << "Testing read of non linear transform (double): " << std::endl;
  try
  {
    nlreader->Update();
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << "Error while reading the transforms" << std::endl;
    std::cerr << excp << std::endl;
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED]" << std::endl;

  std::cout << "Comparing of non linear transform (double) : " << std::endl;
  TransformFileReader::TransformListType list = *nlreader->GetTransformList();
  std::cout << "Read :" << list.size() << " transformations" << std::endl;

  if (list.front()->GetTransformTypeAsString() != "DisplacementFieldTransform_double_3_3")
  {
    std::cerr << "Read back transform of type:" << list.front()->GetTransformTypeAsString() << std::endl;
    return EXIT_FAILURE;
  }
  DisplacementFieldTransform::Pointer disp2 = static_cast<DisplacementFieldTransform *>(list.front().GetPointer());
  DisplacementFieldType::ConstPointer field2 = disp2->GetDisplacementField();

  itk::ImageRegionConstIterator<DisplacementFieldType> it2(field2, field2->GetLargestPossibleRegion());
  if (tolerance == 0.0)
  {
    for (it.GoToBegin(), it2.GoToBegin(); !it.IsAtEnd() && !it2.IsAtEnd(); ++it, ++it2)
    {
      if (it.Value() != it2.Value())
      {
        std::cout << "Original Pixel (" << it.Value() << ") doesn't match read-in Pixel (" << it2.Value() << " ) "
                  << std::endl
                  << " in " << nonlinear_transform << std::endl;
        return EXIT_FAILURE;
      }
    }
  }
  else
  { // account for rounding errors
    for (it.GoToBegin(), it2.GoToBegin(); !it.IsAtEnd() && !it2.IsAtEnd(); ++it, ++it2)
    {
      if ((it.Value() - it2.Value()).GetSquaredNorm() > tolerance)
      {
        std::cout << "Original Pixel (" << it.Value() << ") doesn't match read-in Pixel (" << it2.Value() << " ) "
                  << " in " << nonlinear_transform << std::endl;
        return EXIT_FAILURE;
      }
    }
  }


  return EXIT_SUCCESS;
}


static int
check_nonlinear_float(const char * nonlinear_transform)
{
  double tolerance = 1e-5;

  using TransformFileWriterFloat = itk::TransformFileWriterTemplate<float>;
  using TransformFileReaderFloat = itk::TransformFileReaderTemplate<float>;

  using DisplacementFieldTransform = itk::DisplacementFieldTransform<float, 3>;
  using DisplacementFieldType = DisplacementFieldTransform::DisplacementFieldType;

  DisplacementFieldTransform::Pointer disp = DisplacementFieldTransform::New();
  DisplacementFieldType::Pointer      field = DisplacementFieldType::New();

  // create zero displacement field
  DisplacementFieldType::SizeType  imageSize3D = { { 10, 10, 10 } };
  DisplacementFieldType::IndexType startIndex3D = { { 0, 0, 0 } };

  double                            spacing[] = { 2.0, 2.0, 2.0 };
  double                            origin[] = { -10.0, -10.0, -10.0 };
  DisplacementFieldType::RegionType region;

  region.SetSize(imageSize3D);
  region.SetIndex(startIndex3D);

  field->SetLargestPossibleRegion(region);
  field->SetBufferedRegion(region);
  field->SetRequestedRegion(region);

  field->SetSpacing(spacing);
  field->SetOrigin(origin);
  field->Allocate();

  DisplacementFieldType::PixelType zeroDisplacement;
  zeroDisplacement.Fill(0.0);
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

  disp->Print(std::cout);

  TransformFileWriterFloat::Pointer nlwriter;
  TransformFileReaderFloat::Pointer nlreader;

  nlreader = TransformFileReaderFloat::New();
  nlwriter = TransformFileWriterFloat::New();
  nlwriter->AddTransform(disp);
  nlwriter->SetFileName(nonlinear_transform);
  nlreader->SetFileName(nonlinear_transform);

  // Testing writing
  std::cout << "Testing write of non linear transform (float): " << std::endl;

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

  // Testing writing
  std::cout << "Testing read of non linear transform (float) : " << std::endl;
  try
  {
    nlreader->Update();
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << "Error while reading the transforms" << std::endl;
    std::cerr << excp << std::endl;
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED]" << std::endl;

  std::cout << "Comparing of non linear transform : " << std::endl;
  TransformFileReaderFloat::TransformListType list = *nlreader->GetTransformList();
  std::cout << "Read :" << list.size() << " transformations" << std::endl;

  if (list.front()->GetTransformTypeAsString() != "DisplacementFieldTransform_float_3_3")
  {
    std::cerr << "Read back transform of type:" << list.front()->GetTransformTypeAsString() << std::endl;
    return EXIT_FAILURE;
  }
  DisplacementFieldTransform::Pointer disp2 = static_cast<DisplacementFieldTransform *>(list.front().GetPointer());
  DisplacementFieldType::ConstPointer field2 = disp2->GetDisplacementField();

  itk::ImageRegionConstIterator<DisplacementFieldType> it2(field2, field2->GetLargestPossibleRegion());
  if (tolerance == 0.0)
  {
    for (it.GoToBegin(), it2.GoToBegin(); !it.IsAtEnd() && !it2.IsAtEnd(); ++it, ++it2)
    {
      if (it.Value() != it2.Value())
      {
        std::cout << "Original Pixel (" << it.Value() << ") doesn't match read-in Pixel (" << it2.Value() << " ) "
                  << std::endl
                  << " in " << nonlinear_transform << std::endl;
        return EXIT_FAILURE;
      }
    }
  }
  else
  { // account for rounding errors
    for (it.GoToBegin(), it2.GoToBegin(); !it.IsAtEnd() && !it2.IsAtEnd(); ++it, ++it2)
    {
      if ((it.Value() - it2.Value()).GetSquaredNorm() > tolerance)
      {
        std::cout << "Original Pixel (" << it.Value() << ") doesn't match read-in Pixel (" << it2.Value() << " ) "
                  << " in " << nonlinear_transform << std::endl;
        return EXIT_FAILURE;
      }
    }
  }


  return EXIT_SUCCESS;
}

static int
secondTest()
{
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
  reader->SetFileName("Rotation.xfm");

  reader->Update();

  const TransformFileReader::TransformListType * list = reader->GetTransformList();
  auto                                           lit = list->begin();
  while (lit != list->end())
  {
    (*lit)->Print(std::cout);
    lit++;
  }
  return EXIT_SUCCESS;
}


static int
check_composite(const char * transform_file)
{
  using AffineTransformType = itk::AffineTransform<double, 3>;
  using CompositeTransformType = itk::CompositeTransform<double, 3>;

  const double tolerance = 1e-5;

  AffineTransformType::Pointer    affine1 = AffineTransformType::New();
  AffineTransformType::Pointer    affine2 = AffineTransformType::New();
  CompositeTransformType::Pointer compositeTransform = CompositeTransformType::New();

  itk::ObjectFactoryBase::RegisterFactory(itk::MINCTransformIOFactory::New());

  // Set it's parameters
  AffineTransformType::OutputVectorType rot_axis;
  rot_axis[0] = 0.0;
  rot_axis[1] = 1.0;
  rot_axis[2] = 0.0;
  // Set it's parameters
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

  reader = TransformFileReader::New();
  writer = TransformFileWriter::New();
  writer->AddTransform(compositeTransform);

  writer->SetFileName(transform_file);
  reader->SetFileName(transform_file);

  compositeTransform->Print(std::cout);
  try
  {
    writer->Update();
    std::cout << std::endl;
    std::cout << "Testing read : " << std::endl;
    reader->Update();
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << "Error while saving the transforms" << std::endl;
    std::cerr << excp << std::endl;
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
  }

  try
  {
    TransformFileReader::TransformListType list = *reader->GetTransformList();

    // MINC XFM internally collapeses two concatenated linear transforms into one
    if (list.front()->GetTransformTypeAsString() != "AffineTransform_double_3_3")
    {
      std::cerr << "Read back transform of type:" << list.front()->GetTransformTypeAsString() << std::endl;
      return EXIT_FAILURE;
    }
    AffineTransformType::Pointer affine_xfm = static_cast<AffineTransformType *>(list.front().GetPointer());

    std::cout << "Read transform : " << std::endl;
    affine_xfm->Print(std::cout);

    vnl_random randgen(12345678);

    AffineTransformType::InputPointType pnt, pnt2;

    std::cout << "Testing that transformations are the same ..." << std::endl;
    for (int i = 0; i < point_counter; i++)
    {
      AffineTransformType::OutputPointType v1;
      AffineTransformType::OutputPointType v2;

      RandomPoint<double>(randgen, pnt, 100);
      pnt2 = pnt;
      v1 = compositeTransform->TransformPoint(pnt);
      v2 = affine_xfm->TransformPoint(pnt2);

      if ((v1 - v2).GetSquaredNorm() > tolerance)
      {
        std::cerr << "Original Pixel (" << v1 << ") doesn't match read-in Pixel (" << v2 << " ) "
                  << " in " << compositeTransform << " at " << pnt << std::endl;
        return EXIT_FAILURE;
      }
    }
    std::cout << " Done !" << std::endl;
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
check_composite2(const char * transform_file, const char * transform_grid_file)
{
  const double tolerance = 1e-5;

  std::filebuf fb;
  fb.open(transform_file, std::ios::out);
  std::ostream os(&fb);
  std::cout << "Testing reading of composite transform" << std::endl << std::endl << std::endl << std::endl;
  // create concatenation of two transforms:
  // nonlinear grid with shift by 1
  // rotation by 45 deg
  os << "MNI Transform File" << std::endl;
  os << std::endl;
  os << "Transform_Type = Grid_Transform;" << std::endl;
  os << "Displacement_Volume = " << transform_grid_file << ";" << std::endl;
  os << "Transform_Type = Linear;" << std::endl;

  os << "Linear_Transform =" << std::endl;
  os << " 0.70710676908493 -0.70710676908493 0 0" << std::endl;
  os << " 0.70710676908493 0.70710676908493 0 0" << std::endl;
  os << " 0 0 1 0;" << std::endl;

  fb.close();

  try
  {
    // generate field
    using DisplacementFieldTransform = itk::DisplacementFieldTransform<double, 3>;
    using DisplacementFieldType = DisplacementFieldTransform::DisplacementFieldType;

    DisplacementFieldType::Pointer field = DisplacementFieldType::New();

    // create zero displacement field
    DisplacementFieldType::SizeType  imageSize3D = { { 10, 10, 10 } };
    DisplacementFieldType::IndexType startIndex3D = { { 0, 0, 0 } };

    double                            spacing[] = { 2.0, 2.0, 2.0 };
    double                            origin[] = { -10.0, -10.0, -10.0 };
    DisplacementFieldType::RegionType region;

    region.SetSize(imageSize3D);
    region.SetIndex(startIndex3D);

    field->SetLargestPossibleRegion(region);
    field->SetBufferedRegion(region);
    field->SetRequestedRegion(region);

    field->SetSpacing(spacing);
    field->SetOrigin(origin);
    field->Allocate();

    DisplacementFieldType::PixelType displacement;
    displacement.Fill(0.0);
    displacement[0] = 1.0;
    field->FillBuffer(displacement);

    using MincWriterType = itk::ImageFileWriter<DisplacementFieldType>;

    typename MincWriterType::Pointer writer = MincWriterType::New();
    // expecting .mnc here
    writer->SetFileName(transform_grid_file);

    writer->SetInput(field);
    writer->Update();
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << "Error while writing the deformation field" << std::endl;
    std::cerr << excp << std::endl;
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
  }


  try
  {
    TransformFileReader::Pointer reader;
    using CompositeTransformType = itk::CompositeTransform<double, 3>;

    reader = TransformFileReader::New();
    reader->SetFileName(transform_file);
    reader->Update();

    const TransformFileReader::TransformListType * list = reader->GetTransformList();

    if (list->size() != 2)
    {
      std::cerr << "Unexpected number of transforms:" << list->size() << std::endl;
      std::cerr << "Expected:2" << std::endl;
      std::cout << "[FAILED]" << std::endl;
      return EXIT_FAILURE;
    }
    std::cout << "Reading back transforms : " << list->size() << std::endl << std::endl;

    using CompositeTransformType = itk::CompositeTransform<double, 3>;
    using TransformType = itk::Transform<double, 3>;

    CompositeTransformType::Pointer _xfm = CompositeTransformType::New();
    for (const auto & it : *list)
    {
      it->Print(std::cout);
      std::cout << std::endl;
      _xfm->AddTransform(static_cast<TransformType *>(it.GetPointer()));
    }
    std::cout << std::endl;
    std::cout << std::endl;
    std::cout << std::endl;

    _xfm->Print(std::cout);

    std::cout << "Testing that transformations is expected ..." << std::endl;

    CompositeTransformType::InputPointType  pnt;
    CompositeTransformType::OutputPointType v, v2;

    pnt[0] = 1.0;
    pnt[1] = pnt[2] = 0.0;
    // expected transform: shift by 1 , rotate by 45 deg
    v2[0] = v[1] = sqrt(2);
    v2[2] = 0.0;

    v = _xfm->TransformPoint(pnt);

    std::cout << "In:" << pnt << " Out:" << v << std::endl;
    if ((v - v2).GetSquaredNorm() > tolerance)
    {
      std::cerr << "Expected coordinates (" << v << ") doesn't match read-in coordinates (" << v2 << " ) "
                << " in " << _xfm << " at " << pnt << std::endl;
      return EXIT_FAILURE;
    }


    std::cout << " Done !" << std::endl;
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

  int result1 = check_linear("itkIOTransformMINCTestTransformLinear.xfm");
  int result2 = check_nonlinear_double("itkIOTransformMINCTestTransformNonLinear.xfm");
  int result3 = check_nonlinear_float("itkIOTransformMINCTestTransformNonLinear_float.xfm");
  int result4 = secondTest();
  int result5 = check_composite("itkIOTransformMINCTestTransformComposite.xfm");
  int result6 = check_composite2("itkIOTransformMINCTestTransformComposite2.xfm",
                                 "itkIOTransformMINCTestTransformComposite2_grid_0.mnc");

  return !(result1 == EXIT_SUCCESS && result2 == EXIT_SUCCESS && result3 == EXIT_SUCCESS && result4 == EXIT_SUCCESS &&
           result5 == EXIT_SUCCESS && result6 == EXIT_SUCCESS);
}
