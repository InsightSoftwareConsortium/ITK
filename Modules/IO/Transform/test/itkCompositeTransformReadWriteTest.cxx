/*=========================================================================
 *
 *  Copyright Insight Software Consortium
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
#include "itkTransformFileReader.h"
#include "itkTransformFileWriter.h"
#include "itkCompositeTransform.h"
#include "itkVersorRigid3DTransform.h"
#include "itkAffineTransform.h"
#include "itksys/SystemTools.hxx"
#include <iostream>

template<typename TCompositeTransform>
int CompositeTest(const char *extension)
{
  typedef typename TCompositeTransform::Superclass CSuperclass;
  typedef typename CSuperclass::ScalarType         CScalarType;

  typedef itk::AffineTransform<CScalarType,3>      AffineType;
  typedef itk::VersorRigid3DTransform<CScalarType> VersorRigid3DType;

  // make a versor rigid 3d transform
  typename VersorRigid3DType::Pointer versorRigid3DXfrm = VersorRigid3DType::New();
  typename VersorRigid3DType::AxisType versorAxis;
  versorAxis[0] = 0.0;
  versorAxis[1] = -1.0;
  versorAxis[2] = 0.0;
  typename VersorRigid3DType::AngleType versorAngle = 1.5;

  versorRigid3DXfrm->SetRotation(versorAxis,versorAngle);

  // make an affine transform.
  typename AffineType::Pointer affineXfrm = AffineType::New();
  typename AffineType::OutputVectorType affineAxis;
  affineAxis[0] = affineAxis[1] = 0.0;
  affineAxis[2] = 1.0;
  typename AffineType::ScalarType affineAngle = -0.5;
  affineXfrm->Rotate3D(affineAxis,affineAngle);
  typename AffineType::OutputVectorType affineTranslation;
  affineTranslation[0] = 1.0;
  affineTranslation[1] = -2.0;
  affineTranslation[2] = 3.0;
  affineXfrm->Translate(affineTranslation);

  //
  // make a composite
  typename TCompositeTransform::Pointer compXfrm =
    TCompositeTransform::New();
  //
  // add the 2 transforms
  compXfrm->AddTransform(versorRigid3DXfrm);
  compXfrm->AddTransform(affineXfrm);

  //
  // write the transform to a file
  typename itk::TransformFileWriter::Pointer writer =
    itk::TransformFileWriter::New();
  std::string xfrmFileName = affineXfrm->GetTransformTypeAsString();
  xfrmFileName += extension;
  writer->SetFileName(xfrmFileName);
  writer->SetInput(compXfrm.GetPointer());
  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Error while saving the transform"
              << xfrmFileName<< std::endl;
    std::cerr << excp << std::endl;
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
    }


  //
  // read the transform back in
  typename itk::TransformFileReader::Pointer reader =
    itk::TransformFileReader::New();
  reader->SetFileName(xfrmFileName);
  try
    {
    reader->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Error while reading the transform"
              << xfrmFileName << std::endl;
    std::cerr << excp << std::endl;
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
    }
  typename itk::TransformFileReader::TransformListType *list;
  list = reader->GetTransformList();
  typename itk::TransformFileReader::TransformType::Pointer fileXfrm =
    (* (list->begin()) );

  typename TCompositeTransform::Pointer compXfrm2 = dynamic_cast<TCompositeTransform *>(fileXfrm.GetPointer());
  if(compXfrm2.IsNull())
    {
    std::cerr << "First transform in " << xfrmFileName
              << " is not a composite transform of the correct type, reports "
              << fileXfrm->GetTransformTypeAsString()
              << std::endl;
    return EXIT_FAILURE;
    }
  typedef typename CSuperclass::OutputPointType PointType;

  PointType point1;
  point1[0] = 17.0; point1[1] = -333.0; point1[2] = 43.7;

  PointType originalXfrmPoint = compXfrm->TransformPoint(point1);
  PointType fromDiskXfrmPoint = compXfrm2->TransformPoint(point1);

  double xfrmDistance = point1.EuclideanDistanceTo(originalXfrmPoint);
  double errorDistance = originalXfrmPoint.EuclideanDistanceTo(fromDiskXfrmPoint);
  if(errorDistance > 0.0)
    {
    if(errorDistance > (xfrmDistance/1000.0))
      {
      std::cerr << "Difference between transform "
                << "created and transform read from disk"
                << "results in a transform error of "
                << errorDistance << std::endl;
      return EXIT_FAILURE;
      }
    }
  return EXIT_SUCCESS;
}

int itkCompositeTransformReadWriteTest(int argc, char *argv[])
{
  // use relative paths, but change dir int0
  // data dir passed on command line before running.
  if (argc > 1)
    {
    itksys::SystemTools::ChangeDirectory(argv[1]);
    }

  typedef itk::CompositeTransform<double, 3>         CompositeTransformDoubleType;
  typedef itk::CompositeTransform<float, 3>          CompositeTransformFloatType;

  int status[4];
  status[0] = CompositeTest<CompositeTransformDoubleType>(".txt");
  status[1] = CompositeTest<CompositeTransformFloatType>(".txt");
  status[2] = CompositeTest<CompositeTransformDoubleType>(".hdf5");
  status[3] = CompositeTest<CompositeTransformFloatType>(".hdf5");
  for(unsigned i = 0; i < 4; ++i)
    {
    if(status[i] == EXIT_FAILURE)
      {
      return EXIT_FAILURE;
      }
    }
  return EXIT_SUCCESS;
}
