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
#ifndef itkMeshIOTestHelper_h
#define itkMeshIOTestHelper_h

#include "itkArray.h"
#include "itkCommonEnums.h"
#include "itkCovariantVector.h"
#include "itkFixedArray.h"
#include "itkDiffusionTensor3D.h"
#include "itkMacro.h"
#include "itkMatrix.h"
#include "itkMeshIOBase.h"
#include "itkRGBAPixel.h"
#include "itkRGBPixel.h"
#include "itkSymmetricSecondRankTensor.h"
#include "itkVariableLengthVector.h"
#include "itkVariableSizeMatrix.h"
#include "itkVector.h"
#include <complex>
#include <iostream>


// Define a local macro for variable to command testing to avoid including
// itkTestingMacros.h, which causes link issues as the module hosting this
// file is not a testing module.
#define LOCAL_ITK_TEST_SET_GET_VALUE(variable, command)               \
  ITK_GCC_PRAGMA_PUSH                                                 \
  ITK_GCC_SUPPRESS_Wfloat_equal                                       \
  if (variable != command)                                            \
  {                                                                   \
    std::cerr << "Error in " << #command << std::endl;                \
    std::cerr << "  In " __FILE__ ", line " << __LINE__ << std::endl; \
    std::cerr << "Expected " << variable << std::endl;                \
    std::cerr << "but got  " << command << std::endl;                 \
    return EXIT_FAILURE;                                              \
  }                                                                   \
  ITK_GCC_PRAGMA_POP                                                  \
  ITK_MACROEND_NOOP_STATEMENT


template <typename TMeshIO>
int
TestBaseClassMethodsMeshIO(typename TMeshIO::Pointer meshIO)
{
  using FloatType = float;

  constexpr FloatType floatValue = 1.0;
  bool                usePointPixel = true;
  meshIO->SetPixelType(floatValue, usePointPixel);
  LOCAL_ITK_TEST_SET_GET_VALUE(1, meshIO->GetNumberOfPointPixelComponents());
  LOCAL_ITK_TEST_SET_GET_VALUE(itk::MeshIOBase::MapComponentType<FloatType>::CType,
                               meshIO->GetPointPixelComponentType());
  LOCAL_ITK_TEST_SET_GET_VALUE(itk::IOPixelEnum::SCALAR, meshIO->GetPointPixelType());

  usePointPixel = false;
  meshIO->SetPixelType(floatValue, usePointPixel);
  LOCAL_ITK_TEST_SET_GET_VALUE(1, meshIO->GetNumberOfCellPixelComponents());
  LOCAL_ITK_TEST_SET_GET_VALUE(itk::MeshIOBase::MapComponentType<FloatType>::CType,
                               meshIO->GetCellPixelComponentType());
  LOCAL_ITK_TEST_SET_GET_VALUE(itk::IOPixelEnum::SCALAR, meshIO->GetCellPixelType());


  using RGBPixelType = itk::RGBPixel<FloatType>;

  const RGBPixelType rgbValue{ 1.0 };
  usePointPixel = true;
  meshIO->SetPixelType(rgbValue, usePointPixel);
  LOCAL_ITK_TEST_SET_GET_VALUE(3, meshIO->GetNumberOfPointPixelComponents());
  LOCAL_ITK_TEST_SET_GET_VALUE(itk::MeshIOBase::MapComponentType<FloatType>::CType,
                               meshIO->GetPointPixelComponentType());
  LOCAL_ITK_TEST_SET_GET_VALUE(itk::IOPixelEnum::RGB, meshIO->GetPointPixelType());

  usePointPixel = false;
  meshIO->SetPixelType(rgbValue, usePointPixel);
  LOCAL_ITK_TEST_SET_GET_VALUE(3, meshIO->GetNumberOfCellPixelComponents());
  LOCAL_ITK_TEST_SET_GET_VALUE(itk::MeshIOBase::MapComponentType<FloatType>::CType,
                               meshIO->GetCellPixelComponentType());
  LOCAL_ITK_TEST_SET_GET_VALUE(itk::IOPixelEnum::RGB, meshIO->GetCellPixelType());


  using RGBAPixelType = itk::RGBAPixel<FloatType>;

  auto rgbaValue = itk::MakeFilled<RGBAPixelType>(1.0);
  usePointPixel = true;
  meshIO->SetPixelType(rgbaValue, usePointPixel);
  LOCAL_ITK_TEST_SET_GET_VALUE(4, meshIO->GetNumberOfPointPixelComponents());
  LOCAL_ITK_TEST_SET_GET_VALUE(itk::MeshIOBase::MapComponentType<FloatType>::CType,
                               meshIO->GetPointPixelComponentType());
  LOCAL_ITK_TEST_SET_GET_VALUE(itk::IOPixelEnum::RGBA, meshIO->GetPointPixelType());

  usePointPixel = false;
  meshIO->SetPixelType(rgbaValue, usePointPixel);
  LOCAL_ITK_TEST_SET_GET_VALUE(4, meshIO->GetNumberOfCellPixelComponents());
  LOCAL_ITK_TEST_SET_GET_VALUE(itk::MeshIOBase::MapComponentType<FloatType>::CType,
                               meshIO->GetCellPixelComponentType());
  LOCAL_ITK_TEST_SET_GET_VALUE(itk::IOPixelEnum::RGBA, meshIO->GetCellPixelType());


  constexpr itk::SizeValueType length = 5;
  using VectorPixelType = itk::Vector<FloatType, length>;

  auto vectorValue = itk::MakeFilled<VectorPixelType>(1.0);
  usePointPixel = true;
  meshIO->SetPixelType(vectorValue, usePointPixel);
  LOCAL_ITK_TEST_SET_GET_VALUE(length, meshIO->GetNumberOfPointPixelComponents());
  LOCAL_ITK_TEST_SET_GET_VALUE(itk::MeshIOBase::MapComponentType<FloatType>::CType,
                               meshIO->GetPointPixelComponentType());
  LOCAL_ITK_TEST_SET_GET_VALUE(itk::IOPixelEnum::VECTOR, meshIO->GetPointPixelType());

  usePointPixel = false;
  meshIO->SetPixelType(vectorValue, usePointPixel);
  LOCAL_ITK_TEST_SET_GET_VALUE(length, meshIO->GetNumberOfCellPixelComponents());
  LOCAL_ITK_TEST_SET_GET_VALUE(itk::MeshIOBase::MapComponentType<FloatType>::CType,
                               meshIO->GetCellPixelComponentType());
  LOCAL_ITK_TEST_SET_GET_VALUE(itk::IOPixelEnum::VECTOR, meshIO->GetCellPixelType());


  using CovariantVectorPixelType = itk::CovariantVector<FloatType, length>;

  auto covariantVectorValue = itk::MakeFilled<CovariantVectorPixelType>(1.0);
  usePointPixel = true;
  meshIO->SetPixelType(covariantVectorValue, usePointPixel);
  LOCAL_ITK_TEST_SET_GET_VALUE(length, meshIO->GetNumberOfPointPixelComponents());
  LOCAL_ITK_TEST_SET_GET_VALUE(itk::MeshIOBase::MapComponentType<FloatType>::CType,
                               meshIO->GetPointPixelComponentType());
  LOCAL_ITK_TEST_SET_GET_VALUE(itk::IOPixelEnum::COVARIANTVECTOR, meshIO->GetPointPixelType());

  usePointPixel = false;
  meshIO->SetPixelType(covariantVectorValue, usePointPixel);
  LOCAL_ITK_TEST_SET_GET_VALUE(length, meshIO->GetNumberOfCellPixelComponents());
  LOCAL_ITK_TEST_SET_GET_VALUE(itk::MeshIOBase::MapComponentType<FloatType>::CType,
                               meshIO->GetCellPixelComponentType());
  LOCAL_ITK_TEST_SET_GET_VALUE(itk::IOPixelEnum::COVARIANTVECTOR, meshIO->GetCellPixelType());


  using FixedArrayPixelType = itk::FixedArray<FloatType, length>;

  auto fixedArrayValue = itk::MakeFilled<FixedArrayPixelType>(1.0);
  usePointPixel = true;
  meshIO->SetPixelType(fixedArrayValue, usePointPixel);
  LOCAL_ITK_TEST_SET_GET_VALUE(length, meshIO->GetNumberOfPointPixelComponents());
  LOCAL_ITK_TEST_SET_GET_VALUE(itk::MeshIOBase::MapComponentType<FloatType>::CType,
                               meshIO->GetPointPixelComponentType());
  LOCAL_ITK_TEST_SET_GET_VALUE(itk::IOPixelEnum::FIXEDARRAY, meshIO->GetPointPixelType());

  usePointPixel = false;
  meshIO->SetPixelType(fixedArrayValue, usePointPixel);
  LOCAL_ITK_TEST_SET_GET_VALUE(length, meshIO->GetNumberOfCellPixelComponents());
  LOCAL_ITK_TEST_SET_GET_VALUE(itk::MeshIOBase::MapComponentType<FloatType>::CType,
                               meshIO->GetCellPixelComponentType());
  LOCAL_ITK_TEST_SET_GET_VALUE(itk::IOPixelEnum::FIXEDARRAY, meshIO->GetCellPixelType());


  using SymmetricSecondRankTensorPixelType = itk::SymmetricSecondRankTensor<FloatType, length>;

  auto symmetricSecondRankTensorValue = itk::MakeFilled<SymmetricSecondRankTensorPixelType>(1.0);
  usePointPixel = true;
  meshIO->SetPixelType(symmetricSecondRankTensorValue, usePointPixel);
  LOCAL_ITK_TEST_SET_GET_VALUE(length * (length + 1) / 2, meshIO->GetNumberOfPointPixelComponents());
  LOCAL_ITK_TEST_SET_GET_VALUE(itk::MeshIOBase::MapComponentType<FloatType>::CType,
                               meshIO->GetPointPixelComponentType());
  LOCAL_ITK_TEST_SET_GET_VALUE(itk::IOPixelEnum::SYMMETRICSECONDRANKTENSOR, meshIO->GetPointPixelType());

  usePointPixel = false;
  meshIO->SetPixelType(symmetricSecondRankTensorValue, usePointPixel);
  LOCAL_ITK_TEST_SET_GET_VALUE(length * (length + 1) / 2, meshIO->GetNumberOfCellPixelComponents());
  LOCAL_ITK_TEST_SET_GET_VALUE(itk::MeshIOBase::MapComponentType<FloatType>::CType,
                               meshIO->GetCellPixelComponentType());
  LOCAL_ITK_TEST_SET_GET_VALUE(itk::IOPixelEnum::SYMMETRICSECONDRANKTENSOR, meshIO->GetCellPixelType());


  using DiffusionTensor3DPixelType = itk::DiffusionTensor3D<FloatType>;

  auto diffusionTensor3DPixelValue = itk::MakeFilled<DiffusionTensor3DPixelType>(1.0);
  usePointPixel = true;
  meshIO->SetPixelType(diffusionTensor3DPixelValue, usePointPixel);
  LOCAL_ITK_TEST_SET_GET_VALUE(6, meshIO->GetNumberOfPointPixelComponents());
  LOCAL_ITK_TEST_SET_GET_VALUE(itk::MeshIOBase::MapComponentType<FloatType>::CType,
                               meshIO->GetPointPixelComponentType());
  LOCAL_ITK_TEST_SET_GET_VALUE(itk::IOPixelEnum::DIFFUSIONTENSOR3D, meshIO->GetPointPixelType());

  usePointPixel = false;
  meshIO->SetPixelType(diffusionTensor3DPixelValue, usePointPixel);
  LOCAL_ITK_TEST_SET_GET_VALUE(6, meshIO->GetNumberOfCellPixelComponents());
  LOCAL_ITK_TEST_SET_GET_VALUE(itk::MeshIOBase::MapComponentType<FloatType>::CType,
                               meshIO->GetCellPixelComponentType());
  LOCAL_ITK_TEST_SET_GET_VALUE(itk::IOPixelEnum::DIFFUSIONTENSOR3D, meshIO->GetCellPixelType());


  constexpr itk::SizeValueType rows = 2;
  constexpr itk::SizeValueType cols = 2;
  using MatrixPixelType = itk::Matrix<FloatType, rows, cols>;

  auto matrixPixelValue = itk::MakeFilled<MatrixPixelType>(1.0);
  usePointPixel = true;
  meshIO->SetPixelType(matrixPixelValue, usePointPixel);
  LOCAL_ITK_TEST_SET_GET_VALUE(rows * cols, meshIO->GetNumberOfPointPixelComponents());
  LOCAL_ITK_TEST_SET_GET_VALUE(itk::MeshIOBase::MapComponentType<FloatType>::CType,
                               meshIO->GetPointPixelComponentType());
  LOCAL_ITK_TEST_SET_GET_VALUE(itk::IOPixelEnum::MATRIX, meshIO->GetPointPixelType());

  usePointPixel = false;
  meshIO->SetPixelType(matrixPixelValue, usePointPixel);
  LOCAL_ITK_TEST_SET_GET_VALUE(rows * cols, meshIO->GetNumberOfCellPixelComponents());
  LOCAL_ITK_TEST_SET_GET_VALUE(itk::MeshIOBase::MapComponentType<FloatType>::CType,
                               meshIO->GetCellPixelComponentType());
  LOCAL_ITK_TEST_SET_GET_VALUE(itk::IOPixelEnum::MATRIX, meshIO->GetCellPixelType());


  using ComplexPixelType = std::complex<FloatType>;

  const ComplexPixelType complexPixelValue(1.0, 1.0);
  usePointPixel = true;
  meshIO->SetPixelType(complexPixelValue, usePointPixel);
  LOCAL_ITK_TEST_SET_GET_VALUE(2, meshIO->GetNumberOfPointPixelComponents());
  LOCAL_ITK_TEST_SET_GET_VALUE(itk::MeshIOBase::MapComponentType<FloatType>::CType,
                               meshIO->GetPointPixelComponentType());
  LOCAL_ITK_TEST_SET_GET_VALUE(itk::IOPixelEnum::COMPLEX, meshIO->GetPointPixelType());

  usePointPixel = false;
  meshIO->SetPixelType(complexPixelValue, usePointPixel);
  LOCAL_ITK_TEST_SET_GET_VALUE(2, meshIO->GetNumberOfCellPixelComponents());
  LOCAL_ITK_TEST_SET_GET_VALUE(itk::MeshIOBase::MapComponentType<FloatType>::CType,
                               meshIO->GetCellPixelComponentType());
  LOCAL_ITK_TEST_SET_GET_VALUE(itk::IOPixelEnum::COMPLEX, meshIO->GetCellPixelType());


  using ArrayPixelType = itk::Array<FloatType>;

  const ArrayPixelType arrayPixelValue{};
  usePointPixel = true;
  meshIO->SetPixelType(arrayPixelValue, usePointPixel);
  LOCAL_ITK_TEST_SET_GET_VALUE(arrayPixelValue.Size(), meshIO->GetNumberOfPointPixelComponents());
  LOCAL_ITK_TEST_SET_GET_VALUE(itk::MeshIOBase::MapComponentType<FloatType>::CType,
                               meshIO->GetPointPixelComponentType());
  LOCAL_ITK_TEST_SET_GET_VALUE(itk::IOPixelEnum::ARRAY, meshIO->GetPointPixelType());

  usePointPixel = false;
  meshIO->SetPixelType(arrayPixelValue, usePointPixel);
  LOCAL_ITK_TEST_SET_GET_VALUE(arrayPixelValue.Size(), meshIO->GetNumberOfCellPixelComponents());
  LOCAL_ITK_TEST_SET_GET_VALUE(itk::MeshIOBase::MapComponentType<FloatType>::CType,
                               meshIO->GetCellPixelComponentType());
  LOCAL_ITK_TEST_SET_GET_VALUE(itk::IOPixelEnum::ARRAY, meshIO->GetCellPixelType());


  using VariableLengthVectorPixelType = itk::VariableLengthVector<FloatType>;

  const VariableLengthVectorPixelType variableLengthVectorValue{};
  usePointPixel = true;
  meshIO->SetPixelType(variableLengthVectorValue, usePointPixel);
  LOCAL_ITK_TEST_SET_GET_VALUE(variableLengthVectorValue.Size(), meshIO->GetNumberOfPointPixelComponents());
  LOCAL_ITK_TEST_SET_GET_VALUE(itk::MeshIOBase::MapComponentType<FloatType>::CType,
                               meshIO->GetPointPixelComponentType());
  LOCAL_ITK_TEST_SET_GET_VALUE(itk::IOPixelEnum::VARIABLELENGTHVECTOR, meshIO->GetPointPixelType());

  usePointPixel = false;
  meshIO->SetPixelType(variableLengthVectorValue, usePointPixel);
  LOCAL_ITK_TEST_SET_GET_VALUE(variableLengthVectorValue.Size(), meshIO->GetNumberOfCellPixelComponents());
  LOCAL_ITK_TEST_SET_GET_VALUE(itk::MeshIOBase::MapComponentType<FloatType>::CType,
                               meshIO->GetCellPixelComponentType());
  LOCAL_ITK_TEST_SET_GET_VALUE(itk::IOPixelEnum::VARIABLELENGTHVECTOR, meshIO->GetCellPixelType());


  using VariableSizeMatrixType = itk::VariableSizeMatrix<FloatType>;

  const VariableSizeMatrixType matrix{};
  usePointPixel = true;
  meshIO->SetPixelType(matrix, usePointPixel);
  LOCAL_ITK_TEST_SET_GET_VALUE(matrix.Rows() * matrix.Cols(), meshIO->GetNumberOfPointPixelComponents());
  LOCAL_ITK_TEST_SET_GET_VALUE(itk::MeshIOBase::MapComponentType<FloatType>::CType,
                               meshIO->GetPointPixelComponentType());
  LOCAL_ITK_TEST_SET_GET_VALUE(itk::IOPixelEnum::VARIABLESIZEMATRIX, meshIO->GetPointPixelType());

  usePointPixel = false;
  meshIO->SetPixelType(matrix, usePointPixel);
  LOCAL_ITK_TEST_SET_GET_VALUE(matrix.Rows() * matrix.Cols(), meshIO->GetNumberOfCellPixelComponents());
  LOCAL_ITK_TEST_SET_GET_VALUE(itk::MeshIOBase::MapComponentType<FloatType>::CType,
                               meshIO->GetCellPixelComponentType());
  LOCAL_ITK_TEST_SET_GET_VALUE(itk::IOPixelEnum::VARIABLESIZEMATRIX, meshIO->GetCellPixelType());


  // ToDo see how the above change the below
  // Do this only for the last pixel type
  const itk::IOComponentEnum floatComponent = itk::IOComponentEnum::FLOAT;
  std::cout << "ComponentSize: " << meshIO->GetComponentSize(floatComponent) << std::endl;

  std::cout << "ComponentTypeAsString: " << itk::MeshIOBase::GetComponentTypeAsString(floatComponent) << std::endl;

  const itk::CommonEnums::IOPixel pixelType = itk::CommonEnums::IOPixel::SCALAR;
  std::cout << "PixelTypeAsString: " << itk::MeshIOBase::GetPixelTypeAsString(pixelType) << std::endl;

  const itk::CommonEnums::IOComponent pointComponentType = itk::CommonEnums::IOComponent::FLOAT;
  meshIO->SetPointComponentType(pointComponentType);
  LOCAL_ITK_TEST_SET_GET_VALUE(pointComponentType, meshIO->GetPointComponentType());

  const itk::CommonEnums::IOComponent cellComponentType = itk::CommonEnums::IOComponent::FLOAT;
  meshIO->SetCellComponentType(cellComponentType);
  LOCAL_ITK_TEST_SET_GET_VALUE(cellComponentType, meshIO->GetCellComponentType());

  constexpr unsigned int pointDimension = 2;
  meshIO->SetPointDimension(pointDimension);
  LOCAL_ITK_TEST_SET_GET_VALUE(pointDimension, meshIO->GetPointDimension());

  constexpr itk::MeshIOBase::SizeValueType numberOfPoints = 400;
  meshIO->SetNumberOfPoints(numberOfPoints);
  LOCAL_ITK_TEST_SET_GET_VALUE(numberOfPoints, meshIO->GetNumberOfPoints());

  constexpr itk::MeshIOBase::SizeValueType numberOfCells = 100;
  meshIO->SetNumberOfCells(numberOfCells);
  LOCAL_ITK_TEST_SET_GET_VALUE(numberOfCells, meshIO->GetNumberOfCells());

  constexpr itk::MeshIOBase::SizeValueType numberOfPointPixels = 200;
  meshIO->SetNumberOfPointPixels(numberOfPointPixels);
  LOCAL_ITK_TEST_SET_GET_VALUE(numberOfPointPixels, meshIO->GetNumberOfPointPixels());

  constexpr itk::MeshIOBase::SizeValueType numberOfCellPixels = 600;
  meshIO->SetNumberOfCellPixels(numberOfCellPixels);
  LOCAL_ITK_TEST_SET_GET_VALUE(numberOfCellPixels, meshIO->GetNumberOfCellPixels());

  constexpr itk::MeshIOBase::SizeValueType cellBufferSize = 1000;
  meshIO->SetCellBufferSize(cellBufferSize);
  LOCAL_ITK_TEST_SET_GET_VALUE(cellBufferSize, meshIO->GetCellBufferSize());

  const itk::IOFileEnum fileType = itk::IOFileEnum::ASCII;
  meshIO->SetFileType(fileType);
  LOCAL_ITK_TEST_SET_GET_VALUE(fileType, meshIO->GetFileType());

  std::cout << "FileTypeAsString: " << meshIO->GetFileTypeAsString(fileType) << std::endl;

  const itk::IOByteOrderEnum ioByteOrder = itk::IOByteOrderEnum::BigEndian;
  meshIO->SetByteOrder(ioByteOrder);
  LOCAL_ITK_TEST_SET_GET_VALUE(ioByteOrder, meshIO->GetByteOrder());

  std::cout << "ByteOrderAsString: " << meshIO->GetByteOrderAsString(ioByteOrder) << std::endl;

  const itk::MeshIOBase::ArrayOfExtensionsType supportedReadExtensions = meshIO->GetSupportedReadExtensions();
  std::cout << "SupportedReadExtensions: " << std::endl;
  for (const auto & ext : supportedReadExtensions)
  {
    std::cout << ext << std::endl;
  }

  const itk::MeshIOBase::ArrayOfExtensionsType supportedWriteExtensions = meshIO->GetSupportedWriteExtensions();
  std::cout << "SupportedWriteExtensions: " << std::endl;
  for (const auto & ext : supportedWriteExtensions)
  {
    std::cout << ext << std::endl;
  }


  return EXIT_SUCCESS;
}

namespace itk::MeshIOTestHelper
{

template <typename T>
std::shared_ptr<void>
MakeSharedArray(const std::size_t bufferSize)
{
  return std::shared_ptr<void>(new T[bufferSize], std::default_delete<T[]>());
}

inline std::shared_ptr<void>
AllocateBuffer(itk::IOComponentEnum componentType, itk::SizeValueType bufferSize)
{
  switch (componentType)
  {
    case itk::IOComponentEnum::SCHAR:
      return MakeSharedArray<char>(bufferSize);
    case itk::IOComponentEnum::UCHAR:
      return MakeSharedArray<unsigned char>(bufferSize);
    case itk::IOComponentEnum::USHORT:
      return MakeSharedArray<unsigned short>(bufferSize);
    case itk::IOComponentEnum::SHORT:
      return MakeSharedArray<short>(bufferSize);
    case itk::IOComponentEnum::UINT:
      return MakeSharedArray<unsigned int>(bufferSize);
    case itk::IOComponentEnum::INT:
      return MakeSharedArray<int>(bufferSize);
    case itk::IOComponentEnum::ULONG:
      return MakeSharedArray<unsigned long>(bufferSize);
    case itk::IOComponentEnum::LONG:
      return MakeSharedArray<long>(bufferSize);
    case itk::IOComponentEnum::LONGLONG:
      return MakeSharedArray<long long>(bufferSize);
    case itk::IOComponentEnum::ULONGLONG:
      return MakeSharedArray<unsigned long long>(bufferSize);
    case itk::IOComponentEnum::FLOAT:
      return MakeSharedArray<float>(bufferSize);
    case itk::IOComponentEnum::DOUBLE:
      return MakeSharedArray<double>(bufferSize);
    case itk::IOComponentEnum::LDOUBLE:
      return MakeSharedArray<long double>(bufferSize);
    case itk::IOComponentEnum::UNKNOWNCOMPONENTTYPE:
      break;
    default:
      break;
  }

  return nullptr;
}

} // namespace itk::MeshIOTestHelper


#endif
