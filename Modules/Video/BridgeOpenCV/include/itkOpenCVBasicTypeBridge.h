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
#ifndef itkOpenCVBasicTypeBridge_h
#define itkOpenCVBasicTypeBridge_h

#include "itkPoint.h"
#include "itkSize.h"
#include "itkIndex.h"
#include "itkMatrix.h"

#include "opencv2/core/version.hpp"
#if !defined(CV_VERSION_EPOCH)
// OpenCV 3.x
#  include "opencv2/core.hpp"
#else
// OpenCV 2.4.x
#  include "cv.h"
#endif

namespace itk
{
template <typename TITKData, typename TOpenCVData>
struct OpenCVBasicTypeBridge
{};

template <typename TPoint>
struct OpenCVBasicTypeBridge<TPoint, cv::Point_<typename TPoint::CoordRepType>>
{
  using ITKDataType = TPoint;
  using CoordinateType = typename TPoint::CoordRepType;
  using OpenCVDataType = cv::Point_<CoordinateType>;

  static ITKDataType
  FromOpenCVToITK(const OpenCVDataType & iP)
  {
    ITKDataType oP;
    oP[0] = iP.x;
    oP[1] = iP.y;

    return oP;
  }

  static OpenCVDataType
  FromITKToOpenCV(const ITKDataType & iP)
  {
    return OpenCVDataType(iP[0], iP[1]);
  }

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(SameDimensionCheck, (Concept::SameDimension<TPoint::PointDimension, 2>));
#endif
};

template <typename TPoint>
struct OpenCVBasicTypeBridge<TPoint, cv::Point3_<typename TPoint::CoordRepType>>
{
  using ITKDataType = TPoint;
  using CoordinateType = typename TPoint::CoordRepType;
  using OpenCVDataType = cv::Point3_<CoordinateType>;

  static ITKDataType
  FromOpenCVToITK(const OpenCVDataType & iP)
  {
    ITKDataType oP;
    oP[0] = iP.x;
    oP[1] = iP.y;
    oP[2] = iP.z;

    return oP;
  }

  static OpenCVDataType
  FromITKToOpenCV(const ITKDataType & iP)
  {
    return OpenCVDataType(iP[0], iP[1], iP[2]);
  }

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(SameDimensionCheck, (Concept::SameDimension<TPoint::PointDimension, 3>));
#endif
};

template <>
struct OpenCVBasicTypeBridge<itk::Index<2>, cv::Point>
{
  using ITKDataType = itk::Index<2>;
  using OpenCVDataType = cv::Point;

  static ITKDataType
  FromOpenCVToITK(const OpenCVDataType & iP)
  {
    ITKDataType oP;
    oP[0] = static_cast<itk::IndexValueType>(iP.x);
    oP[1] = static_cast<itk::IndexValueType>(iP.y);

    return oP;
  }

  static OpenCVDataType
  FromITKToOpenCV(const ITKDataType & iP)
  {
    return OpenCVDataType(static_cast<int>(iP[0]), static_cast<int>(iP[1]));
  }
};

template <>
struct OpenCVBasicTypeBridge<itk::Index<3>, cv::Point3i>
{
  using ITKDataType = itk::Index<3>;
  using OpenCVDataType = cv::Point3i;

  static ITKDataType
  FromOpenCVToITK(const OpenCVDataType & iP)
  {
    ITKDataType oP;
    oP[0] = static_cast<itk::IndexValueType>(iP.x);
    oP[1] = static_cast<itk::IndexValueType>(iP.y);
    oP[2] = static_cast<itk::IndexValueType>(iP.z);

    return oP;
  }

  static OpenCVDataType
  FromITKToOpenCV(const ITKDataType & iP)
  {
    return OpenCVDataType(static_cast<int>(iP[0]), static_cast<int>(iP[1]), static_cast<int>(iP[2]));
  }
};

template <>
struct OpenCVBasicTypeBridge<itk::Size<2>, cv::Size>
{
  using ITKDataType = itk::Size<2>;
  using OpenCVDataType = cv::Size;

  static ITKDataType
  FromOpenCVToITK(const OpenCVDataType & iP)
  {
    ITKDataType oP;
    oP[0] = static_cast<itk::SizeValueType>(iP.width);
    oP[1] = static_cast<itk::SizeValueType>(iP.height);

    return oP;
  }

  static OpenCVDataType
  FromITKToOpenCV(const ITKDataType & iP)
  {
    return OpenCVDataType(static_cast<int>(iP[0]), static_cast<int>(iP[1]));
  }
};

template <typename T, unsigned int VRows, unsigned int VColumns>
struct OpenCVBasicTypeBridge<itk::Matrix<T, VRows, VColumns>, cv::Matx<T, VRows, VColumns>>
{
  using ITKDataType = itk::Matrix<T, VRows, VColumns>;
  using OpenCVDataType = cv::Matx<T, VRows, VColumns>;

  static ITKDataType
  FromOpenCVToITK(const OpenCVDataType & iP)
  {
    return ITKDataType(typename ITKDataType::InternalMatrixType().copy_in(iP.val));
  }

  static OpenCVDataType
  FromITKToOpenCV(const ITKDataType & iP)
  {
    OpenCVDataType oM;
    iP.GetVnlMatrix().copy_out(oM.val);
    return oM;
  }
};

template <typename TVector>
struct OpenCVBasicTypeBridge<TVector, cv::Vec<typename TVector::ValueType, TVector::Dimension>>
{
  using ITKDataType = TVector;
  using ValueType = typename TVector::ValueType;
  using OpenCVDataType = cv::Vec<ValueType, TVector::Dimension>;

  static ITKDataType
  FromOpenCVToITK(const OpenCVDataType & iP)
  {
    return ITKDataType(iP.val);
  }

  static OpenCVDataType
  FromITKToOpenCV(const ITKDataType & iP)
  {
    OpenCVDataType oM;
    std::copy(iP.Begin(), iP.End(), oM.val);
    return oM;
  }
};

} // namespace itk
#endif
