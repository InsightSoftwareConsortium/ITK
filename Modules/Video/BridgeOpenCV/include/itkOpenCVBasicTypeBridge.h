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
#ifndef itkOpenCVBasicTypeBridge_h
#define itkOpenCVBasicTypeBridge_h

#include "itkPoint.h"
#include "itkSize.h"
#include "itkIndex.h"
#include "itkMatrix.h"

#include "opencv2/core/version.hpp"
#if !defined(CV_VERSION_EPOCH)
// OpenCV 3.x
#include "opencv2/core.hpp"
#else
// OpenCV 2.4.x
#include "cv.h"
#endif

namespace itk
{
template< typename TITKData, typename TOpenCVData >
struct OpenCVBasicTypeBridge
{};

template< typename TPoint >
struct OpenCVBasicTypeBridge< TPoint, cv::Point_< typename TPoint::CoordRepType > >
{
  typedef TPoint                        ITKDataType;
  typedef typename TPoint::CoordRepType CoordinateType;
  typedef cv::Point_< CoordinateType >  OpenCVDataType;

  static ITKDataType FromOpenCVToITK( const OpenCVDataType& iP )
    {
    ITKDataType oP;
    oP[0] = iP.x;
    oP[1] = iP.y;

    return oP;
    }

  static OpenCVDataType FromITKToOpenCV( const ITKDataType & iP )
    {
    return OpenCVDataType( iP[0], iP[1] );
    }

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( SameDimensionCheck,
                   ( Concept::SameDimension< TPoint::PointDimension, 2 > ) );
#endif

};

template< typename TPoint >
struct OpenCVBasicTypeBridge< TPoint, cv::Point3_< typename TPoint::CoordRepType > >
{
  typedef TPoint                        ITKDataType;
  typedef typename TPoint::CoordRepType CoordinateType;
  typedef cv::Point3_< CoordinateType > OpenCVDataType;

  static ITKDataType FromOpenCVToITK( const OpenCVDataType& iP )
    {
    ITKDataType oP;
    oP[0] = iP.x;
    oP[1] = iP.y;
    oP[2] = iP.z;

    return oP;
    }

  static OpenCVDataType FromITKToOpenCV( const ITKDataType & iP )
    {
    return OpenCVDataType( iP[0], iP[1], iP[2] );
    }

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( SameDimensionCheck,
                   ( Concept::SameDimension< TPoint::PointDimension, 3 > ) );
#endif
};

template<>
struct OpenCVBasicTypeBridge< itk::Index< 2 >, cv::Point >
{
  typedef itk::Index< 2 >  ITKDataType;
  typedef cv::Point        OpenCVDataType;

  static ITKDataType FromOpenCVToITK( const OpenCVDataType& iP )
    {
    ITKDataType oP;
    oP[0] = static_cast< itk::IndexValueType >( iP.x );
    oP[1] = static_cast< itk::IndexValueType >( iP.y );

    return oP;
    }

  static OpenCVDataType FromITKToOpenCV( const ITKDataType & iP )
    {
    return OpenCVDataType( static_cast< int >( iP[0] ), static_cast< int >( iP[1] ) );
    }
};

template<>
struct OpenCVBasicTypeBridge< itk::Index< 3 >, cv::Point3i >
{
  typedef itk::Index< 3 >  ITKDataType;
  typedef cv::Point3i      OpenCVDataType;

  static ITKDataType FromOpenCVToITK( const OpenCVDataType& iP )
    {
    ITKDataType oP;
    oP[0] = static_cast< itk::IndexValueType >( iP.x );
    oP[1] = static_cast< itk::IndexValueType >( iP.y );
    oP[2] = static_cast< itk::IndexValueType >( iP.z );

    return oP;
    }

  static OpenCVDataType FromITKToOpenCV( const ITKDataType & iP )
    {
    return OpenCVDataType( static_cast< int >( iP[0] ), static_cast< int >( iP[1] ), static_cast< int >( iP[2] ) );
    }
};

template<>
struct OpenCVBasicTypeBridge< itk::Size< 2 >, cv::Size >
{
  typedef itk::Size< 2 >    ITKDataType;
  typedef cv::Size          OpenCVDataType;

  static ITKDataType FromOpenCVToITK( const OpenCVDataType& iP )
    {
    ITKDataType oP;
    oP[0] = static_cast< itk::SizeValueType >( iP.width );
    oP[1] = static_cast< itk::SizeValueType >( iP.height );

    return oP;
    }

  static OpenCVDataType FromITKToOpenCV( const ITKDataType & iP )
    {
    return OpenCVDataType( static_cast< int >( iP[0] ), static_cast< int >( iP[1] ) );
    }
};

template< typename T, unsigned int NRows, unsigned int NColumns >
struct OpenCVBasicTypeBridge< itk::Matrix< T, NRows, NColumns >, cv::Matx< T, NRows, NColumns > >
{
  typedef itk::Matrix< T, NRows, NColumns > ITKDataType;
  typedef cv::Matx< T, NRows, NColumns >    OpenCVDataType;

  static ITKDataType FromOpenCVToITK( const OpenCVDataType& iP )
    {
    return ITKDataType( typename ITKDataType::InternalMatrixType().copy_in( iP.val ) );
    }

  static OpenCVDataType FromITKToOpenCV( const ITKDataType & iP )
    {
    OpenCVDataType oM;
    iP.GetVnlMatrix().copy_out( oM.val );
    return oM;
    }

};

template< typename TVector >
struct OpenCVBasicTypeBridge< TVector, cv::Vec< typename TVector::ValueType, TVector::Dimension > >
{
  typedef TVector                                   ITKDataType;
  typedef typename TVector::ValueType               ValueType;
  typedef cv::Vec< ValueType, TVector::Dimension >  OpenCVDataType;

  static ITKDataType FromOpenCVToITK( const OpenCVDataType& iP )
    {
    return ITKDataType( iP.val );
    }

  static OpenCVDataType FromITKToOpenCV( const ITKDataType & iP )
    {
    OpenCVDataType oM;
    std::copy( iP.Begin(), iP.End(), oM.val );
    return oM;
    }
};

}
#endif
