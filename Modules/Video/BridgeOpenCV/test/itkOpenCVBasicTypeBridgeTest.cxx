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
#include "itkOpenCVBasicTypeBridge.h"

namespace FromOpenCV
{
int Point2ConversionTest()
{
  typedef itk::Point< int, 2 >    ITKPoint2iType;
  typedef itk::Point< float, 2 >  ITKPoint2fType;
  typedef itk::Point< double, 2 > ITKPoint2dType;

  cv::Point   cvA( 1, 2 );
  cv::Point2f cvB( 0.1, 0.2 );
  cv::Point2d cvC( -0.4, 3.2 );

  ITKPoint2iType itkA = itk::OpenCVBasicTypeBridge< ITKPoint2iType, cv::Point >::FromOpenCVToITK( cvA );
  ITKPoint2fType itkB = itk::OpenCVBasicTypeBridge< ITKPoint2fType, cv::Point2f >::FromOpenCVToITK( cvB );
  ITKPoint2dType itkC = itk::OpenCVBasicTypeBridge< ITKPoint2dType, cv::Point2d >::FromOpenCVToITK( cvC );

  if( ( itkA[0] != cvA.x ) && ( itkA[1] != cvA.y ) )
    {
    std::cerr << "itkA != cvA" << std::endl;
    return EXIT_FAILURE;
    }
  if( ( itkB[0] != cvB.x ) && ( itkB[1] != cvB.y ) )
    {
    std::cerr << "itkA != cvA" << std::endl;
    return EXIT_FAILURE;
    }
  if( ( itkC[0] != cvC.x ) && ( itkC[1] != cvC.y ) )
    {
    std::cerr << "itkA != cvA" << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}

int Point3ConversionTest()
{
  typedef itk::Point< int, 3 >    ITKPoint3iType;
  typedef itk::Point< float, 3 >  ITKPoint3fType;
  typedef itk::Point< double, 3 > ITKPoint3dType;

  cv::Point3i cvA( 1, 2, 3 );
  cv::Point3f cvB( 0.1, 0.2, 0.3 );
  cv::Point3d cvC( -0.4, 3.2, 5.5 );

  ITKPoint3iType itkA = itk::OpenCVBasicTypeBridge< ITKPoint3iType, cv::Point3i >::FromOpenCVToITK( cvA );
  ITKPoint3fType itkB = itk::OpenCVBasicTypeBridge< ITKPoint3fType, cv::Point3f >::FromOpenCVToITK( cvB );
  ITKPoint3dType itkC = itk::OpenCVBasicTypeBridge< ITKPoint3dType, cv::Point3d >::FromOpenCVToITK( cvC );

  if( ( itkA[0] != cvA.x ) && ( itkA[1] != cvA.y ) && ( itkA[2] != cvA.z ))
    {
    std::cerr << "itkA != cvA" << std::endl;
    return EXIT_FAILURE;
    }
  if( ( itkB[0] != cvB.x ) && ( itkB[1] != cvB.y ) && ( itkB[2] != cvB.z ) )
    {
    std::cerr << "itkA != cvA" << std::endl;
    return EXIT_FAILURE;
    }
  if( ( itkC[0] != cvC.x ) && ( itkC[1] != cvC.y ) && ( itkC[2] != cvC.z ) )
    {
    std::cerr << "itkA != cvA" << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}

int PointToVectorConversionTest()
{
  typedef itk::Point< double, 3 > ITKPoint3dType;
  typedef cv::Vec< double, 3 >    cvVectorType;

  cvVectorType cvU;
  cvU[0] = -0.4;
  cvU[1] = 3.2;
  cvU[2] = 5.5;

  ITKPoint3dType itkA = itk::OpenCVBasicTypeBridge< ITKPoint3dType, cvVectorType >::FromOpenCVToITK( cvU );

  for( unsigned int dim = 0; dim < 3; dim++ )
    {
    if( itkA[dim] != cvU[dim] )
      {
      std::cerr << "itkA != cvU" << std::endl;
      return EXIT_FAILURE;
      }
    }
  return EXIT_SUCCESS;
}

int Index2ConversionTest()
{
  typedef itk::Index< 2 > ITKIndexType;

  cv::Point cvA( 1, 2 );

  ITKIndexType itkA = itk::OpenCVBasicTypeBridge< ITKIndexType, cv::Point >::FromOpenCVToITK( cvA );

  if( ( itkA[0] != cvA.x ) && ( itkA[1] != cvA.y ) )
    {
    std::cerr << "itkA != cvA" << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}

int Index3ConversionTest()
{
  typedef itk::Index< 3 > ITKIndexType;

  cv::Point3i cvA( 1, 2, 3 );

  ITKIndexType itkA = itk::OpenCVBasicTypeBridge< ITKIndexType, cv::Point3i >::FromOpenCVToITK( cvA );

  if( ( itkA[0] != cvA.x ) && ( itkA[1] != cvA.y ) && ( itkA[2] != cvA.z ) )
    {
    std::cerr << "itkA != cvA" << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}

int Size2ConversionTest()
{
  typedef itk::Size< 2 > ITKSizeType;

  cv::Size cvA( 1, 2 );

  ITKSizeType itkA = itk::OpenCVBasicTypeBridge< ITKSizeType, cv::Size >::FromOpenCVToITK( cvA );

  if( ( itkA[0] != static_cast< itk::SizeValueType >( cvA.width ) ) &&
      ( itkA[1] != static_cast< itk::SizeValueType >( cvA.height ) ) )
    {
    std::cerr << "itkA != cvA" << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}

int MatrixConversionTest()
{
  const unsigned int NRows = 2;
  const unsigned int NColumns = 3;
  typedef itk::Matrix< double, NRows, NColumns > ITKMatrixType;
  typedef cv::Matx< double, NRows, NColumns >    CVMatrixType;

  CVMatrixType cvA;
  int k = 1;
  for( unsigned int i = 0; i < NRows; i++ )
    {
    for( unsigned int j = 0; j < NColumns; j++ )
      {
      cvA( i, j ) = static_cast< double >( k++ );
      }
    }

  ITKMatrixType ITKA = itk::OpenCVBasicTypeBridge< ITKMatrixType, CVMatrixType >::FromOpenCVToITK( cvA );

  int oResult = EXIT_SUCCESS;

  for( unsigned int i = 0; i < NRows; i++ )
    {
    for( unsigned int j = 0; j < NColumns; j++ )
      {
      if( cvA( i, j ) != ITKA[i][j] )
        {
        std::cerr << "cvA(" << i << ", " << j <<") != ITKA[" << i << "][" << j <<"]" << std::endl;
        std::cerr << cvA( i, j ) << " != " << ITKA[i][j] << std::endl;

        std::cerr << "***" << std::endl;
        std::cerr << ITKA << std::endl;

        oResult = EXIT_FAILURE;
        }
      }
    }

  return oResult;
}

int VectorConversionTest()
{
  const unsigned int Dimension = 10;
  typedef itk::Vector< double, Dimension >  ITKVectorType;
  typedef cv::Vec< double, Dimension >      CVVectorType;

  CVVectorType cvA;
  for( unsigned int i = 0; i < Dimension; i++ )
    {
    cvA[i] = static_cast< double >( i );
    }

  ITKVectorType ITKA = itk::OpenCVBasicTypeBridge< ITKVectorType, CVVectorType >::FromOpenCVToITK( cvA );

  int oResult = EXIT_SUCCESS;

  for( unsigned int i = 0; i < Dimension; i++ )
    {
    if( cvA[i] != ITKA[i] )
      {
      std::cerr << "cvA[" << i <<"] != ITKA[" << i << "]" << std::endl;
      std::cerr << cvA[i] << " != " << ITKA[i] << std::endl;

      std::cerr << "***" << std::endl;
      std::cerr << ITKA << std::endl;

      oResult = EXIT_FAILURE;
      }
    }

  return oResult;
}

int ToITK()
{
   if( Point2ConversionTest() != EXIT_SUCCESS )
    {
    std::cerr << "Point2ConversionTest Fails" << std::endl;
    return EXIT_FAILURE;
    }
  if( Point3ConversionTest() != EXIT_SUCCESS )
    {
    std::cerr << "Point3ConversionTest Fails" << std::endl;
    return EXIT_FAILURE;
    }
  if( Index2ConversionTest() != EXIT_SUCCESS )
    {
    std::cerr << "Index2ConverstionTest Fails" << std::endl;
    return EXIT_FAILURE;
    }
  if( Index3ConversionTest() != EXIT_SUCCESS )
    {
    std::cerr << "Index3ConverstionTest Fails" << std::endl;
    return EXIT_FAILURE;
    }
  if( Size2ConversionTest() != EXIT_SUCCESS )
    {
    std::cerr << "Size2ConverstionTest Fails" << std::endl;
    return EXIT_FAILURE;
    }
  if( MatrixConversionTest() != EXIT_SUCCESS )
    {
    std::cerr << "MatrixConversionTest Fails" << std::endl;
    return EXIT_FAILURE;
    }
  if( VectorConversionTest() != EXIT_SUCCESS )
    {
    std::cerr << "VectorConversionTest Fails" << std::endl;
    return EXIT_FAILURE;
    }
   if( PointToVectorConversionTest() != EXIT_SUCCESS )
    {
    std::cerr << "PointToVectorConversionTest Fails" << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
}

namespace FromITK
{
int Point2ConversionTest()
{
  typedef itk::Point< int, 2 >    ITKPoint2iType;
  typedef itk::Point< float, 2 >  ITKPoint2fType;
  typedef itk::Point< double, 2 > ITKPoint2dType;

  ITKPoint2iType itkA;
  itkA[0] = 1;
  itkA[1] = 2;
  cv::Point   cvA = itk::OpenCVBasicTypeBridge< ITKPoint2iType, cv::Point >::FromITKToOpenCV( itkA );

  ITKPoint2fType itkB;
  itkB[0] = 0.1;
  itkB[1] = 0.2;
  cv::Point2f   cvB = itk::OpenCVBasicTypeBridge< ITKPoint2fType, cv::Point2f >::FromITKToOpenCV( itkB );

  ITKPoint2dType itkC;
  itkC[0] = -0.4;
  itkC[1] = 3.2;
  cv::Point2d cvC = itk::OpenCVBasicTypeBridge< ITKPoint2dType, cv::Point2d >::FromITKToOpenCV( itkC );

  if( ( itkA[0] != cvA.x ) && ( itkA[1] != cvA.y ) )
    {
    std::cerr << "itkA != cvA" << std::endl;
    return EXIT_FAILURE;
    }
  if( ( itkB[0] != cvB.x ) && ( itkB[1] != cvB.y ) )
    {
    std::cerr << "itkB != cvB" << std::endl;
    return EXIT_FAILURE;
    }
  if( ( itkC[0] != cvC.x ) && ( itkC[1] != cvC.y ) )
    {
    std::cerr << "itkC != cvC" << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}

int Point3ConversionTest()
{
  typedef itk::Point< int, 3 >    ITKPoint3iType;
  typedef itk::Point< float, 3 >  ITKPoint3fType;
  typedef itk::Point< double, 3 > ITKPoint3dType;

  ITKPoint3iType itkA;
  itkA[0] = 1;
  itkA[1] = 2;
  itkA[2] = 3;

  ITKPoint3fType itkB;
  itkB[0] = 0.1;
  itkB[1] = 0.2;
  itkB[2] = 0.3;

  ITKPoint3dType itkC;
  itkC[0] = -0.4;
  itkC[1] = 3.2;
  itkC[2] = 5.5;

  cv::Point3i cvA = itk::OpenCVBasicTypeBridge< ITKPoint3iType, cv::Point3i >::FromITKToOpenCV( itkA );
  cv::Point3f cvB = itk::OpenCVBasicTypeBridge< ITKPoint3fType, cv::Point3f >::FromITKToOpenCV( itkB );
  cv::Point3d cvC = itk::OpenCVBasicTypeBridge< ITKPoint3dType, cv::Point3d >::FromITKToOpenCV( itkC );

  if( ( itkA[0] != cvA.x ) && ( itkA[1] != cvA.y ) && ( itkA[2] != cvA.z ))
    {
    std::cerr << "itkA != cvA" << std::endl;
    return EXIT_FAILURE;
    }
  if( ( itkB[0] != cvB.x ) && ( itkB[1] != cvB.y ) && ( itkB[2] != cvB.z ) )
    {
    std::cerr << "itkB != cvB" << std::endl;
    return EXIT_FAILURE;
    }
  if( ( itkC[0] != cvC.x ) && ( itkC[1] != cvC.y ) && ( itkC[2] != cvC.z ) )
    {
    std::cerr << "itkC != cvC" << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}

int Index2ConversionTest()
{
  typedef itk::Index< 2 > ITKIndexType;

  ITKIndexType itkA;
  itkA[0] = 1;
  itkA[1] = 2;

  cv::Point cvA = itk::OpenCVBasicTypeBridge< ITKIndexType, cv::Point >::FromITKToOpenCV( itkA );

  if( ( itkA[0] != cvA.x ) && ( itkA[1] != cvA.y ) )
    {
    std::cerr << "itkA != cvA" << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}

int Index3ConversionTest()
{
  typedef itk::Index< 3 > ITKIndexType;

  ITKIndexType itkA;
  itkA[0] = 1;
  itkA[1] = 2;
  itkA[2] = 3;

  cv::Point3i cvA = itk::OpenCVBasicTypeBridge< ITKIndexType, cv::Point3i >::FromITKToOpenCV( itkA );

  if( ( itkA[0] != cvA.x ) && ( itkA[1] != cvA.y ) && ( itkA[2] != cvA.z ) )
    {
    std::cerr << "itkA != cvA" << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}

int Size2ConversionTest()
{
  typedef itk::Size< 2 > ITKSizeType;

  ITKSizeType itkA;
  itkA[0] = 1;
  itkA[1] = 2;
  cv::Size cvA = itk::OpenCVBasicTypeBridge< ITKSizeType, cv::Size >::FromITKToOpenCV( itkA );

  if( ( itkA[0] != static_cast< itk::SizeValueType >( cvA.width ) ) &&
      ( itkA[1] != static_cast< itk::SizeValueType >( cvA.height ) ) )
    {
    std::cerr << "itkA != cvA" << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}

int MatrixConversionTest()
{
  const unsigned int NRows = 2;
  const unsigned int NColumns = 3;
  typedef itk::Matrix< double, NRows, NColumns > ITKMatrixType;
  typedef cv::Matx< double, NRows, NColumns >    CVMatrixType;

  ITKMatrixType itkA;
  int k = 1;
  for( unsigned int i = 0; i < NRows; i++ )
    {
    for( unsigned int j = 0; j < NColumns; j++ )
      {
        itkA[i][j] = static_cast< double >( k++ );
      }
    }

  CVMatrixType cvA = itk::OpenCVBasicTypeBridge< ITKMatrixType, CVMatrixType >::FromITKToOpenCV( itkA );

  int oResult = EXIT_SUCCESS;

  for( unsigned int i = 0; i < NRows; i++ )
    {
    for( unsigned int j = 0; j < NColumns; j++ )
      {
      if( cvA( i, j ) != itkA[i][j] )
        {
        std::cerr << "cvA(" << i << ", " << j <<") != itkA[" << i << "][" << j <<"]" << std::endl;
        std::cerr << cvA( i, j ) << " != " << itkA[i][j] << std::endl;

        std::cerr << "***" << std::endl;
        std::cerr << itkA << std::endl;

        oResult = EXIT_FAILURE;
        }
      }
    }

  return oResult;
}

int VectorConversionTest()
{
  const unsigned int Dimension = 10;
  typedef itk::Vector< double, Dimension >  ITKVectorType;
  typedef cv::Vec< double, Dimension >      CVVectorType;

  ITKVectorType itkA;
  for( unsigned int i = 0; i < Dimension; i++ )
    {
    itkA[i] = static_cast< double >( i );
    }

  CVVectorType cvA = itk::OpenCVBasicTypeBridge< ITKVectorType, CVVectorType >::FromITKToOpenCV( itkA );

  int oResult = EXIT_SUCCESS;

  for( unsigned int i = 0; i < Dimension; i++ )
    {
    if( cvA[i] != itkA[i] )
      {
      std::cerr << "cvA[" << i <<"] != itkA[" << i << "]" << std::endl;
      std::cerr << cvA[i] << " != " << itkA[i] << std::endl;

      std::cerr << "***" << std::endl;
      std::cerr << itkA << std::endl;

      oResult = EXIT_FAILURE;
      }
    }

  return oResult;
}

int PointToVectorConversionTest()
{
  typedef itk::Point< double, 3 > ITKPoint3dType;
  typedef cv::Vec< double, 3 >    cvVectorType;

  ITKPoint3dType itkA;
  itkA[0] = -0.4;
  itkA[1] = 3.2;
  itkA[2] = 5.5;

  cvVectorType cvU = itk::OpenCVBasicTypeBridge< ITKPoint3dType, cvVectorType >::FromITKToOpenCV( itkA );

  for( unsigned int dim = 0; dim < 3; dim++ )
    {
    if( itkA[dim] != cvU[dim] )
      {
      std::cerr << "itkA != cvU" << std::endl;
      return EXIT_FAILURE;
      }
    }
  return EXIT_SUCCESS;
}

int ToOpenCV()
{
   if( Point2ConversionTest() != EXIT_SUCCESS )
    {
    std::cerr << "Point2ConversionTest Fails" << std::endl;
    return EXIT_FAILURE;
    }
  if( Point3ConversionTest() != EXIT_SUCCESS )
    {
    std::cerr << "Point3ConversionTest Fails" << std::endl;
    return EXIT_FAILURE;
    }
  if( Index2ConversionTest() != EXIT_SUCCESS )
    {
    std::cerr << "Index2ConverstionTest Fails" << std::endl;
    return EXIT_FAILURE;
    }
  if( Index3ConversionTest() != EXIT_SUCCESS )
    {
    std::cerr << "Index3ConverstionTest Fails" << std::endl;
    return EXIT_FAILURE;
    }
  if( Size2ConversionTest() != EXIT_SUCCESS )
    {
    std::cerr << "Size2ConverstionTest Fails" << std::endl;
    return EXIT_FAILURE;
    }
  if( MatrixConversionTest() != EXIT_SUCCESS )
    {
    std::cerr << "MatrixConversionTest Fails" << std::endl;
    return EXIT_FAILURE;
    }
  if( VectorConversionTest() != EXIT_SUCCESS )
    {
    std::cerr << "VectorConversionTest Fails" << std::endl;
    return EXIT_FAILURE;
    }
  if( PointToVectorConversionTest() != EXIT_SUCCESS )
    {
    std::cerr << "PointToVectorConversionTest Fails" << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
}
int itkOpenCVBasicTypeBridgeTest( int, char* [] )
{
  if( FromOpenCV::ToITK() != EXIT_SUCCESS )
    {
    std::cerr << "FromOpenCVToITK Fails" << std::endl;
    return EXIT_FAILURE;
    }
  if( FromITK::ToOpenCV() != EXIT_SUCCESS )
    {
    std::cerr << "FromOpenCVToITK Fails" << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
