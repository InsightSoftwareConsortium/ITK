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

/**
 * This tests that kernel transforms are initialized correctly when they are loaded
 * A transform is created, written to a file, loaded, and the result of the transform
 * are compared to the expected results.
 */

#include "itkTransformFileWriter.h"
#include "itkTransformFileReader.h"
#include "itkElasticBodySplineKernelTransform.h"
#include "itkElasticBodyReciprocalSplineKernelTransform.h"
#include "itkThinPlateSplineKernelTransform.h"
#include "itkThinPlateR2LogRSplineKernelTransform.h"
#include "itkVolumeSplineKernelTransform.h"
#include "itksys/SystemTools.hxx"
#include "itkTestingMacros.h"
#include "itkHDF5TransformIOFactory.h"
#include "itkHDF5TransformIO.h"
#include <itkTransformFactory.h>

template<typename TransformType>
static int ReadWriteTest(const char * const fileName)
{
  const double epsilon = 1e-12;
  typedef typename TransformType::ScalarType ParametersValueType;

  // Create transform
  typename TransformType::Pointer     transform       = TransformType::New();
  typedef typename TransformType::InputPointType PointType;
  typedef typename TransformType::PointsIterator PointsIteratorType;
  typedef typename TransformType::PointSetType   PointSetType;

  PointType sourcePoint;
  PointType targetPoint;
  PointType mappedPoint;

  // Reserve memory for the number of points
  typename PointSetType::Pointer sourceLandmarks = PointSetType::New();
  typename PointSetType::Pointer targetLandmarks = PointSetType::New();

  sourceLandmarks->GetPoints()->Reserve( 4 );
  targetLandmarks->GetPoints()->Reserve( 4 );

  // Create landmark sets
  PointsIteratorType sourceit = sourceLandmarks->GetPoints()->Begin();
  PointsIteratorType targetit = targetLandmarks->GetPoints()->Begin();

  PointsIteratorType sourceend = sourceLandmarks->GetPoints()->End();

  for (int i = 0; i < 2; i++)
    {
    for (int j = 0; j < 2; j++)
      {
      sourcePoint[0] = j;
      sourcePoint[1] = i;
      sourceit.Value() = sourcePoint;
      targetPoint[0] = 3*j;
      targetPoint[1] = 3*i;
      targetit.Value() = targetPoint;
      sourceit++;
      targetit++;
      }
    }

  transform->SetSourceLandmarks( sourceLandmarks );
  transform->SetTargetLandmarks( targetLandmarks );
  transform->ComputeWMatrix();

  sourceit = sourceLandmarks->GetPoints()->Begin();
  targetit = targetLandmarks->GetPoints()->Begin();

  sourceend = sourceLandmarks->GetPoints()->End();
  while( sourceit != sourceend )
    {
    sourcePoint = sourceit.Value();
    targetPoint = targetit.Value();
    mappedPoint = transform->TransformPoint(sourcePoint);
    std::cout << sourcePoint << " : " << targetPoint;
    std::cout << " warps to: " << mappedPoint << std::endl;
    if( mappedPoint.EuclideanDistanceTo( targetPoint ) > epsilon )
      {
      return EXIT_FAILURE;
      }
    ++sourceit;
    ++targetit;
    }

  // Write transform to file
  typedef typename itk::TransformFileWriterTemplate<ParametersValueType>      TransformWriterType;
  typename TransformWriterType::Pointer transformWriter = TransformWriterType::New();
  transformWriter->SetFileName( fileName );
  transformWriter->AddTransform( transform );
  TRY_EXPECT_NO_EXCEPTION( transformWriter->Update() );

  // Read transform file
  typedef typename itk::TransformFileReaderTemplate<ParametersValueType>      TransformReaderType;
  typename TransformReaderType::Pointer transformReader = TransformReaderType::New();
  transformReader->SetFileName(fileName);
  TRY_EXPECT_NO_EXCEPTION( transformReader->Update() );

  // Compare read transform results with expected results
  const typename TransformReaderType::TransformListType * list = transformReader->GetTransformList();
  if ( list->size() != 1 )
    {
    std::cerr << "Failure: There should be only one transform in the file!" << std::endl;
    return EXIT_FAILURE;
    }

  TransformType* readTransform = static_cast<TransformType*>(list->front().GetPointer());
  sourceit = sourceLandmarks->GetPoints()->Begin();
  targetit = targetLandmarks->GetPoints()->Begin();
  sourceend = sourceLandmarks->GetPoints()->End();
  while( sourceit != sourceend )
    {
    sourcePoint = sourceit.Value();
    targetPoint = targetit.Value();
    mappedPoint = readTransform->TransformPoint(sourcePoint);
    std::cout << sourcePoint << " : " << targetPoint;
    std::cout << " warps to: " << mappedPoint << std::endl;
    if( mappedPoint.EuclideanDistanceTo( targetPoint ) > epsilon )
      {
      return EXIT_FAILURE;
      }
    ++sourceit;
    ++targetit;
    }
  return EXIT_SUCCESS;
}

int itkThinPlateTransformWriteReadTest( int argc, char *argv[] )
{
  if( argc != 2 )
    {
    std::cerr << "Usage: " << argv[0] << " outputDirectory" << std::endl;
    return EXIT_FAILURE;
    }

  itksys::SystemTools::ChangeDirectory(argv[1]);

  typedef itk::ElasticBodySplineKernelTransform<double, 2>           EBSTransform2DType;
  typedef itk::ElasticBodyReciprocalSplineKernelTransform<double, 2> EBRSTransform2DType;
  typedef itk::ThinPlateSplineKernelTransform<double, 2>             TPSTransform2DType;
  typedef itk::ThinPlateR2LogRSplineKernelTransform<double, 2>       TPR2LRSTransform2DType;
  typedef itk::VolumeSplineKernelTransform<double, 2>                VSTransform2DType;

  // Registers all transforms that are tested
  itk::TransformFactory<EBSTransform2DType>::RegisterTransform();
  itk::TransformFactory<EBRSTransform2DType>::RegisterTransform();
  itk::TransformFactory<TPSTransform2DType>::RegisterTransform();
  itk::TransformFactory<TPR2LRSTransform2DType>::RegisterTransform();
  itk::TransformFactory<VSTransform2DType>::RegisterTransform();

  // Register transform IO used to save the transforms
  itk::ObjectFactoryBase::RegisterFactory(itk::HDF5TransformIOFactory::New() );

  // Run tests
  int resultEBS = ReadWriteTest<EBSTransform2DType>("ElasticBodySplineKernelTransform_double_2.h5");
  int resultEBRS = ReadWriteTest<EBRSTransform2DType>("ElasticBodyReciprocalSplineKernelTransform_double_2.h5");
  int resultTPS = ReadWriteTest<TPSTransform2DType>("ThinPlateSplineKernelTransform_double_2.h5");
  int resultTPR2 = ReadWriteTest<TPR2LRSTransform2DType>("ThinPlateR2LogRSplineKernelTransform_double_2.h5");
  int resultVS = ReadWriteTest<VSTransform2DType>("VolumeSplineKernelTransform_double_2.h5");

  // Check results
  if( resultEBS != EXIT_SUCCESS
   || resultEBRS != EXIT_SUCCESS
   || resultTPS != EXIT_SUCCESS
   || resultTPR2 != EXIT_SUCCESS
   || resultVS != EXIT_SUCCESS
   )
   {
     return EXIT_FAILURE;
   }
   return EXIT_SUCCESS;
}
