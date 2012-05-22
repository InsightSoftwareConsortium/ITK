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
#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkPointSet.h"
#include "itkFEMScatteredDataPointSetToImageFilter.h"
/**
 * In this test, we create a simple 2D mesh and feature points to do evaluation.
 *
 * Example:
 * The image is 5x5, the mesh is 2x2, and four feature points represent four
 * cases: two points are on the boundary, one point is inside the element, and
 * one point is the node.
 *
 * 4   ----------------
 *   |    |   |   |   |
 * 3 |----|---|-------*
 *   |    |   |   |   |
 * 2 |----|---*---|---|
 *   |    |   |   |   |
 * 1 |----|---|---*---|
 *   |    |   |   |   |
 *    ----*-----------
 *  0     1   2   3   4
 *
 */
int itkFEMScatteredDataPointSetToImageFilterTest(int, char *[])
{
  const unsigned int ParametricDimension = 2;
  const unsigned int DataDimension = 2;

  typedef float                                         RealType;
  typedef itk::Vector<RealType, DataDimension>          VectorType;
  typedef itk::Matrix<RealType, DataDimension,
      DataDimension>                                    MatrixType;
  typedef itk::Image<VectorType, ParametricDimension>   DeformationFieldType;
  typedef itk::PointSet
    <VectorType, ParametricDimension>                   FeaturePointSetType;
  typedef itk::PointSet
    <MatrixType, ParametricDimension>                   TensorPointSetType;
  typedef itk::PointSet
    <RealType, ParametricDimension>                     ConfidencePointSetType;
  typedef itk::Mesh< VectorType, ParametricDimension >  MeshType;
  typedef FeaturePointSetType::PointType                PointType;

  typedef itk::fem::FEMScatteredDataPointSetToImageFilter
    <FeaturePointSetType, MeshType, DeformationFieldType, ConfidencePointSetType,
    TensorPointSetType>                FilterType;

  typedef itk::ImageRegionConstIterator< DeformationFieldType > ConstIteratorType;

  FilterType::Pointer filter = FilterType::New();

  //Construct a feature point set
  FeaturePointSetType::Pointer featurePoints = FeaturePointSetType::New();
  PointType p0;
  PointType p1;
  PointType p2;
  PointType p3;
  //point is on the bottom boundary
  p0[0] = 1.0;
  p0[1] = 0.0;
  //point is inside a element
  p1[0] = 3.0;
  p1[1] = 1.0;
  //point is the node
  p2[0] = 2.0;
  p2[1] = 2.0;
  //point is on the right boundary
  p3[0] = 4.0;
  p3[1] = 3.0;

  VectorType u0;
  VectorType u1;
  VectorType u2;
  VectorType u3;

  u0[0] = 1.0;
  u0[1] = 1.0;
  u1[0] = 1.0;
  u1[1] = 1.0;
  u2[0] = 1.0;
  u2[1] = 1.0;
  u3[0] = 1.0;
  u3[1] = 1.0;

  featurePoints->SetPoint(0, p0);
  featurePoints->SetPoint(1, p1);
  featurePoints->SetPoint(2, p2);
  featurePoints->SetPoint(3, p3);

  featurePoints->SetPointData(0, u0);
  featurePoints->SetPointData(1, u1);
  featurePoints->SetPointData(2, u2);
  featurePoints->SetPointData(3, u3);

  //Set the feature point set
  filter->SetInput(featurePoints);

  // Set the parameters for a rectilinear mesh.
  // Ingore this setting if users provide a mesh
  DeformationFieldType::SpacingType elementSpacing;
  elementSpacing[0] = 2.0;
  elementSpacing[1] = 2.0;
  filter->SetElementSpacing(elementSpacing);

  //Set the output
  DeformationFieldType::SizeType size;
  size[0] = 5;
  size[1] = 5;
  filter->SetSize(size);

  DeformationFieldType::SpacingType spacing;
  spacing[0] = 1.0;
  spacing[1] = 1.0;
  filter->SetSpacing(spacing);

  DeformationFieldType::PointType origin;
  origin[0] = 0.0;
  origin[1] = 0.0;
  filter->SetOrigin(origin);

  filter->Update();

  DeformationFieldType::Pointer DF = filter->GetOutput();

  ConstIteratorType constIterator( DF, DF->GetRequestedRegion() );

  //examine the results
  bool hasError = false;
  VectorType realDisplacement;
  realDisplacement[0] = 1.0;
  realDisplacement[1] = 1.0;
  for(constIterator.GoToBegin(); !constIterator.IsAtEnd(); ++constIterator)
    {
    VectorType simulatedDisplacement = constIterator.Get();
    VectorType error = simulatedDisplacement - realDisplacement;

    if(error.GetNorm() > 0.0001)
      {
      hasError = true;
      }
    }

  if( hasError )
    {
    std::cout << "Test FAILED!" << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test PASSED!" << std::endl;
  return EXIT_SUCCESS;
}
