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

#include "itkMRIBiasFieldCorrectionFilter.h"
#include "itkShapeDetectionLevelSetImageFilter.h"
#include "itkSphereMeshSource.h"
#include "itkThresholdSegmentationLevelSetImageFilter.h"
#include "itkVoronoiPartitioningImageFilter.h"
#include "itkVoronoiSegmentationImageFilter.h"
#include "itkVoronoiSegmentationRGBImageFilter.h"
#include "itkWatershedBoundaryResolver.h"
#include "itkWatershedEquivalenceRelabeler.h"
#include "itkWatershedImageFilter.h"

int main(int , char* [])
{
  typedef itk::Image<float,2> InputType;
  typedef itk::Image<float,2> OutputType;

  typedef itk::Image<unsigned char,2>  CharType;

  typedef itk::Mesh<double> MeshType;

  typedef itk::Vector<float,3>         RGBVectorType;
  typedef itk::Image<RGBVectorType, 2> RGBVectorImageType;

  itk::ShapeDetectionLevelSetFunction<InputType,InputType>::Pointer ShapeDetectionLevelSetFunctionObj =
    itk::ShapeDetectionLevelSetFunction<InputType,InputType>::New();
  std:: cout << "-------------ShapeDetectionLevelSetFunction " << ShapeDetectionLevelSetFunctionObj;

  itk::ShapeDetectionLevelSetImageFilter<InputType,InputType,float>::Pointer ShapeDetectionLevelSetImageFilterObj =
    itk::ShapeDetectionLevelSetImageFilter<InputType,InputType,float>::New();
  std:: cout << "-------------ShapeDetectionLevelSetImageFilter " << ShapeDetectionLevelSetImageFilterObj;

  itk::SphereMeshSource<MeshType>::Pointer SphereMeshSourceObj =
    itk::SphereMeshSource<MeshType>::New();
  std:: cout << "-------------SphereMeshSource " << SphereMeshSourceObj;

  itk::ThresholdSegmentationLevelSetFunction<InputType,InputType>::Pointer ThresholdSegmentationLevelSetFunctionObj =
    itk::ThresholdSegmentationLevelSetFunction<InputType,InputType>::New();
  std:: cout << "-------------ThresholdSegmentationLevelSetFunction " << ThresholdSegmentationLevelSetFunctionObj;

  itk::ThresholdSegmentationLevelSetImageFilter<InputType,InputType,float>::Pointer ThresholdSegmentationLevelSetImageFilterObj =
    itk::ThresholdSegmentationLevelSetImageFilter<InputType,InputType,float>::New();
  std:: cout << "-------------ThresholdSegmentationLevelSetImageFilter " << ThresholdSegmentationLevelSetImageFilterObj;

  itk::VoronoiDiagram2D<double>::Pointer VoronoiDiagram2DObj =
    itk::VoronoiDiagram2D<double>::New();
  std:: cout << "-------------VoronoiDiagram2D " << VoronoiDiagram2DObj;

  itk::VoronoiDiagram2DGenerator<double>::Pointer VoronoiDiagram2DGeneratorObj =
    itk::VoronoiDiagram2DGenerator<double>::New();
  std:: cout << "-------------VoronoiDiagram2DGenerator " << VoronoiDiagram2DGeneratorObj;

  itk::VoronoiPartitioningImageFilter<InputType,OutputType>::Pointer VoronoiPartitioningImageFilterObj =
    itk::VoronoiPartitioningImageFilter<InputType,OutputType>::New();
  std:: cout << "-------------VoronoiPartitioningImageFilter " << VoronoiPartitioningImageFilterObj;

  itk::VoronoiSegmentationImageFilter<InputType,OutputType>::Pointer VoronoiSegmentationImageFilterObj =
    itk::VoronoiSegmentationImageFilter<InputType,OutputType>::New();
  std:: cout << "-------------VoronoiSegmentationImageFilter " << VoronoiSegmentationImageFilterObj;

  itk::VoronoiSegmentationImageFilterBase<InputType,OutputType>::Pointer VoronoiSegmentationImageFilterBaseObj =
    itk::VoronoiSegmentationImageFilterBase<InputType,OutputType>::New();
  std:: cout << "-------------VoronoiSegmentationImageFilterBase " << VoronoiSegmentationImageFilterBaseObj;

  itk::VoronoiSegmentationRGBImageFilter<RGBVectorImageType,CharType>::Pointer VoronoiSegmentationRGBImageFilterObj =
    itk::VoronoiSegmentationRGBImageFilter<RGBVectorImageType,CharType>::New();
  std:: cout << "-------------VoronoiSegmentationRGBImageFilter " << VoronoiSegmentationRGBImageFilterObj;

  itk::watershed::Boundary<double,3>::Pointer WatershedBoundaryObj =
    itk::watershed::Boundary<double,3>::New();
  std:: cout << "-------------WatershedBoundary " << WatershedBoundaryObj;

  itk::watershed::BoundaryResolver<double,3>::Pointer WatershedBoundaryResolverObj =
    itk::watershed::BoundaryResolver<double,3>::New();
  std:: cout << "-------------WatershedBoundaryResolver " << WatershedBoundaryResolverObj;

  itk::watershed::EquivalenceRelabeler<double,3>::Pointer WatershedEquivalenceRelabelerObj =
    itk::watershed::EquivalenceRelabeler<double,3>::New();
  std:: cout << "-------------WatershedEquivalenceRelabeler " << WatershedEquivalenceRelabelerObj;

  itk::WatershedImageFilter<InputType>::Pointer WatershedImageFilterObj =
    itk::WatershedImageFilter<InputType>::New();
  std:: cout << "-------------WatershedImageFilter " << WatershedImageFilterObj;

  itk::WatershedMiniPipelineProgressCommand::Pointer WatershedMiniPipelineProgressCommandObj =
    itk::WatershedMiniPipelineProgressCommand::New();
  std:: cout << "-------------WatershedMiniPipelineProgressCommand " << WatershedMiniPipelineProgressCommandObj;

  itk::watershed::Relabeler<double,3>::Pointer WatershedRelabelerObj =
    itk::watershed::Relabeler<double,3>::New();
  std:: cout << "-------------WatershedRelabeler " << WatershedRelabelerObj;

  itk::watershed::SegmentTable<double>::Pointer WatershedSegmentTableObj =
    itk::watershed::SegmentTable<double>::New();
  std:: cout << "-------------WatershedSegmentTable " << WatershedSegmentTableObj;

  itk::watershed::SegmentTree<double>::Pointer WatershedSegmentTreeObj =
    itk::watershed::SegmentTree<double>::New();
  std:: cout << "-------------WatershedSegmentTree " << WatershedSegmentTreeObj;

  itk::watershed::SegmentTreeGenerator<double>::Pointer WatershedSegmentTreeGeneratorObj =
    itk::watershed::SegmentTreeGenerator<double>::New();
  std:: cout << "-------------WatershedSegmentTreeGenerator " << WatershedSegmentTreeGeneratorObj;

  itk::watershed::Segmenter<InputType>::Pointer WatershedSegmenterObj =
    itk::watershed::Segmenter<InputType>::New();
  std:: cout << "-------------WatershedSegmenter " << WatershedSegmenterObj;

  itk::MRASlabIdentifier<InputType>::Pointer MRASlabIdentifierObj =
    itk::MRASlabIdentifier<InputType>::New();
  std:: cout << "-------------MRASlabIdentifier " << MRASlabIdentifierObj;
  return EXIT_SUCCESS;

}
