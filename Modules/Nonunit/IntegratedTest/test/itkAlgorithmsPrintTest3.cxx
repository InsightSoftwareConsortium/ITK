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

int
main(int, char *[])
{
  using InputType = itk::Image<float, 2>;
  using OutputType = itk::Image<float, 2>;

  using CharType = itk::Image<unsigned char, 2>;

  using MeshType = itk::Mesh<double>;

  using RGBVectorType = itk::Vector<float, 3>;
  using RGBVectorImageType = itk::Image<RGBVectorType, 2>;

  const itk::ShapeDetectionLevelSetFunction<InputType, InputType>::Pointer ShapeDetectionLevelSetFunctionObj =
    itk::ShapeDetectionLevelSetFunction<InputType, InputType>::New();
  std::cout << "-------------ShapeDetectionLevelSetFunction " << ShapeDetectionLevelSetFunctionObj;

  const itk::ShapeDetectionLevelSetImageFilter<InputType, InputType, float>::Pointer
    ShapeDetectionLevelSetImageFilterObj = itk::ShapeDetectionLevelSetImageFilter<InputType, InputType, float>::New();
  std::cout << "-------------ShapeDetectionLevelSetImageFilter " << ShapeDetectionLevelSetImageFilterObj;

  const itk::SphereMeshSource<MeshType>::Pointer SphereMeshSourceObj = itk::SphereMeshSource<MeshType>::New();
  std::cout << "-------------SphereMeshSource " << SphereMeshSourceObj;

  const itk::ThresholdSegmentationLevelSetFunction<InputType, InputType>::Pointer
    ThresholdSegmentationLevelSetFunctionObj = itk::ThresholdSegmentationLevelSetFunction<InputType, InputType>::New();
  std::cout << "-------------ThresholdSegmentationLevelSetFunction " << ThresholdSegmentationLevelSetFunctionObj;

  const itk::ThresholdSegmentationLevelSetImageFilter<InputType, InputType, float>::Pointer
    ThresholdSegmentationLevelSetImageFilterObj =
      itk::ThresholdSegmentationLevelSetImageFilter<InputType, InputType, float>::New();
  std::cout << "-------------ThresholdSegmentationLevelSetImageFilter " << ThresholdSegmentationLevelSetImageFilterObj;

  const itk::VoronoiDiagram2D<double>::Pointer VoronoiDiagram2DObj = itk::VoronoiDiagram2D<double>::New();
  std::cout << "-------------VoronoiDiagram2D " << VoronoiDiagram2DObj;

  const itk::VoronoiDiagram2DGenerator<double>::Pointer VoronoiDiagram2DGeneratorObj =
    itk::VoronoiDiagram2DGenerator<double>::New();
  std::cout << "-------------VoronoiDiagram2DGenerator " << VoronoiDiagram2DGeneratorObj;

  const itk::VoronoiPartitioningImageFilter<InputType, OutputType>::Pointer VoronoiPartitioningImageFilterObj =
    itk::VoronoiPartitioningImageFilter<InputType, OutputType>::New();
  std::cout << "-------------VoronoiPartitioningImageFilter " << VoronoiPartitioningImageFilterObj;

  const itk::VoronoiSegmentationImageFilter<InputType, OutputType>::Pointer VoronoiSegmentationImageFilterObj =
    itk::VoronoiSegmentationImageFilter<InputType, OutputType>::New();
  std::cout << "-------------VoronoiSegmentationImageFilter " << VoronoiSegmentationImageFilterObj;

  const itk::VoronoiSegmentationImageFilterBase<InputType, OutputType>::Pointer VoronoiSegmentationImageFilterBaseObj =
    itk::VoronoiSegmentationImageFilterBase<InputType, OutputType>::New();
  std::cout << "-------------VoronoiSegmentationImageFilterBase " << VoronoiSegmentationImageFilterBaseObj;

  const itk::VoronoiSegmentationRGBImageFilter<RGBVectorImageType, CharType>::Pointer
    VoronoiSegmentationRGBImageFilterObj = itk::VoronoiSegmentationRGBImageFilter<RGBVectorImageType, CharType>::New();
  std::cout << "-------------VoronoiSegmentationRGBImageFilter " << VoronoiSegmentationRGBImageFilterObj;

  const itk::watershed::Boundary<double, 3>::Pointer WatershedBoundaryObj = itk::watershed::Boundary<double, 3>::New();
  std::cout << "-------------WatershedBoundary " << WatershedBoundaryObj;

  const itk::watershed::BoundaryResolver<double, 3>::Pointer WatershedBoundaryResolverObj =
    itk::watershed::BoundaryResolver<double, 3>::New();
  std::cout << "-------------WatershedBoundaryResolver " << WatershedBoundaryResolverObj;

  const itk::watershed::EquivalenceRelabeler<double, 3>::Pointer WatershedEquivalenceRelabelerObj =
    itk::watershed::EquivalenceRelabeler<double, 3>::New();
  std::cout << "-------------WatershedEquivalenceRelabeler " << WatershedEquivalenceRelabelerObj;

  const itk::WatershedImageFilter<InputType>::Pointer WatershedImageFilterObj =
    itk::WatershedImageFilter<InputType>::New();
  std::cout << "-------------WatershedImageFilter " << WatershedImageFilterObj;

  const itk::WatershedMiniPipelineProgressCommand::Pointer WatershedMiniPipelineProgressCommandObj =
    itk::WatershedMiniPipelineProgressCommand::New();
  std::cout << "-------------WatershedMiniPipelineProgressCommand " << WatershedMiniPipelineProgressCommandObj;

  const itk::watershed::Relabeler<double, 3>::Pointer WatershedRelabelerObj =
    itk::watershed::Relabeler<double, 3>::New();
  std::cout << "-------------WatershedRelabeler " << WatershedRelabelerObj;

  const itk::watershed::SegmentTable<double>::Pointer WatershedSegmentTableObj =
    itk::watershed::SegmentTable<double>::New();
  std::cout << "-------------WatershedSegmentTable " << WatershedSegmentTableObj;

  const itk::watershed::SegmentTree<double>::Pointer WatershedSegmentTreeObj =
    itk::watershed::SegmentTree<double>::New();
  std::cout << "-------------WatershedSegmentTree " << WatershedSegmentTreeObj;

  const itk::watershed::SegmentTreeGenerator<double>::Pointer WatershedSegmentTreeGeneratorObj =
    itk::watershed::SegmentTreeGenerator<double>::New();
  std::cout << "-------------WatershedSegmentTreeGenerator " << WatershedSegmentTreeGeneratorObj;

  const itk::watershed::Segmenter<InputType>::Pointer WatershedSegmenterObj =
    itk::watershed::Segmenter<InputType>::New();
  std::cout << "-------------WatershedSegmenter " << WatershedSegmenterObj;

  const itk::MRASlabIdentifier<InputType>::Pointer MRASlabIdentifierObj = itk::MRASlabIdentifier<InputType>::New();
  std::cout << "-------------MRASlabIdentifier " << MRASlabIdentifierObj;
  return EXIT_SUCCESS;
}
