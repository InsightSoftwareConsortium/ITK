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

#include "itkContourExtractor2DImageFilter.h"
#include "itkLabelToRGBImageFilter.h"
#include "itkLabelOverlayImageFilter.h"
#include "itkRGBPixel.h"
#include "itkQuadEdgeMesh.h"

#include "itkValuedRegionalMinimaImageFilter.h"
#include "itkValuedRegionalMaximaImageFilter.h"
#include "itkRegionalMaximaImageFilter.h"
#include "itkRegionalMinimaImageFilter.h"

#include "itkConformalFlatteningMeshFilter.h"

#include "itkVTKPolyDataReader.h"
#include "itkVTKPolyDataWriter.h"

#include "itkImageKernelOperator.h"

#include "itkHessianToObjectnessMeasureImageFilter.h"
#include "itkMultiScaleHessianBasedMeasureImageFilter.h"

int
itkReviewPrintTest(int, char *[])
{
  using Input2DImageType = itk::Image<float, 2>;
  using CharType = itk::Image<unsigned char, 2>;
  using RGBPixelType = itk::RGBPixel<unsigned char>;
  using RGBImageType = itk::Image<RGBPixelType, 2>;

  using VectorType = itk::Vector<float, 2>;
  using VectorImageType = itk::Image<VectorType, 2>;

  using QuadEdgeMeshType = itk::QuadEdgeMesh<double, 3>;

  using PixelType = unsigned short;
  using MeshType = itk::Mesh<float, 3>;

  using RealPixelType = itk::NumericTraits<PixelType>::RealType;
  using HessianPixelType = itk::SymmetricSecondRankTensor<RealPixelType, 3>;
  using HessianImageType = itk::Image<HessianPixelType, 3>;
  using Input3DImageType = itk::Image<PixelType, 3>;
  using OutputImageType = itk::Image<PixelType, 3>;

  // Dummy variable just to force the full instantiation of the class
  auto dummyImage = CharType::New();
  auto dummy2 = VectorImageType::New();

  itk::ContourExtractor2DImageFilter<Input2DImageType>::Pointer ContourExtractor2DImageFilterObj =
    itk::ContourExtractor2DImageFilter<Input2DImageType>::New();
  std::cout << "-------------ContourExtractor2DImageFilter " << ContourExtractor2DImageFilterObj;

  itk::LabelToRGBImageFilter<CharType, RGBImageType>::Pointer LabelToRGBImageFilterObj =
    itk::LabelToRGBImageFilter<CharType, RGBImageType>::New();
  std::cout << "-------------LabelToRGBImageFilter " << LabelToRGBImageFilterObj;

  itk::LabelOverlayImageFilter<Input2DImageType, CharType, RGBImageType>::Pointer LabelOverlayImageFilterObj =
    itk::LabelOverlayImageFilter<Input2DImageType, CharType, RGBImageType>::New();
  std::cout << "-------------LabelOverlayImageFilter " << LabelOverlayImageFilterObj;

  auto QuadEdgeMeshObj = QuadEdgeMeshType::New();
  std::cout << "-------------QuadEdgeMesh " << QuadEdgeMeshObj;

  itk::ValuedRegionalMaximaImageFilter<Input2DImageType, Input2DImageType>::Pointer ValuedRegionalMaximaImageFilterObj =
    itk::ValuedRegionalMaximaImageFilter<Input2DImageType, Input2DImageType>::New();
  std::cout << "-------------ValuedRegionalMaximaImageFilterObj " << ValuedRegionalMaximaImageFilterObj;

  itk::ValuedRegionalMinimaImageFilter<Input2DImageType, Input2DImageType>::Pointer ValuedRegionalMinimaImageFilterObj =
    itk::ValuedRegionalMinimaImageFilter<Input2DImageType, Input2DImageType>::New();
  std::cout << "-------------ValuedRegionalMinimaImageFilterObj " << ValuedRegionalMinimaImageFilterObj;

  itk::RegionalMaximaImageFilter<Input2DImageType, Input2DImageType>::Pointer RegionalMaximaImageFilterObj =
    itk::RegionalMaximaImageFilter<Input2DImageType, Input2DImageType>::New();
  std::cout << "-------------RegionalMaximaImageFilterObj " << RegionalMaximaImageFilterObj;

  itk::RegionalMinimaImageFilter<Input2DImageType, Input2DImageType>::Pointer RegionalMinimaImageFilterObj =
    itk::RegionalMinimaImageFilter<Input2DImageType, Input2DImageType>::New();
  std::cout << "-------------RegionalMinimaImageFilterObj " << RegionalMinimaImageFilterObj;

  itk::ConformalFlatteningMeshFilter<MeshType, MeshType>::Pointer ConformalFlatteningMeshFilterObj =
    itk::ConformalFlatteningMeshFilter<MeshType, MeshType>::New();
  std::cout << "--------------ConformalFlatteningMeshFilterObj " << ConformalFlatteningMeshFilterObj;

  itk::VTKPolyDataReader<MeshType>::Pointer VTKPolyDataReaderObj = itk::VTKPolyDataReader<MeshType>::New();
  std::cout << "--------------VTKPolyDataReaderObj " << VTKPolyDataReaderObj;

  itk::VTKPolyDataWriter<MeshType>::Pointer VTKPolyDataWriterObj = itk::VTKPolyDataWriter<MeshType>::New();
  std::cout << "--------------VTKPolyDataWriterObj " << VTKPolyDataWriterObj;

  itk::ImageKernelOperator<float> kernelOperator;
  std::cout << "--------------ImageKernelOperatorObj ";
  kernelOperator.Print(std::cout);

  itk::HessianToObjectnessMeasureImageFilter<HessianImageType, OutputImageType>::Pointer ObjectnessFilterObject =
    itk::HessianToObjectnessMeasureImageFilter<HessianImageType, OutputImageType>::New();

  std::cout << "---------------------------------ObjectnessFilterObject " << ObjectnessFilterObject;

  itk::MultiScaleHessianBasedMeasureImageFilter<Input3DImageType, HessianImageType, OutputImageType>::Pointer
    MultiScaleHessianFilter =
      itk::MultiScaleHessianBasedMeasureImageFilter<Input3DImageType, HessianImageType, OutputImageType>::New();

  return EXIT_SUCCESS;
}
