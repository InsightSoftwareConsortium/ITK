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
#include "itkVectorMagnitudeImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkMesh.h"

#include "itkBSplineScatteredDataPointSetToGradientImageFilter.h"
#include "itkImageToPointSetFilter.h"
#include "itkImageToImageOfVectorsFilter.h"

int
itkBSplineScatteredDataPointSetToGradientImageFilterTest(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Usage: " << argv[0];
    std::cerr << " inputImage outputImage ";
    std::cerr << std::endl;
    return EXIT_FAILURE;
  }

  const unsigned int                       Dimension = 2;
  typedef float                            PixelType;
  typedef itk::Image<PixelType, Dimension> InputImageType;

  typedef itk::ImageFileReader<InputImageType> ReaderType;
  ReaderType::Pointer                          reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  typedef itk::ImageToImageOfVectorsFilter<InputImageType, 1> ImageToVectorImageFilterType;
  ImageToVectorImageFilterType::Pointer                       imageToVI = ImageToVectorImageFilterType::New();
  imageToVI->SetInput(0, reader->GetOutput());
  typedef ImageToVectorImageFilterType::OutputImageType VectorImageType;
  try
  {
    reader->UpdateOutputInformation();
  }
  catch (itk::ExceptionObject & ex)
  {
    std::cerr << "Exception caught!" << std::endl;
    std::cerr << ex << std::endl;
    return EXIT_FAILURE;
  }


  typedef itk::Mesh<VectorImageType::PixelType, Dimension> MeshType;
  typedef InputImageType                                   OutputImageType;

  typedef itk::ImageToPointSetFilter<VectorImageType, MeshType> ImageToPointSetType;
  ImageToPointSetType::Pointer                                  imageToPointSet = ImageToPointSetType::New();
  imageToPointSet->SetInput(0, imageToVI->GetOutput());

  typedef itk::BSplineScatteredDataPointSetToGradientImageFilter<MeshType, PixelType> PointSetToGradientFilterType;
  PointSetToGradientFilterType::Pointer pointSetToGradient = PointSetToGradientFilterType::New();
  pointSetToGradient->SetInput(imageToPointSet->GetOutput());
  pointSetToGradient->SetOrigin(reader->GetOutput()->GetOrigin());
  pointSetToGradient->SetSpacing(reader->GetOutput()->GetSpacing());
  pointSetToGradient->SetSize(reader->GetOutput()->GetLargestPossibleRegion().GetSize());
  PointSetToGradientFilterType::ArrayType ncps;
  ncps.Fill(16);
  pointSetToGradient->SetNumberOfControlPoints(ncps);
  pointSetToGradient->SetNumberOfLevels(3);

  typedef itk::VectorMagnitudeImageFilter<PointSetToGradientFilterType::OutputImageType, OutputImageType>
                                       GradientMagnitudeFilterType;
  GradientMagnitudeFilterType::Pointer gradientMagnitude = GradientMagnitudeFilterType::New();
  gradientMagnitude->SetInput(pointSetToGradient->GetOutput());

  typedef itk::ImageFileWriter<OutputImageType> WriterType;
  WriterType::Pointer                           writer = WriterType::New();
  writer->SetInput(gradientMagnitude->GetOutput());
  writer->SetFileName(argv[2]);

  try
  {
    writer->Update();
  }
  catch (itk::ExceptionObject & ex)
  {
    std::cerr << "Exception caught!" << std::endl;
    std::cerr << ex << std::endl;
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
