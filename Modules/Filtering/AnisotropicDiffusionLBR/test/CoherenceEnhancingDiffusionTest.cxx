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

#include <iostream>
#include <fstream>
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkCoherenceEnhancingDiffusionImageFilter.h"
#include "itkTestingMacros.h"

namespace
{

using namespace itk;

int
Execute(int argc, char * argv[]);

template <int VDimension>
int
Execute(int argc, char * argv[], itk::ImageIOBase::IOComponentEnum, int nComponents);

template <int VDimension, typename ScalarType, typename ComponentType>
int
Execute(int argc, char * argv[], int nComponents);

template <int VDimension, typename ScalarType, typename PixelType, typename ExportPixelType>
int
Execute(int argc, char * argv[]);


inline int
Execute(int argc, char * argv[])
{
  using std::cerr;
  using std::endl;
  using namespace itk;

  if (argc < 3)
  {
    itkGenericExceptionMacro(
      "Usage: " << argv[0]
                << " <inputImage> <outputImage> [diffusionTime] [lambda] [enhancement] [noiseScale]"
                   " [featureScale] [exponent]\n"
                   "  inputImage:    Input image filename (2D or 3D). Required.\n"
                   "  outputImage:   Output image filename. Required.\n"
                   "  diffusionTime: Suggested range 0.5-5, up to 50 for strong noise. Default 2.\n"
                   "  lambda:        Small values detect more edges. Suggested range 0.0001-0.05. "
                   "Default 0.01.\n"
                   "  enhancement:   One of CED, cCED, EED, cEED, Isotropic. Default CED.\n"
                   "  noiseScale:    Suggested range 0.5-4. Default 1.\n"
                   "  featureScale:  Suggested range 2-6. Default 2.\n"
                   "  exponent:      Diffusion exponent.");
  }

  const char * imageFileName = argv[1];

  itk::ImageIOBase::Pointer imageIO = itk::ImageIOFactory::CreateImageIO(imageFileName, itk::IOFileModeEnum::ReadMode);
  if (imageIO.IsNull())
  {
    itkGenericExceptionMacro("Could not create ImageIO for file: " << imageFileName);
  }
  imageIO->SetFileName(imageFileName);
  imageIO->ReadImageInformation();

  const unsigned int                      imageDimension = imageIO->GetNumberOfDimensions();
  const itk::ImageIOBase::IOComponentEnum componentType = imageIO->GetComponentType();
  const unsigned int                      nComponents = imageIO->GetNumberOfComponents();

  switch (imageDimension)
  {
    case 2:
      return Execute<2>(argc, argv, componentType, nComponents);
    case 3:
      return Execute<3>(argc, argv, componentType, nComponents);
    default:
      itkGenericExceptionMacro("Sorry, unsupported image dimension.");
  }
}

template <int Dimension>
int
Execute(int argc, char * argv[], itk::IOComponentEnum componentType, int nComponents)
{
  switch (componentType)
  {
    case itk::IOComponentEnum::UCHAR:
      return Execute<Dimension, float, unsigned char>(argc, argv, nComponents);
    case itk::IOComponentEnum::FLOAT:
      return Execute<Dimension, float, float>(argc, argv, nComponents);
    case itk::IOComponentEnum::DOUBLE:
      return Execute<Dimension, double, double>(argc, argv, nComponents);
    default:
      itkGenericExceptionMacro("Sorry, unsupported component type");
  }
}

template <int Dimension, typename ScalarType, typename ComponentType>
int
Execute(int argc, char * argv[], int nComponents)
{
  switch (nComponents)
  {
    case 1:
      return Execute<Dimension, ScalarType, ScalarType, ComponentType>(argc, argv);
    case 2:
      return Execute<Dimension, ScalarType, Vector<ScalarType, 2>, Vector<ComponentType, 2>>(argc, argv);
    case 3:
      return Execute<Dimension, ScalarType, Vector<ScalarType, 3>, Vector<ComponentType, 3>>(argc, argv);
    default:
      itkGenericExceptionMacro("Sorry, unsupported number of components");
  }
}

template <int Dimension, typename ScalarType, typename PixelType, typename ExportPixelType>
int
Execute(int argc, char * argv[])
{
  using ImageType = Image<PixelType, Dimension>;

  using ReaderType = ImageFileReader<ImageType>;
  typename ReaderType::Pointer reader = ReaderType::New();

  const char * imageFileName = argv[1];
  const char * outputFileName = argv[2];
  reader->SetFileName(imageFileName);

  using DiffusionFilterType = CoherenceEnhancingDiffusionImageFilter<ImageType, ScalarType>;
  typename DiffusionFilterType::Pointer diffusionFilter = DiffusionFilterType::New();
  diffusionFilter->SetInput(reader->GetOutput());

  int argIndex = 3;
  if (argIndex < argc)
  {
    const double diffusionTime = std::stod(argv[argIndex++]);
    if (diffusionTime == 0)
      itkGenericExceptionMacro("Error: Unrecognized diffusion time (third argument).\n");
    diffusionFilter->SetDiffusionTime(diffusionTime);
  }

  if (argIndex < argc)
  {
    const double lambda = std::stod(argv[argIndex++]);
    if (lambda == 0.)
      itkGenericExceptionMacro("Error: Unrecognized lambda (fourth argument).\n");
    diffusionFilter->SetLambda(lambda);
  }

  if (argIndex < argc)
  {
    const char * enhancement = argv[argIndex++];
    if (!strcmp(enhancement, "EED"))
      diffusionFilter->SetEnhancement(DiffusionFilterType::EED); // Weickert's exponent : 4.
    else if (!strcmp(enhancement, "cEED"))
      diffusionFilter->SetEnhancement(DiffusionFilterType::cEED); // Weickert's exponent : 4.
    else if (!strcmp(enhancement, "CED"))
      diffusionFilter->SetEnhancement(DiffusionFilterType::CED); // Weickert's exponent : 2.
    else if (!strcmp(enhancement, "cCED"))
      diffusionFilter->SetEnhancement(DiffusionFilterType::cCED); // Weickert's exponent : 2.
    else if (!strcmp(enhancement, "Isotropic"))
      diffusionFilter->SetEnhancement(DiffusionFilterType::Isotropic); // Perona-Mali's exponent: 2.
    else
      itkGenericExceptionMacro("Error: Unrecognized enhancement (fifth argument).\n");
  }

  if (argIndex < argc)
  {
    const double noiseScale = std::stod(argv[argIndex++]);
    if (noiseScale == 0.)
      itkGenericExceptionMacro("Error: Unrecognized noiseScale (sixth argument).\n");
    diffusionFilter->SetNoiseScale(noiseScale);
  }

  if (argIndex < argc)
  {
    const double featureScale = std::stod(argv[argIndex++]);
    if (featureScale == 0.)
      itkGenericExceptionMacro("Error: Unrecognized featureScale (seventh argument).\n");
    diffusionFilter->SetFeatureScale(featureScale);
  }

  if (argIndex < argc)
  {
    const double exponent = std::stod(argv[argIndex++]);
    if (exponent == 0.)
      itkGenericExceptionMacro("Error: Unrecognized exponent (eighth argument).\n");
    diffusionFilter->SetExponent(exponent);
  }

  if (argIndex < argc)
  {
    itkGenericExceptionMacro("Error: excessive number of arguments");
  }

  using ExportImageType = Image<ExportPixelType, Dimension>;
  using CasterType = CastImageFilter<ImageType, ExportImageType>;
  typename CasterType::Pointer caster = CasterType::New();
  caster->SetInput(diffusionFilter->GetOutput());

  // using ScalarImageType = typename DiffusionFilterType::ScalarImageType;
  using WriterType = ImageFileWriter<ExportImageType>;
  typename WriterType::Pointer writer = WriterType::New();
  writer->SetInput(caster->GetOutput());
  writer->SetFileName(outputFileName);

  writer->Update();

  return EXIT_SUCCESS;
}

} // end anonymous namespace

int
CoherenceEnhancingDiffusionTest(int argc, char * argv[])
{
  ITK_TRY_EXPECT_NO_EXCEPTION(Execute(argc, argv));

  return EXIT_SUCCESS;
}
