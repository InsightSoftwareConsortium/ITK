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
#include "itkImageFileReader.h"


#include "itkShapeKeepNObjectsLabelMapFilter.h"
#include "itkLabelImageToShapeLabelMapFilter.h"
#include "itkMath.h"
#include "itkTestingMacros.h"


int
itkShapeLabelObjectAccessorsTest1(int argc, char * argv[])
{
  if (argc != 2)
  {
    std::cerr << "Missing parameters." << '\n';
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " input ";
    std::cerr << '\n';
    return EXIT_FAILURE;
  }

  constexpr unsigned int dim = 3;

  using PixelType = unsigned char;
  using ImageType = itk::Image<PixelType, dim>;
  using ShapeLabelObjectType = itk::ShapeLabelObject<PixelType, dim>;
  using LabelMapType = itk::LabelMap<ShapeLabelObjectType>;
  using ReaderType = itk::ImageFileReader<ImageType>;

  // Exercise the attribute translation code and verify that
  // translations are correct
  int status = EXIT_SUCCESS;

  const std::vector<std::string> attributes{ "Label",
                                             "NumberOfPixels",
                                             "PhysicalSize",
                                             "Centroid",
                                             "BoundingBox",
                                             "NumberOfPixelsOnBorder",
                                             "PerimeterOnBorder",
                                             "FeretDiameter",
                                             "PrincipalMoments",
                                             "PrincipalAxes",
                                             "Elongation",
                                             "Perimeter",
                                             "Roundness",
                                             "EquivalentSphericalRadius",
                                             "EquivalentSphericalPerimeter",
                                             "EquivalentEllipsoidDiameter",
                                             "Flatness",
                                             "PerimeterOnBorderRatio",
                                             "OrientedBoundingBoxSize",
                                             "OrientedBoundingBoxOrigin" };

  for (const auto & attribute : attributes)
  {
    if (ShapeLabelObjectType::GetNameFromAttribute(ShapeLabelObjectType::GetAttributeFromName(attribute)) != attribute)
    {
      std::cout << "Attribute translation for " << attribute << " failed." << '\n';
      std::cout << "   Received "
                << ShapeLabelObjectType::GetNameFromAttribute(ShapeLabelObjectType::GetAttributeFromName(attribute))
                << " but expected " << attribute << '\n';
      status = EXIT_FAILURE;
    }
  }
  auto reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  using I2LType = itk::LabelImageToShapeLabelMapFilter<ImageType, LabelMapType>;
  auto i2l = I2LType::New();
  i2l->SetInput(reader->GetOutput());
  i2l->SetComputePerimeter(true);
  i2l->SetComputeOrientedBoundingBox(true);
  i2l->Update();

  LabelMapType * labelMap = i2l->GetOutput();
  std::cout << "File " << argv[1] << " has " << labelMap->GetNumberOfLabelObjects() << " labels." << '\n';

  // Retrieve all attributes
  for (unsigned int n = 0; n < labelMap->GetNumberOfLabelObjects(); ++n)
  {
    ShapeLabelObjectType * labelObject = labelMap->GetNthLabelObject(n);
    std::cout << "Label: " << itk::NumericTraits<LabelMapType::LabelType>::PrintType(labelObject->GetLabel()) << '\n';
    std::cout << "    BoundingBox: " << labelObject->GetBoundingBox() << '\n';
    std::cout << "    NumberOfPixels: " << labelObject->GetNumberOfPixels() << '\n';
    std::cout << "    PhysicalSize: " << labelObject->GetPhysicalSize() << '\n';
    std::cout << "    Centroid: " << labelObject->GetCentroid() << '\n';
    std::cout << "    NumberOfPixelsOnBorder: " << labelObject->GetNumberOfPixelsOnBorder() << '\n';
    std::cout << "    PerimeterOnBorder: " << labelObject->GetPerimeterOnBorder() << '\n';
    std::cout << "    FeretDiameter: " << labelObject->GetFeretDiameter() << '\n';
    std::cout << "    PrincipalMoments: " << labelObject->GetPrincipalMoments() << '\n';
    std::cout << "    PrincipalAxes: " << labelObject->GetPrincipalAxes() << '\n';
    std::cout << "    Elongation: " << labelObject->GetElongation() << '\n';
    std::cout << "    Perimeter: " << labelObject->GetPerimeter() << '\n';
    std::cout << "    Roundness: " << labelObject->GetRoundness() << '\n';
    std::cout << "    EquivalentSphericalRadius: " << labelObject->GetEquivalentSphericalRadius() << '\n';
    std::cout << "    EquivalentSphericalPerimeter: " << labelObject->GetEquivalentSphericalPerimeter() << '\n';
    std::cout << "    EquivalentEllipsoidDiameter: " << labelObject->GetEquivalentEllipsoidDiameter() << '\n';
    std::cout << "    Flatness: " << labelObject->GetFlatness() << '\n';
    std::cout << "    PerimeterOnBorderRatio: " << labelObject->GetPerimeterOnBorderRatio() << '\n';
    std::cout << "    OrientedBoundingBoxSize: " << labelObject->GetOrientedBoundingBoxSize() << '\n';
    std::cout << "    OrientedBoundingBoxOrigin: " << labelObject->GetOrientedBoundingBoxOrigin() << '\n';
  }
  for (unsigned int n = 0; n < labelMap->GetNumberOfLabelObjects(); ++n)
  {
    ShapeLabelObjectType * labelCopy = labelMap->GetNthLabelObject(n);
    ShapeLabelObjectType * labelObject = labelMap->GetNthLabelObject(0);
    labelObject->CopyAttributesFrom<ShapeLabelObjectType>(labelCopy);
    if (labelCopy->GetLabel() != labelObject->GetLabel())
    {
      std::cout << "CopyAttributesFrom failed for attribute: "
                << "Label" << '\n';
      status = EXIT_FAILURE;
    }
    if (labelCopy->GetBoundingBox() != labelObject->GetBoundingBox())
    {
      std::cout << "CopyAttributeFrom failed for attribute "
                << "BoundingBox" << '\n';
      status = EXIT_FAILURE;
    }
    if (labelCopy->GetNumberOfPixels() != labelObject->GetNumberOfPixels())
    {
      std::cout << "CopyAttributeFrom failed for attribute "
                << "NumberOfPixels" << '\n';
      status = EXIT_FAILURE;
    }
    if (itk::Math::NotExactlyEquals(labelCopy->GetPhysicalSize(), labelObject->GetPhysicalSize()))
    {
      std::cout << "CopyAttributeFrom failed for attribute "
                << "PhysicalSize" << '\n';
      status = EXIT_FAILURE;
    }
    if (labelCopy->GetCentroid() != labelObject->GetCentroid())
    {
      std::cout << "CopyAttributeFrom failed for attribute "
                << "Centroid" << '\n';
      status = EXIT_FAILURE;
    }
    if (labelCopy->GetNumberOfPixelsOnBorder() != labelObject->GetNumberOfPixelsOnBorder())
    {
      std::cout << "CopyAttributeFrom failed for attribute "
                << "NumberOfPixelsOnBorder" << '\n';
      status = EXIT_FAILURE;
    }
    if (itk::Math::NotExactlyEquals(labelCopy->GetPerimeterOnBorder(), labelObject->GetPerimeterOnBorder()))
    {
      std::cout << "CopyAttributeFrom failed for attribute "
                << "PerimeterOnBorder" << '\n';
      status = EXIT_FAILURE;
    }
    if (itk::Math::NotExactlyEquals(labelCopy->GetFeretDiameter(), labelObject->GetFeretDiameter()))
    {
      std::cout << "CopyAttributeFrom failed for attribute "
                << "FeretDiameter" << '\n';
      status = EXIT_FAILURE;
    }
    if (labelCopy->GetPrincipalMoments() != labelObject->GetPrincipalMoments())
    {
      std::cout << "CopyAttributeFrom failed for attribute "
                << "PrincipalMoments" << '\n';
      status = EXIT_FAILURE;
    }
    if (labelCopy->GetPrincipalAxes() != labelObject->GetPrincipalAxes())
    {
      std::cout << "CopyAttributeFrom failed for attribute "
                << "PrincipalAxes" << '\n';
      status = EXIT_FAILURE;
    }
    if (itk::Math::NotExactlyEquals(labelCopy->GetElongation(), labelObject->GetElongation()))
    {
      std::cout << "CopyAttributeFrom failed for attribute "
                << "Elongation" << '\n';
      status = EXIT_FAILURE;
    }
    if (itk::Math::NotExactlyEquals(labelCopy->GetPerimeter(), labelObject->GetPerimeter()))
    {
      std::cout << "CopyAttributeFrom failed for attribute "
                << "Perimeter" << '\n';
      status = EXIT_FAILURE;
    }
    if (itk::Math::NotExactlyEquals(labelCopy->GetRoundness(), labelObject->GetRoundness()))
    {
      std::cout << "CopyAttributeFrom failed for attribute "
                << "Roundness" << '\n';
      status = EXIT_FAILURE;
    }
    if (itk::Math::NotExactlyEquals(labelCopy->GetEquivalentSphericalRadius(),
                                    labelObject->GetEquivalentSphericalRadius()))
    {
      std::cout << "CopyAttributeFrom failed for attribute "
                << "EquivalentSphericalRadius" << '\n';
      status = EXIT_FAILURE;
    }
    if (itk::Math::NotExactlyEquals(labelCopy->GetEquivalentSphericalPerimeter(),
                                    labelObject->GetEquivalentSphericalPerimeter()))
    {
      std::cout << "CopyAttributeFrom failed for attribute "
                << "EquivalentSphericalPerimeter" << '\n';
      status = EXIT_FAILURE;
    }
    if (labelCopy->GetEquivalentEllipsoidDiameter() != labelObject->GetEquivalentEllipsoidDiameter())
    {
      std::cout << "CopyAttributeFrom failed for attribute "
                << "EquivalentEllipsoidDiameter" << '\n';
      status = EXIT_FAILURE;
    }
    if (itk::Math::NotExactlyEquals(labelCopy->GetFlatness(), labelObject->GetFlatness()))
    {
      std::cout << "CopyAttributeFrom failed for attribute "
                << "Flatness" << '\n';
      status = EXIT_FAILURE;
    }
    if (itk::Math::NotExactlyEquals(labelCopy->GetPerimeterOnBorderRatio(), labelObject->GetPerimeterOnBorderRatio()))
    {
      std::cout << "CopyAttributeFrom failed for attribute "
                << "PerimeterOnBorderRatio" << '\n';
      status = EXIT_FAILURE;
    }
    if (labelCopy->GetOrientedBoundingBoxSize() != labelObject->GetOrientedBoundingBoxSize())
    {
      std::cout << "CopyAttributeFrom failed for attribute "
                << "OrientedBoundingBoxSize" << '\n';
      status = EXIT_FAILURE;
    }
    if (labelCopy->GetOrientedBoundingBoxOrigin() != labelObject->GetOrientedBoundingBoxOrigin())
    {
      std::cout << "CopyAttributeFrom failed for attribute "
                << "OrientedBoundingBoxOrigin" << '\n';
      status = EXIT_FAILURE;
    }
  }
  // Check that the accessors match the Get's

  for (unsigned int n = 0; n < labelMap->GetNumberOfLabelObjects(); ++n)
  {
    const itk::Functor::LabelLabelObjectAccessor<ShapeLabelObjectType> accessorLabel;
    ShapeLabelObjectType *                                             l = labelMap->GetNthLabelObject(n);
    if (l->GetLabel() != accessorLabel(l))
    {
      std::cout << "l->GetLabel2() != accessorLabel(l)" << '\n';
      status = EXIT_FAILURE;
    }
    const itk::Functor::BoundingBoxLabelObjectAccessor<ShapeLabelObjectType> accessorBoundingBox;
    if (l->GetBoundingBox() != accessorBoundingBox(l))
    {
      std::cout << "l->GetBoundingBox() != accessorBoundingBox(l)" << '\n';
      status = EXIT_FAILURE;
    }
    const itk::Functor::NumberOfPixelsLabelObjectAccessor<ShapeLabelObjectType> accessorSize;
    if (l->GetNumberOfPixels() != accessorSize(l))
    {
      std::cout << "l->GetNumberOfPixels() != accessorSize(l)" << '\n';
      status = EXIT_FAILURE;
    }
    const itk::Functor::PhysicalSizeLabelObjectAccessor<ShapeLabelObjectType> accessorPhysicalSize;
    if (itk::Math::NotExactlyEquals(l->GetPhysicalSize(), accessorPhysicalSize(l)))
    {
      std::cout << "l->GetPhysicalSize() != accessorPhysicalSize(l)" << '\n';
      status = EXIT_FAILURE;
    }
    const itk::Functor::CentroidLabelObjectAccessor<ShapeLabelObjectType> accessorCentroid;
    if (l->GetCentroid() != accessorCentroid(l))
    {
      std::cout << "l->GetCentroid() != accessorCentroid(l)" << '\n';
      status = EXIT_FAILURE;
    }
    const itk::Functor::NumberOfPixelsOnBorderLabelObjectAccessor<ShapeLabelObjectType> accessorSizeOnBorder;
    if (l->GetNumberOfPixelsOnBorder() != accessorSizeOnBorder(l))
    {
      std::cout << "l->GetNumberOfPixelsOnBorder() != accessorSizeOnBorder(l)" << '\n';
      status = EXIT_FAILURE;
    }
    const itk::Functor::PerimeterOnBorderLabelObjectAccessor<ShapeLabelObjectType> accessorPerimeterOnBorder;
    if (itk::Math::NotExactlyEquals(l->GetPerimeterOnBorder(), accessorPerimeterOnBorder(l)))
    {
      std::cout << "l->GetPerimeterOnBorder() != accessorPerimeterOnBorder(l)" << '\n';
      status = EXIT_FAILURE;
    }
    const itk::Functor::FeretDiameterLabelObjectAccessor<ShapeLabelObjectType> accessorFeretDiameter;
    if (itk::Math::NotExactlyEquals(l->GetFeretDiameter(), accessorFeretDiameter(l)))
    {
      std::cout << "l->GetFeretDiameter() != accessorFeretDiameter(l)" << '\n';
      status = EXIT_FAILURE;
    }
    const itk::Functor::PrincipalMomentsLabelObjectAccessor<ShapeLabelObjectType> accessorPrincipalMoments;
    if (l->GetPrincipalMoments() != accessorPrincipalMoments(l))
    {
      std::cout << "l->GetPrincipalMoments() != accessorPrincipalMoments(l)" << '\n';
      status = EXIT_FAILURE;
    }
    const itk::Functor::PrincipalAxesLabelObjectAccessor<ShapeLabelObjectType> accessorPrincipalAxes;
    if (l->GetPrincipalAxes() != accessorPrincipalAxes(l))
    {
      std::cout << "l->GetPrincipalAxes() != accessorPrincipalAxes(l)" << '\n';
      status = EXIT_FAILURE;
    }
    const itk::Functor::ElongationLabelObjectAccessor<ShapeLabelObjectType> accessorElongation;
    if (itk::Math::NotExactlyEquals(l->GetElongation(), accessorElongation(l)))
    {
      std::cout << "l->GetElongation() != accessorElongation(l)" << '\n';
      status = EXIT_FAILURE;
    }
    const itk::Functor::PerimeterLabelObjectAccessor<ShapeLabelObjectType> accessorPerimeter;
    if (itk::Math::NotExactlyEquals(l->GetPerimeter(), accessorPerimeter(l)))
    {
      std::cout << "l->GetPerimeter() != accessorPerimeter(l)" << '\n';
      status = EXIT_FAILURE;
    }
    const itk::Functor::RoundnessLabelObjectAccessor<ShapeLabelObjectType> accessorRoundness;
    if (itk::Math::NotExactlyEquals(l->GetRoundness(), accessorRoundness(l)))
    {
      std::cout << "l->GetRoundness() != accessorRoundness(l)" << '\n';
      status = EXIT_FAILURE;
    }
    const itk::Functor::EquivalentSphericalRadiusLabelObjectAccessor<ShapeLabelObjectType>
      accessorEquivalentSphericalRadius;
    if (itk::Math::NotExactlyEquals(l->GetEquivalentSphericalRadius(), accessorEquivalentSphericalRadius(l)))
    {
      std::cout << "l->GetEquivalentSphericalRadius() != accessorEquivalentSphericalRadius(l)" << '\n';
      status = EXIT_FAILURE;
    }
    const itk::Functor::EquivalentSphericalPerimeterLabelObjectAccessor<ShapeLabelObjectType>
      accessorEquivalentSphericalPerimeter;
    if (itk::Math::NotExactlyEquals(l->GetEquivalentSphericalPerimeter(), accessorEquivalentSphericalPerimeter(l)))
    {
      std::cout << "l->GetEquivalentSphericalPerimeter() != accessorEquivalentSphericalPerimeter(l)" << '\n';
      status = EXIT_FAILURE;
    }
    const itk::Functor::EquivalentEllipsoidDiameterLabelObjectAccessor<ShapeLabelObjectType>
      accessorEquivalentEllipsoidDiameter;
    if (l->GetEquivalentEllipsoidDiameter() != accessorEquivalentEllipsoidDiameter(l))
    {
      std::cout << "l->GetEquivalentEllipsoidDiameter() != accessorEquivalentEllipsoidDiameter(l)" << '\n';
      status = EXIT_FAILURE;
    }
    const itk::Functor::FlatnessLabelObjectAccessor<ShapeLabelObjectType> accessorFlatness;
    if (itk::Math::NotExactlyEquals(l->GetFlatness(), accessorFlatness(l)))
    {
      std::cout << "l->GetFlatness() != accessorFlatness(l)" << '\n';
      status = EXIT_FAILURE;
    }
    const itk::Functor::PerimeterOnBorderRatioLabelObjectAccessor<ShapeLabelObjectType> accessorPerimeterOnBorderRatio;
    if (itk::Math::NotExactlyEquals(l->GetPerimeterOnBorderRatio(), accessorPerimeterOnBorderRatio(l)))
    {
      std::cout << "l->GetPerimeterOnBorderRatio() != accessorPerimeterOnBorderRatio(l)" << '\n';
      status = EXIT_FAILURE;
    }
  }

  // Cover PrintSelf
  for (unsigned int n = 0; n < labelMap->GetNumberOfLabelObjects(); ++n)
  {
    ShapeLabelObjectType * l = labelMap->GetNthLabelObject(n);
    std::cout << "Print ShapeLabelObject " << n << '\n';
    l->Print(std::cout);
  }

  // Check transforms
  for (unsigned int n = 0; n < labelMap->GetNumberOfLabelObjects(); ++n)
  {
    ShapeLabelObjectType *                             l = labelMap->GetNthLabelObject(n);
    const ShapeLabelObjectType::AffineTransformPointer principleToPhysical =
      l->GetPrincipalAxesToPhysicalAxesTransform();
    std::cout << "Print principleToPhysical " << n << '\n';
    principleToPhysical->Print(std::cout);

    const ShapeLabelObjectType::AffineTransformPointer physicalToPrinciple =
      l->GetPhysicalAxesToPrincipalAxesTransform();
    std::cout << "Print physicalToPrinciple " << n << '\n';
    physicalToPrinciple->Print(std::cout);
  }
  return status;
}
