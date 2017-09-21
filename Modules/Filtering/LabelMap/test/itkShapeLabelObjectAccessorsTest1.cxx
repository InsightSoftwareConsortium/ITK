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
#include "itkImageFileReader.h"


#include "itkShapeKeepNObjectsLabelMapFilter.h"
#include "itkLabelImageToShapeLabelMapFilter.h"
#include "itkMath.h"


int itkShapeLabelObjectAccessorsTest1(int argc, char * argv[])
{
  if( argc != 2 )
    {
    std::cerr << "Usage: " << argv[0];
    std::cerr << " input ";
    std::cerr << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int dim = 3;

  typedef unsigned char                           PixelType;
  typedef itk::Image< PixelType, dim >            ImageType;
  typedef itk::ShapeLabelObject< PixelType, dim > ShapeLabelObjectType;
  typedef itk::LabelMap< ShapeLabelObjectType >   LabelMapType;
  typedef itk::ImageFileReader< ImageType >       ReaderType;

  // Exercise the attribute translation code and verify that
  // translations are correct
  int status = EXIT_SUCCESS;

  std::vector<std::string> attributes;
  attributes.push_back("Label");
  attributes.push_back("NumberOfPixels");
  attributes.push_back("PhysicalSize");
  attributes.push_back("Centroid");
  attributes.push_back("BoundingBox");
  attributes.push_back("NumberOfPixelsOnBorder");
  attributes.push_back("PerimeterOnBorder");
  attributes.push_back("FeretDiameter");
  attributes.push_back("PrincipalMoments");
  attributes.push_back("PrincipalAxes");
  attributes.push_back("Elongation");
  attributes.push_back("Perimeter");
  attributes.push_back("Roundness");
  attributes.push_back("EquivalentSphericalRadius");
  attributes.push_back("EquivalentSphericalPerimeter");
  attributes.push_back("EquivalentEllipsoidDiameter");
  attributes.push_back("Flatness");
  attributes.push_back("PerimeterOnBorderRatio");
  attributes.push_back("OrientedBoundingBoxSize");
  attributes.push_back("OrientedBoundingBoxOrigin");
  for (size_t a = 0; a < attributes.size(); a++)
    {
    if (ShapeLabelObjectType::GetNameFromAttribute(ShapeLabelObjectType::GetAttributeFromName(attributes[a])) != attributes[a])
      {
      std::cout << "Attribute translation for " << attributes[a] << " failed." << std::endl;
      std::cout << "   Received " << ShapeLabelObjectType::GetNameFromAttribute(ShapeLabelObjectType::GetAttributeFromName(attributes[a])) << " but expected " << attributes[a] << std::endl;
      status = EXIT_FAILURE;
      }
    }
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  typedef itk::LabelImageToShapeLabelMapFilter< ImageType, LabelMapType> I2LType;
  I2LType::Pointer i2l = I2LType::New();
  i2l->SetInput( reader->GetOutput() );
  i2l->SetComputePerimeter(true);
  i2l->SetComputeOrientedBoundingBox(true);
  i2l->Update();

  LabelMapType *labelMap = i2l->GetOutput();
  std::cout << "File " << argv[1] << " has " << labelMap->GetNumberOfLabelObjects() << " labels." << std::endl;

  // Retrieve all attributes
  for (unsigned int n = 0; n < labelMap->GetNumberOfLabelObjects(); n++)
    {
    ShapeLabelObjectType *labelObject = labelMap->GetNthLabelObject(n);
    std::cout << "Label: "
              << itk::NumericTraits<LabelMapType::LabelType>::PrintType(labelObject->GetLabel()) << std::endl;
    std::cout << "    BoundingBox: "
              << labelObject->GetBoundingBox() << std::endl;
    std::cout << "    NumberOfPixels: "
              << labelObject->GetNumberOfPixels() << std::endl;
    std::cout << "    PhysicalSize: "
              << labelObject->GetPhysicalSize() << std::endl;
    std::cout << "    Centroid: "
              << labelObject->GetCentroid() << std::endl;
    std::cout << "    NumberOfPixelsOnBorder: "
              << labelObject->GetNumberOfPixelsOnBorder() << std::endl;
    std::cout << "    PerimeterOnBorder: "
              << labelObject->GetPerimeterOnBorder() << std::endl;
    std::cout << "    FeretDiameter: "
              << labelObject->GetFeretDiameter() << std::endl;
    std::cout << "    PrincipalMoments: "
              << labelObject->GetPrincipalMoments() << std::endl;
    std::cout << "    PrincipalAxes: "
              << labelObject->GetPrincipalAxes() << std::endl;
    std::cout << "    Elongation: "
              << labelObject->GetElongation() << std::endl;
    std::cout << "    Perimeter: "
              << labelObject->GetPerimeter() << std::endl;
    std::cout << "    Roundness: "
              << labelObject->GetRoundness() << std::endl;
    std::cout << "    EquivalentSphericalRadius: "
              << labelObject->GetEquivalentSphericalRadius() << std::endl;
    std::cout << "    EquivalentSphericalPerimeter: "
              << labelObject->GetEquivalentSphericalPerimeter() << std::endl;
    std::cout << "    EquivalentEllipsoidDiameter: "
              << labelObject->GetEquivalentEllipsoidDiameter() << std::endl;
    std::cout << "    Flatness: "
              << labelObject->GetFlatness() << std::endl;
    std::cout << "    PerimeterOnBorderRatio: "
              << labelObject->GetPerimeterOnBorderRatio() << std::endl;
    std::cout << "    OrientedBoundingBoxSize: "
              << labelObject->GetOrientedBoundingBoxSize() << std::endl;
    std::cout << "    OrientedBoundingBoxOrigin: "
              << labelObject->GetOrientedBoundingBoxOrigin() << std::endl;

    }
  for (unsigned int n = 0; n < labelMap->GetNumberOfLabelObjects(); n++)
    {
    ShapeLabelObjectType *labelCopy = labelMap->GetNthLabelObject(n);
    ShapeLabelObjectType *labelObject = labelMap->GetNthLabelObject(0);
    labelObject->CopyAttributesFrom<ShapeLabelObjectType>(labelCopy);
    if (labelCopy->GetLabel() != labelObject->GetLabel())
      {
      std::cout << "CopyAttributesFrom failed for attribute: " << "Label" << std::endl;
      status = EXIT_FAILURE;
      }
    if (labelCopy->GetBoundingBox() != labelObject->GetBoundingBox())
      {
      std::cout << "CopyAttributeFrom failed for attribute " << "BoundingBox" << std::endl;
      status = EXIT_FAILURE;
      }
    if (labelCopy->GetNumberOfPixels() != labelObject->GetNumberOfPixels())
      {
      std::cout << "CopyAttributeFrom failed for attribute " << "NumberOfPixels" << std::endl;
      status = EXIT_FAILURE;
      }
    if (itk::Math::NotExactlyEquals(labelCopy->GetPhysicalSize(), labelObject->GetPhysicalSize()))
      {
      std::cout << "CopyAttributeFrom failed for attribute " << "PhysicalSize" << std::endl;
      status = EXIT_FAILURE;
      }
    if (labelCopy->GetCentroid() != labelObject->GetCentroid())
      {
      std::cout << "CopyAttributeFrom failed for attribute " << "Centroid" << std::endl;
      status = EXIT_FAILURE;
      }
    if (labelCopy->GetNumberOfPixelsOnBorder() != labelObject->GetNumberOfPixelsOnBorder())
      {
      std::cout << "CopyAttributeFrom failed for attribute " << "NumberOfPixelsOnBorder" << std::endl;
      status = EXIT_FAILURE;
      }
    if (itk::Math::NotExactlyEquals(labelCopy->GetPerimeterOnBorder(), labelObject->GetPerimeterOnBorder()))
      {
      std::cout << "CopyAttributeFrom failed for attribute " << "PerimeterOnBorder" << std::endl;
      status = EXIT_FAILURE;
      }
    if (itk::Math::NotExactlyEquals(labelCopy->GetFeretDiameter(), labelObject->GetFeretDiameter()))
      {
      std::cout << "CopyAttributeFrom failed for attribute " << "FeretDiameter" << std::endl;
      status = EXIT_FAILURE;
      }
    if (labelCopy->GetPrincipalMoments() != labelObject->GetPrincipalMoments())
      {
      std::cout << "CopyAttributeFrom failed for attribute " << "PrincipalMoments" << std::endl;
      status = EXIT_FAILURE;
      }
    if (labelCopy->GetPrincipalAxes() != labelObject->GetPrincipalAxes())
      {
      std::cout << "CopyAttributeFrom failed for attribute " << "PrincipalAxes" << std::endl;
      status = EXIT_FAILURE;
      }
    if (itk::Math::NotExactlyEquals(labelCopy->GetElongation(), labelObject->GetElongation()))
      {
      std::cout << "CopyAttributeFrom failed for attribute " << "Elongation" << std::endl;
      status = EXIT_FAILURE;
      }
    if (itk::Math::NotExactlyEquals(labelCopy->GetPerimeter(), labelObject->GetPerimeter()))
      {
      std::cout << "CopyAttributeFrom failed for attribute " << "Perimeter" << std::endl;
      status = EXIT_FAILURE;
      }
    if (itk::Math::NotExactlyEquals(labelCopy->GetRoundness(), labelObject->GetRoundness()))
      {
      std::cout << "CopyAttributeFrom failed for attribute " << "Roundness" << std::endl;
      status = EXIT_FAILURE;
      }
    if (itk::Math::NotExactlyEquals(labelCopy->GetEquivalentSphericalRadius(), labelObject->GetEquivalentSphericalRadius()))
      {
      std::cout << "CopyAttributeFrom failed for attribute " << "EquivalentSphericalRadius" << std::endl;
      status = EXIT_FAILURE;
      }
    if (itk::Math::NotExactlyEquals(labelCopy->GetEquivalentSphericalPerimeter(), labelObject->GetEquivalentSphericalPerimeter()))
      {
      std::cout << "CopyAttributeFrom failed for attribute " << "EquivalentSphericalPerimeter" << std::endl;
      status = EXIT_FAILURE;
      }
    if (labelCopy->GetEquivalentEllipsoidDiameter() != labelObject->GetEquivalentEllipsoidDiameter())
      {
      std::cout << "CopyAttributeFrom failed for attribute " << "EquivalentEllipsoidDiameter" << std::endl;
      status = EXIT_FAILURE;
      }
    if (itk::Math::NotExactlyEquals(labelCopy->GetFlatness(), labelObject->GetFlatness()))
      {
      std::cout << "CopyAttributeFrom failed for attribute " << "Flatness" << std::endl;
      status = EXIT_FAILURE;
      }
    if (itk::Math::NotExactlyEquals(labelCopy->GetPerimeterOnBorderRatio(), labelObject->GetPerimeterOnBorderRatio()))
      {
      std::cout << "CopyAttributeFrom failed for attribute " << "PerimeterOnBorderRatio" << std::endl;
      status = EXIT_FAILURE;
      }
   if (labelCopy->GetOrientedBoundingBoxSize() != labelObject->GetOrientedBoundingBoxSize())
      {
      std::cout << "CopyAttributeFrom failed for attribute " << "OrientedBoundingBoxSize" << std::endl;
      status = EXIT_FAILURE;
      }
   if (labelCopy->GetOrientedBoundingBoxOrigin() != labelObject->GetOrientedBoundingBoxOrigin())
      {
      std::cout << "CopyAttributeFrom failed for attribute " << "OrientedBoundingBoxOrigin" << std::endl;
      status = EXIT_FAILURE;
      }
    }
  // Check that the accessors match the Get's

  for (unsigned int n = 0; n < labelMap->GetNumberOfLabelObjects(); n++)
    {
    itk::Functor::LabelLabelObjectAccessor< ShapeLabelObjectType >  accessorLabel;
    ShapeLabelObjectType *l = labelMap->GetNthLabelObject(n);
    if (l->GetLabel() != accessorLabel(l))
      {
      std::cout << "l->GetLabel2() != accessorLabel(l)" << std::endl;
      status = EXIT_FAILURE;
      }
    itk::Functor::BoundingBoxLabelObjectAccessor< ShapeLabelObjectType > accessorBoundingBox;
    if (l->GetBoundingBox() != accessorBoundingBox(l))
      {
      std::cout << "l->GetBoundingBox() != accessorBoundingBox(l)" << std::endl;
      status = EXIT_FAILURE;
      }
    itk::Functor::NumberOfPixelsLabelObjectAccessor< ShapeLabelObjectType > accessorSize;
    if (l->GetNumberOfPixels() != accessorSize(l))
      {
      std::cout << "l->GetNumberOfPixels() != accessorSize(l)" << std::endl;
      status = EXIT_FAILURE;
      }
    itk::Functor::PhysicalSizeLabelObjectAccessor< ShapeLabelObjectType > accessorPhysicalSize;
    if (itk::Math::NotExactlyEquals(l->GetPhysicalSize(), accessorPhysicalSize(l)))
      {
      std::cout << "l->GetPhysicalSize() != accessorPhysicalSize(l)" << std::endl;
      status = EXIT_FAILURE;
      }
    itk::Functor::CentroidLabelObjectAccessor< ShapeLabelObjectType > accessorCentroid;
    if (l->GetCentroid() != accessorCentroid(l))
      {
      std::cout << "l->GetCentroid() != accessorCentroid(l)" << std::endl;
      status = EXIT_FAILURE;
      }
    itk::Functor::NumberOfPixelsOnBorderLabelObjectAccessor< ShapeLabelObjectType > accessorSizeOnBorder;
    if (l->GetNumberOfPixelsOnBorder() != accessorSizeOnBorder(l))
      {
      std::cout << "l->GetNumberOfPixelsOnBorder() != accessorSizeOnBorder(l)" << std::endl;
      status = EXIT_FAILURE;
      }
    itk::Functor::PerimeterOnBorderLabelObjectAccessor< ShapeLabelObjectType > accessorPerimeterOnBorder;
    if (itk::Math::NotExactlyEquals(l->GetPerimeterOnBorder(), accessorPerimeterOnBorder(l)))
      {
      std::cout << "l->GetPerimeterOnBorder() != accessorPerimeterOnBorder(l)" << std::endl;
      status = EXIT_FAILURE;
      }
    itk::Functor::FeretDiameterLabelObjectAccessor< ShapeLabelObjectType > accessorFeretDiameter;
    if (itk::Math::NotExactlyEquals(l->GetFeretDiameter(), accessorFeretDiameter(l)))
      {
      std::cout << "l->GetFeretDiameter() != accessorFeretDiameter(l)" << std::endl;
      status = EXIT_FAILURE;
      }
    itk::Functor::PrincipalMomentsLabelObjectAccessor< ShapeLabelObjectType > accessorPrincipalMoments;
    if (l->GetPrincipalMoments() != accessorPrincipalMoments(l))
      {
      std::cout << "l->GetPrincipalMoments() != accessorPrincipalMoments(l)" << std::endl;
      status = EXIT_FAILURE;
      }
    itk::Functor::PrincipalAxesLabelObjectAccessor< ShapeLabelObjectType > accessorPrincipalAxes;
    if (l->GetPrincipalAxes() != accessorPrincipalAxes(l))
      {
      std::cout << "l->GetPrincipalAxes() != accessorPrincipalAxes(l)" << std::endl;
      status = EXIT_FAILURE;
      }
    itk::Functor::ElongationLabelObjectAccessor< ShapeLabelObjectType > accessorElongation;
    if (itk::Math::NotExactlyEquals(l->GetElongation(), accessorElongation(l)))
      {
      std::cout << "l->GetElongation() != accessorElongation(l)" << std::endl;
      status = EXIT_FAILURE;
      }
    itk::Functor::PerimeterLabelObjectAccessor< ShapeLabelObjectType > accessorPerimeter;
    if (itk::Math::NotExactlyEquals(l->GetPerimeter(), accessorPerimeter(l)))
      {
      std::cout << "l->GetPerimeter() != accessorPerimeter(l)" << std::endl;
      status = EXIT_FAILURE;
      }
    itk::Functor::RoundnessLabelObjectAccessor< ShapeLabelObjectType > accessorRoundness;
    if (itk::Math::NotExactlyEquals(l->GetRoundness(), accessorRoundness(l)))
      {
      std::cout << "l->GetRoundness() != accessorRoundness(l)" << std::endl;
      status = EXIT_FAILURE;
      }
    itk::Functor::EquivalentSphericalRadiusLabelObjectAccessor< ShapeLabelObjectType > accessorEquivalentSphericalRadius;
    if (itk::Math::NotExactlyEquals(l->GetEquivalentSphericalRadius(), accessorEquivalentSphericalRadius(l)))
      {
      std::cout << "l->GetEquivalentSphericalRadius() != accessorEquivalentSphericalRadius(l)" << std::endl;
      status = EXIT_FAILURE;
      }
    itk::Functor::EquivalentSphericalPerimeterLabelObjectAccessor< ShapeLabelObjectType > accessorEquivalentSphericalPerimeter;
    if (itk::Math::NotExactlyEquals(l->GetEquivalentSphericalPerimeter(), accessorEquivalentSphericalPerimeter(l)))
      {
      std::cout << "l->GetEquivalentSphericalPerimeter() != accessorEquivalentSphericalPerimeter(l)" << std::endl;
      status = EXIT_FAILURE;
      }
    itk::Functor::EquivalentEllipsoidDiameterLabelObjectAccessor< ShapeLabelObjectType > accessorEquivalentEllipsoidDiameter;
    if (l->GetEquivalentEllipsoidDiameter() != accessorEquivalentEllipsoidDiameter(l))
      {
      std::cout << "l->GetEquivalentEllipsoidDiameter() != accessorEquivalentEllipsoidDiameter(l)" << std::endl;
      status = EXIT_FAILURE;
      }
    itk::Functor::FlatnessLabelObjectAccessor< ShapeLabelObjectType > accessorFlatness;
    if (itk::Math::NotExactlyEquals(l->GetFlatness(), accessorFlatness(l)))
      {
      std::cout << "l->GetFlatness() != accessorFlatness(l)" << std::endl;
      status = EXIT_FAILURE;
      }
    itk::Functor::PerimeterOnBorderRatioLabelObjectAccessor< ShapeLabelObjectType > accessorPerimeterOnBorderRatio;
    if (itk::Math::NotExactlyEquals(l->GetPerimeterOnBorderRatio(), accessorPerimeterOnBorderRatio(l)))
      {
      std::cout << "l->GetPerimeterOnBorderRatio() != accessorPerimeterOnBorderRatio(l)" << std::endl;
      status = EXIT_FAILURE;
      }
    }

  // Cover PrintSelf
  for (unsigned int n = 0; n < labelMap->GetNumberOfLabelObjects(); n++)
    {
    ShapeLabelObjectType *l = labelMap->GetNthLabelObject(n);
    std::cout << "Print ShapeLabelObject " << n << std::endl;
    l->Print(std::cout);
    }

  // Check transforms
  for (unsigned int n = 0; n < labelMap->GetNumberOfLabelObjects(); n++)
    {
    ShapeLabelObjectType *l = labelMap->GetNthLabelObject(n);
    ShapeLabelObjectType::AffineTransformPointer principleToPhysical = l->GetPrincipalAxesToPhysicalAxesTransform();
    std::cout << "Print principleToPhysical " << n << std::endl;
    principleToPhysical->Print(std::cout);

    ShapeLabelObjectType::AffineTransformPointer physicalToPrinciple = l->GetPhysicalAxesToPrincipalAxesTransform();
    std::cout << "Print physicalToPrinciple " << n << std::endl;
    physicalToPrinciple->Print(std::cout);
    }
  return status;
}
