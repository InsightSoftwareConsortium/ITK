/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkShapeLabelObjectAccessorsTest1.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkSimpleFilterWatcher.h"

#include "itkLabelObject.h"
#include "itkShapeLabelObject.h"
#include "itkShapeLabelObjectAccessors.h"
#include "itkLabelMap.h"

#include "itkShapeKeepNObjectsLabelMapFilter.h"
#include "itkLabelImageToShapeLabelMapFilter.h"
#include "itkLabelMapToLabelImageFilter.h"

#include "itkTestingMacros.h"

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
  attributes.push_back("Size");
  attributes.push_back("PhysicalSize");
  attributes.push_back("RegionElongation");
  attributes.push_back("SizeRegionRatio");
  attributes.push_back("Centroid");
  attributes.push_back("Region");
  attributes.push_back("SizeOnBorder");
  attributes.push_back("PhysicalSizeOnBorder");
  attributes.push_back("FeretDiameter");
  attributes.push_back("BinaryPrincipalMoments");
  attributes.push_back("BinaryPrincipalAxes");
  attributes.push_back("BinaryElongation");
  attributes.push_back("Perimeter");
  attributes.push_back("Roundness");
  attributes.push_back("EquivalentRadius");
  attributes.push_back("EquivalentPerimeter");
  attributes.push_back("EquivalentEllipsoidSize");
  attributes.push_back("BinaryFlatness");
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
  i2l->Update();

  typedef LabelMapType::LabelObjectContainerType LabelObjectContainerType;
  LabelMapType *labelMap = i2l->GetOutput();
  LabelObjectContainerType container = labelMap->GetLabelObjectContainer();
  std::cout << "File " << argv[1] << " has " << labelMap->GetNumberOfLabelObjects() << " labels." << std::endl;
 
  // Retrieve all attributes
  for (unsigned int n = 0; n < labelMap->GetNumberOfLabelObjects(); n++)
    {
    ShapeLabelObjectType *labelObject = labelMap->GetNthLabelObject(n);
    std::cout << "Label: " 
              << itk::NumericTraits<LabelMapType::LabelType>::PrintType(labelObject->GetLabel()) << std::endl;
    std::cout << "    Region: "
              << labelObject->GetRegion() << std::endl;
    std::cout << "    Size: "
              << labelObject->GetSize() << std::endl;
    std::cout << "    PhysicalSize: "
              << labelObject->GetPhysicalSize() << std::endl;
    std::cout << "    Centroid: "
              << labelObject->GetCentroid() << std::endl;
    std::cout << "    RegionElongation: "
              << labelObject->GetRegionElongation() << std::endl;
    std::cout << "    SizeRegionRatio: "
              << labelObject->GetSizeRegionRatio() << std::endl;
    std::cout << "    SizeOnBorder: "
              << labelObject->GetSizeOnBorder() << std::endl;
    std::cout << "    PhysicalSizeOnBorder: "
              << labelObject->GetPhysicalSizeOnBorder() << std::endl;
    std::cout << "    FeretDiameter: "
              << labelObject->GetFeretDiameter() << std::endl;
    std::cout << "    BinaryPrincipalMoments: "
              << labelObject->GetBinaryPrincipalMoments() << std::endl;
    std::cout << "    BinaryPrincipalAxes: "
              << labelObject->GetBinaryPrincipalAxes() << std::endl;
    std::cout << "    BinaryElongation: "
              << labelObject->GetBinaryElongation() << std::endl;
    std::cout << "    Perimeter: "
              << labelObject->GetPerimeter() << std::endl;
    std::cout << "    Roundness: "
              << labelObject->GetRoundness() << std::endl;
    std::cout << "    EquivalentRadius: "
              << labelObject->GetEquivalentRadius() << std::endl;
    std::cout << "    EquivalentPerimeter: "
              << labelObject->GetEquivalentPerimeter() << std::endl;
    std::cout << "    EquivalentEllipsoidSize: "
              << labelObject->GetEquivalentEllipsoidSize() << std::endl;
    std::cout << "    BinaryFlatness: "
              << labelObject->GetBinaryFlatness() << std::endl;
    }
  for (unsigned int n = 0; n < labelMap->GetNumberOfLabelObjects(); n++)
    {
    ShapeLabelObjectType *labelCopy = labelMap->GetNthLabelObject(n);
    ShapeLabelObjectType *labelObject = labelMap->GetNthLabelObject(0);
    labelObject->CopyAttributesFrom(labelCopy);
    if (labelCopy->GetLabel() != labelObject->GetLabel())
      {
      std::cout << "CopyAttributesFrom failed for attribute: " << "Label" << std::endl;
      status = EXIT_FAILURE;
      }
    if (labelCopy->GetRegion() != labelObject->GetRegion())
      {
      std::cout << "CopyAttributeFrom failed for attribute " << "Region" << std::endl;
      status = EXIT_FAILURE;
      }
    if (labelCopy->GetSize() != labelObject->GetSize())
      {
      std::cout << "CopyAttributeFrom failed for attribute " << "Size" << std::endl;
      status = EXIT_FAILURE;
      }
    if (labelCopy->GetPhysicalSize() != labelObject->GetPhysicalSize())
      {
      std::cout << "CopyAttributeFrom failed for attribute " << "PhysicalSize" << std::endl;
      status = EXIT_FAILURE;
      }
    if (labelCopy->GetCentroid() != labelObject->GetCentroid())
      {
      std::cout << "CopyAttributeFrom failed for attribute " << "Centroid" << std::endl;
      status = EXIT_FAILURE;
      }
    if (labelCopy->GetRegionElongation() != labelObject->GetRegionElongation())
      {
      std::cout << "CopyAttributeFrom failed for attribute " << "RegionElongation" << std::endl;
      status = EXIT_FAILURE;
      }
    if (labelCopy->GetSizeRegionRatio() != labelObject->GetSizeRegionRatio())
      {
      std::cout << "CopyAttributeFrom failed for attribute " << "SizeRegionRatio" << std::endl;
      status = EXIT_FAILURE;
      }
    if (labelCopy->GetSizeOnBorder() != labelObject->GetSizeOnBorder())
      {
      std::cout << "CopyAttributeFrom failed for attribute " << "SizeOnBorder" << std::endl;
      status = EXIT_FAILURE;
      }
    if (labelCopy->GetPhysicalSizeOnBorder() != labelObject->GetPhysicalSizeOnBorder())
      {
      std::cout << "CopyAttributeFrom failed for attribute " << "PhysicalSizeOnBorder" << std::endl;
      status = EXIT_FAILURE;
      }
    if (labelCopy->GetFeretDiameter() != labelObject->GetFeretDiameter())
      {
      std::cout << "CopyAttributeFrom failed for attribute " << "FeretDiameter" << std::endl;
      status = EXIT_FAILURE;
      }
    if (labelCopy->GetBinaryPrincipalMoments() != labelObject->GetBinaryPrincipalMoments())
      {
      std::cout << "CopyAttributeFrom failed for attribute " << "BinaryPrincipalMoments" << std::endl;
      status = EXIT_FAILURE;
      }
    if (labelCopy->GetBinaryPrincipalAxes() != labelObject->GetBinaryPrincipalAxes())
      {
      std::cout << "CopyAttributeFrom failed for attribute " << "BinaryPrincipalAxes" << std::endl;
      status = EXIT_FAILURE;
      }
    if (labelCopy->GetBinaryElongation() != labelObject->GetBinaryElongation())
      {
      std::cout << "CopyAttributeFrom failed for attribute " << "BinaryElongation" << std::endl;
      status = EXIT_FAILURE;
      }
    if (labelCopy->GetPerimeter() != labelObject->GetPerimeter())
      {
      std::cout << "CopyAttributeFrom failed for attribute " << "Perimeter" << std::endl;
      status = EXIT_FAILURE;
      }
    if (labelCopy->GetRoundness() != labelObject->GetRoundness())
      {
      std::cout << "CopyAttributeFrom failed for attribute " << "Roundness" << std::endl;
      status = EXIT_FAILURE;
      }
    if (labelCopy->GetEquivalentRadius() != labelObject->GetEquivalentRadius())
      {
      std::cout << "CopyAttributeFrom failed for attribute " << "EquivalentRadius" << std::endl;
      status = EXIT_FAILURE;
      }
    if (labelCopy->GetEquivalentPerimeter() != labelObject->GetEquivalentPerimeter())
      {
      std::cout << "CopyAttributeFrom failed for attribute " << "EquivalentPerimeter" << std::endl;
      status = EXIT_FAILURE;
      }
    if (labelCopy->GetEquivalentEllipsoidSize() != labelObject->GetEquivalentEllipsoidSize())
      {
      std::cout << "CopyAttributeFrom failed for attribute " << "EquivalentEllipsoidSize" << std::endl;
      status = EXIT_FAILURE;
      }
    if (labelCopy->GetBinaryFlatness() != labelObject->GetBinaryFlatness())
      {
      std::cout << "CopyAttributeFrom failed for attribute " << "BinaryFlatness" << std::endl;
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
    itk::Functor::RegionLabelObjectAccessor< ShapeLabelObjectType > accessorRegion;
    if (l->GetRegion() != accessorRegion(l))
      {
      std::cout << "l->GetRegion() != accessorRegion(l)" << std::endl;
      status = EXIT_FAILURE;
      }
    itk::Functor::SizeLabelObjectAccessor< ShapeLabelObjectType > accessorSize;
    if (l->GetSize() != accessorSize(l))
      {
      std::cout << "l->GetSize() != accessorSize(l)" << std::endl;
      status = EXIT_FAILURE;
      }
    itk::Functor::PhysicalSizeLabelObjectAccessor< ShapeLabelObjectType > accessorPhysicalSize;
    if (l->GetPhysicalSize() != accessorPhysicalSize(l))
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
    itk::Functor::RegionElongationLabelObjectAccessor< ShapeLabelObjectType > accessorRegionElongation;
    if (l->GetRegionElongation() != accessorRegionElongation(l))
      {
      std::cout << "l->GetRegionElongation() != accessorRegionElongation(l)" << std::endl;
      status = EXIT_FAILURE;
      }
    itk::Functor::SizeRegionRatioLabelObjectAccessor< ShapeLabelObjectType > accessorSizeRegionRatio;
    if (l->GetSizeRegionRatio() != accessorSizeRegionRatio(l))
      {
      std::cout << "l->GetSizeRegionRatio() != accessorSizeRegionRatio(l)" << std::endl;
      status = EXIT_FAILURE;
      }
    itk::Functor::SizeOnBorderLabelObjectAccessor< ShapeLabelObjectType > accessorSizeOnBorder;
    if (l->GetSizeOnBorder() != accessorSizeOnBorder(l))
      {
      std::cout << "l->GetSizeOnBorder() != accessorSizeOnBorder(l)" << std::endl;
      status = EXIT_FAILURE;
      }
    itk::Functor::PhysicalSizeOnBorderLabelObjectAccessor< ShapeLabelObjectType > accessorPhysicalSizeOnBorder;
    if (l->GetPhysicalSizeOnBorder() != accessorPhysicalSizeOnBorder(l))
      {
      std::cout << "l->GetPhysicalSizeOnBorder() != accessorPhysicalSizeOnBorder(l)" << std::endl;
      status = EXIT_FAILURE;
      }
    itk::Functor::FeretDiameterLabelObjectAccessor< ShapeLabelObjectType > accessorFeretDiameter;
    if (l->GetFeretDiameter() != accessorFeretDiameter(l))
      {
      std::cout << "l->GetFeretDiameter() != accessorFeretDiameter(l)" << std::endl;
      status = EXIT_FAILURE;
      }
    itk::Functor::BinaryPrincipalMomentsLabelObjectAccessor< ShapeLabelObjectType > accessorBinaryPrincipalMoments;
    if (l->GetBinaryPrincipalMoments() != accessorBinaryPrincipalMoments(l))
      {
      std::cout << "l->GetBinaryPrincipalMoments() != accessorBinaryPrincipalMoments(l)" << std::endl;
      status = EXIT_FAILURE;
      }
    itk::Functor::BinaryPrincipalAxesLabelObjectAccessor< ShapeLabelObjectType > accessorBinaryPrincipalAxes;
    if (l->GetBinaryPrincipalAxes() != accessorBinaryPrincipalAxes(l))
      {
      std::cout << "l->GetBinaryPrincipalAxes() != accessorBinaryPrincipalAxes(l)" << std::endl;
      status = EXIT_FAILURE;
      }
    itk::Functor::BinaryElongationLabelObjectAccessor< ShapeLabelObjectType > accessorBinaryElongation;
    if (l->GetBinaryElongation() != accessorBinaryElongation(l))
      {
      std::cout << "l->GetBinaryElongation() != accessorBinaryElongation(l)" << std::endl;
      status = EXIT_FAILURE;
      }
    itk::Functor::PerimeterLabelObjectAccessor< ShapeLabelObjectType > accessorPerimeter;
    if (l->GetPerimeter() != accessorPerimeter(l))
      {
      std::cout << "l->GetPerimeter() != accessorPerimeter(l)" << std::endl;
      status = EXIT_FAILURE;
      }
    itk::Functor::RoundnessLabelObjectAccessor< ShapeLabelObjectType > accessorRoundness;
    if (l->GetRoundness() != accessorRoundness(l))
      {
      std::cout << "l->GetRoundness() != accessorRoundness(l)" << std::endl;
      status = EXIT_FAILURE;
      }
    itk::Functor::EquivalentRadiusLabelObjectAccessor< ShapeLabelObjectType > accessorEquivalentRadius;
    if (l->GetEquivalentRadius() != accessorEquivalentRadius(l))
      {
      std::cout << "l->GetEquivalentRadius() != accessorEquivalentRadius(l)" << std::endl;
      status = EXIT_FAILURE;
      }
    itk::Functor::EquivalentPerimeterLabelObjectAccessor< ShapeLabelObjectType > accessorEquivalentPerimeter;
    if (l->GetEquivalentPerimeter() != accessorEquivalentPerimeter(l))
      {
      std::cout << "l->GetEquivalentPerimeter() != accessorEquivalentPerimeter(l)" << std::endl;
      status = EXIT_FAILURE;
      }
    itk::Functor::EquivalentEllipsoidSizeLabelObjectAccessor< ShapeLabelObjectType > accessorEquivalentEllipsoidSize;
    if (l->GetEquivalentEllipsoidSize() != accessorEquivalentEllipsoidSize(l))
      {
      std::cout << "l->GetEquivalentEllipsoidSize() != accessorEquivalentEllipsoidSize(l)" << std::endl;
      status = EXIT_FAILURE;
      }
    itk::Functor::BinaryFlatnessLabelObjectAccessor< ShapeLabelObjectType > accessorBinaryFlatness;
    if (l->GetBinaryFlatness() != accessorBinaryFlatness(l))
      {
      std::cout << "l->GetBinaryFlatness() != accessorBinaryFlatness(l)" << std::endl;
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
    ShapeLabelObjectType::AffineTransformPointer principleToPhysical = l->GetBinaryPrincipalAxesToPhysicalAxesTransform();
    std::cout << "Print principleToPhysical " << n << std::endl;
    principleToPhysical->Print(std::cout);

    ShapeLabelObjectType::AffineTransformPointer physicalToPrinciple = l->GetPhysicalAxesToBinaryPrincipalAxesTransform();
    std::cout << "Print physicalToPrinciple " << n << std::endl;
    physicalToPrinciple->Print(std::cout);
    }
  return status;
}
