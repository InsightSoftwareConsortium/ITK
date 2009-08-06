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

  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );
 
  typedef itk::LabelImageToShapeLabelMapFilter< ImageType, LabelMapType> I2LType;
  I2LType::Pointer i2l = I2LType::New();
  i2l->SetInput( reader->GetOutput() );
  i2l->Update();

  typedef LabelMapType::LabelObjectContainerType LabelObjectContainerType;
  LabelMapType *labelMap = i2l->GetOutput();
  LabelObjectContainerType container = labelMap->GetLabelObjectContainer();
  std::cout << "File " << argv[1] << " has " << labelMap->GetNumberOfLabelObjects() << " labels." << std::endl;
 
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

  // Check that the accessors match the Get's
  int status = EXIT_SUCCESS;
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
  return status;
}
