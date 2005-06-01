/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAlgorithmsPrintTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include "itkImage.h"
#include "itkVector.h"
#include "itkPoint.h"
#include "itkMesh.h"
#include "itkEllipseSpatialObject.h"

#include "itkAntiAliasBinaryImageFilter.h"
#include "itkBalloonForceFilter.h"
#include "itkBinaryMask3DMeshSource.h"
#include "itkBinaryMinMaxCurvatureFlowFunction.h"
#include "itkBinaryMinMaxCurvatureFlowImageFilter.h"
#include "itkCannySegmentationLevelSetFunction.h"
#include "itkCannySegmentationLevelSetImageFilter.h"
#include "itkConnectedRegionsMeshFilter.h"
#include "itkCurvatureFlowFunction.h"
#include "itkCurvatureFlowImageFilter.h"
#include "itkDeformableMesh3DFilter.h"
#include "itkDemonsRegistrationFilter.h"
#include "itkDemonsRegistrationFunction.h"
#include "itkExtensionVelocitiesImageFilter.h"
#include "itkFEMRegistrationFilter.h"
#include "itkFastMarchingExtensionImageFilter.h"
#include "itkFastMarchingImageFilter.h"

int main (int , char* [])
{
  typedef itk::Image<float,2> InputType; 
  typedef itk::Image<float,2> OutputType;
  typedef itk::Image<bool,2> BinaryImageType;
  typedef itk::Image<unsigned short,2> UShortImageType;
  typedef itk::Image<unsigned char,2> CharType;
  
  typedef itk::Mesh<double>  MeshType;
  
  typedef itk::Vector<float,2> VectorType;
  typedef itk::Image<VectorType, 2> VectorImageType;
  
  // Used for NormalizedCorrelationPointSetToImageMetric
  typedef itk::PointSet<float,2> PointSetType;
  
  itk::AntiAliasBinaryImageFilter<InputType,OutputType>::Pointer AntiAliasBinaryImageFilterObj =
    itk::AntiAliasBinaryImageFilter<InputType,OutputType>::New();
  std:: cout << "-------------AntiAliasBinaryImageFilter " << AntiAliasBinaryImageFilterObj;

  itk::BalloonForceFilter<MeshType,MeshType>::Pointer BalloonForceFilterObj =
    itk::BalloonForceFilter<MeshType,MeshType>::New();
  std:: cout << "-------------BalloonForceFilter " << BalloonForceFilterObj;

  itk::BinaryMask3DMeshSource<InputType,MeshType>::Pointer BinaryMask3DMeshSourceObj =
    itk::BinaryMask3DMeshSource<InputType,MeshType>::New();
  std:: cout << "-------------BinaryMask3DMeshSource " << BinaryMask3DMeshSourceObj;

  itk::BinaryMinMaxCurvatureFlowFunction<InputType>::Pointer BinaryMinMaxCurvatureFlowFunctionObj =
    itk::BinaryMinMaxCurvatureFlowFunction<InputType>::New();
  std:: cout << "-------------BinaryMinMaxCurvatureFlowFunction " << BinaryMinMaxCurvatureFlowFunctionObj;

  itk::BinaryMinMaxCurvatureFlowImageFilter<InputType,OutputType>::Pointer BinaryMinMaxCurvatureFlowImageFilterObj =
    itk::BinaryMinMaxCurvatureFlowImageFilter<InputType,OutputType>::New();
  std:: cout << "-------------BinaryMinMaxCurvatureFlowImageFilter " << BinaryMinMaxCurvatureFlowImageFilterObj;

  itk::CannySegmentationLevelSetFunction<InputType,InputType>::Pointer CannySegmentationLevelSetFunctionObj =
    itk::CannySegmentationLevelSetFunction<InputType,InputType>::New();
  std:: cout << "-------------CannySegmentationLevelSetFunction " << CannySegmentationLevelSetFunctionObj;

  itk::CannySegmentationLevelSetImageFilter<InputType,OutputType>::Pointer CannySegmentationLevelSetImageFilterObj =
    itk::CannySegmentationLevelSetImageFilter<InputType,OutputType>::New();
  std:: cout << "-------------CannySegmentationLevelSetImageFilter " << CannySegmentationLevelSetImageFilterObj;

  itk::ConnectedRegionsMeshFilter<MeshType,MeshType>::Pointer ConnectedRegionsMeshFilterObj =
    itk::ConnectedRegionsMeshFilter<MeshType,MeshType>::New();
  std:: cout << "-------------ConnectedRegionsMeshFilter " << ConnectedRegionsMeshFilterObj;

  itk::CurvatureFlowFunction<InputType>::Pointer CurvatureFlowFunctionObj =
    itk::CurvatureFlowFunction<InputType>::New();
  std:: cout << "-------------CurvatureFlowFunction " << CurvatureFlowFunctionObj;

  itk::CurvatureFlowImageFilter<InputType,OutputType>::Pointer CurvatureFlowImageFilterObj =
    itk::CurvatureFlowImageFilter<InputType,OutputType>::New();
  std:: cout << "-------------CurvatureFlowImageFilter " << CurvatureFlowImageFilterObj;

  itk::DeformableMesh3DFilter<MeshType,MeshType>::Pointer DeformableMesh3DFilterObj =
    itk::DeformableMesh3DFilter<MeshType,MeshType>::New();
  std:: cout << "-------------DeformableMesh3DFilter " << DeformableMesh3DFilterObj;

  itk::DemonsRegistrationFilter<InputType,OutputType,VectorImageType>::Pointer DemonsRegistrationFilterObj =
    itk::DemonsRegistrationFilter<InputType,OutputType,VectorImageType>::New();
  std:: cout << "-------------DemonsRegistrationFilter " << DemonsRegistrationFilterObj;

  itk::DemonsRegistrationFunction<InputType,OutputType,VectorImageType>::Pointer DemonsRegistrationFunctionObj =
    itk::DemonsRegistrationFunction<InputType,OutputType,VectorImageType>::New();
  std:: cout << "-------------DemonsRegistrationFunction " << DemonsRegistrationFunctionObj;

  itk::ExtensionVelocitiesImageFilter<InputType,float,1>::Pointer ExtensionVelocitiesImageFilterObj =
    itk::ExtensionVelocitiesImageFilter<InputType,float,1>::New();
  std:: cout << "-------------ExtensionVelocitiesImageFilter " << ExtensionVelocitiesImageFilterObj;

  itk::fem::FEMRegistrationFilter<InputType,InputType>::Pointer FEMRegistrationFilterObj =
    itk::fem::FEMRegistrationFilter<InputType,InputType>::New();
  std:: cout << "-------------FEMRegistrationFilter " << FEMRegistrationFilterObj;

  itk::FastMarchingExtensionImageFilter<InputType,float>::Pointer FastMarchingExtensionImageFilterObj =
    itk::FastMarchingExtensionImageFilter<InputType,float>::New();
  std:: cout << "-------------FastMarchingExtensionImageFilter " << FastMarchingExtensionImageFilterObj;

  itk::FastMarchingImageFilter<InputType>::Pointer FastMarchingImageFilterObj =
    itk::FastMarchingImageFilter<InputType>::New();
  std:: cout << "-------------FastMarchingImageFilter " << FastMarchingImageFilterObj;

  return 0;

}
