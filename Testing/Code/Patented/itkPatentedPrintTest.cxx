/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPatentedPrintTest.cxx
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

#include "itkShapeDetectionLevelSetFunction.h"
#include "itkShapeDetectionLevelSetImageFilter.h"
#include "itkSimpleFuzzyConnectednessImageFilterBase.h"
#include "itkSimpleFuzzyConnectednessRGBImageFilter.h"
#include "itkSimpleFuzzyConnectednessScalarImageFilter.h"
#include "itkVectorFuzzyConnectednessImageFilter.h"

int main(int , char* [])
{
  typedef itk::Image<float,2> InputType; 
  typedef itk::Image<float,2> OutputType;
  typedef itk::Image<unsigned short,2> UShortImageType;
  typedef itk::Image<unsigned char,2> CharType;

  typedef itk::Vector<float,2> VectorType;
  typedef itk::Image<VectorType, 2> VectorImageType;

  itk::ShapeDetectionLevelSetFunction<InputType,InputType>::Pointer ShapeDetectionLevelSetFunctionObj =
  itk::ShapeDetectionLevelSetFunction<InputType,InputType>::New();
  std:: cout << "-------------ShapeDetectionLevelSetFunction " << ShapeDetectionLevelSetFunctionObj;

  itk::ShapeDetectionLevelSetImageFilter<InputType,InputType,float>::Pointer ShapeDetectionLevelSetImageFilterObj =
    itk::ShapeDetectionLevelSetImageFilter<InputType,InputType,float>::New();
  std:: cout << "-------------ShapeDetectionLevelSetImageFilter " << ShapeDetectionLevelSetImageFilterObj;

  itk::SimpleFuzzyConnectednessImageFilterBase<InputType,OutputType>::Pointer SimpleFuzzyConnectednessImageFilterBaseObj =
    itk::SimpleFuzzyConnectednessImageFilterBase<InputType,OutputType>::New();
  std:: cout << "-------------SimpleFuzzyConnectednessImageFilterBase " << SimpleFuzzyConnectednessImageFilterBaseObj;

  itk::SimpleFuzzyConnectednessRGBImageFilter<VectorImageType,CharType>::Pointer SimpleFuzzyConnectednessRGBImageFilterObj =
    itk::SimpleFuzzyConnectednessRGBImageFilter<VectorImageType,CharType>::New();
  std:: cout << "-------------SimpleFuzzyConnectednessRGBImageFilter " << SimpleFuzzyConnectednessRGBImageFilterObj;

  itk::SimpleFuzzyConnectednessScalarImageFilter<UShortImageType,CharType>::Pointer SimpleFuzzyConnectednessScalarImageFilterObj =
    itk::SimpleFuzzyConnectednessScalarImageFilter<UShortImageType,CharType>::New();
  std:: cout << "-------------SimpleFuzzyConnectednessScalarImageFilter " << SimpleFuzzyConnectednessScalarImageFilterObj;

  itk::VectorFuzzyConnectednessImageFilter<VectorImageType,CharType>::Pointer VectorFuzzyConnectednessImageFilterObj =
    itk::VectorFuzzyConnectednessImageFilter<VectorImageType,CharType>::New();
  std:: cout << "-------------VectorFuzzyConnectednessImageFilter " << VectorFuzzyConnectednessImageFilterObj;

  return 0;

}
