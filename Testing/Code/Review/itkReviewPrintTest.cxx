/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkReviewPrintTest.cxx
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

#include "itkContourExtractor2DImageFilter.h"
#include "itkLabelOverlayImageFilter.h"
#include "itkRGBPixel.h"
#include "itkQuadEdgeMesh.h"

#include "itkMinimumProjectionImageFilter.h"
#include "itkBinaryProjectionImageFilter.h"
#include "itkMedianProjectionImageFilter.h"
#include "itkSumProjectionImageFilter.h"
#include "itkSigmaProjectionImageFilter.h"
#include "itkMeanProjectionImageFilter.h"
#include "itkMaximumProjectionImageFilter.h"


int main(int , char* [])
{
  typedef itk::Image<float,2>           InputType; 
  typedef itk::Image<float,2>           OutputType;
  typedef itk::Image<unsigned short,2>  UShortImageType;
  typedef itk::Image<unsigned char,2>   CharType;
  typedef itk::RGBPixel<unsigned char>  RGBPixelType;
  typedef itk::Image< RGBPixelType, 2 > RGBImageType;
  typedef itk::Image<unsigned char,2>   CharType;

  typedef itk::Vector<float,2>      VectorType;
  typedef itk::Image<VectorType, 2> VectorImageType;

  typedef itk::QuadEdgeMesh< double, 3 > QuadEdgeMeshType;

  itk::ContourExtractor2DImageFilter<InputType>::Pointer
    ContourExtractor2DImageFilterObj =
    itk::ContourExtractor2DImageFilter<InputType>::New();
  std:: cout << "-------------ContourExtractor2DImageFilter "
             << ContourExtractor2DImageFilterObj;

  itk::LabelOverlayImageFilter<InputType,CharType,RGBImageType>::Pointer
    LabelOverlayImageFilterObj =
    itk::LabelOverlayImageFilter<InputType,CharType,RGBImageType>::New();
  std:: cout << "-------------LabelOverlayImageFilter "
             << LabelOverlayImageFilterObj;

  QuadEdgeMeshType::Pointer QuadEdgeMeshObj = QuadEdgeMeshType::New();
  std:: cout << "-------------QuadEdgeMesh "
             << QuadEdgeMeshObj;

  itk::MinimumProjectionImageFilter<InputType,InputType>::Pointer
    MinimumProjectionImageFilterObj =
    itk::MinimumProjectionImageFilter<InputType,InputType>::New();
  std:: cout << "-------------MinimumProjectionImageFilterObj "
             << MinimumProjectionImageFilterObj;

  itk::MaximumProjectionImageFilter<InputType,InputType>::Pointer
    MaximumProjectionImageFilterObj =
    itk::MaximumProjectionImageFilter<InputType,InputType>::New();
  std:: cout << "-------------MaximumProjectionImageFilterObj "
             << MaximumProjectionImageFilterObj;

  itk::MeanProjectionImageFilter<InputType,InputType>::Pointer
    MeanProjectionImageFilterObj =
    itk::MeanProjectionImageFilter<InputType,InputType>::New();
  std:: cout << "-------------MeanProjectionImageFilterObj "
             << MeanProjectionImageFilterObj;

  itk::MedianProjectionImageFilter<InputType,InputType>::Pointer
    MedianProjectionImageFilterObj =
    itk::MedianProjectionImageFilter<InputType,InputType>::New();
  std:: cout << "-------------MedianProjectionImageFilterObj "
             << MedianProjectionImageFilterObj;

  itk::SumProjectionImageFilter<InputType,InputType>::Pointer
    SumProjectionImageFilterObj =
    itk::SumProjectionImageFilter<InputType,InputType>::New();
  std:: cout << "-------------SumProjectionImageFilterObj "
             << SumProjectionImageFilterObj;

  itk::SigmaProjectionImageFilter<InputType,InputType>::Pointer
    SigmaProjectionImageFilterObj =
    itk::SigmaProjectionImageFilter<InputType,InputType>::New();
  std:: cout << "-------------SigmaProjectionImageFilterObj "
             << SigmaProjectionImageFilterObj;

  itk::BinaryProjectionImageFilter<InputType,InputType>::Pointer
    BinaryProjectionImageFilterObj =
    itk::BinaryProjectionImageFilter<InputType,InputType>::New();
  std:: cout << "-------------BinaryProjectionImageFilterObj "
             << BinaryProjectionImageFilterObj;

  return 0;

}
