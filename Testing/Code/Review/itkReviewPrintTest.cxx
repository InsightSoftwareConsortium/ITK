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
#include "itkBinaryThresholdProjectionImageFilter.h"
#include "itkMedianProjectionImageFilter.h"
#include "itkSumProjectionImageFilter.h"
#include "itkStandardDeviationProjectionImageFilter.h"
#include "itkMeanProjectionImageFilter.h"
#include "itkMaximumProjectionImageFilter.h"

#include "itkValuedRegionalMinimaImageFilter.h"
#include "itkValuedRegionalMaximaImageFilter.h"
#include "itkRegionalMaximaImageFilter.h"
#include "itkRegionalMinimaImageFilter.h"

#include "itkNeuralNetworkFileReader.h"
#include "itkNeuralNetworkFileWriter.h"

#include "itkConformalFlatteningMeshFilter.h"

#include "itkVTKPolyDataReader.h"

int main(int , char* [])
{
  typedef itk::Image<float,2>           InputType;
  typedef itk::Image<float,2>           OutputType;
  typedef itk::Image<unsigned char,2>   CharType;
  typedef itk::RGBPixel<unsigned char>  RGBPixelType;
  typedef itk::Image< RGBPixelType, 2 > RGBImageType;

  typedef itk::Vector<float,2>      VectorType;
  typedef itk::Image<VectorType, 2> VectorImageType;

  typedef itk::QuadEdgeMesh< double, 3 > QuadEdgeMeshType;

  typedef itk::Vector<double, 2> MeasurementVectorType;
  typedef itk::Vector<double, 1> TargetVectorType;

  typedef itk::Mesh< float, 3 >     MeshType;

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

  itk::StandardDeviationProjectionImageFilter<InputType,InputType>::Pointer
    StandardDeviationProjectionImageFilterObj =
    itk::StandardDeviationProjectionImageFilter<InputType,InputType>::New();
  std:: cout << "-------------StandardDeviationProjectionImageFilterObj "
             << StandardDeviationProjectionImageFilterObj;

  itk::BinaryProjectionImageFilter<InputType,InputType>::Pointer
    BinaryProjectionImageFilterObj =
    itk::BinaryProjectionImageFilter<InputType,InputType>::New();
  std:: cout << "-------------BinaryProjectionImageFilterObj "
             << BinaryProjectionImageFilterObj;

  itk::BinaryThresholdProjectionImageFilter<InputType,InputType>::Pointer
    BinaryThresholdProjectionImageFilterObj =
    itk::BinaryThresholdProjectionImageFilter<InputType,InputType>::New();
  std:: cout << "-------------BinaryThresholdProjectionImageFilterObj "
             << BinaryThresholdProjectionImageFilterObj;

  itk::ValuedRegionalMaximaImageFilter<InputType,InputType>::Pointer
    ValuedRegionalMaximaImageFilterObj =
    itk::ValuedRegionalMaximaImageFilter<InputType,InputType>::New();
  std:: cout << "-------------ValuedRegionalMaximaImageFilterObj "
             << ValuedRegionalMaximaImageFilterObj;

  itk::ValuedRegionalMinimaImageFilter<InputType,InputType>::Pointer
    ValuedRegionalMinimaImageFilterObj =
    itk::ValuedRegionalMinimaImageFilter<InputType,InputType>::New();
  std:: cout << "-------------ValuedRegionalMinimaImageFilterObj "
             << ValuedRegionalMinimaImageFilterObj;

  itk::RegionalMaximaImageFilter<InputType,InputType>::Pointer
    RegionalMaximaImageFilterObj =
    itk::RegionalMaximaImageFilter<InputType,InputType>::New();
  std:: cout << "-------------RegionalMaximaImageFilterObj "
             << RegionalMaximaImageFilterObj;

  itk::RegionalMinimaImageFilter<InputType,InputType>::Pointer
    RegionalMinimaImageFilterObj =
    itk::RegionalMinimaImageFilter<InputType,InputType>::New();
  std:: cout << "-------------RegionalMinimaImageFilterObj "
             << RegionalMinimaImageFilterObj;

  itk::NeuralNetworkFileReader<MeasurementVectorType,TargetVectorType>::Pointer
    NeuralNetworkFileReaderObj =
              itk::NeuralNetworkFileReader<MeasurementVectorType,TargetVectorType>::New();
  std:: cout << "-------------NeuralNetworkFileReaderObj "
             << NeuralNetworkFileReaderObj;

  itk::NeuralNetworkFileWriter<MeasurementVectorType,TargetVectorType>::Pointer
    NeuralNetworkFileWriterObj =
              itk::NeuralNetworkFileWriter<MeasurementVectorType,TargetVectorType>::New();
  std:: cout << "-------------NeuralNetworkFileWriterObj "
             << NeuralNetworkFileWriterObj;

  itk::ConformalFlatteningMeshFilter<MeshType,MeshType>::Pointer
    ConformalFlatteningMeshFilterObj =
    itk::ConformalFlatteningMeshFilter<MeshType,MeshType>::New();
  std::cout << "--------------ConformalFlatteningMeshFilterObj "
            << ConformalFlatteningMeshFilterObj;

  itk::VTKPolyDataReader<MeshType>::Pointer
    VTKPolyDataReaderObj =
    itk::VTKPolyDataReader<MeshType>::New();
  std::cout << "--------------VTKPolyDataReaderObj "
            << VTKPolyDataReaderObj;

  return EXIT_SUCCESS;
}
