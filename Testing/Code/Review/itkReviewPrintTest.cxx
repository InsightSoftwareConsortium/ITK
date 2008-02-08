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
#include "itkLabelToRGBImageFilter.h"
#include "itkLabelOverlayImageFilter.h"
#include "itkRGBPixel.h"
#include "itkQuadEdgeMesh.h"

#include "itkValuedRegionalMinimaImageFilter.h"
#include "itkValuedRegionalMaximaImageFilter.h"
#include "itkRegionalMaximaImageFilter.h"
#include "itkRegionalMinimaImageFilter.h"

#include "itkNeuralNetworkFileReader.h"
#include "itkNeuralNetworkFileWriter.h"

#include "itkConformalFlatteningMeshFilter.h"

#include "itkVTKPolyDataReader.h"
#include "itkVTKPolyDataWriter.h"

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

  typedef unsigned short         PixelType;
  typedef itk::Mesh< float, 3 >  MeshType;

  // Dummy variable just to force the full instantiation of the class
  CharType::Pointer dummyImage = CharType::New();

  itk::ContourExtractor2DImageFilter<InputType>::Pointer
    ContourExtractor2DImageFilterObj =
    itk::ContourExtractor2DImageFilter<InputType>::New();
  std:: cout << "-------------ContourExtractor2DImageFilter "
             << ContourExtractor2DImageFilterObj;

  itk::LabelToRGBImageFilter<CharType,RGBImageType>::Pointer
    LabelToRGBImageFilterObj =
    itk::LabelToRGBImageFilter<CharType,RGBImageType>::New();
  std:: cout << "-------------LabelToRGBImageFilter "
             << LabelToRGBImageFilterObj;

  itk::LabelOverlayImageFilter<InputType,CharType,RGBImageType>::Pointer
    LabelOverlayImageFilterObj =
    itk::LabelOverlayImageFilter<InputType,CharType,RGBImageType>::New();
  std:: cout << "-------------LabelOverlayImageFilter "
             << LabelOverlayImageFilterObj;

  QuadEdgeMeshType::Pointer QuadEdgeMeshObj = QuadEdgeMeshType::New();
  std:: cout << "-------------QuadEdgeMesh "
             << QuadEdgeMeshObj;

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

  itk::NeuralNetworkFileReader<itk::Statistics::OneHiddenLayerBackPropagationNeuralNetwork<MeasurementVectorType, TargetVectorType> >::Pointer
    NeuralNetworkFileReaderObj =
              itk::NeuralNetworkFileReader<itk::Statistics::OneHiddenLayerBackPropagationNeuralNetwork<MeasurementVectorType, TargetVectorType> >::New();
  std:: cout << "-------------NeuralNetworkFileReaderObj "
             << NeuralNetworkFileReaderObj;

  itk::NeuralNetworkFileWriter<itk::Statistics::OneHiddenLayerBackPropagationNeuralNetwork<MeasurementVectorType, TargetVectorType> >::Pointer
    NeuralNetworkFileWriterObj =
              itk::NeuralNetworkFileWriter<itk::Statistics::OneHiddenLayerBackPropagationNeuralNetwork<MeasurementVectorType, TargetVectorType> >::New();
  std:: cout << "-------------NeuralNetworkFileWriterObj "
             << NeuralNetworkFileWriterObj;

  itk::ConformalFlatteningMeshFilter<PixelType>::Pointer
    ConformalFlatteningMeshFilterObj =
    itk::ConformalFlatteningMeshFilter<PixelType>::New();
  std::cout << "--------------ConformalFlatteningMeshFilterObj "
            << ConformalFlatteningMeshFilterObj;

  itk::VTKPolyDataReader<MeshType>::Pointer VTKPolyDataReaderObj =
    itk::VTKPolyDataReader<MeshType>::New();
  std::cout << "--------------VTKPolyDataReaderObj "
           << VTKPolyDataReaderObj;

  itk::VTKPolyDataWriter<MeshType>::Pointer VTKPolyDataWriterObj =
    itk::VTKPolyDataWriter<MeshType>::New();
  std::cout << "--------------VTKPolyDataWriterObj "
            << VTKPolyDataWriterObj;

  return EXIT_SUCCESS;
}
