/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    ReadResampleWriteNifti.cxx
Language:  C++
Date:      Date: 2010/12/13
Version:   1.0
Author:    Jian Wu (eewujian@hotmail.com)
           Univerisity of Florida
       Virginia Commonwealth University

This Program read a 3D image volume, downsample it, and save it in NIFTI
image format.

This program was modified from the ITK example--ResampleImageFilter2.cxx

=========================================================================*/
#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkNiftiImageIO.h"
#include "itkResampleImageFilter.h"
#include "itkAffineTransform.h"
#include "itkLinearInterpolateImageFunction.h"


int
main(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  inputImageFile  outputImageFile" << std::endl;
    return EXIT_FAILURE;
  }

  if (argc > 3)
  {
    std::cerr << "Too many arguments" << std::endl;
  }

  const unsigned int Dimension = 3;

  double outputSpacing[Dimension];
  outputSpacing[0] = 2.0; // pixel spacing in millimeters along X
  outputSpacing[1] = 2.0; // pixel spacing in millimeters along Y
  outputSpacing[2] = 2.0; // pixel spacing in millimeters along Z

  typedef short InputPixelType;
  typedef short OutputPixelType;

  typedef itk::Image<InputPixelType, Dimension>  InputImageType;
  typedef itk::Image<OutputPixelType, Dimension> OutputImageType;


  typedef itk::ImageFileReader<InputImageType>  ReaderType;
  typedef itk::ImageFileWriter<OutputImageType> WriterType;

  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(argv[1]);
  reader->Update();

  typedef itk::ResampleImageFilter<InputImageType, OutputImageType> FilterType;

  FilterType::Pointer                             filter = FilterType::New();
  typedef itk::AffineTransform<double, Dimension> TransformType;
  TransformType::Pointer                          transform = TransformType::New();

  typedef itk::LinearInterpolateImageFunction<InputImageType, double> InterpolatorType;
  InterpolatorType::Pointer                                           interpolator = InterpolatorType::New();
  filter->SetInterpolator(interpolator);

  filter->SetDefaultPixelValue(0);

  InputImageType::SpacingType inputSpacing = reader->GetOutput()->GetSpacing();
  InputImageType::RegionType  inputRegion = reader->GetOutput()->GetLargestPossibleRegion();
  InputImageType::SizeType    inputSize = inputRegion.GetSize();

  double resampleRatio[Dimension];
  resampleRatio[0] = outputSpacing[0] / inputSpacing[0]; // resample ratio along X
  resampleRatio[1] = outputSpacing[1] / inputSpacing[1]; // resample ratio along Y
  resampleRatio[2] = outputSpacing[2] / inputSpacing[2]; // resample ratio along Z


  filter->SetOutputSpacing(outputSpacing);

  filter->SetOutputOrigin(reader->GetOutput()->GetOrigin());


  InputImageType::SizeType outputSize;

  outputSize[0] = floor(inputSize[0] / resampleRatio[0] + 0.5); // number of pixels along X
  outputSize[1] = floor(inputSize[1] / resampleRatio[1] + 0.5); // number of pixels along Y
  outputSize[2] = floor(inputSize[2] / resampleRatio[2] + 0.5); // number of pixels along Z

  filter->SetSize(outputSize);

  filter->SetInput(reader->GetOutput());

  transform->SetIdentity();
  filter->SetTransform(transform);

  WriterType::Pointer writer = WriterType::New();

  typedef itk::NiftiImageIO ImageIOType;
  ImageIOType::Pointer      niftiIO = ImageIOType::New();

  //  The NiftiImageIO object is then connected to the
  //  ImageFileWriter.  This will short-circuit the action of the
  //  ImageIOFactory mechanism. The ImageFileWriter will
  //  not attempt to look for other ImageIO objects capable of
  //  performing the writing tasks. It will simply invoke the one provided by
  //  the user.
  writer->SetImageIO(niftiIO);
  writer->SetFileName(argv[2]);
  writer->SetInput(filter->GetOutput());
  writer->Update();

  return EXIT_SUCCESS;
}
