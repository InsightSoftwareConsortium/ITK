/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    WarpImageFilter1.cxx
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

//  Software Guide : BeginLatex
//
//  This example illustrates how to use the WarpImageFilter and a
//  deformation field for resampling an image. This is typically done
//  as the last step of a deformable registration algorithm.
//
//  \index{itk::WarpImageFilter}
//
//  Software Guide : EndLatex 


#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"


// Software Guide : BeginCodeSnippet
#include "itkWarpImageFilter.h"
#include "itkLinearInterpolateImageFunction.h"
// Software Guide : EndCodeSnippet


int main( int argc, char * argv[] )
{
  if( argc < 4 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  inputImageFile  inputDeformationField  outputImageFile" << std::endl;
    return EXIT_FAILURE;
    }

  const     unsigned int   Dimension = 2;

  // Software Guide : BeginLatex
  // The deformation field is represented as an image of vector pixel types. The
  // dimenstion of the vectors is the same as the dimension of the input image.
  // Each vector in the deformation field represents the distance between a 
  // geometric point in the input space and a point in the output space such that:
  // \begin{equation}
  // p_{in} = p_{out} + distance
  // \end{equation}
  // Software Guide : EndLatex
  // Software Guide : BeginCodeSnippet
  typedef   float VectorComponentType;
  typedef   itk::Vector< VectorComponentType, Dimension > VectorPixelType;
  typedef   itk::Image< VectorPixelType,  Dimension >   DeformationFieldType;
  
  typedef   unsigned char  PixelType;
  typedef   itk::Image< PixelType,  Dimension >   ImageType;
  // Software Guide : EndCodeSnippet


  typedef   itk::ImageFileReader< ImageType >  ReaderType;
  typedef   itk::ImageFileWriter< ImageType >  WriterType;
  
  // Software Guide : BeginLatex
  // The field is read from a file, through a reader instantiated over the
  // vector pixel types.
  // Software Guide : EndLatex
  // Software Guide : BeginCodeSnippet
  typedef   itk::ImageFileReader< DeformationFieldType >  FieldReaderType;
  // Software Guide : EndCodeSnippet

  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer = WriterType::New();

  reader->SetFileName( argv[1] );
  writer->SetFileName( argv[3] );

  // Software Guide : BeginCodeSnippet
  FieldReaderType::Pointer fieldReader = FieldReaderType::New();
  fieldReader->SetFileName( argv[2] );
  fieldReader->Update();

  DeformationFieldType::ConstPointer deformationField = fieldReader->GetOutput();
  // Software Guide : EndCodeSnippet



  // Software Guide : BeginLatex
  // The \doxygen{WarpImageFilter} is templated over the input image type, output
  // image type and the deformation field type. 
  // Software Guide : EndLatex
  // Software Guide : BeginCodeSnippet
  typedef itk::WarpImageFilter< ImageType, 
                                ImageType, 
                                DeformationFieldType  >  FilterType;

  FilterType::Pointer filter = FilterType::New();
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  // Typically the mapped position does not correspond to an integer pixel position 
  // in the input image. Interpolation via an image function is used to compute 
  // values at non-integer positions.
  // This is done via the \code{SetInterpolator()} method.
  // \index{itk::Warp\-Image\-Filter!SetInterpolator()}
  // Software Guide : EndLatex
  // Software Guide : BeginCodeSnippet
  typedef itk::LinearInterpolateImageFunction< 
                       ImageType, double >  InterpolatorType;

  InterpolatorType::Pointer interpolator = InterpolatorType::New();

  filter->SetInterpolator( interpolator );
  // Software Guide : EndCodeSnippet

  // SoftwareGuide : BeginLatex
  // The output image spacing and origin may be set via SetOutputSpacing(), 
  // SetOutputOrigin(). This is taken from the deformation field.
  // Software Guide : EndLatex
  // Software Guide : BeginCodeSnippet
  filter->SetOutputSpacing( deformationField->GetSpacing() );
  filter->SetOutputOrigin(  deformationField->GetOrigin() );

  filter->SetDeformationField( deformationField );
  // Software Guide : EndCodeSnippet

  filter->SetInput( reader->GetOutput() );
  writer->SetInput( filter->GetOutput() );


  
  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Exception thrown " << std::endl;
    std::cerr << excp << std::endl;
    }

  return EXIT_SUCCESS;

}

