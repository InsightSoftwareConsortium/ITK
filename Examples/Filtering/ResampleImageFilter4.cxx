/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    ResampleImageFilter4.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

//  Software Guide : BeginLatex
//
//  The following example illustrates how to rotate an image around its center.
//
//
//  Software Guide : EndLatex 


#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkResampleImageFilter.h"
#include "itkAffineTransform.h"
#include "itkLinearInterpolateImageFunction.h"



int main( int argc, char ** argv )
{


  if( argc < 4 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  inputImageFile  outputImageFile  degrees" << std::endl; 
    return 1;
    }


  const     unsigned int   Dimension = 2;
  typedef   unsigned char  InputPixelType;
  typedef   unsigned char  OutputPixelType;

  typedef itk::Image< InputPixelType,  Dimension >   InputImageType;
  typedef itk::Image< OutputPixelType, Dimension >   OutputImageType;


  typedef itk::ImageFileReader< InputImageType  >  ReaderType;
  typedef itk::ImageFileWriter< OutputImageType >  WriterType;

  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer = WriterType::New();

  reader->SetFileName( argv[1] );
  writer->SetFileName( argv[2] );

  const double angleInDegrees = atof( argv[3] );

  typedef itk::ResampleImageFilter<
                  InputImageType, OutputImageType >  FilterType;

  FilterType::Pointer filter = FilterType::New();

  typedef itk::AffineTransform< double, Dimension >  TransformType;

  TransformType::Pointer transform = TransformType::New();
 

  typedef itk::LinearInterpolateImageFunction< 
                       InputImageType, double >  InterpolatorType;
  InterpolatorType::Pointer interpolator = InterpolatorType::New();
 
  filter->SetInterpolator( interpolator );

  filter->SetDefaultPixelValue( 100 );



  //  Software Guide : BeginLatex
  //
  //  The parameters of the output image are taken from the input image.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  reader->Update();
  const double * spacing = reader->GetOutput()->GetSpacing();
  const double * origin  = reader->GetOutput()->GetOrigin();
  InputImageType::SizeType size = 
      reader->GetOutput()->GetLargestPossibleRegion().GetSize();

  filter->SetOutputOrigin( origin );
  filter->SetOutputSpacing( spacing );
  filter->SetSize( size );
  // Software Guide : EndCodeSnippet




  filter->SetInput( reader->GetOutput() );
  writer->SetInput( filter->GetOutput() );




  //  Software Guide : BeginLatex
  //
  //  Rotations are performed around the origin of physical coordinates --- not
  //  the image origin nor the image center. Hence, the process of positioning
  //  the output image frame as it is shown in Figure
  //  \ref{fig:ResampleImageFilterTransformComposition6} requires three steps.
  //  First, the image origin must be moved to the origin of the coordinate
  //  system, this is done by applying a translation equal to the negative
  //  values of the image origin.
  //
  //  \index{itk::AffineTransform!Translate()}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  TransformType::OutputVectorType translation1;

  const double imageCenterX = origin[0] + spacing[0] * size[0] / 2.0;
  const double imageCenterY = origin[1] + spacing[1] * size[1] / 2.0;

  translation1[0] =   -imageCenterX;
  translation1[1] =   -imageCenterY;
  
  transform->Translate( translation1 );
  // Software Guide : EndCodeSnippet

std::cout << "imageCenterX = " << imageCenterX << std::endl;
std::cout << "imageCenterY = " << imageCenterY << std::endl;


  //  Software Guide : BeginLatex
  //
  //  
  //  Software Guide : EndLatex 
  
  // Software Guide : BeginCodeSnippet
  const double degreesToRadians = atan(1.0) / 45.0;
  const double angle = angleInDegrees * degreesToRadians;

  transform->Rotate2D( angle, false );
  // Software Guide : EndCodeSnippet





  //  Software Guide : BeginLatex
  //
  //  The third and final step implies to translate the image origin back to
  //  its previous location. This is be done by applying a translation equal to
  //  the origin values. 
  //
  //  \index{itk::AffineTransform!Translate()}
  //
  //  Software Guide : EndLatex 
  
  // Software Guide : BeginCodeSnippet
  TransformType::OutputVectorType translation2;

  translation2[0] =   imageCenterX;
  translation2[1] =   imageCenterY;
  
  transform->Translate( translation2, false );

  filter->SetTransform( transform );
  // Software Guide : EndCodeSnippet

 

  
  try 
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & excep )
    {
    std::cerr << "Exception catched !" << std::endl;
    std::cerr << excep << std::endl;
    }


 

  return 0;


}

