/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    CastingImageFilters.cxx
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
//  Due to the use of generic programming in the toolkit, most of the types are
//  resolved at compiling time. Few decisions on type conversion are left to
//  run time. It is up to the user to anticipate the pixel type conversions
//  required in the data pipeline. It is not desirable in medical imaging to
//  let PixelTypes to be transparent since this may lead to unadvertedly
//  loosing valuable information.
//
//  This section introduces the mechanisms for implementing explicit
//  casting on the images that flow through the pipeline. The following four
//  filters are treated in this section \code{CastImageFilter}, 
//  \code{RescaleIntensityImageFilter},  
//  \code{ShiftScaleImageFilter} and \code{NormalizeImageFilter}.  These filters
//  are totally independent between them. There are presented together here
//  only with the purpose of comparing their individual features.
//
//  The \code{itk::CastImageFilter} is a very simple filter that will act
//  pixel-wise on an input image by casting every pixel value to the pixel type
//  of the output image. Note that this filter do not perform any arithmetical
//  operation on the intensities. The actual effect of the filter is just
//  equivalent to blindly performing C-Style casting on every pixel.
//
//  The \code{itk::RescaleIntensityImageFilter} acts by linearly scaling the
//  pixels values in such a way that the minimum and maximum values of the
//  input image will be mapped to a minimum and maximum values provided by the
//  user. This is a typical action for forcing the dynamic range of the image
//  to fit in a particular scale. This is common in image display, for example.
//  The linear transformation applied by this filter can be expressed as 
//
//  \[ outputPixel = \frac{( inputPixel - inpMin)}{(inpMax-inpMin)} * (outMax -
//  outMin ) + outMin \]
//
//  The \code{itk::ShiftScaleImageFilter} also applies a linear transformation
//  to the intensities of the input image. However the transformation is
//  specified by the user in the form of a multiplying factor and a value to be
//  added. This can be expresses as 
//
//  \[  outputValue = inputPixel * Scale + Shift \].
//
//  The \code{itk::NormalizeImageFilter} applies a linear transformation to the
//  intensities of the input image. The parameters of the transformation are
//  computed internally in such a way that the statistical distribution of gray
//  levels on the output image will have zero mean and a variance of one. This
//  intensity correction is particularly useful in registration applications as
//  a pre-processing step leading to the evaluation of Mutual Information
//  metrics. The linear transformation of this filter can be described as:
//
//  \[ outputPixel = \frac{( inputPixel - mean )}{ \sqrt{ variance } } \]
//
//  \index{itk::CastImageFilter}
//  \index{itk::RescaleIntensityImageFilter}
//  \index{itk::ShiftScaleImageFilter}
//  \index{itk::NormalizeImageFilter}
//
//  As usual, the first step required to use these filters is to include their
//  header files.
//
//  Software Guide : EndLatex 


#include "itkImage.h"
#include "itkImageFileReader.h"

// Software Guide : BeginCodeSnippet
#include "itkCastImageFilter.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkShiftScaleImageFilter.h"
#include "itkNormalizeImageFilter.h"
// Software Guide : EndCodeSnippet


int main( int argc, char ** argv )
{

  //  Software Guide : BeginLatex
  //
  //  Let's define pixel types for the input and output images.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef   unsigned char    InputPixelType;
  typedef   float            OutputPixelType;
  // Software Guide : EndCodeSnippet




  //  Software Guide : BeginLatex
  //
  //  Then, let's define the input and output image types.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef itk::Image< InputPixelType,  3 >   InputImageType;
  typedef itk::Image< OutputPixelType, 3 >   OutputImageType;
  // Software Guide : EndCodeSnippet



  typedef itk::ImageFileReader< InputImageType >  ReaderType;




  //  Software Guide : BeginLatex
  //
  //  The filters can be instantiated now using the defined image types.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef itk::CastImageFilter<
               InputImageType, OutputImageType >  CastFilterType;

  typedef itk::RescaleIntensityImageFilter<
               InputImageType, OutputImageType >  RescaleFilterType;

  typedef itk::ShiftScaleImageFilter<
               InputImageType, OutputImageType >  ShiftScaleFilterType;

  typedef itk::NormalizeImageFilter<
               InputImageType, OutputImageType >  NormalizeFilterType;
  // Software Guide : EndCodeSnippet

  ReaderType::Pointer reader = ReaderType::New();


  //  Software Guide : BeginLatex
  //
  //  Object filters are created by invoking the \code{New()} operator and
  //  assigning the result to SmartPointers.
  //
  //  Software Guide : EndLatex 



  // Software Guide : BeginCodeSnippet
  CastFilterType::Pointer       castFilter       = CastFilterType::New();
  RescaleFilterType::Pointer    rescaleFilter    = RescaleFilterType::New();
  ShiftScaleFilterType::Pointer shiftFilter      = ShiftScaleFilterType::New();
  NormalizeFilterType::Pointer  normzalizeFilter = NormalizeFilterType::New();
  // Software Guide : EndCodeSnippet



  reader->SetFileName( argv[1] );
  


  //  Software Guide : BeginLatex
  //
  //  The output of a reader filter (whose creation is not shown here) is now
  //  connected as input to the various casting filters.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  castFilter->SetInput(       reader->GetOutput() );
  shiftFilter->SetInput(      reader->GetOutput() );
  rescaleFilter->SetInput(    reader->GetOutput() );
  normzalizeFilter->SetInput( reader->GetOutput() );
  // Software Guide : EndCodeSnippet





  //  Software Guide : BeginLatex
  //
  //  We proceed now to setup the parameters required by each particular
  //  filter. The \code{CastImageFilter} and the \code{NormalizeImageFilter} do
  //  not require any parameters. The \code{RescaleIntensityImageFilter}, on
  //  the other hand, requires the user to provide the desired minimum and
  //  maximum pixel values of the output image. This is done by using the
  //  \code{SetOutputMinimum()} and \code{SetOutputMaximum()} methods as
  //  illustrated below.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  rescaleFilter->SetOutputMinimum(  10 );
  rescaleFilter->SetOutputMaximum( 250 );
  // Software Guide : EndCodeSnippet




  //  Software Guide : BeginLatex
  //
  //  The \code{ShiftScaleImageFilter} requires the user to provide a factor to
  //  multiply all the pixels values and an additional value to be added after
  //  the multiplication. The methos \code{SetShift()} and \code{SetScale()}
  //  are used for this purpose.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  shiftFilter->SetShift( 25 );
  shiftFilter->SetScale( 1.2 );
  // Software Guide : EndCodeSnippet





  //  Software Guide : BeginLatex
  //
  //  Finally, the execution of the filters can be triggered by invoking the
  //  \code{Update()} method.
  //
  //  Software Guide : EndLatex 


  // Software Guide : BeginCodeSnippet
  castFilter->Update();
  shiftFilter->Update();
  rescaleFilter->Update();
  normzalizeFilter->Update();
  // Software Guide : EndCodeSnippet



  return 0;

}

