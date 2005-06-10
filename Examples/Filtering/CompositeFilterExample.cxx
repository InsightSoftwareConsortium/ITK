/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    CompositeFilterExample.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$
  Author:    Gavin Baker <gavinb@cs.mu.oz.au>

  Copyright (c) 2005 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

//  Software Guide : BeginLatex
//
//  The composite filter we will build combines three filters: a gradient
//  magnutide operator, which will calculate the first-order derivative of
//  the image; a thresholding step to select edges over a given strength;
//  and finally a rescaling filter, to ensure the resulting image data is
//  visible by scaling the intensity to the full spectrum of the output
//  image type.
//
//  Since this filter takes an image and produces another image (of
//  identical type), we will specialise the ImageToImageFilter:
//
//  Software Guide : EndLatex

//  Software Guide : BeginCodeSnippet
#include "itkImageToImageFilter.h"
//  Software Guide : EndCodeSnippet

//  Software Guide : BeginLatex
//
//  Next we include headers for the component filters:
//
//  Software Guide : EndLatex

//  Software Guide : BeginCodeSnippet
#include "itkGradientMagnitudeImageFilter.h"
#include "itkThresholdImageFilter.h"
#include "itkRescaleIntensityImageFilter.h"
//  Software Guide : EndCodeSnippet

#include "itkNumericTraits.h"

//  Software Guide : BeginLatex
//
//  Now we can declare the filter itself.  It is within the ITK namespace,
//  and we decide to make it use the same image type for both input and
//  output, thus the template declaration needs only one parameter.
//  Deriving from \code{ImageToImageFilter} provides default behaviour for
//  several important aspects, notably allocating the output image (and
//  making it the same dimensions as the input).
//
//  Software Guide : EndLatex

//  Software Guide : BeginCodeSnippet
namespace itk {

template <class TImageType>
class ITK_EXPORT CompositeExampleImageFilter :
    public ImageToImageFilter<TImageType, TImageType>
{
public:
//  Software Guide : EndCodeSnippet

//  Software Guide : BeginLatex
//
//  Next we have the standard declarations, used for object creation with
//  the object factory:
//
//  Software Guide : EndLatex

//  Software Guide : BeginCodeSnippet
  typedef CompositeExampleImageFilter               Self;
  typedef ImageToImageFilter<TImageType,TImageType> Superclass;
  typedef SmartPointer<Self>                        Pointer;
  typedef SmartPointer<const Self>                  ConstPointer;
//  Software Guide : EndCodeSnippet

  /** Method for creation through object factory */
  itkNewMacro(Self);

  /** Run-time type information */
  itkTypeMacro(CompositeExampleImageFilter, ImageToImageFilter);

  /** Display */
  void PrintSelf( std::ostream& os, Indent indent ) const;

//  Software Guide : BeginLatex
//
//  Here we declare an alias (to save typing) for the image's pixel type,
//  which determines the type of the threshold value.  We then use the
//  convenience macros to define the Get and Set methods for this parameter.
//
//  Software Guide : EndLatex

//  Software Guide : BeginCodeSnippet

  typedef typename TImageType::PixelType PixelType;

  itkGetMacro( Threshold, PixelType);
  itkSetMacro( Threshold, PixelType);

//  Software Guide : EndCodeSnippet

protected:

  CompositeExampleImageFilter();

//  Software Guide : BeginLatex
//
//  Now we can declare the component filter types, templated over the
//  enclosing image type:
//
//  Software Guide : EndLatex

//  Software Guide : BeginCodeSnippet
protected:

  typedef ThresholdImageFilter< TImageType > ThresholdType;
  typedef GradientMagnitudeImageFilter< TImageType, TImageType > GradientType;
  typedef RescaleIntensityImageFilter< TImageType, TImageType > RescalerType;
//  Software Guide : EndCodeSnippet

    void GenerateData();

private:

  CompositeExampleImageFilter(Self&);   // intentionally not implemented
  void operator=(const Self&);          // intentionally not implemented

//  Software Guide : BeginLatex
//
//  The component filters are declared as data members, all using the smart
//  pointer types.
//
//  Software Guide : EndLatex

//  Software Guide : BeginCodeSnippet

  typename GradientType::Pointer     m_GradientFilter;
  typename ThresholdType::Pointer    m_ThresholdFilter;
  typename RescalerType::Pointer     m_RescaleFilter;

  PixelType m_Threshold;
};

} /* namespace itk */
//  Software Guide : EndCodeSnippet

 
//  Software Guide : BeginLatex
//
//  The constructor sets up the pipeline, which involves creating the
//  stages, connecting them together, and setting default parameters.
//
//  Software Guide : EndLatex

namespace itk 
{

//  Software Guide : BeginCodeSnippet
template <class TImageType>
CompositeExampleImageFilter<TImageType>
::CompositeExampleImageFilter()
{
  m_GradientFilter = GradientType::New();
  m_ThresholdFilter = ThresholdType::New();
  m_RescaleFilter = RescalerType::New();

  m_ThresholdFilter->SetInput( m_GradientFilter->GetOutput() );
  m_RescaleFilter->SetInput( m_ThresholdFilter->GetOutput() );

  m_Threshold = 1;

  m_RescaleFilter->SetOutputMinimum(NumericTraits<PixelType>::NonpositiveMin());
  m_RescaleFilter->SetOutputMaximum(NumericTraits<PixelType>::max());
}
//  Software Guide : EndCodeSnippet


//  Software Guide : BeginLatex
//
//  The \code{GenerateData()} is where the composite magic happens.  First,
//  we connect the first component filter to the inputs of the composite
//  filter (the actual input, supplied by the upstream stage).  Then we
//  graft the output of the last stage onto the output of the composite,
//  which ensures the filter regions are updated.  We force the composite
//  pipeline to be processed by calling \code{Update()} on the final stage,
//  then graft the output back onto the output of the enclosing filter, so
//  it has the result available to the downstream filter.
//
//  Software Guide : EndLatex

//  Software Guide : BeginCodeSnippet
template <class TImageType>
void
CompositeExampleImageFilter<TImageType>::
GenerateData()
{
  m_GradientFilter->SetInput( this->GetInput() );

  m_ThresholdFilter->ThresholdBelow( this->m_Threshold );

  m_RescaleFilter->GraftOutput( this->GetOutput() );
  m_RescaleFilter->Update();
  this->GraftOutput( m_RescaleFilter->GetOutput() );
}
//  Software Guide : EndCodeSnippet
 
//  Software Guide : BeginLatex
//
//  Finally we define the \code{PrintSelf} method, which (by convention)
//  prints the filter parameters.  Note how it invokes the superclass to
//  print itself first, and also how the indentation prefixes each line.
//
//  Software Guide : EndLatex
//
//  Software Guide : BeginCodeSnippet

template <class TImageType>
void
CompositeExampleImageFilter<TImageType>::
PrintSelf( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf(os,indent);

  os
    << indent << "Threshold:" << this->m_Threshold
    << std::endl;
}

} /* end namespace itk */

//  Software Guide : EndCodeSnippet



//  Software Guide : BeginLatex
//
//  It is important to note that in the above example, none of the internal
//  details of the pipeline were exposed to users of the class.  The interface
//  consisted of the Threshold parameter (which happened to change the value in
//  the component filter) and the regular ImageToImageFilter interface.  This
//  example pipeline is illustrated in
//  Figure~\ref{fig:CompositeExamplePipeline}.
//
//  Software Guide : EndLatex

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

int main( int argc, char* argv[] )
{
  typedef itk::Image<short, 2>                        ImageType;
  typedef itk::ImageFileReader<ImageType>             ReaderType;
  typedef itk::ImageFileWriter<ImageType>             WriterType;

  typedef itk::CompositeExampleImageFilter<ImageType> FilterType;

  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer = WriterType::New();
  FilterType::Pointer filter = FilterType::New();

  reader->SetFileName( argv[1] );
  filter->SetInput( reader->GetOutput() );
  filter->SetThreshold( 20 );
  writer->SetInput( filter->GetOutput() );
  writer->SetFileName( argv[2] );

  try
    {
    writer->Update();
    }
  catch ( itk::ExceptionObject e )
    {
    std::cerr << "Error: " << e << std::endl;
    }

  return 0;
}
