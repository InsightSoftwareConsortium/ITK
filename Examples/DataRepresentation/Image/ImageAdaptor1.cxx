/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    ImageAdaptor1.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif


// Software Guide : BeginLatex
//
// This example illustrates the use of ImageAdaptors and PixelAccessors.  We
// show here how an ImageAdaptor can be used to cast the pixel type of an
// image.  In particular, an image of pixel type \code{unsigned char} is
// \emph{adapted} to make it look as and image of pixel type \code{float}.  The
// adaptor prevents the use of redundant memory allocation for representing the
// same information already available in the input image. This is done at the
// expense of some computational time. However, the only case in which
// \code{ImageAdaptors} are disadvantageous with respect to the
// \doxygen{CastImageFilter} is when the filter downstream in the pipeline is
// executed multiple times. In such situation a \code{CastImageFilter} will
// cache its output after the first execution and will not rexecute when the
// filter downstream is updated. The ImageAdaptor, on the other hand, will
// compute the casting every time the filter downstream is updated.
//
// ImageAdators are especially interesting in cases when spare access to pixels
// is performed, since the actual conversion is only occurring on the fly
// during pixel access.
// 
// \index{itk::ImageAdaptor!Instantiation}
// \index{itk::ImageAdaptor!Header}
// \index{itk::RGBPixel!Instantiation}
// \index{itk::RGBPixel!Header}
// \index{itk::RedPixelAccessor!Instantiation}
// \index{itk::RedPixelAccessor!Header}
//
// First, the header file of the \doxygen{ImageAdaptor} class must be included,
// along with the headerfor the \doxygen{RGBPixel} and the
// \doxygen{RedPixelAccessor}.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkImageAdaptor.h"
#include "itkRGBPixel.h"
#include "itkRedPixelAccessor.h"
// Software Guide : EndCodeSnippet

#include "itkImageRegionIteratorWithIndex.h"

//  Software Guide : BeginLatex
//
//  
//
//  Software Guide : EndLatex 



// Software Guide : BeginCodeSnippet
typedef itk::RGBPixel< float >         InputPixelType;
const unsigned int Dimension = 2;

typedef itk::Image< InputPixelType, Dimension >   ImageType;

typedef itk::RedPixelAccessor<float>      RedAccessorType;

typedef itk::ImageAdaptor< ImageType, RedAccessorType > RedAdaptorType;
// Software Guide : EndCodeSnippet

// Software Guide : BeginCodeSnippet
typedef itk::ImageRegionIteratorWithIndex< ImageType >       IteratorType;

typedef itk::ImageRegionIteratorWithIndex< RedAdaptorType >  RedIteratorType;



//-------------------------
//
//   Main code
//
//-------------------------
int main() {


  ImageType::SizeType size;
  size[0] = 2;
  size[1] = 2;

  ImageType::IndexType index;
  index[0] = 0;
  index[1] = 0;

  ImageType::RegionType region;
  region.SetIndex( index );
  region.SetSize(  size  );

  ImageType::Pointer image = ImageType::New();


  image->SetLargestPossibleRegion( region );
  image->SetBufferedRegion( region );
  image->SetRequestedRegion( region );
  image->Allocate();
  
  IteratorType  it1( image, image->GetRequestedRegion() );
  
  // Value to initialize the pixels
  ImageType::PixelType::ComponentType colorInit[3] = {1.0f, 0.5f, 0.5f};
  ImageType::PixelType color = colorInit;
  
  // Initializing all the pixel in the image
  it1.GoToBegin();
  while( !it1.IsAtEnd() )
  {
    it1.Set(color);
    ++it1;
  }

  // Reading the values to verify the image content
  std::cout << "--- Before --- " << std::endl;
  it1.GoToBegin();
  while( !it1.IsAtEnd() )
  {
    const ImageType::PixelType c( it1.Get() );
    std::cout << c.GetRed()   << "  ";
    std::cout << c.GetGreen() << "  ";
    std::cout << c.GetBlue()  << std::endl;
    ++it1;
  }



  RedAdaptorType::Pointer adaptor = RedAdaptorType::New();
  adaptor->SetImage( image );

 
  RedIteratorType  it2( adaptor, adaptor->GetRequestedRegion() );

  // Set the values of the Red component of image, using adaptor
  it2.GoToBegin();
  while( !it2.IsAtEnd() )
  {
    it2.Set( 0.4 );
    ++it2;
  }


  std::cout << "--- After --- " << std::endl;

  it1.GoToBegin();
  while( !it1.IsAtEnd() )
  {
    const ImageType::PixelType c( it1.Get() );
    std::cout << c.GetRed()   << "  ";
    std::cout << c.GetGreen() << "  ";
    std::cout << c.GetBlue()  << std::endl;
    ++it1;
  }


// Software Guide : EndCodeSnippet
  return 0;
}



