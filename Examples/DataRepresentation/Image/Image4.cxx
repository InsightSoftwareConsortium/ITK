/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    Image4.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

// Software Guide : BeginLatex
//
// Even though ITK can be used for performing general image processing tasks,
// the main target of the toolkit is the processing of medical image data. 
// In that perspective, additional information about the images is considered
// mandatory. In particular the information associated with the physical
// spacing between pixels and the position of the image in space with 
// respect to some world coordinates reference system are considered to
// be extremly important.
//
// The values assiged to origin and spacing are fundamental in many
// applications. Registration, for example, is entirely performed in
// physical coordinates. Poorly defined spacing and origins will result
// in inconsistent results in such processes. Medical images lacking 
// spatial information should not be used for purposes like medical
// diagnosis, image analysis, feature extraction, assisted radiation
// therapy or image guided surgery. In other words, medical images
// lacking spacing are not only useless but rather hazardous. 
//
// Software Guide : EndLatex 



#include "itkImage.h"
#include "itkPoint.h"

int main()
{

  typedef itk::Image< unsigned short, 3 > ImageType;

  ImageType::Pointer image = ImageType::New();

  ImageType::IndexType start;
  ImageType::SizeType  size;

  size[0]  = 200;  // size along X
  size[1]  = 200;  // size along Y
  size[2]  = 200;  // size along Z

  start[0] =   0;  // first index on X
  start[1] =   0;  // first index on Y
  start[2] =   0;  // first index on Z

  ImageType::RegionType region;
  
  image->SetRegions( region );
  image->Allocate();

  image->FillBuffer( 0 );

  // Software Guide : BeginLatex
  //
  // Image spacing is represented in an C-like array of doubles
  // whose size matches the dimension of the image. In order
  // to manually set the spacing of the image, an array of 
  // the corresponding type must be created. The elements of
  // the array should then be initialized with the spacing
  // between the centers of the adjacent pixels. The following 
  // code illustrates the methods available in the image class 
  // for dealing with spacing and origin.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet 
  double spacing[ ImageType::ImageDimension ];

  spacing[0] = 0.33; // spacing in millimeters along X
  spacing[1] = 0.33; // spacing in millimeters along Y
  spacing[2] = 1.20; // spacing in millimeters along Z
  // Software Guide : EndCodeSnippet 




  // Software Guide : BeginLatex
  //
  // The array can be assigned to the image using 
  // the \texttt{SetSpacing()} method.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet 
  image->SetSpacing( spacing );
  // Software Guide : EndCodeSnippet 



  // Software Guide : BeginLatex
  //
  // Image origin is managed in a similar way to the spacing.
  // A C-like array of doubles matching the dimension of the
  // image must be allocated first. The coordinates of the
  // origin can then be assigned to every component. These
  // coordinates correspond to the position of the first 
  // pixel of the image with respect to an arbitrary reference
  // system in physical space. It is the user's responsibility
  // to make sure that multiple images used in the same application
  // are using a consisten reference system. This is extremly
  // important in image registration applications.
  // 
  // The following code illustrates the creation and assignment
  // of a variable suitable for initializing the image origin.
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet 
  double origin[ ImageType::ImageDimension ];

  origin[0] = 0.0;  // coordinates of the 
  origin[1] = 0.0;  // first pixel in N-D
  origin[2] = 0.0;

  image->SetOrigin( origin );
  // Software Guide : EndCodeSnippet 


  // Software Guide : BeginLatex
  //
  // Once the spacing and origin of the image have been initialized
  // the image is in condition to map pixel indices to physical 
  // space coordinates and back. The following code illustrates 
  // how a point in physical space can be mapped into an image index
  // for the purpose of reading the content of the closest pixel.
  //
  // First, a PointType must be declared. The point type is templated
  // over the type used to represent coordinates and over the dimension
  // of the space. In this particular case, the dimension of the 
  // point must match the dimension of the image. 
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet 
  typedef itk::Point< double, ImageType::ImageDimension > PointType;
  // Software Guide : EndCodeSnippet 



  // Software Guide : BeginLatex
  //
  // A point, like the index, is a relatively small and simple object.
  // for this reason it is not reference counted like the large data 
  // objects in ITK and as a consequence it is not managed by SmartPointers.
  // Point objects are simply declared as instances of any other C++ class.
  // Once the point is declared, its components can be accessed using 
  // traditional array notation. In particular the \texttt{[]} operator
  // is available. For efficiency reasons, no bound checking is performed
  // on the index used to access a particular point component. It is the
  // user's responsibility to make sure that the index is in the range
  // $\{0,Dimension-1\}$
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet 
  PointType point;

  point[0] = 1.45;
  point[1] = 7.21;
  point[2] = 9.28;
  // Software Guide : EndCodeSnippet 

  // Software Guide : BeginLatex
  // 
  // The image can map this point to an index using the values of the
  // current spacing and origin. An index object must be provided to
  // receive the results of the mapping. The index object can be 
  // instantiated by using the \texttt{IndexType} defined in the Image
  // type.
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet 
  ImageType::IndexType pixelIndex;
  // Software Guide : EndCodeSnippet 


  // Software Guide : BeginLatex
  // 
  // The \texttt{TransformPhysicalPointToIndex()} method of the image class
  // will compute the pixel index closest to the point provided. The method
  // checks for this index to be contained inside the current buffered pixel 
  // data. The method returns a boolean indicating whether the resulting 
  // index falls inside the buffered region or not. The output index should 
  // not be used when the returned value of the method is \texttt{false}.
  //
  // The following lines illustrate the point to index mapping and the subsequent
  // use of the pixel index for accessing pixel data from the image. 
  //
  // Software Guide : EndLatex


  // Software Guide : BeginCodeSnippet 
  image->TransformPhysicalPointToIndex( point, pixelIndex ); 

  ImageType::PixelType pixelValue = image->GetPixel( pixelIndex );
  // Software Guide : EndCodeSnippet 


  // Software Guide : BeginLatex
  // 
  // Remember that \texttt{GetPixel()} and \texttt{SetPixel()} are very 
  // inefficient methods for accessing pixel data. Image iterators should be
  // used for when massive access to pixel data is required.
  //
  // Software Guide : EndLatex



  return 0;

}

