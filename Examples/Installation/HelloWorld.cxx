//  Software Guide : BeginLatex
//
//  The following code demonstrates a small \code{Insight} program.
//
//  Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkImage.h"
#include <iostream>

int main()
{
  typedef itk::Image< unsigned short, 3 > ImageType;

  ImageType::Pointer image = ImageType::New();

  std::cout << "ITK Hello World !" << std::endl;

  return 0;
}
// Software Guide : EndCodeSnippet

//  Software Guide : BeginLatex
//
//  This code instantiates a $3D$ image\footnote{Also known as a
//  \emph{volume}} whose pixels are represented with type \code{unsigned
//  short}. The image is then constructed and assigned to a
//  \code{SmartPointer}. Although later in the text we will discuss
//  \code{SmartPointer}'s in detail, for now think of it as a handle on
//  an instance of an object (see section \ref{sec:SmartPointers} for
//  more information). 
//  Similarly, the \code{Image} class will be described in section 
//  \ref{sec:ImageSection}.
//
//  Software Guide : EndLatex 


