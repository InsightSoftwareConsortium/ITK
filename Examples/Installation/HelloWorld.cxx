//  Software Guide : BeginLatex
//
//  The following code is the minimal \code{Insight} program.
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
