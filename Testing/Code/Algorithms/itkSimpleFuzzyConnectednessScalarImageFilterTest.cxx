/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSimpleFuzzyConnectednessScalarImageFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkSimpleFuzzyConnectednessScalarImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include <iomanip>

const int WIDTH = 20;
const int HEIGHT = 20;
const unsigned short TestingImage [400]={
297,277,317,289,300,312,306,283,282,308,308,342,335,325,315,300,304,318,307,308,

319,276,311,282,309,273,308,277,296,313,308,333,322,317,302,330,339,340,325,315,

272,316,296,299,298,310,271,319,315,280,338,342,349,349,330,319,313,314,342,301,

274,274,312,282,277,303,313,300,275,292,341,336,324,310,337,323,322,347,337,305,

296,272,304,304,281,304,302,284,315,270,325,349,337,317,308,332,324,303,334,325,

291,272,289,317,289,310,305,316,292,307,307,343,341,329,309,308,340,323,307,325,

274,286,282,291,270,296,274,288,274,275,341,301,325,333,321,305,347,346,327,317,

282,315,270,314,290,304,297,304,309,290,309,338,341,319,325,344,301,349,328,302,

314,289,296,270,274,277,317,280,278,285,315,347,314,316,307,336,341,335,330,337,

281,291,317,317,302,304,272,277,318,319,305,322,337,334,327,303,321,310,334,314,

321,311,328,326,331,308,325,348,334,346,309,316,308,349,322,349,304,331,304,321,

346,302,344,314,311,338,320,310,331,330,322,323,329,331,342,341,331,336,328,318,

309,336,327,345,312,309,330,334,329,317,324,304,337,330,331,334,340,307,328,343,

345,330,336,302,333,348,315,328,315,308,305,343,342,337,307,316,303,303,332,341,

327,322,320,314,323,325,307,316,336,315,341,347,343,336,315,347,306,303,339,326,

330,347,303,343,332,316,305,325,311,314,345,327,333,305,324,318,324,339,325,319,

334,326,330,319,300,335,305,331,343,324,337,324,319,339,327,317,347,331,308,318,

306,337,347,330,301,316,302,331,306,342,343,329,336,342,300,306,335,330,310,303,

308,331,317,315,318,333,340,340,326,330,339,345,307,331,320,312,306,342,303,321,

328,315,327,311,315,305,340,306,314,339,344,339,337,330,318,342,311,343,311,312
};



int itkSimpleFuzzyConnectednessScalarImageFilterTest(int, char* [] ){
  int i, j;
  typedef itk::Image<bool,2> BinaryImage2D;
  typedef itk::Image<unsigned short,2> UShortImage2D;
  typedef itk::SimpleFuzzyConnectednessScalarImageFilter<UShortImage2D,BinaryImage2D> FuzzyUShort;

  FuzzyUShort::Pointer testFuzzy=FuzzyUShort::New();
  UShortImage2D::Pointer inputimg=UShortImage2D::New();
  UShortImage2D::SizeType size={{HEIGHT,WIDTH}};
  UShortImage2D::IndexType index;
  index.Fill(0);
  UShortImage2D::RegionType region;

  region.SetSize(size);
  region.SetIndex(index);

  inputimg->SetLargestPossibleRegion( region );
  inputimg->SetBufferedRegion( region );
  inputimg->SetRequestedRegion( region );
  inputimg->Allocate();



/* Testing Image:
   background: uniform distributed random number ~(300-350) 
   object (rectangle), value ~(270-320) 
   all pre-generated on a Windows Based PC using rand()
  */


  itk::ImageRegionIteratorWithIndex <UShortImage2D> it(inputimg, region);
  int k=0;
  while( !it.IsAtEnd()) {    
    it.Set(TestingImage[k]);
    k++;
    ++it;
  }


/* print the input image */ 
  std::cout<<"The Input Image"<<std::endl;
  it.GoToBegin();
  for(i = 0;i < HEIGHT; i++){
    for (j = 0; j < WIDTH; j++){
    std::cout << std::setw(4) << it.Get()<<" ";
      ++it;
    }
    std::cout<<std::endl;
  }


/* execute the segmentation subroutine*/
/* set input and the seed */
  testFuzzy->SetInput(inputimg);
  index[0] = 5;
  index[1] = 5;
  testFuzzy->SetObjectsSeed(index);

/* set the parameters */
  testFuzzy->SetParameters(270.0,2500.0,1.0,1.0,1.0);
  testFuzzy->SetThreshold(0.5);
  testFuzzy->Update();

/* printout the segmentation result */
  std::cout<<"Segmentation Result"<<std::endl;
  itk::ImageRegionIteratorWithIndex <BinaryImage2D> ot(testFuzzy->GetOutput(), region);
  for(i = 0;i < HEIGHT; i++){
    for (j = 0; j < WIDTH; j++){
      std::cout<<ot.Get();
      ++ot;
    }
    std::cout<<std::endl;
  }


  testFuzzy->UpdateThreshold(0.8);
  std::cout<<std::endl<<"Update threshold"<<std::endl;
  ot.GoToBegin();
  for(i = 0;i < HEIGHT; i++){
    for (j = 0; j < WIDTH; j++){
      std::cout<<ot.Get();
      ++ot;
    }
    std::cout<<std::endl;
  }

  return 0;
}


