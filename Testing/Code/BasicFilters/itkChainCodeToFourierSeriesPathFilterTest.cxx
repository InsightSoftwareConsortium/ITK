/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkChainCodeToFourierSeriesPathFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include <iostream>
#include <math.h>
#include "itkPolyLineParametricPath.h"
#include "itkChainCodePath.h"
#include "itkFourierSeriesPath.h"
#include "itkPathToChainCodePathFilter.h"
#include "itkChainCodeToFourierSeriesPathFilter.h"

int itkChainCodeToFourierSeriesPathFilterTest(int, char*[])
{
  typedef itk::PolyLineParametricPath<2>       InPathType;
  typedef itk::ChainCodePath<2>                ChainPathType;
  typedef itk::FourierSeriesPath<2>            FSPathType;

  typedef InPathType::VertexType               VertexType;
  typedef InPathType::OffsetType               OffsetType;
  typedef InPathType::InputType                InPathInputType;

  typedef itk::PathToChainCodePathFilter<InPathType,ChainPathType>  Filter1Type;
  typedef itk::ChainCodeToFourierSeriesPathFilter<ChainPathType,FSPathType>
                                                                    Filter2Type;

  bool passed = true;


  InPathType::Pointer             inPath;
  ChainPathType::Pointer          chainPath;
  FSPathType::Pointer             outPath;

  // Setup the path
  std::cout << "Making a triangle Path with v0 at (30,30) -> (30,33) -> (33,33)" << std::endl;
  VertexType        v;
  inPath = InPathType::New();
  
  v.Fill(30);
  inPath->AddVertex(v);
  v[0]=30;
  v[1]=33;
  inPath->AddVertex(v);
  v.Fill(33);
  inPath->AddVertex(v);
  v.Fill(30);
  inPath->AddVertex(v);
  
  // Setup the first filter
  Filter1Type::Pointer filter1 = Filter1Type::New();
  filter1->SetInput(inPath);
  chainPath=filter1->GetOutput();
  
  // Setup the second filter
  Filter2Type::Pointer filter2 = Filter2Type::New();
  filter2->SetInput(filter1->GetOutput());
  outPath=filter2->GetOutput();
  
  filter2->Update();
  std::cout << "PathToChainCodePathFilter:  open test path is "
      << chainPath->NumberOfSteps() << " steps" << std::endl;
  if( chainPath->NumberOfSteps() != 9 )
    {
    passed = false;
    }
  std::cout << "ChainCodeToFourierSeriesPathFilter:  smoothed path is from ["
      << outPath->Evaluate(0.0) << "] to [" << outPath->Evaluate(1.0)
      << "] with a center at [" << outPath->Evaluate(0.5) << "]." << std::endl;
  // Floating point can be inprecise, so convert to rounded int for comparison:
  if( int(0.5+1000*(outPath->Evaluate(1.0))[0]) !=
      int(0.5+1000*(outPath->Evaluate(0.0))[0]) ||
      int(0.5+1000*(outPath->Evaluate(1.0))[1]) !=
      int(0.5+1000*(outPath->Evaluate(0.0))[1]) ||
      int(0.5+(outPath->Evaluate(0.5))[0]) < 31 ||
      int(0.5+(outPath->Evaluate(0.5))[0]) > 32 ||
      int(0.5+(outPath->Evaluate(0.5))[1]) != 33)
    {
    passed = false;
    }
  
  
  
  if (passed)
    {
    std::cout << "PathToChainCodePathFilter tests passed" << std::endl;
    return EXIT_SUCCESS;
    }
  else
    {
    std::cout << "PathToChainCodePathFilter tests failed" << std::endl;
    return EXIT_FAILURE;
    }
}
