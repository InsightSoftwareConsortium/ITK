/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkOrthogonallyCorrected2DParametricPathTest.cxx
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

#include <iostream>
#include "itkOrthogonallyCorrected2DParametricPath.h"
#include "itkPolyLineParametricPath.h"

int itkOrthogonallyCorrected2DParametricPathTest(int, char*[])
{
  typedef  itk::OrthogonallyCorrected2DParametricPath   PathType;
  typedef  itk::PolyLineParametricPath<2>               OriginalPathType;
  typedef  PathType::InputType          InputType;
  typedef  PathType::IndexType          IndexType;
  typedef  PathType::OffsetType         OffsetType;
  typedef  OriginalPathType::VertexType VertexType;
  typedef  PathType::OrthogonalCorrectionTableType OrthogonalCorrectionTableType;
  typedef  PathType::OrthogonalCorrectionTablePointer OrthogonalCorrectionTablePointer;
  
  bool passed = true;

  InputType   input;
  OffsetType  offset;
  VertexType  v;

  // Original Path
  OriginalPathType::Pointer originalPath =  OriginalPathType::New();
  v.Fill(2);
  originalPath->AddVertex(v);
  v[0]=4;
  v[1]=13;
  originalPath->AddVertex(v);
  v[0]=12;
  v[1]=14;
  originalPath->AddVertex(v);
  v[0]=13;
  v[1]=3;
  originalPath->AddVertex(v);
  v.Fill(2);
  originalPath->AddVertex(v);

  // 24 Alternating offsets
  OrthogonalCorrectionTablePointer correctionTable = OrthogonalCorrectionTableType::New();
  for(int i=0; i<24; i++)
    {
    correctionTable->InsertElement(i, 1-(i%2) ); // alternates 1, 0
    }

  // Create the corrected path
  PathType::Pointer path = PathType::New();
  path->SetOriginalPath(originalPath);
  path->SetOrthogonalCorrectionTable(correctionTable);
  
  
  
  // Test the corrected path
  
  std::cout << "Evaluating at 0, 0.5, and 3.99999:  " << path->Evaluate(0) << ", "
       << path->Evaluate(0.5) << ", " << path->Evaluate(3.99999) << std::endl;
  
  std::cout << "Evaluating to an index at 0, 0.5, and 1.0:  "
       << path->EvaluateToIndex(0) << ", " << path->EvaluateToIndex(0.5)
       << ", " << path->EvaluateToIndex(0.0) << std::endl;
  if( int(0.5+1000*(path->Evaluate(0.0))[0]) != 1016 ||
      int(0.5+1000*(path->Evaluate(0.0))[1]) != 2179 )
    {
    std::cout << "OrthogonallyCorrected2DParametricPathTest:  EvaluateToIndex() Failed" << std::endl;
    passed = false;
    }
  
  input = 0;
  offset = path->IncrementInput( input );
  std::cout << "Incrementing the input from 0 to " << input << ":  " << offset << std::endl;
  
  input = 0.5;
  offset = path->IncrementInput( input );
  std::cout << "Incrementing the input from 0.5 to " << input << ":  " << offset << std::endl;
  
  if (passed)
    {
    std::cout << "OrthogonallyCorrected2DParametricPath tests passed" << std::endl;
    return EXIT_SUCCESS;
    }
  else
    {
    std::cout << "OrthogonallyCorrected2DParametricPath tests failed" << std::endl;
    return EXIT_FAILURE;
    }
}
