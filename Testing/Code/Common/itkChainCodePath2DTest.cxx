/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkChainCodePath2DTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include <iostream.h>
#include "itkChainCodePath2D.h"

int itkChainCodePath2DTest(int, char*[])
{
  typedef  itk::ChainCodePath2D       PathType;
  typedef  PathType::IndexType        IndexType;
  typedef  PathType::OffsetType       OffsetType;
  typedef  PathType::ChainCodeType    ChainCodeType;
  
  bool passed = true;


  IndexType   index;
  OffsetType  offset;
  
  PathType::Pointer path = PathType::New();
  
  index[0]=3;
  index[1]=5;
  path->SetStart(index);
  
  for(int i=0; i<8; i++)
    {
    path->InsertStep(i*2,  i+1);
    path->InsertStep(i*2+1,i+1);
    }
  
  std::cout << "Path is " << path->NumberOfSteps() << " steps:  \""
       << path->GetChainCodeAsString() << "\"." << std::endl;
  
  offset[0]=0;
  offset[1]=-1;
  path->InsertStep(5,offset); // insert new step 5 = 5
  offset = path->Evaluate(5);
  std::cout <<"Inserted new step[5] of 5 = ("<<offset[0]<<","<<offset[1]<<")"<<std::endl;
  
  path->ChangeStep(8,3); // rotate the second 4 (now step 8) up to a 3
  offset = path->Evaluate(8);
  std::cout <<"Changed step[8] to 3 = ("<<offset[0]<<","<<offset[1]<<")"<<std::endl;
  
  std::cout << "Path is " << path->NumberOfSteps() << " steps:  \""
       << path->GetChainCodeAsString() << "\"." << std::endl;
  if( path->NumberOfSteps() != 17 )
    {
    passed = false;
    }
  
  
  index=path->GetStart();
  std::cout <<"Starting at index ("<<index[0]<<","<<index[1]<<")" << std::endl;
  for(unsigned int input=0;;)
    {
    offset=path->IncrementInput(input);
    if( offset[0] || offset[1] )
      {
      index=path->EvaluateToIndex(input);

      std::cout <<"Step["<<input-1<<"] is ("<<offset[0]<<","<<offset[1]<<")";
      std::cout <<"\t to index ("<<index[0]<<","<<index[1]<<")" << std::endl;
      }
    else
      break;
    }
  if( index != path->GetStart() )
    {
    passed = false;
    }

  if (passed)
    {
    std::cout << "ChainCode2D tests passed" << std::endl;
    return EXIT_SUCCESS;
    }
  else
    {
    std::cout << "ChainCode2D tests failed" << std::endl;
    return EXIT_FAILURE;
    }
}
