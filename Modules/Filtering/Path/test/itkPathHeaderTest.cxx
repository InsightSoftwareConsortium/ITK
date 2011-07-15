/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include <iostream>

#include "itkPolyLineParametricPath.hxx"
#include "itkOrthogonalSwath2DPathFilter.hxx"
#include "itkPathToChainCodePathFilter.hxx"
#include "itkPathIterator.hxx"
#include "itkPathFunctions.h"
#include "itkImageAndPathToImageFilter.hxx"
#include "itkPathSource.hxx"
#include "itkPathToPathFilter.hxx"
#include "itkChainCodePath2D.h"
#include "itkChainCodePath.hxx"
#include "itkPath.hxx"
#include "itkExtractOrthogonalSwath2DImageFilter.hxx"
#include "itkFourierSeriesPath.h"
#include "itkPathToImageFilter.hxx"
#include "itkOrthogonallyCorrected2DParametricPath.h"
#include "itkParametricPath.h"
#include "itkChainCodeToFourierSeriesPathFilter.hxx"
#include "itkParametricPath.hxx"
#include "itkPathAndImageToPathFilter.hxx"
#include "itkFourierSeriesPath.hxx"
#include "itkPathConstIterator.hxx"
#include "itkPathSource.h"
#include "itkPath.h"



int itkPathHeaderTest ( int , char * [] )
{

  return EXIT_SUCCESS;
}
