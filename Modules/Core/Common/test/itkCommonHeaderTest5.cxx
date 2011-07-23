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

#include "itkUnaryCorrespondenceMatrix.hxx"
#include "itkUnaryFunctorImageFilter.hxx"
#include "itkValarrayImageContainer.h"
#include "itkVariableLengthVector.hxx"
#include "itkVariableSizeMatrix.hxx"
#include "itkVector.hxx"
#include "itkVectorContainer.hxx"
#include "itkVectorImage.hxx"
#include "itkVectorImageNeighborhoodAccessorFunctor.h"
#include "itkVectorNeighborhoodInnerProduct.hxx"
#include "itkVersion.h"
#include "itkVersor.hxx"
#include "itkVertexCell.hxx"
#include "itkWeakPointer.h"
#include "itkXMLFileOutputWindow.h"
#include "itkXMLFilterWatcher.h"
#include "itkZeroFluxNeumannBoundaryCondition.hxx"
#include "VNLIterativeSparseSolverTraits.h"
#include "itkAtanRegularizedHeavisideStepFunction.h"
#include "itkAtanRegularizedHeavisideStepFunction.hxx"
#include "itkHeavisideStepFunction.h"
#include "itkHeavisideStepFunction.hxx"
#include "itkHeavisideStepFunctionBase.h"
#include "itkRegularizedHeavisideStepFunction.h"
#include "itkRegularizedHeavisideStepFunction.hxx"
#include "itkSinRegularizedHeavisideStepFunction.h"
#include "itkSinRegularizedHeavisideStepFunction.hxx"

int itkCommonHeaderTest5 ( int , char * [] )
{

  return EXIT_SUCCESS;
}
