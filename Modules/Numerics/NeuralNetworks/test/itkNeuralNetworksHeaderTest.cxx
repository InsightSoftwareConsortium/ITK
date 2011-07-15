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

#include "itkBackPropagationLayer.hxx"
#include "itkBatchSupervisedTrainingFunction.hxx"
#include "itkCompletelyConnectedWeightSet.hxx"
#include "itkErrorBackPropagationLearningFunctionBase.hxx"
#include "itkErrorBackPropagationLearningWithMomentum.hxx"
#include "itkErrorFunctionBase.h"
#include "itkGaussianRadialBasisFunction.hxx"
#include "itkGaussianTransferFunction.hxx"
#include "itkHardLimitTransferFunction.hxx"
#include "itkIdentityTransferFunction.hxx"
#include "itkInputFunctionBase.h"
#include "itkIterativeSupervisedTrainingFunction.hxx"
#include "itkLayerBase.hxx"
#include "itkLogSigmoidTransferFunction.hxx"
#include "itkMeanSquaredErrorFunction.hxx"
#include "itkMultilayerNeuralNetworkBase.hxx"
#include "itkMultiquadricRadialBasisFunction.hxx"
#include "itkNeuralNetworkObject.hxx"
#include "itkNNetDistanceMetricBase.h"
#include "itkOneHiddenLayerBackPropagationNeuralNetwork.hxx"
#include "itkProductInputFunction.hxx"
#include "itkQuickPropLearningRule.hxx"
#include "itkRBFBackPropagationLearningFunction.hxx"
#include "itkRBFLayer.hxx"
#include "itkRBFNetwork.hxx"
#include "itkSigmoidTransferFunction.hxx"
#include "itkSignedHardLimitTransferFunction.hxx"
#include "itkSquaredDifferenceErrorFunction.hxx"
#include "itkSumInputFunction.hxx"
#include "itkSymmetricSigmoidTransferFunction.hxx"
#include "itkTanHTransferFunction.hxx"
#include "itkTanSigmoidTransferFunction.hxx"
#include "itkTrainingFunctionBase.hxx"
#include "itkTransferFunctionBase.h"
#include "itkTwoHiddenLayerBackPropagationNeuralNetwork.hxx"
#include "itkWeightSetBase.hxx"

int itkNeuralNetworksHeaderTest ( int , char * [] )
{

  return EXIT_SUCCESS;
}
