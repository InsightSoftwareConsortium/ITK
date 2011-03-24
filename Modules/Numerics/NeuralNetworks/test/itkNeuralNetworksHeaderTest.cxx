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

#include "itkBackPropagationLayer.txx"
#include "itkBatchSupervisedTrainingFunction.txx"
#include "itkCompletelyConnectedWeightSet.txx"
#include "itkErrorBackPropagationLearningFunctionBase.txx"
#include "itkErrorBackPropagationLearningWithMomentum.txx"
#include "itkErrorFunctionBase.h"
#include "itkGaussianRadialBasisFunction.txx"
#include "itkGaussianTransferFunction.txx"
#include "itkHardLimitTransferFunction.txx"
#include "itkIdentityTransferFunction.txx"
#include "itkInputFunctionBase.h"
#include "itkIterativeSupervisedTrainingFunction.txx"
#include "itkLayerBase.txx"
#include "itkLogSigmoidTransferFunction.txx"
#include "itkMeanSquaredErrorFunction.txx"
#include "itkMultilayerNeuralNetworkBase.txx"
#include "itkMultiquadricRadialBasisFunction.txx"
#include "itkNeuralNetworkObject.txx"
#include "itkNNetDistanceMetricBase.h"
#include "itkOneHiddenLayerBackPropagationNeuralNetwork.txx"
#include "itkProductInputFunction.txx"
#include "itkQuickPropLearningRule.txx"
#include "itkRBFBackPropagationLearningFunction.txx"
#include "itkRBFLayer.txx"
#include "itkRBFNetwork.txx"
#include "itkSigmoidTransferFunction.txx"
#include "itkSignedHardLimitTransferFunction.txx"
#include "itkSquaredDifferenceErrorFunction.txx"
#include "itkSumInputFunction.txx"
#include "itkSymmetricSigmoidTransferFunction.txx"
#include "itkTanHTransferFunction.txx"
#include "itkTanSigmoidTransferFunction.txx"
#include "itkTrainingFunctionBase.txx"
#include "itkTransferFunctionBase.h"
#include "itkTwoHiddenLayerBackPropagationNeuralNetwork.txx"
#include "itkWeightSetBase.txx"

int itkNeuralNetworksHeaderTest ( int , char * [] )
{

  return EXIT_SUCCESS;
}
