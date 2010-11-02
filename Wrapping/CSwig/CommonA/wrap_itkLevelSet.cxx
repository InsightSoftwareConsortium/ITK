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
#include "itkLevelSet.h"
#include "itkVectorContainer.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigMacros.h"
namespace _LSN_ {
    typedef itk::LevelSetNode<float         , 2 >::LevelSetNode itkLevelSetNodeF2 ;
    typedef itk::LevelSetNode<double        , 2 >::LevelSetNode itkLevelSetNodeD2 ;
    typedef itk::LevelSetNode<unsigned char , 2 >::LevelSetNode itkLevelSetNodeUC2;
    typedef itk::LevelSetNode<unsigned short, 2 >::LevelSetNode itkLevelSetNodeUS2;
    typedef itk::LevelSetNode<unsigned int  , 2 >::LevelSetNode itkLevelSetNodeUI2;
    typedef itk::LevelSetNode<signed char   , 2 >::LevelSetNode itkLevelSetNodeSC2;
    typedef itk::LevelSetNode<signed short  , 2 >::LevelSetNode itkLevelSetNodeSS2;
    typedef itk::LevelSetNode<signed int    , 2 >::LevelSetNode itkLevelSetNodeSI2;
    typedef itk::LevelSetNode<float         , 3 >::LevelSetNode itkLevelSetNodeF3 ;
    typedef itk::LevelSetNode<double        , 3 >::LevelSetNode itkLevelSetNodeD3 ;
    typedef itk::LevelSetNode<unsigned char , 3 >::LevelSetNode itkLevelSetNodeUC3;
    typedef itk::LevelSetNode<unsigned short, 3 >::LevelSetNode itkLevelSetNodeUS3;
    typedef itk::LevelSetNode<unsigned int  , 3 >::LevelSetNode itkLevelSetNodeUI3;
    typedef itk::LevelSetNode<signed char   , 3 >::LevelSetNode itkLevelSetNodeSC3;
    typedef itk::LevelSetNode<signed short  , 3 >::LevelSetNode itkLevelSetNodeSS3;
    typedef itk::LevelSetNode<signed int    , 3 >::LevelSetNode itkLevelSetNodeSI3;
}

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkLevelSet);
  namespace wrappers
  {

    ITK_WRAP_OBJECT2(VectorContainer, unsigned int, _LSN_::itkLevelSetNodeF2, itkNodeContainerF2);
    ITK_WRAP_OBJECT2(VectorContainer, unsigned int, _LSN_::itkLevelSetNodeD2, itkNodeContainerD2);
    ITK_WRAP_OBJECT2(VectorContainer, unsigned int, _LSN_::itkLevelSetNodeUC2, itkNodeContainerUC2);
    ITK_WRAP_OBJECT2(VectorContainer, unsigned int, _LSN_::itkLevelSetNodeUS2, itkNodeContainerUS2);

    ITK_WRAP_OBJECT2(VectorContainer, unsigned int, _LSN_::itkLevelSetNodeUI2, itkNodeContainerUI2);
    ITK_WRAP_OBJECT2(VectorContainer, unsigned int, _LSN_::itkLevelSetNodeSC2, itkNodeContainerSC2);
    ITK_WRAP_OBJECT2(VectorContainer, unsigned int, _LSN_::itkLevelSetNodeSS2, itkNodeContainerSS2);
    ITK_WRAP_OBJECT2(VectorContainer, unsigned int, _LSN_::itkLevelSetNodeSI2, itkNodeContainerSI2);

    ITK_WRAP_OBJECT2(VectorContainer, unsigned int, _LSN_::itkLevelSetNodeF3, itkNodeContainerF3);
    ITK_WRAP_OBJECT2(VectorContainer, unsigned int, _LSN_::itkLevelSetNodeD3, itkNodeContainerD3);
    ITK_WRAP_OBJECT2(VectorContainer, unsigned int, _LSN_::itkLevelSetNodeUC3, itkNodeContainerUC3);
    ITK_WRAP_OBJECT2(VectorContainer, unsigned int, _LSN_::itkLevelSetNodeUS3, itkNodeContainerUS3);

    ITK_WRAP_OBJECT2(VectorContainer, unsigned int, _LSN_::itkLevelSetNodeUI3, itkNodeContainerUI3);
    ITK_WRAP_OBJECT2(VectorContainer, unsigned int, _LSN_::itkLevelSetNodeSC3, itkNodeContainerSC3);
    ITK_WRAP_OBJECT2(VectorContainer, unsigned int, _LSN_::itkLevelSetNodeSS3, itkNodeContainerSS3);
    ITK_WRAP_OBJECT2(VectorContainer, unsigned int, _LSN_::itkLevelSetNodeSI3, itkNodeContainerSI3);
  }
}

#endif
