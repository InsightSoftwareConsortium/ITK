/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkLevelSet.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkLevelSet.h"
#include "itkVectorContainer.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigMacros.h"
namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkLevelSet);
  namespace wrappers
  {
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

    typedef itk::VectorContainer<unsigned int, itkLevelSetNodeF2  >::VectorContainer itkNodeContainerF2;
    typedef itk::VectorContainer<unsigned int, itkLevelSetNodeD2  >::VectorContainer itkNodeContainerD2;
    typedef itk::VectorContainer<unsigned int, itkLevelSetNodeUC2 >::VectorContainer itkNodeContainerUC2;
    typedef itk::VectorContainer<unsigned int, itkLevelSetNodeUS2 >::VectorContainer itkNodeContainerUS2;
    typedef itk::VectorContainer<unsigned int, itkLevelSetNodeUI2 >::VectorContainer itkNodeContainerUI2;
    typedef itk::VectorContainer<unsigned int, itkLevelSetNodeSC2 >::VectorContainer itkNodeContainerSC2;
    typedef itk::VectorContainer<unsigned int, itkLevelSetNodeSS2 >::VectorContainer itkNodeContainerSS2;
    typedef itk::VectorContainer<unsigned int, itkLevelSetNodeSI2 >::VectorContainer itkNodeContainerSI2;
    typedef itk::VectorContainer<unsigned int, itkLevelSetNodeF3  >::VectorContainer itkNodeContainerF3;
    typedef itk::VectorContainer<unsigned int, itkLevelSetNodeD3  >::VectorContainer itkNodeContainerD3;
    typedef itk::VectorContainer<unsigned int, itkLevelSetNodeUC3 >::VectorContainer itkNodeContainerUC3;
    typedef itk::VectorContainer<unsigned int, itkLevelSetNodeUS3 >::VectorContainer itkNodeContainerUS3;
    typedef itk::VectorContainer<unsigned int, itkLevelSetNodeUI3 >::VectorContainer itkNodeContainerUI3;
    typedef itk::VectorContainer<unsigned int, itkLevelSetNodeSC3 >::VectorContainer itkNodeContainerSC3;
    typedef itk::VectorContainer<unsigned int, itkLevelSetNodeSS3 >::VectorContainer itkNodeContainerSS3;
    typedef itk::VectorContainer<unsigned int, itkLevelSetNodeSI3 >::VectorContainer itkNodeContainerSI3;
  }
}

#endif
