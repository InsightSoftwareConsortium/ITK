/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNewTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include <iostream>
#include "itkMesh.h"
#include "itkCellInterfaceVisitor.h"
#include "itkCreateObjectFunction.h"
#include "itkPixelAccessor.h"
#include "itkBackwardDifferenceOperator.h"
#include "itkForwardDifferenceOperator.h"
#include "itkAddImageFilter.h"
#include "itkBinaryFunctorImageFilter.h"

// #include "itkDerivativeImageFilter.h"
// #include "itkDiscreteGaussianImageFilter.h"
// #include "itkGradientMagnitudeImageFilter.h"
// #include "itkMultiplyImageFilter.h"
// #include "itkRecursiveGaussianImageFilter.h"
// #include "itkFirstDerivativeRecursiveGaussianImageFilter.h"
// #include "itkSecondDerivativeRecursiveGaussianImageFilter.h"
// #include "itkSubtractImageFilter.h"
// #include "itkTernaryFunctorImageFilter.h"
// #include "itkTernaryAddImageFilter.h"
// #include "itkTernaryMagnitudeImageFilter.h"
// #include "itkTernaryMagnitudeSquaredImageFilter.h"
// #include "itkRelabelWatershedImageFilter.h"
// #include "itkWatershedImageFilter.h"
// #include "itkGaussianOperator.h"
// #include "itkImageAdaptor.h"
// #include "itkImageContainerInterface.h"
// #include "itkImageLinearIterator.h"
// #include "itkImageMomentsCalculator.h"
// #include "itkImageSliceIterator.h"
// #include "itkKalmanLinearEstimator.h"
// #include "itkMeshRegion.h"
// #include "itkNonThreadedShrinkImageFilterFilter.h"
// #include "itkNumericTraits.h"
// #include "itkRandomAccessNeighborhoodIterator.h"
// #include "itkRegion.h"
// #include "itkRegistrationMapper.h"
// #include "itkRegistrationMapperImage.h"
// #include "itkRegistrationMapperProcrustes.h"
// #include "itkRegistrationTransform.h"
// #include "itkSimilarityRegistrationMetric.h"
// #include "itkProcrustesRegistrationMetric.h"
// #include "itkOptimizer.h"
// #include "itkAmoebaOptimizer.h"
// #include "itkConjugateGradientOptimizer.h"
// #include "itkLBFGSOptimizer.h"
// #include "itkLevenbergMarquardtOptimizer.h"
// #include "itkNonLinearOptimizer.h"
// #include "itkRegistrationTransformation.h"
// #include "itkAffineTransform.h"
// #include "itkRegistrator3D2D.h"
// #include "itkRegistrator3D2DBatch.h"
// #include "itkRegistrator3D2DRecursive.h"
// #include "itkRGBPixel.h"
// #include "itkScalarVector.h"
// #include "itkVersion.h"




  class Bogus
  {
   public:
    // typedef Bogus Self;
    // typedef SmartPointer<Self> Pointer;
    // itkNewMacro(Self);
//     static Bogus* New() { return new Bogus(); };
//     void Register() {};
//     void UnRegister() {};

    float operator() ( double d, double d2 ) { return (float) d; };
    void Visit ( int, Bogus* ) {};
    int GetCellTopologyId() { return 1; };
    int GetTopologyId() { return 1; };
  };

int itkNewTest ( int argc, char* argv[] )
{
  // Call New and Print on as many classes as possible

  // CellInterfaceVisitorImplementation
  itk::CellInterfaceVisitorImplementation<float, itk::Mesh<float>::CellTraits, Bogus, Bogus>::Pointer CIVI = itk::CellInterfaceVisitorImplementation<float, itk::Mesh<float>::CellTraits, Bogus, Bogus>::New();

  // CreateObjectFunction
  // itk::CreateObjectFunction<itk::Mesh<int> >::Pointer COF = itk::CreateObjectFunction<itk::Mesh<int> >::New();
  // itk::Mesh<int>::Pointer B = COF->CreateObject();

  // PixelAccessor
  // unused: float f = 100.0;
  // unused: double d = itk::PixelAccessor<float, double>::Get ( f );

  // BackwardDifferenceOperator
  // Bad
  // typedef itk::BackwardDifferenceOperator<double> iDHBO;
  // iDHBO::Pointer dhbo = iDHBO::New();

  // ForwardDifferenceOperator
  // Bad
  // typedef itk::ForwardDifferenceOperator<double> iDHFO;
  // iDHFO::Pointer dhfo = iDHFO::New();

  // AddImageFilter
  typedef itk::AddImageFilter<itk::Image<double>, itk::Image<double>, itk::Image<double> > iFIA;
  iFIA::Pointer FIA = iFIA::New();
  
  // BinaryImageFilter
  typedef itk::BinaryFunctorImageFilter<itk::Image<double>, itk::Image<double>, itk::Image<double>, Bogus > iFIB;
  iFIB::Pointer FIB = iFIB::New();
  
  return 0;
  
}
