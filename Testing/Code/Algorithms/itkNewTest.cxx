/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkNewTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#include <iostream>
#include "itkMesh.h"
#include "itkCellInterfaceVisitor.h"
#include "itkCreateObjectFunction.h"
#include "itkPixelAccessor.h"
#include "itkBackwardDifferenceOperator.h"
#include "itkForwardDifferenceOperator.h"
#include "itkAddImageFilter.h"
#include "itkBinaryImageFilter.h"

// #include "itkDerivativeImageFilter.h"
// #include "itkDiscreteGaussianImageFilter.h"
// #include "itkGradientMagnitudeImageFilter.h"
// #include "itkMultiplyImageFilter.h"
// #include "itkRecursiveGaussianImageFilter.h"
// #include "itkFirstDerivativeRecursiveGaussianImageFilter.h"
// #include "itkSecondDerivativeRecursiveGaussianImageFilter.h"
// #include "itkSubtractImageFilter.h"
// #include "itkTernaryImageFilter.h"
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

int main ( int argc, char* argv[] )
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
  typedef itk::BinaryImageFilter<itk::Image<double>, itk::Image<double>, itk::Image<double>, Bogus > iFIB;
  iFIB::Pointer FIB = iFIB::New();
  
  return 0;
  
}
