/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkProbabilityDistributionTest.cxx
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

#include "itkProbabilityDistribution.h"
#include "itkObjectFactory.h"

namespace itk {
namespace Statistics {

class ProbabilityDistributionTestingHelper : public ProbabilityDistribution
{
public:
  typedef ProbabilityDistributionTestingHelper     Self;
  typedef ProbabilityDistribution                  Superclass;
  typedef SmartPointer<Self>                       Pointer;
  typedef SmartPointer<const Self>                 ConstPointer;

  itkTypeMacro(ProbabilityDistributionTestingHelper, ProbabilityDistribution);

  itkNewMacro(Self);

  virtual unsigned long GetNumberOfParameters() const { return 42; }
  virtual double EvaluatePDF(double ) const { return 42.0; }
  virtual double EvaluatePDF(double , const ParametersType&) const { return 42.0; }
  virtual double EvaluateCDF(double ) const { return 42.0; }
  virtual double EvaluateCDF(double , const ParametersType&) const { return 42.0; }
  virtual double EvaluateInverseCDF(double ) const  { return 42.0; }
  virtual double EvaluateInverseCDF(double , const ParametersType&) const  { return 42.0; }
  virtual bool HasMean() const { return true; }
  virtual bool HasVariance() const { return true; }
  virtual double GetMean() const { return 42.0; }
  virtual double GetVariance() const { return 42.0; }

  void RunTests()
    {
    std::cout << "Superclass name = " << this->Superclass::GetNameOfClass() << std::endl;
    std::cout << "Parameters = " << this->Superclass::GetParameters() << std::endl;
    }
};

}
}

int itkProbabilityDistributionTest(int, char* [] )
{
  std::cout << "itkProbabilityDistributionTest Test \n \n";

  typedef itk::Statistics::ProbabilityDistributionTestingHelper DistributionType;

  DistributionType::Pointer distributionFunction = DistributionType::New();

  std::cout << "GetNameOfClass() = " << distributionFunction->GetNameOfClass() << std::endl;
  std::cout << "HasMean()        = " << distributionFunction->HasMean() << std::endl;
  std::cout << "HasVariance()    = " << distributionFunction->HasVariance() << std::endl;
  std::cout << "Number of parameters = " << distributionFunction->GetNumberOfParameters() << std::endl;

  distributionFunction->Print( std::cout );

  distributionFunction->RunTests();

  return EXIT_SUCCESS;
}
