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

#include "itkAnnulusOperator.h"
#include "itkStdStreamStateSave.h"

int itkAnnulusOperatorTest(int, char* [] )
{
// Save the format stream variables for std::cout
// They will be restored when coutState goes out of scope
// scope.
  itk::StdStreamStateSave coutState(std::cout);

  const unsigned int Dimension = 2;
  typedef float                                        PixelType;
  typedef itk::AnnulusOperator< PixelType, Dimension > OperatorType;


  OperatorType normalizedAnnulus;
  normalizedAnnulus.NormalizeOn();
  normalizedAnnulus.SetInnerRadius( 3 );
  normalizedAnnulus.SetThickness( 2 );
  normalizedAnnulus.BrightCenterOff();
  try
    {
    normalizedAnnulus.CreateOperator();
    }
  catch (itk::ExceptionObject &e)
    {
    std::cout << e;
    return EXIT_FAILURE;
    }
  catch (std::exception &e)
    {
    std::cout << "Std exception" << e.what();
    return EXIT_FAILURE;
    }
  catch (...)
    {
    std::cout << "Unknown exception" << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Normalized annulus" << std::endl;
  normalizedAnnulus.Print( std::cout );
  std::cout << std::endl << std::endl;

  OperatorType::SizeType normalizedAnnulusSize;
  normalizedAnnulusSize = normalizedAnnulus.GetSize();

  std::cout << std::endl;
  std::cout << "Annulus, N = " << normalizedAnnulusSize
            << ", r = " << normalizedAnnulus.GetInnerRadius()
            << ", t = " << normalizedAnnulus.GetThickness()
            << std::endl;
  unsigned int i, j, k;

  for (i=0, k=0; i < normalizedAnnulusSize[1]; ++i)
    {
    for (j=0; j < normalizedAnnulusSize[0]; ++j, ++k)
      {
      std::cout.width(6);
      std::cout.precision(3);
      std::cout << normalizedAnnulus[k] << "\t";
      }
    std::cout << std::endl;
    }

  // Compute again having the center bright
  std::cout << std::endl << std::endl;
  std::cout << "Rebuiling annulus with a bright center";
  normalizedAnnulus.BrightCenterOn();
  normalizedAnnulus.CreateOperator();
  std::cout << ", N = " << normalizedAnnulusSize
            << ", r = " << normalizedAnnulus.GetInnerRadius()
            << ", t = " << normalizedAnnulus.GetThickness()
            << std::endl;


  for (i=0, k=0; i < normalizedAnnulusSize[1]; ++i)
    {
    for (j=0; j < normalizedAnnulusSize[0]; ++j, ++k)
      {
      std::cout.width(6);
      std::cout.precision(3);
      std::cout << normalizedAnnulus[k] << "\t";
      }
    std::cout << std::endl;
    }

  // Compute an annulus by hand
  std::cout << std::endl << std::endl;
  std::cout << "Creating an annulus by specifying values";
  OperatorType annulus;
  annulus.NormalizeOff();
  annulus.SetInnerRadius( 2 );
  annulus.SetThickness( 1 );
  annulus.SetExteriorValue( 1 );
  annulus.SetAnnulusValue( 8 );
  annulus.SetInteriorValue( 4 );
  annulus.CreateOperator();

  OperatorType::SizeType annulusSize;
  annulusSize = annulus.GetSize();
  std::cout << ", N = " << annulusSize
            << ", r = " << annulus.GetInnerRadius()
            << ", t = " << annulus.GetThickness()
            << ", i = " << annulus.GetInteriorValue()
            << ", a = " << annulus.GetAnnulusValue()
            << ", e = " << annulus.GetExteriorValue()
            << std::endl;

  for (i=0, k=0; i < annulusSize[1]; ++i)
    {
    for (j=0; j < annulusSize[0]; ++j, ++k)
      {
      std::cout.width(3);
      std::cout.precision(3);
      std::cout << annulus[k] << "\t";
      }
    std::cout << std::endl;
    }

  // Try making the inner radius 0
  std::cout << std::endl << std::endl;
  std::cout << "Setting inner radius to zero";
  annulus.SetInnerRadius(0);
  annulus.CreateOperator();

  annulusSize = annulus.GetSize();
  std::cout << ", N = " << annulusSize
            << ", r = " << annulus.GetInnerRadius()
            << ", t = " << annulus.GetThickness()
            << ", i = " << annulus.GetInteriorValue()
            << ", a = " << annulus.GetAnnulusValue()
            << ", e = " << annulus.GetExteriorValue()
            << std::endl;

  for (i=0, k=0; i < annulusSize[1]; ++i)
    {
    for (j=0; j < annulusSize[0]; ++j, ++k)
      {
      std::cout.width(3);
      std::cout.precision(3);
      std::cout << annulus[k] << "\t";
      }
    std::cout << std::endl;
    }


  // Try making the thickness 0
  std::cout << std::endl << std::endl;
  std::cout << "Setting thickness radius to zero";
  annulus.SetInnerRadius(2);
  annulus.SetThickness(0);
  annulus.CreateOperator();

  annulusSize = annulus.GetSize();
  std::cout << ", N = " << annulusSize
            << ", r = " << annulus.GetInnerRadius()
            << ", t = " << annulus.GetThickness()
            << ", i = " << annulus.GetInteriorValue()
            << ", a = " << annulus.GetAnnulusValue()
            << ", e = " << annulus.GetExteriorValue()
            << std::endl;

  for (i=0, k=0; i < annulusSize[1]; ++i)
    {
    for (j=0; j < annulusSize[0]; ++j, ++k)
      {
      std::cout.width(3);
      std::cout.precision(3);
      std::cout << annulus[k] << "\t";
      }
    std::cout << std::endl;
    }


  // Specifying a spacing
  std::cout << std::endl << std::endl;
  std::cout << "Specifying a spacing";
  OperatorType::SpacingType spacing;
  spacing[0] = 0.5;
  spacing[1] = 0.25;
  annulus.SetInnerRadius(2);
  annulus.SetThickness(1);
  annulus.SetSpacing(spacing);
  annulus.CreateOperator();

  annulusSize = annulus.GetSize();
  std::cout << ", N = " << annulusSize
            << ", r = " << annulus.GetInnerRadius()
            << ", t = " << annulus.GetThickness()
            << ", s = " << annulus.GetSpacing()
            << ", i = " << annulus.GetInteriorValue()
            << ", a = " << annulus.GetAnnulusValue()
            << ", e = " << annulus.GetExteriorValue()
            << std::endl;

  for (i=0, k=0; i < annulusSize[1]; ++i)
    {
    for (j=0; j < annulusSize[0]; ++j, ++k)
      {
      std::cout.width(3);
      std::cout.precision(3);
      std::cout << annulus[k] << "\t";
      }
    std::cout << std::endl;
    }

  return EXIT_SUCCESS;
}
