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
#ifndef _itkCumulativeGaussianCostFunction_cxx
#define _itkCumulativeGaussianCostFunction_cxx

#include "itkCumulativeGaussianCostFunction.h"
#include "itkMath.h"

namespace itk
{
CumulativeGaussianCostFunction
::CumulativeGaussianCostFunction()
{
  // Initial values for fit error and range dimension.
  m_RangeDimension = 0;
  m_OriginalDataArray = new MeasureType();
  m_MeasurePointer = new MeasureType();
}

CumulativeGaussianCostFunction
::~CumulativeGaussianCostFunction()
{
  delete m_OriginalDataArray;
  delete m_MeasurePointer;
}

void
CumulativeGaussianCostFunction
::SetOriginalDataArray(MeasureType *setOriginalDataArray)
{
  // Set the original data array.
  m_OriginalDataArray->SetSize(m_RangeDimension);

  for ( int i = 0; i < (int)( setOriginalDataArray->GetNumberOfElements() ); i++ )
    {
    m_OriginalDataArray->put( i, setOriginalDataArray->get(i) );
    }
}

double
CumulativeGaussianCostFunction
::CalculateFitError(MeasureType *setTestArray)
{
  // Use root mean square error as a measure of fit quality.
  unsigned int numberOfElements = m_OriginalDataArray->GetNumberOfElements();

  if ( numberOfElements != setTestArray->GetNumberOfElements() )
    {
    return 1;
    }
  double fitError = 0.0;
  for ( int i = 0; i < (int)( numberOfElements ); i++ )
    {
    fitError += std::pow( ( setTestArray->get(i) - m_OriginalDataArray->get(i) ), 2 );
    }
  return ( std::sqrt( ( 1 / numberOfElements ) * fitError ) );
}

double
CumulativeGaussianCostFunction
::EvaluateCumulativeGaussian(double argument) const
{
  // Evaluate the Cumulative Gaussian for a given argument.
  double erfValue;

  // Out of bounds of the table, but it's close to 1 or -1.
  if ( argument < -3 || argument > 3 )
    {
    if ( argument > 0 )
      {
      erfValue = 1;
      }
    else
      {
      erfValue = -1;
      }
    }

  // Interpolation between table lookup entries.
  else
    {
      // Tabulated error function evaluated for 0 to 299.
    double y[300] =
      {
      0,          .011283416, .022564575, .033841222, .045111106, .056371978, .067621594, .07885772,  .090078126,
      .101280594,
      .112462916, .123622896, .134758352, .145867115, .156947033, .167995971, .179011813, .189992461, .200935839,
      .211839892,
      .222702589, .233521923, .244295911, .255022599, .265700058, .276326389, .286899723, .297418219, .307880068,
      .318283496,
      .328626759, .33890815,  .349125995, .359278655, .369364529, .379382053, .3893297,   .399205983, .409009452,
      .418738698,
      .428392352, .43796909,  .447467618, .456886694, .466225115, .475481719, .484655389, .49374505,  .50274967,
      .51166826,
      .520499876, .529243617, .537898627, .546464093, .554939245, .563323359, .571615763, .579815806, .5879229,
      .595936496,
      .60385609,  .611681217, .61941146,  .627046441, .634585826, .642029324, .649376683, .656627696, .663782195,
      .670840052,
      .677801193, .684665264, .691432825, .698103704, .704677825, .71115543,  .717536534, .723821437, .730010238,
      .73610324,
      .74210079,  .748003138, .75381059,  .759523625, .76514256,  .770667933, .776100122, .781439725, .786687219,
      .791843127,
      .796908113, .801882743, .80676762,  .811563474, .816270948, .820890718, .825423575, .82987023,  .834231422,
      .838508001,
      .842700735, .846810448, .850837952, .854784156, .8586499,   .862436067, .866143531, .86977325,  .873326119,
      .876803068,
      .880205041, .88353297,  .886787854, .88997064,  .893082302, .896123821, .899096169, .90200037,  .904837402,
      .907608265,
      .91031396,  .912955492, .915533856, .918050082, .920505165, .922900112, .925235928, .927513617, .929734183,
      .931898615,
      .934007929, .936063109, .938065143, .940015016, .941913707, .943762189, .94556143,  .947312386, .949016025,
      .950673287,
      .952285112, .953852432, .955376173, .956857248, .958296565, .959695022, .961053506, .962372893, .963654059,
      .964897859,
      .966105142, .967276744, .968413493, .969516206, .970585687, .971622731, .97262812,  .973602626, .974547008,
      .975462012,
      .97634838,  .977206834, .978038086, .978842837, .979621778, .980375583, .98110492,  .98181044,  .982492786,
      .983152586,
      .983790458, .984407007, .985002827, .985578499, .986134593, .98667167,  .987190274, .987690941, .988174195,
      .988640548,
      .989090501, .989524544, .989943156, .990346805, .990735947, .99111103,  .991472488, .991820747, .992156222,
      .992479318,
      .992790429, .99308994,  .993378225, .99365565,  .99392257,  .994179333, .994426275, .994663724, .994892,
      .995111413,
      .995322265, .995524849, .995719451, .995906348, .996085809, .996258096, .996423462, .996582153, .996734409,
      .99688046,
      .997020533, .997154845, .997283607, .997407023, .997525293, .997638607, .997747152, .997851108, .997950649,
      .998045943,
      .998137154, .998224438, .998307948, .998387832, .998464231, .998537283, .998607121, .998673872, .998737661,
      .998798606,
      .998856823, .998912423, .998965513, .999016195, .99906457,  .999110733, .999154777, .99919679,  .999236858,
      .999275064,
      .999311486, .999346202, .999379283, .999410802, .999440826, .99946942,  .999496646, .999522566, .999547236,
      .999570712,
      .999593048, .999614295, .999634501, .999653714, .999671979, .99968934,  .999705837, .999721511, .9997364,
      .999750539,
      .999763966, .999776711, .999788809, .999800289, .999811181, .999821512, .999831311, .999840601, .999849409,
      .999857757,
      .999865667, .999873162, .999880261, .999886985, .999893351, .999899378, .999905082, .99991048,  .999915587,
      .999920418,
      .999924987, .999929307, .99993339,  .99993725,  .999940898, .999944344, .999947599, .999950673, .999953576,
      .999956316,
      .999958902, .999961343, .999963645, .999965817, .999967866, .999969797, .999971618, .999973334, .999974951,
      .999976474
      };

    if ( argument > 0 )
      {
      int temp = (int)( argument * 100 );
      if (Math::AlmostEquals( argument, temp ))
        {
        erfValue = .999976474;
        }
      else
        {
        double slope = ( y[temp + 1] - y[temp] ) / ( ( (float)temp + 1 ) / 100 - ( (float)temp / 100 ) );
        erfValue = slope * ( argument - ( (float)temp + 1 ) / 100 ) + y[temp + 1];
        }
      }
    else
      {
      int    temp = -(int)( argument * 100 );
      double slope = ( -y[temp + 1] + y[temp] ) / ( -( (float)temp + 1 ) / 100 + ( (float)temp / 100 ) );
      erfValue = ( slope * ( argument + ( (float)temp + 1 ) / 100 ) - y[temp + 1] );
      }
    }
  return erfValue;
}

CumulativeGaussianCostFunction::MeasureType
CumulativeGaussianCostFunction
::GetValue(const ParametersType & parameters) const
{
  for ( unsigned int i = 0; i < m_RangeDimension; i++ )
    {
    m_Measure[i] =  parameters.get(2)
                   + ( ( parameters.get(3)
                         - parameters.get(2) )
                       * ( EvaluateCumulativeGaussian( ( i
                                                         - parameters.get(0) )
                                                       / ( parameters.get(1) * std::sqrt(2.0) ) ) + 1 ) / 2 );
    }

  return m_Measure;
}

CumulativeGaussianCostFunction::MeasureType *
CumulativeGaussianCostFunction
::GetValuePointer(ParametersType & parameters)
{
  m_MeasurePointer->SetSize(m_RangeDimension);

  for ( unsigned int i = 0; i < m_RangeDimension; i++ )
    {
    m_MeasurePointer->put( i, parameters.get(2)
                           + ( ( parameters.get(3)
                                 - parameters.get(2) )
                               * ( EvaluateCumulativeGaussian( ( i
                                                                 - parameters.get(0) )
                                                               / ( parameters.get(1) * std::sqrt(2.0) ) ) + 1 ) / 2 ) );
    }

  return m_MeasurePointer;
}

unsigned int
CumulativeGaussianCostFunction
::GetNumberOfParameters() const
{
  // Return the number of parameters.
  return SpaceDimension;
}

unsigned int
CumulativeGaussianCostFunction
::GetNumberOfValues() const
{
  // Return the number of data samples.
  return m_RangeDimension;
}

void
CumulativeGaussianCostFunction
::Initialize(unsigned int rangeDimension)
{
  // Initialize the arrays.
  m_RangeDimension = rangeDimension;
  m_Measure.SetSize(m_RangeDimension);
}

void
CumulativeGaussianCostFunction
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Range Dimension = " << m_RangeDimension
     << std::endl;
}
} // end namespace itk
#endif
