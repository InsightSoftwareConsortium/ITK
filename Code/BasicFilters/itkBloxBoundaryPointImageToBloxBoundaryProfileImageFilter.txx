/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBloxBoundaryPointImageToBloxBoundaryProfileImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkBloxBoundaryPointImageToBloxBoundaryProfileImageFilter_txx
#define __itkBloxBoundaryPointImageToBloxBoundaryProfileImageFilter_txx

#include "itkFloodFilledSpatialFunctionConditionalIterator.h"
#include "itkBloxBoundaryPointImageToBloxBoundaryProfileImageFilter.h"
#include "itkImageRegionConstIterator.h"
#include "itkMultipleValuedCostFunction.h"
#include "itkLevenbergMarquardtOptimizer.h"

#include "iostream.h"
typedef vnl_matrix<double> MatrixType;
typedef vnl_vector<double> VectorType;

const double INV_SQRT_TWO_PI         = 0.398942280401; // 1/sqrt(2*pi)
const double SQUARE_ROOT_OF_TWO      = 1.41421356237;  // sqrt(2)

namespace itk
{

// Boundary profiles are formed by fitting the intensity profile
// to a cumulative Gaussian. BoundaryProfileCostFunction contains
// functions needed for the vnl_levenberg_marquardt optimizer class

class BoundaryProfileCostFunction : public MultipleValuedCostFunction
{
public:

  // Standard class typedefs
  typedef BoundaryProfileCostFunction  Self;
  typedef MultipleValuedCostFunction   Superclass;
  typedef SmartPointer<Self>           Pointer;
  typedef SmartPointer<const Self>     ConstPointer;
  itkNewMacro( Self );

  // Typedef for parameter type
  typedef Superclass::ParametersType ParametersType;

  // Typedef for derivative type
  typedef Superclass::DerivativeType DerivativeType;

  // Typedef for measure type

  typedef Superclass::MeasureType MeasureType;
           
  itkStaticConstMacro(SpaceDimension, unsigned int, 4); // number of parameters 
  
  BoundaryProfileCostFunction(){}

  ~BoundaryProfileCostFunction(){}

  void Initialize(unsigned int setRangeDimension)
  {
    m_RangeDimension  = setRangeDimension;
    m_Measure.resize(m_RangeDimension);
    m_Derivative.resize(SpaceDimension,m_RangeDimension);
    m_TheoreticalData.resize(m_RangeDimension);
  }

  // Error function look up table
  double erf(double argument) const
  {
    int temp = 0;
    double erfValue = 0;
    double slope = 0;

    double y[300] =
    {
    0,          .011283416, .022564575, .033841222, .045111106, .056371978, .067621594, .07885772,  .090078126, .101280594,
    .112462916, .123622896, .134758352, .145867115, .156947033, .167995971, .179011813, .189992461, .200935839, .211839892,
    .222702589, .233521923, .244295911, .255022599, .265700058, .276326389, .286899723, .297418219, .307880068, .318283496,
    .328626759, .33890815,  .349125995, .359278655, .369364529, .379382053, .3893297,   .399205983, .409009452, .418738698,
    .428392352, .43796909,  .447467618, .456886694, .466225115, .475481719, .484655389, .49374505,  .50274967,  .51166826,
    .520499876, .529243617, .537898627, .546464093, .554939245, .563323359, .571615763, .579815806, .5879229,   .595936496,
    .60385609,  .611681217, .61941146,  .627046441, .634585826, .642029324, .649376683, .656627696, .663782195, .670840052,
    .677801193, .684665264, .691432825, .698103704, .704677825, .71115543,  .717536534, .723821437, .730010238, .73610324,
    .74210079,  .748003138, .75381059,  .759523625, .76514256,  .770667933, .776100122, .781439725, .786687219, .791843127,
    .796908113, .801882743, .80676762,  .811563474, .816270948, .820890718, .825423575, .82987023,  .834231422, .838508001,
    .842700735, .846810448, .850837952, .854784156, .8586499,   .862436067, .866143531, .86977325,  .873326119, .876803068,
    .880205041, .88353297,  .886787854, .88997064,  .893082302, .896123821, .899096169, .90200037,  .904837402, .907608265,
    .91031396,  .912955492, .915533856, .918050082, .920505165, .922900112, .925235928, .927513617, .929734183, .931898615,
    .934007929, .936063109, .938065143, .940015016, .941913707, .943762189, .94556143,  .947312386, .949016025, .950673287,
    .952285112, .953852432, .955376173, .956857248, .958296565, .959695022, .961053506, .962372893, .963654059, .964897859,
    .966105142, .967276744, .968413493, .969516206, .970585687, .971622731, .97262812,  .973602626, .974547008, .975462012,
    .97634838,  .977206834, .978038086, .978842837, .979621778, .980375583, .98110492,  .98181044,  .982492786, .983152586,
    .983790458, .984407007, .985002827, .985578499, .986134593, .98667167,  .987190274, .987690941, .988174195, .988640548,
    .989090501, .989524544, .989943156, .990346805, .990735947, .99111103,  .991472488, .991820747, .992156222, .992479318,
    .992790429, .99308994,  .993378225, .99365565,  .99392257,  .994179333, .994426275, .994663724, .994892,    .995111413,
    .995322265, .995524849, .995719451, .995906348, .996085809, .996258096, .996423462, .996582153, .996734409, .99688046,
    .997020533, .997154845, .997283607, .997407023, .997525293, .997638607, .997747152, .997851108, .997950649, .998045943,
    .998137154, .998224438, .998307948, .998387832, .998464231, .998537283, .998607121, .998673872, .998737661, .998798606,
    .998856823, .998912423, .998965513, .999016195, .99906457,  .999110733, .999154777, .99919679,  .999236858, .999275064,
    .999311486, .999346202, .999379283, .999410802, .999440826, .99946942,  .999496646, .999522566, .999547236, .999570712,
    .999593048, .999614295, .999634501, .999653714, .999671979, .99968934,  .999705837, .999721511, .9997364,   .999750539,
    .999763966, .999776711, .999788809, .999800289, .999811181, .999821512, .999831311, .999840601, .999849409, .999857757,
    .999865667, .999873162, .999880261, .999886985, .999893351, .999899378, .999905082, .99991048,  .999915587, .999920418,
    .999924987, .999929307, .99993339,  .99993725,  .999940898, .999944344, .999947599, .999950673, .999953576, .999956316,
    .999958902, .999961343, .999963645, .999965817, .999967866, .999969797, .999971618, .999973334, .999974951, .999976474
    };


    if(argument < -3 || argument > 3)
      {
      if(argument > 0) erfValue = 1;
      else erfValue = -1;
      }

    // interpolation between table lookup entries
    else
      {
      if(argument > 0)
        {
        temp = (int)(argument * 100);
        if(argument == (int)temp) erfValue = .999976474;
        else
          {
          slope = (y[temp + 1] - y[temp])/(((float)temp + 1)/100 - ((float)temp/100));
          erfValue = slope * (argument - ((float)temp + 1)/100) + y[temp+1];
          }
        }
        else
          {
          temp = -(int)(argument * 100);
          slope = (-y[temp + 1] + y[temp])/(-((float)temp + 1)/100 + ((float)temp/100));
          erfValue = (slope * (argument + ((float)temp + 1)/100) - y[temp+1]);
          }
      }
    return erfValue;
  }

  MeasureType GetValue( const ParametersType & parameters ) const
  {
    double a = parameters[0];  // lower intensity
    double b = parameters[1];  // upper intensity
    double c = parameters[2];  // mean
    double d = parameters[3];  // standard deviation

    double arg = 0;
    for( unsigned int x = 0; x < m_RangeDimension; x++ )
      {
      const double xd = static_cast<double>( x );

      arg = ((xd-c)/(d*SQUARE_ROOT_OF_TWO));
      m_Measure[x]  = (a+.5*(b-a)*(1 + erf(arg)));
      m_Measure[x] -= m_TheoreticalData[x];
      }
    return m_Measure;
  }

  void GetDerivative( const ParametersType & parameters,
                            DerivativeType  & derivative ) const
  {
    // double a = parameters[0]; not used
    double b = parameters[1];
    double c = parameters[2];
    double d = parameters[3];

    double arg = 0;
    for( unsigned int x = 0; x<m_RangeDimension; x++ )
      {
      arg = ((x-c)/(d*SQUARE_ROOT_OF_TWO));

      m_Derivative[0][x] =  1;
      m_Derivative[1][x] =  (.5*(1 + erf(arg)));
      m_Derivative[2][x] =  -(INV_SQRT_TWO_PI*b*(1/d)*(exp(arg*arg)));
      m_Derivative[3][x] =  -(INV_SQRT_TWO_PI*b*(x - c)*(1/(d*d))*exp(arg*arg));
      }
    derivative = m_Derivative;
  }

  unsigned int GetNumberOfParameters(void) const
  {
    return SpaceDimension;
  }

  unsigned int GetNumberOfValues(void) const
  {
    return m_RangeDimension;
  }

  void SetTheoreticalData(double * setTheoreticalData)
  {
    for( unsigned int x = 0; x<m_RangeDimension; x++ )
      m_TheoreticalData[x] = setTheoreticalData[x];
  }

private:

  mutable MeasureType       m_Measure;
  mutable DerivativeType    m_Derivative;
          MeasureType       m_TheoreticalData;

  unsigned int m_RangeDimension;
};

////////////////////////////////////////////////////////////////////////////////
//============================================================================//
//============================================================================//
//=============itkBloxBoundaryPointImageToBloxProfileImageFilter==============//
//============================================================================//
//============================================================================//
////////////////////////////////////////////////////////////////////////////////

template< typename TSourceImage >
BloxBoundaryPointImageToBloxBoundaryProfileImageFilter< TSourceImage >
::BloxBoundaryPointImageToBloxBoundaryProfileImageFilter()
{
  // NOTE: Be sure to call Initialize function to set variables
  m_UniqueAxis = 0;
  m_SymmetricAxes = 0;
  m_NumberOfBins = 0;
  m_SplatMethod = 0;
  m_SpaceDimension = 0;
  m_NumBoundaryProfiles = 0;
  m_UseGradient = false;

  itkDebugMacro(<< "itkBloxBoundaryPointImageToBloxBoundaryProfileImageFilter::itkBloxBoundaryPointImageToBloxBoundaryProfileImageFilter() called");
}

template< typename TSourceImage >
BloxBoundaryPointImageToBloxBoundaryProfileImageFilter< TSourceImage >
::~BloxBoundaryPointImageToBloxBoundaryProfileImageFilter()
{
  delete [] m_Accumulator;
  delete [] m_Normalizer;
  delete [] m_NormalizedAccumulator;
  delete [] m_FinalParameters;
};

template< typename TSourceImage >
bool
BloxBoundaryPointImageToBloxBoundaryProfileImageFilter< TSourceImage >
::AddSplatToAccumulatorAndNormalizer(int binNumber, double weight, double sourcePixelValue)
{
  // Add results of splat to the accumulator and normalizer
  if(binNumber >= 0 && static_cast<unsigned int>(binNumber) < m_NumberOfBins)
    {
    m_Accumulator[binNumber] += weight * sourcePixelValue;
    m_Normalizer[binNumber]  += weight;
    return(1);
    }
  else
    return(0);
}

template< typename TSourceImage >
double
BloxBoundaryPointImageToBloxBoundaryProfileImageFilter< TSourceImage >
::FindAccumulatorMaximum()
{
  // Find the maximum value of the accumulator
  double maximum = m_NormalizedAccumulator[0];

  for(unsigned int i = 0; i < m_NumberOfBins; ++i)
    {
    double temp = m_NormalizedAccumulator[i];
    for(unsigned int j = 0; j < m_NumberOfBins; ++j)
      {
      if(temp >= maximum)
        maximum = temp;
      }
    }
    return maximum;
}

template< typename TSourceImage >
double
BloxBoundaryPointImageToBloxBoundaryProfileImageFilter< TSourceImage >
::FindAccumulatorMinimum()
{
  // Find the minimum value of the accumulator
  double minimum = m_NormalizedAccumulator[0];
  for(unsigned int  i = 0; i < m_NumberOfBins; ++i)
    {
    double temp = m_NormalizedAccumulator[i];
    for(unsigned int j = 0; j < m_NumberOfBins; ++j)
      {
      if(temp <= minimum)
        minimum = temp;
      }
    }
  return minimum;
}

template< typename TSourceImage >
int
BloxBoundaryPointImageToBloxBoundaryProfileImageFilter< TSourceImage >
::FitProfile()
{
  // The Levenberg-Marquardt Optimizer is used temporarily yielding less than
  // desirable results. Any optimizer can be used here. A fast
  // one specialized for fitting Gaussians will replace these methods
  // shortly.
  m_FinalParameters = new double[BoundaryProfileCostFunction::SpaceDimension];

  typedef LevenbergMarquardtOptimizer OptimizerType;

  typedef OptimizerType::InternalOptimizerType vnlOptimizerType;

  // Declaration of a itkOptimizer
  OptimizerType::Pointer  Optimizer = OptimizerType::New();

  // Declaration of the CostFunction adaptor
  BoundaryProfileCostFunction::Pointer costFunction = BoundaryProfileCostFunction::New();

  costFunction->Initialize(m_NumberOfBins);

  costFunction->SetTheoreticalData(m_NormalizedAccumulator);

  try
    {
    Optimizer->SetCostFunction( costFunction.GetPointer() );
    }
  catch( ExceptionObject & e )
    {
    std::cout << "Exception thrown ! " << std::endl;
    std::cout << "An error ocurred during Optimization" << std::endl;
    std::cout << e << std::endl;
    return EXIT_FAILURE;
    }

  const double F_Tolerance      = 1e-15;  // Function value tolerance
  const double G_Tolerance      = 1e-17;  // Gradient magnitude tolerance
  const double X_Tolerance      = 1e-16;  // Search space tolerance
  const double Epsilon_Function = 1e-10;  // Step
  const int    Max_Iterations   =   100;  // Maximum number of iterations

  Optimizer->SetUseCostFunctionGradient(m_UseGradient);
  vnlOptimizerType * vnlOptimizer = Optimizer->GetOptimizer();

  vnlOptimizer->set_f_tolerance( F_Tolerance );
  vnlOptimizer->set_g_tolerance( G_Tolerance );
  vnlOptimizer->set_x_tolerance( X_Tolerance );
  vnlOptimizer->set_epsilon_function( Epsilon_Function );
  vnlOptimizer->set_max_function_evals( Max_Iterations );

  // Initialize the optimizer
  typedef BoundaryProfileCostFunction::ParametersType ParametersType;
  ParametersType  initialValue(BoundaryProfileCostFunction::SpaceDimension);

  initialValue[0] = FindAccumulatorMinimum();
  initialValue[1] = FindAccumulatorMaximum() - FindAccumulatorMinimum();
  initialValue[2] = 5;
  initialValue[3] = 2;

  OptimizerType::ParametersType currentValue(BoundaryProfileCostFunction::SpaceDimension);

  currentValue = initialValue;

  Optimizer->SetInitialPosition( currentValue );

  try
    {
    Optimizer->StartOptimization();
    }
  catch( ExceptionObject & e )
    {
    std::cout << "Exception thrown ! " << std::endl;
    std::cout << "An error ocurred during Optimization" << std::endl;
    std::cout << "Location    = " << e.GetLocation()    << std::endl;
    std::cout << "Description = " << e.GetDescription() << std::endl;
    return EXIT_FAILURE;
    }

  OptimizerType::ParametersType finalPosition;
  finalPosition = Optimizer->GetCurrentPosition();

  m_FinalParameters[0] = finalPosition[0];
  m_FinalParameters[1] = finalPosition[1];
  m_FinalParameters[2] = finalPosition[2];
  m_FinalParameters[3] = finalPosition[3];

  return EXIT_SUCCESS;
}

template< typename TSourceImage >
void
BloxBoundaryPointImageToBloxBoundaryProfileImageFilter< TSourceImage >
::GenerateData()
{
  itkDebugMacro(<< "itkBloxBoundaryPointImageToBloxBoundaryProfileImageFilter::GenerateData() called");

  // Pointers to the source image, the boundary point image, and the output image
  // Get the input and output pointers
  BoundaryPointImagePointer bpPtr
    = dynamic_cast<BoundaryPointImageType*>(ProcessObject::GetInput(0));
  SourceImagePointer sourcePtr
    = dynamic_cast<SourceImageType*>(ProcessObject::GetInput(1));
  OutputImagePointer outputPtr = this->GetOutput(0);

  // Allocate the output
  outputPtr->SetBufferedRegion( outputPtr->GetRequestedRegion() );
  outputPtr->Allocate();

  // Create an iterator to walk the boundary point image
  typedef ImageRegionIterator<BoundaryPointImageType> BPIteratorType;

  BPIteratorType bpIt = BPIteratorType(bpPtr, bpPtr->GetRequestedRegion() );

  // Count number of iterated boundary points
  unsigned int bpCount = 0;

  // Iterate through the bp image (all pixels) and look for boundary profiles
  for ( bpIt.GoToBegin(); !bpIt.IsAtEnd(); ++bpIt)
    {
    // The iterator for accessing linked list info
    typename BloxBoundaryPointPixel<NDimensions>::iterator bpiterator;

    // Walk through all of the elements at the pixel
    for (bpiterator = bpIt.Value().begin(); bpiterator != bpIt.Value().end(); ++bpiterator)
      {

      // Find boundary profiles at this index of the iterator

      // When constructing boundary profiles at a boundary point, we want to sample
      // the voxels within an ellipsoidal region

      //---------Create and initialize a sampling spatial function-----------

      // Symmetric Ellipsoid spatial function typedef
      typedef SymmetricEllipsoidInteriorExteriorSpatialFunction<NDimensions> FunctionType;

      // Point position typedef
      typedef typename FunctionType::InputType SymEllipsoidFunctionVectorType;

      // Create a symmetric ellipsoid spatial function for the source image
      typename FunctionType::Pointer spatialFunc = FunctionType::New();

      // Set the origin of the spatial function to the current boundary point location
      PositionType spatialFunctionOrigin = (*bpiterator)->GetPhysicalPosition();
      spatialFunc->SetCenter(spatialFunctionOrigin);

      // Convert the origin position to a vector
      VectorType spatialFunctionOriginVector;
      spatialFunctionOriginVector.Set_vnl_vector( spatialFunctionOrigin.Get_vnl_vector() );

      // Set the orientation of the ellipsoid to the current boundary point gradient
      Vector<double, NDimensions> orientation;

      CovariantVector<double, NDimensions> gradientNormalized;
      double gradientNorm = (*bpiterator)->GetGradient().GetNorm();

      gradientNormalized = (*bpiterator)->GetGradient()/gradientNorm;

      VectorType orientationVNL;
      for(unsigned int i = 0; i < NDimensions; i++)
        {
        orientation[i] = gradientNormalized[i];
        orientationVNL[i] = gradientNormalized[i];
        }

      // Set the properties of the spatial function
      spatialFunc->SetOrientation(orientation, m_UniqueAxis, m_SymmetricAxes);

      // Create a seed position for the spatial function iterator we'll use shortly
      typename TSourceImage::IndexType seedIndex;

      typedef typename TSourceImage::IndexValueType IndexValueType;

      for(unsigned int i=0; i< NDimensions; i++)
        seedIndex[i] = static_cast<IndexValueType>( spatialFunctionOrigin[i] );

      // Create and initialize a spatial function iterator
      typedef FloodFilledSpatialFunctionConditionalIterator<TSourceImage, FunctionType> IteratorType;
      IteratorType sfi = IteratorType(sourcePtr, spatialFunc, seedIndex);

      // The index of the pixel
      VectorType indexPosition;

      // Reset
      for(unsigned int i = 0; i < m_NumberOfBins; ++i)
        {
        m_Accumulator[i] = 0;
        m_Normalizer[i] = 0;
        m_NormalizedAccumulator[i] = 0;
        }

      // Walk the spatial function
      for( ; !( sfi.IsAtEnd() ); ++sfi)
        {

        VectorType deltaPoint;
        for(unsigned int i = 0; i < NDimensions; i++)
          {
          indexPosition[i] = sfi.GetIndex()[i];
          // Calculate difference in spatial function index and origin
          deltaPoint[i] = indexPosition[i] - spatialFunctionOriginVector[i];
          }
        
        // Project boundary point onto major axis of ellipsoid
        double projOntoMajorAxis = inner_product<double>(deltaPoint.Get_vnl_vector(), orientationVNL.Get_vnl_vector());

        // Length of profile is the length of the ellipsoid's major axis
        double profileLength = m_UniqueAxis;

        // Distance along major axis of ellipsoid from edge of ellipsoid
        double distanceAlongMajorAxisFromEdge = projOntoMajorAxis + profileLength/2;

        // Find bin number to put weighted pixel value into
        double vectorRatio = distanceAlongMajorAxisFromEdge/profileLength;

        int binNumber = (int) (vectorRatio * m_NumberOfBins);
        double binJitter = (vectorRatio * m_NumberOfBins) - binNumber;

        typename TSourceImage::PixelType sourcePixelValue;

        // Get the value of the pixel
        sourcePixelValue = sourcePtr->GetPixel(sfi.GetIndex());

        // Gaussian Splat - Project Gaussian weighted pixel intensities along major axis of ellipsoid (sampling region)
        if(m_SplatMethod == 0)
          {
          double a = 2;
          double b = .6; // for weight .5

          this->AddSplatToAccumulatorAndNormalizer(binNumber-1, double(a*exp(-.5*(pow((binJitter+1)/b, 2)))),
                         sourcePixelValue);
          this->AddSplatToAccumulatorAndNormalizer(binNumber,   double(a*exp(-.5*(pow((binJitter  )/b, 2)))),
                         sourcePixelValue);
          this->AddSplatToAccumulatorAndNormalizer(binNumber+1, double(a*exp(-.5*(pow((binJitter-1)/b, 2)))),
                         sourcePixelValue);
          this->AddSplatToAccumulatorAndNormalizer(binNumber+2, double(a*exp(-.5*(pow((binJitter-2)/b, 2)))),
                         sourcePixelValue);
          }

        // Triangle splat - Project Triangular weighted pixel intensities along major axis of ellipsoid (sampling region)
        else if(m_SplatMethod == 1)
          {
          this->AddSplatToAccumulatorAndNormalizer(binNumber-1, 1-binJitter, sourcePixelValue);
          this->AddSplatToAccumulatorAndNormalizer(binNumber,   2-binJitter, sourcePixelValue);
          this->AddSplatToAccumulatorAndNormalizer(binNumber+1, 1+binJitter, sourcePixelValue);
          this->AddSplatToAccumulatorAndNormalizer(binNumber+2,   binJitter, sourcePixelValue);
          }
        else
          itkDebugMacro(<< "BloxBoundaryProfileImage::FindBoundaryProfilesAtBoundaryPoint - Inappropriate splat method");
        }

        // Normalize the splat accumulator with the normalizer
        this->NormalizeSplatAccumulator();

        // Fit the intensity profile to a Cumulative Gaussian
        this->FitProfile();

        // Create a new boundary profile if within constraints of imaging modality
        if(m_FinalParameters[0] >= 0 && m_FinalParameters[0] <= 255 &&
           m_FinalParameters[1] >= 0 && m_FinalParameters[1] <= 255 &&
           m_FinalParameters[2] >= 0 && m_FinalParameters[2] <= m_UniqueAxis &&
           m_FinalParameters[3] >= 0 && m_FinalParameters[3] <= m_UniqueAxis)
          {
          BloxBoundaryProfileItem<NDimensions>* boundaryProfile = new BloxBoundaryProfileItem<NDimensions>;

          // Set boundary profile parameters
          boundaryProfile->SetProfileLength(static_cast<unsigned int>(m_UniqueAxis));
          boundaryProfile->SetLowerIntensity(m_FinalParameters[0]);
          boundaryProfile->SetUpperIntensity(m_FinalParameters[1]);
          boundaryProfile->SetMean(m_FinalParameters[2]);
          boundaryProfile->SetStandardDeviation(m_FinalParameters[3]);
          boundaryProfile->SetMeanNormalized();
          boundaryProfile->SetStandardDeviationNormalized();
          boundaryProfile->SetOptimalBoundaryLocation(spatialFunctionOriginVector.Get_vnl_vector(), orientationVNL.Get_vnl_vector());

          PositionType optimalBoundaryLocation;
          for(unsigned int i = 0; i < NDimensions; i++)
            optimalBoundaryLocation[i] = boundaryProfile->GetOptimalBoundaryLocation()[i];

          // Figure out the data space coordinates of the optimal boundary location
          IndexType boundaryProfilePosition;

          // Transform optimal boundary location to an index
          outputPtr->TransformPhysicalPointToIndex(optimalBoundaryLocation, boundaryProfilePosition);

          // Store the new boundary profile in the correct spot in output image
          outputPtr->GetPixel(boundaryProfilePosition).push_back(boundaryProfile);

          m_NumBoundaryProfiles++;
          }
        bpCount++;
      }
    }

  std::cout << "# of boundary points = " << bpCount << std::endl
    << "# of boundary profiles = " << m_NumBoundaryProfiles << std::endl;

  itkDebugMacro(<< "Finished constructing for boundary profiles\n"
                << "I made " << m_NumBoundaryProfiles << " boundary profiles\n");
}

template< typename TSourceImage >
void
BloxBoundaryPointImageToBloxBoundaryProfileImageFilter< TSourceImage >
::Initialize(double setUniqueAxis, double setSymmetricAxes, unsigned int numberOfBins,
                unsigned int splatMethod, unsigned int spaceDimension)
{
  m_NumBoundaryProfiles = 0;
  m_UniqueAxis = setUniqueAxis;
  m_SymmetricAxes = setSymmetricAxes;
  m_NumberOfBins = numberOfBins;
  m_SplatMethod = splatMethod;
  m_SpaceDimension = spaceDimension;
  m_Accumulator = new double[m_NumberOfBins];
  m_Normalizer = new double[m_NumberOfBins];
  m_NormalizedAccumulator = new double[m_NumberOfBins];
  m_FinalParameters = new double[m_SpaceDimension];

}

template< typename TSourceImage >
void
BloxBoundaryPointImageToBloxBoundaryProfileImageFilter< TSourceImage >
::NormalizeSplatAccumulator()
{  
  for(unsigned int i = 0; i < m_NumberOfBins; ++i)
    {      
    if(m_Normalizer[i] == 0)
      m_NormalizedAccumulator[i] = 0;
    else
      m_NormalizedAccumulator[i] = m_Accumulator[i] / m_Normalizer[i];
    }
}

template< typename TSourceImage >
void
BloxBoundaryPointImageToBloxBoundaryProfileImageFilter< TSourceImage >
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
}

template< typename TSourceImage >
void
BloxBoundaryPointImageToBloxBoundaryProfileImageFilter< TSourceImage >
::SetInput1(const SourceImageType * image1 )
{
  // Process object is not const-correct so the const casting is required.
  SetNthInput(1,  const_cast<SourceImageType *>( image1 ) );
}

template< typename TSourceImage >
void
BloxBoundaryPointImageToBloxBoundaryProfileImageFilter< TSourceImage >
::SetInput2(const BoundaryPointImageType * image2 )
{
  // Process object is not const-correct so the const casting is required.
  SetNthInput(0, const_cast<BoundaryPointImageType *>( image2 ) );
}

template< typename TSourceImage >
void
BloxBoundaryPointImageToBloxBoundaryProfileImageFilter< TSourceImage >
::SetUseOptimizerGradient(bool useGradient)
{
  m_UseGradient = useGradient;
}

} // end namespace

#endif
