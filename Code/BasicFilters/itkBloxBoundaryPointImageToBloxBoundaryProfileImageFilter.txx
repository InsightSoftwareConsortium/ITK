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
#include "itkCumulativeGaussianOptimizer.h"
#include "itkCumulativeGaussianCostFunction.h"

typedef vnl_matrix<double> MatrixType;
typedef vnl_vector<double> VectorType;

const double INV_SQRT_TWO_PI         = 0.398942280401; // 1/sqrt(2*pi)
const double SQUARE_ROOT_OF_TWO      = 1.41421356237;  // sqrt(2)

namespace itk
{

// A boundary profile is formed by fitting the generated 
// intensity profile across a boundary to a cumulative Gaussian.

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
  m_Accumulator = 0;
  m_Normalizer = 0;
  m_NormalizedAccumulator = 0;
  m_FinalParameters = 0;

  itkDebugMacro(<< "itkBloxBoundaryPointImageToBloxBoundaryProfileImageFilter::itkBloxBoundaryPointImageToBloxBoundaryProfileImageFilter() called");
}

template< typename TSourceImage >
BloxBoundaryPointImageToBloxBoundaryProfileImageFilter< TSourceImage >
::~BloxBoundaryPointImageToBloxBoundaryProfileImageFilter()
{
  if (m_Accumulator)
    {
    delete [] m_Accumulator;
    }
  if (m_Normalizer)
    {
    delete [] m_Normalizer;
    }
  if (m_NormalizedAccumulator)
    {
    delete [] m_NormalizedAccumulator;
    }
  if (m_FinalParameters)
    {
    delete [] m_FinalParameters;
    }
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
  double mean                = m_NumberOfBins/2;
  double standardDeviation   = 2;
  double lowerAsymptote      = FindAccumulatorMinimum();
  double upperAsymptote      = FindAccumulatorMaximum() - FindAccumulatorMinimum();
  double differenceTolerance = 1e-20;

  // Typedef and initialization for the Cumulative Gaussian Optimizer.
  typedef itk::CumulativeGaussianOptimizer CumulativeGaussianOptimizerType;
  CumulativeGaussianOptimizerType::Pointer optimizer = CumulativeGaussianOptimizerType::New();

  // Typedef and initialization for the Cumulative Gaussian Cost Function.
  typedef itk::CumulativeGaussianCostFunction CostFunctionType;
  CostFunctionType::Pointer costFunction = CostFunctionType::New();

  // Declare and initialize the data array.
  CostFunctionType::MeasureType * cumGaussianArray = new CostFunctionType::MeasureType();
  cumGaussianArray->resize(m_NumberOfBins);

  // Set the parameters.
  CostFunctionType::ParametersType parameters;
  parameters.resize(4);
  parameters[0] = lowerAsymptote;
  parameters[1] = upperAsymptote;
  parameters[2] = mean;
  parameters[3] = standardDeviation;

  // Set the range of data sampled from a Cumulative Gaussian.
  costFunction->Initialize(m_NumberOfBins);

  // Set the data array.
  CostFunctionType::MeasureType * normalizedAccumulator = new CostFunctionType::MeasureType();
  normalizedAccumulator->resize(m_NumberOfBins);

  for(int i = 0; i < m_NumberOfBins; i++)
    normalizedAccumulator->put(i, m_NormalizedAccumulator[i]);
  
  costFunction->SetOriginalDataArray(normalizedAccumulator);

  // Set the cost function.
  optimizer->SetCostFunction(costFunction);

  // Set the tolerance for the Gaussian iteration error.
  optimizer->SetDifferenceTolerance(differenceTolerance);

  // Print results after each iteration.
  optimizer->SetVerbose(0);
 
  // Start optimization.
  optimizer->StartOptimization(normalizedAccumulator);

  if(optimizer->GetFitError() < 1e-3)
  {
    m_FinalParameters[0] = optimizer->GetLowerAsymptote();
    m_FinalParameters[1] = optimizer->GetUpperAsymptote();
    m_FinalParameters[2] = optimizer->GetComputedMean();
    m_FinalParameters[3] = optimizer->GetComputedStandardDeviation();

    // Print out the resulting paramters.
//    std::cerr  << "Test Failed with a Fit Error of " << optimizer->GetFitError() << std::endl;
//    std::cerr << "Fitted mean = " << m_FinalParameters[2] << std::endl;
//    std::cerr << "Fitted standard deviation = " << m_FinalParameters[3] << std::endl;
//    std::cerr << "Fitted upper asymptote = " << m_FinalParameters[1] << std::endl;
//    std::cerr << "Fitted lower asymptote = " << m_FinalParameters[0] << std::endl;

    return EXIT_SUCCESS;
  }
  else
    return EXIT_FAILURE;
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

////////////////////////////////////////////////
//////////OPTIMIZER INITIALIZATION//////////////
////////////////////////////////////////////////


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

        // Fit the intensity profile to a Cumulative Gaussian with
        // Levenberg-Marquardt Optimizer. Source of memory leaks?!?!?
        if(this->FitProfile() == EXIT_SUCCESS)
          {

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
            boundaryProfile->SetBoundaryPoint( (*bpiterator) );
            boundaryProfile->SetGradient(&(*bpiterator)->GetGradient());

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

} // end namespace

#endif
