/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkMRIBiasFieldCorrectionFilter.txx
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) Insight Software Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even 
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMRIBiasFieldCorrectionFilter_txx
#define __itkMRIBiasFieldCorrectionFilter_txx

#include "itkMRIBiasFieldCorrectionFilter.h"

namespace itk
{

// =========== MRIBiasEnergyFunction members =================

template<class TImage, class TImageMask, class TBiasField>
MRIBiasEnergyFunction<TImage, TImageMask, TBiasField>
::MRIBiasEnergyFunction()
{
  m_BiasField = 0;
  for (int i = 0; i < SpaceDimension; i++) 
    {
    m_SamplingFactor[i] = 1;
    }
  m_Mask = NULL;
  m_Image = NULL;
}

template<class TImage, class TImageMask, class TBiasField>
void 
MRIBiasEnergyFunction<TImage, TImageMask, TBiasField>
::InitializeDistributions( Array<double> classMeans, 
                           Array<double> classSigmas  )
{
  m_InternalEnergyFunction = 
    new InternalEnergyFunction(classMeans, classSigmas) ;
}

template<class TImage, class TImageMask, class TBiasField>
MRIBiasEnergyFunction<TImage, TImageMask, TBiasField>
::~MRIBiasEnergyFunction()
{
  delete m_InternalEnergyFunction ;
  m_InternalEnergyFunction = 0 ;
}


template<class TImage, class TImageMask, class TBiasField>
unsigned int
MRIBiasEnergyFunction<TImage, TImageMask, TBiasField>
::GetNumberOfParameters(void) const
{
  if( m_BiasField == 0 ) 
    {
    return 0;
    }
  return m_BiasField->GetNumberOfCoefficients();
}




template<class TImage, class TImageMask, class TBiasField>
typename MRIBiasEnergyFunction<TImage, TImageMask, TBiasField>::MeasureType
MRIBiasEnergyFunction<TImage, TImageMask, TBiasField>
::GetValue( const ParametersType & parameters ) const
{
  if ( m_Image.IsNull() )
    {
    itkExceptionMacro(<<"Image is null");
    }

  if( !m_InternalEnergyFunction  )
    {
    itkExceptionMacro(<<"EnergyFunction is null");
    }

  if( m_BiasField == 0 )
    {
    itkExceptionMacro(<<"BiasField is null");
    }

  MeasureType  total = 0.0;

  m_BiasField->SetCoefficients(parameters) ;
  
  // mask, bias and orginal image have the same dimensions and corresponding indexes

  if ( m_SamplingFactor[0] == 1 && m_SamplingFactor[1] == 1 && m_SamplingFactor[2] == 1) 
    {
      typename itk::ImageRegionConstIterator<ImageType> iIter(m_Image, m_Region) ;
      typename TBiasField::SimpleForwardIterator bIter(m_BiasField) ;
      
      bIter.Begin() ;
      iIter.GoToBegin() ;
      total = 0.0;

      // fastest for full sampling
      if (!m_Mask)
   {
     while (!bIter.IsAtEnd())
       {
         double diff = iIter.Get() - bIter.Get() ;
         total = total + (*m_InternalEnergyFunction)(diff);
         ++bIter ;
         ++iIter ;
       }
   } 
      else 
   {
     typename itk::ImageRegionConstIterator<MaskType> mIter(m_Mask, m_Region) ;
     mIter.GoToBegin();
     while (!bIter.IsAtEnd())
       {
         if (mIter.Get() > 0.0) 
      { 
        double diff = iIter.Get() - bIter.Get() ;
        total = total + (*m_InternalEnergyFunction)(diff);
      }
         ++bIter ;
         ++iIter ;
         ++mIter;
       }
   }
    }
  else 
    {
      typename ImageType::IndexType origIndex = m_Region.GetIndex(); 
      typename ImageType::IndexType curIndex; 
      typename TBiasField::IndexType indexBias(SpaceDimension); 
      typename ImageType::SizeType size = m_Region.GetSize();
      // use indexing for incomplete sampling

      if (!m_Mask)
   {
     indexBias[2] = 0;
     for (curIndex[2] = origIndex[2]; curIndex[2] < size[2]; curIndex[2] = curIndex[2] + m_SamplingFactor[2] ) 
       {
         indexBias[1] = 0;
         for (curIndex[1] = origIndex[1]; curIndex[1] < size[1]; curIndex[1] = curIndex[1] + m_SamplingFactor[1]) 
      {
        indexBias[0] = 0;
        for (curIndex[0] = origIndex[0]; curIndex[0] < size[0]; curIndex[0] = curIndex[0] + m_SamplingFactor[0]) 
          {
            const double biasVal = m_BiasField->Evaluate(indexBias);
            const double imageVal = m_Image->GetPixel(curIndex);
            total += (*m_InternalEnergyFunction)(imageVal - biasVal);
            indexBias[0] = indexBias[0] + m_SamplingFactor[0];
          }
        indexBias[1] = indexBias[1] + m_SamplingFactor[1];
      }
         indexBias[2] = indexBias[2] + m_SamplingFactor[2];
       }
   }
      else
   {
     indexBias[2] = 0;
     for (curIndex[2] = origIndex[2]; curIndex[2] < size[2]; curIndex[2] = curIndex[2] + m_SamplingFactor[2] ) 
       {
         indexBias[1] = 0;
         for (curIndex[1] = origIndex[1]; curIndex[1] < size[1]; curIndex[1] = curIndex[1] + m_SamplingFactor[1]) 
      {
        indexBias[0] = 0;
        for (curIndex[0] = origIndex[0]; curIndex[0] < size[0]; curIndex[0] = curIndex[0] + m_SamplingFactor[0]) 
          {
            if (m_Mask->GetPixel(curIndex) > 0.0) 
         {
           const double biasVal = m_BiasField->Evaluate(indexBias);
           const double imageVal = m_Image->GetPixel(curIndex);
           total += (*m_InternalEnergyFunction)(imageVal - biasVal);
         }
            indexBias[0] = indexBias[0] + m_SamplingFactor[0];
          }
        indexBias[1] = indexBias[1] + m_SamplingFactor[1];
      }
         indexBias[2] = indexBias[2] + m_SamplingFactor[2];
       }
   }
    }
  
  //std::cout << "Value  " << total << std::endl;
  
  return total ;
}


// =========== MRIBiasFieldCorrectionFilter members ==================

template<class TInputImage, class TOutputImage, class TMaskImage>
MRIBiasFieldCorrectionFilter<TInputImage, TOutputImage, TMaskImage>
::MRIBiasFieldCorrectionFilter()
{
  m_InputMask = 0 ;
  m_OutputMask = 0 ;

  m_BiasMultiplicative = true ;
  m_BiasFieldDegree = 3 ;
  m_OptimizerInitialRadius = 0.1 ;
  m_VolumeCorrectionMaximumIteration = 1000 ;
  m_InterSliceCorrectionMaximumIteration = 100 ;
  m_OptimizerGrowthFactor = 1.05 ;
  m_OptimizerShrinkFactor = 0.76;
    
  m_EnergyFunction = 0 ;
  m_NormalVariateGenerator = NormalVariateGeneratorType::New() ;
  //m_NormalVariateGenerator->Initialize(3024) ;
  m_NormalVariateGenerator->Initialize(time(NULL)) ;

  if (ImageDimension == 3)
    {
    m_UsingInterSliceIntensityCorrection = true ;
    m_SlicingDirection = 2 ;
    }
  else
    {
    m_UsingInterSliceIntensityCorrection = false ;
    }
    
  m_UsingSlabIdentification = false ;
  m_SlabBackgroundMinimumThreshold = NumericTraits< InputImagePixelType >::min() ;
  m_SlabNumberOfSamples = 10 ;
  m_SlabTolerance = 0.0 ;
  m_UsingBiasFieldCorrection = true ;
  m_GeneratingOutput = true ;

  m_InternalInput = InternalImageType::New() ;

  m_NumberOfLevels = 0;
  this->SetNumberOfLevels( 2 );
}


template <class TInputImage, class TOutputImage, class TMaskImage>
void
MRIBiasFieldCorrectionFilter<TInputImage, TOutputImage,  TMaskImage>
::SetNumberOfLevels(
  unsigned int num )
{
  if( m_NumberOfLevels == num )
    { 
    return; 
    }
  
  this->Modified();

  // clamp value to be at least one
  m_NumberOfLevels = num;
  if( m_NumberOfLevels < 1 ) m_NumberOfLevels = 1;

  // resize the schedules
  ScheduleType temp( m_NumberOfLevels, ImageDimension );
  temp.Fill( 0 );
  m_Schedule = temp;

  // determine initial shrink factor
  unsigned int startfactor = 1;
  startfactor = startfactor << ( m_NumberOfLevels - 1 );

  // set the starting shrink factors
  this->SetStartingShrinkFactors( startfactor );
}


/*
 * Set the starting shrink factors
 */
template <class TInputImage, class TOutputImage, class TMaskImage>
void
MRIBiasFieldCorrectionFilter<TInputImage, TOutputImage,  TMaskImage>
::SetStartingShrinkFactors(
  unsigned int factor )
{

  unsigned int array[ImageDimension];
  for( unsigned int dim = 0; dim < ImageDimension; ++dim )
    {
    array[dim] = factor;
    }

  this->SetStartingShrinkFactors( array );

}


/*
 * Set the starting shrink factors
 */
template <class TInputImage, class TOutputImage, class TMaskImage>
void
MRIBiasFieldCorrectionFilter<TInputImage, TOutputImage,  TMaskImage>
::SetStartingShrinkFactors(
  unsigned int * factors )
{

  for( unsigned int dim = 0; dim < ImageDimension; ++dim )
    {
    m_Schedule[0][dim] = factors[dim];
    if( m_Schedule[0][dim] == 0 ) 
      {
      m_Schedule[0][dim] = 1;
      }
    }

  for( unsigned int level = 1; level < m_NumberOfLevels; ++level )
    {
    for( unsigned int dim = 0; dim < ImageDimension; ++dim )
      {
      m_Schedule[level][dim] = m_Schedule[level-1][dim] / 2;
      if( m_Schedule[level][dim] == 0 ) 
        {
        m_Schedule[level][dim] = 1;
        }
      }
    }

  this->Modified();

}


/*
 * Get the starting shrink factors
 */
template <class TInputImage, class TOutputImage, class TMaskImage>
const unsigned int *
MRIBiasFieldCorrectionFilter<TInputImage, TOutputImage,  TMaskImage>
::GetStartingShrinkFactors() const
{
  return ( m_Schedule.data_block() );
}

/*
 * Set the multi-resolution schedule
 */
template <class TInputImage, class TOutputImage, class TMaskImage>
void
MRIBiasFieldCorrectionFilter<TInputImage, TOutputImage,  TMaskImage>
::SetSchedule(
  const ScheduleType& schedule )
{

  if( schedule.rows() != m_NumberOfLevels ||
      schedule.columns() != ImageDimension )
    {
    itkDebugMacro(<< "Schedule has wrong dimensions" );
    return;
    }

  if( schedule == m_Schedule )
    {
    return;
    }

  this->Modified();
  unsigned int level, dim;
  for( level = 0; level < m_NumberOfLevels; level++ )
    {
    for( dim = 0; dim < ImageDimension; dim++ )
      {

      m_Schedule[level][dim] = schedule[level][dim];
 
      // set schedule to max( 1, min(schedule[level],
      //  schedule[level-1] ); 
      if( level > 0 )
        {
        m_Schedule[level][dim] = vnl_math_min(
          m_Schedule[level][dim], m_Schedule[level-1][dim] );
        }
   
      if( m_Schedule[level][dim] < 1 )
        {
        m_Schedule[level][dim] = 1;
        }

      }
    }
}


/*
 * Is the schedule downward divisible ?
 */
template <class TInputImage, class TOutputImage, class TMaskImage>
bool
MRIBiasFieldCorrectionFilter<TInputImage, TOutputImage,  TMaskImage>
::IsScheduleDownwardDivisible( const ScheduleType& schedule )
{

  unsigned int ilevel, idim;
  for( ilevel = 0; ilevel < schedule.rows() - 1; ilevel++ )
    {
    for( idim = 0; idim < schedule.columns(); idim++ )
      {
      if( schedule[ilevel][idim] == 0 )
        {
        return false;
        }
      if( ( schedule[ilevel][idim] % schedule[ilevel+1][idim] ) > 0 )
        { 
        return false; 
        } 
      }
    }

  return true;
}


template<class TInputImage, class TOutputImage, class TMaskImage>
MRIBiasFieldCorrectionFilter<TInputImage, TOutputImage, TMaskImage>
::~MRIBiasFieldCorrectionFilter()
{
}
  
template<class TInputImage, class TOutputImage, class TMaskImage>
void
MRIBiasFieldCorrectionFilter<TInputImage, TOutputImage, TMaskImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  
  os << indent << "Use interslice intensity correction: " ;
  if ( m_UsingInterSliceIntensityCorrection )
    {
    os << "true" << std::endl ;
    }
  else
    {
    os << "false" << std::endl ;
    }

  os << indent << "Use slab identification: " ;
  if ( m_UsingSlabIdentification )
    {
    os << "true" << std::endl ;
    }
  else
    {
    os << "false" << std::endl ;
    }
    
  os << indent << "Use biasfield correction: " ;
  if ( m_UsingBiasFieldCorrection )
    {
    os << "true" << std::endl ;
    }
  else
    {
    os << "false" << std::endl ;
    }

  os << indent << "Generate output image: " ;
  if ( m_GeneratingOutput )
    {
    os << "true" << std::endl ;
    }
  else
    {
    os << "false" << std::endl ;
    }

  os << indent << "Is bias field multiplicative: " ;
  if ( m_BiasMultiplicative )
    {
    os << "true" << std::endl ;
    }
  else
    {
    os << "false" << std::endl ;
    }

  os << indent << "Biasfield degree = " << m_BiasFieldDegree << std::endl ;
  os << indent << "Optimizer intial radius: " << m_OptimizerInitialRadius 
     << std::endl ;
  os << indent << "Optimizer growth factor: " << m_OptimizerGrowthFactor 
     << std::endl ;
  os << indent << "Optimizer shrink factor: " << m_OptimizerShrinkFactor 
     << std::endl ;
  os << indent << "Volume optimizer max iteration: " 
     << m_VolumeCorrectionMaximumIteration << std::endl ;
  os << indent << "Interslice correction optimizer max iteration: " 
     << m_InterSliceCorrectionMaximumIteration << std::endl ;
  os << indent << "Slicing direction: " << m_SlicingDirection << std::endl ;
    
  os << indent << "InputMask: " ;
  if ( m_InputMask.IsNotNull() )
    {
    os << m_InputMask << std::endl ;
    }
  else
    {
    os << "not set." << std::endl ;
    }

  os << indent << "OutputMask: " ;
  if ( m_OutputMask.IsNotNull() )
    {
    os << m_OutputMask << std::endl ;
    }
  else
    {
    os << "not set." << std::endl ;
    }

  os << indent << "Energy function: " << m_EnergyFunction << std::endl ;
  os << indent << "Normal random variate generator: " 
     << m_NormalVariateGenerator << std::endl ;
  os << indent << "Multires: No. levels: " << m_NumberOfLevels << std::endl;
  os << indent << "Multires: Schedule: " << std::endl;
  os << m_Schedule << std::endl;
}

template<class TInputImage, class TOutputImage, class TMaskImage>
void
MRIBiasFieldCorrectionFilter<TInputImage, TOutputImage, TMaskImage>
::SetInputMask(ImageMaskType* inputMask)
{
  if ( m_InputMask != inputMask )
    {
    if (this->CheckMaskImage(inputMask))
      {
      m_InputMask = inputMask ;
      this->Modified() ;
      }
    else
      {
      itkExceptionMacro("The size of the provided mask image differ from the input image") ;
      }
    }
}

template<class TInputImage, class TOutputImage, class TMaskImage>
void 
MRIBiasFieldCorrectionFilter<TInputImage, TOutputImage, TMaskImage>
::SetOutputMask(ImageMaskType* outputMask)
{
  if ( m_OutputMask != outputMask )
    {
    if (this->CheckMaskImage(outputMask))
      {
      m_OutputMask = outputMask ;
      this->Modified() ;
      }
    else
      {
      itkExceptionMacro("The size of the provided mask image differs from the input image") ;
      }
    }
}



template<class TInputImage, class TOutputImage, class TMaskImage>
void 
MRIBiasFieldCorrectionFilter<TInputImage, TOutputImage, TMaskImage>
::Initialize() throw (ExceptionObject)
{
  m_InternalInput->SetRegions(this->GetInput()->GetLargestPossibleRegion()) ;
  m_InternalInput->Allocate() ;

  // copy the input image to the internal image and cast the type to
  // float (InternalImageType::PixelType)
  CopyAndConvertImage
    (this->GetInput(), m_InternalInput.GetPointer(), 
     this->GetInput()->GetLargestPossibleRegion()) ;

  // if the bias is multiplicative, we will use logarithm
  // the following section applies logarithm to key parameters and 
  // image data
  if (this->IsBiasFieldMultiplicative())
    { 
    const unsigned int size = m_TissueClassMeans.Size();
    for( unsigned int i = 0 ; i < size; i++ ) 
      {
      m_TissueClassSigmas[i] = log(1.0 + m_TissueClassSigmas[i] / 
                                   (m_TissueClassMeans[i] + 1.0)) ;
      m_TissueClassMeans[i] = log(m_TissueClassMeans[i] + 1.0) ;
      }

        
    m_OptimizerInitialRadius = log(1.0 + m_OptimizerInitialRadius) ;
        
    this->Log1PImage(m_InternalInput.GetPointer(), m_InternalInput.GetPointer()) ;
    }



  // initialize the energy function
  if (m_TissueClassMeans.Size() < 1)
    { 
    itkExceptionMacro(<<"Tissue Class Means is empty");
    }

  if (!m_EnergyFunction)
    {
    m_EnergyFunction = EnergyFunctionType::New();
    m_EnergyFunction->InitializeDistributions(m_TissueClassMeans, 
                                              m_TissueClassSigmas) ;
    }


  m_EnergyFunction->SetImage(m_InternalInput.GetPointer()) ;

  if ( m_InputMask.IsNotNull() )
    {
    m_EnergyFunction->SetMask(m_InputMask.GetPointer()) ;
    }
    
}


template<class TInputImage, class TOutputImage, class TMaskImage>
typename MRIBiasFieldCorrectionFilter<TInputImage, TOutputImage, TMaskImage>::BiasFieldType
MRIBiasFieldCorrectionFilter<TInputImage, TOutputImage, TMaskImage>
::EstimateBiasField(InputImageRegionType region, 
                    unsigned int degree,
                    int maximumIteration)
{
  itkDebugMacro(<< "Estimating bias field ");
  
  bool cleanCoeffs = false;
  BiasFieldType::DomainSizeType biasSize ;
  this->GetBiasFieldSize(region, biasSize) ;
  BiasFieldType bias(biasSize.size(), degree , biasSize) ;
  if (bias.GetNumberOfCoefficients() == m_BiasFieldCoefficients.size())
    {
      bias.SetCoefficients(m_BiasFieldCoefficients) ;
    } 
  else
    { // init ot all zero
      cleanCoeffs = true;
      m_BiasFieldCoefficients.clear();
      for ( unsigned int i = 0 ; i < bias.GetNumberOfCoefficients() ; ++i )
   {
     m_BiasFieldCoefficients.push_back(0.0) ;
   }
      bias.SetCoefficients(m_BiasFieldCoefficients) ;
    }

  // update the energy function ;
  m_EnergyFunction->SetBiasField(&bias) ;
  m_EnergyFunction->SetRegion(region) ;

  // initialize the 1+1 optimizer
  OptimizerType::Pointer optimizer = OptimizerType::New() ;
  optimizer->SetDebug(this->GetDebug()) ;
  optimizer->SetNormalVariateGenerator(m_NormalVariateGenerator.GetPointer()) ;
  optimizer->SetCostFunction(m_EnergyFunction.GetPointer()) ;
  optimizer->SetMaximumIteration(maximumIteration) ;
  if (m_OptimizerGrowthFactor > 0)
    {
    if (m_OptimizerShrinkFactor > 0)
      optimizer->Initialize(m_OptimizerInitialRadius, 
                            m_OptimizerGrowthFactor,
                            m_OptimizerShrinkFactor) ;
    else
      optimizer->Initialize(m_OptimizerInitialRadius,
                            m_OptimizerGrowthFactor) ;
    }
  else
    {
    if (m_OptimizerShrinkFactor > 0)
      optimizer->Initialize(m_OptimizerInitialRadius, 
                            m_OptimizerShrinkFactor) ;
    else
      optimizer->Initialize(m_OptimizerInitialRadius) ;
    }

  Array< double > scales(bias.GetNumberOfCoefficients()) ;
  scales.Fill(1.0) ;
  optimizer->SetScales(scales) ;

  int noOfBiasFieldCoefficients = bias.GetNumberOfCoefficients() ;
  const BiasFieldType::CoefficientArrayType tempCoefficients = 
    bias.GetCoefficients() ;
  typename EnergyFunctionType::ParametersType 
    initialPosition( noOfBiasFieldCoefficients );
  for( int i=0; i < noOfBiasFieldCoefficients; i++ )
    {
    initialPosition[i] = tempCoefficients[i];
    }
  optimizer->SetInitialPosition( initialPosition );

  try
    {
    for(unsigned int level = 0; level < m_NumberOfLevels; level++ )
      {
   typename EnergyFunctionType::SamplingFactorType energySampling;
   for( unsigned int dim = 0; dim < ImageDimension; dim++ )
     {
       energySampling[dim] = m_Schedule[level][dim];
     }
   m_EnergyFunction->SetSamplingFactors(energySampling) ;
   optimizer->MaximizeOff();
   optimizer->StartOptimization();
      }
    }
  catch (ExceptionObject& ie)
    {
    std::cout << ie << std::endl ;
    }
  catch (std::exception& e)
    {
    std::cout << e.what() << std::endl ;
    }

  bias.SetCoefficients(optimizer->GetCurrentPosition());
  if (cleanCoeffs) m_BiasFieldCoefficients.clear();

  return bias ;
}


template<class TInputImage, class TOutputImage, class TMaskImage>
void 
MRIBiasFieldCorrectionFilter<TInputImage, TOutputImage, TMaskImage>
::CorrectImage(BiasFieldType& bias,
               InputImageRegionType region)
{
  itkDebugMacro(<< "Correcting the image ");
  typedef InternalImagePixelType Pixel ;
  ImageRegionIterator<InternalImageType> iIter(m_InternalInput.GetPointer(), region) ;
  BiasFieldType::SimpleForwardIterator bIter(&bias) ;

  bIter.Begin() ;
  iIter.GoToBegin() ;

  if ( m_OutputMask.IsNotNull() )
    {
    itkDebugMacro(<< "Output mask is being used") ;
    ImageRegionIterator<ImageMaskType> mIter(m_OutputMask, region) ;
    mIter.GoToBegin() ;
    while (!bIter.IsAtEnd())
      {
      double inputPixel = iIter.Get() ;
      double diff = inputPixel - bIter.Get() ;
      if (mIter.Get() > 0.0)
        {
        iIter.Set( (Pixel) diff) ;
        }
      else
        {
        iIter.Set( (Pixel) inputPixel) ;
        }
      ++mIter ;
      ++bIter ;
      ++iIter ;
      }
    }
  else
    {
    itkDebugMacro(<< "Output mask is not being used") ;
    while (!bIter.IsAtEnd())
      {
      double diff = iIter.Get() - bIter.Get() ;
      iIter.Set( (Pixel) diff) ;
      ++bIter ;
      ++iIter ;
      }
    }
}

  
template<class TInputImage, class TOutputImage, class TMaskImage>
void 
MRIBiasFieldCorrectionFilter<TInputImage, TOutputImage, TMaskImage>
::CorrectInterSliceIntensityInhomogeneity(InputImageRegionType region)
{
  long lastSlice = region.GetIndex()[m_SlicingDirection] + 
    static_cast<long>(region.GetSize()[m_SlicingDirection]) ;
  InputImageRegionType sliceRegion ;
  InputImageIndexType index = region.GetIndex() ;
  InputImageSizeType size = region.GetSize() ;
  size[m_SlicingDirection] = 1 ;
  sliceRegion.SetSize(size) ;
  BiasFieldType::DomainSizeType biasSize ;

  while (index[m_SlicingDirection] < lastSlice)
    {
    itkDebugMacro(<< "    -- slice : " << index[m_SlicingDirection] );

    this->GetBiasFieldSize(sliceRegion, biasSize) ;
    sliceRegion.SetIndex(index) ;
    BiasFieldType bias = 
      this->EstimateBiasField(sliceRegion, 0, 
                              m_InterSliceCorrectionMaximumIteration) ;
    this->CorrectImage(bias, sliceRegion) ;
    index[m_SlicingDirection] += 1 ;
    }
}


template<class TInputImage, class TOutputImage, class TMaskImage>
void 
MRIBiasFieldCorrectionFilter<TInputImage, TOutputImage, TMaskImage>
::GenerateData()
{
  itkDebugMacro(<< "Initializing filter...");
  this->Initialize() ;
  itkDebugMacro(<< "Filter initialized." );

  if (m_UsingSlabIdentification)
    {
    itkDebugMacro(<< "Searching slabs...");

    typename MRASlabIdentifier<InputImageType>::Pointer identifier = 
      MRASlabIdentifier<InputImageType>::New() ;
    // find slabs
    identifier->SetImage(this->GetInput()) ;
    identifier->SetNumberOfSamples( m_SlabNumberOfSamples ) ;
    identifier->SetBackgroundMinimumThreshold( m_SlabBackgroundMinimumThreshold ) ;
    identifier->SetTolerance( m_SlabTolerance ) ;
    identifier->GenerateSlabRegions() ;
    m_Slabs = identifier->GetSlabRegionVector() ;

    itkDebugMacro(<< m_Slabs.size() << " slabs found.");
    }
  else
    {
      // creates a single region which is the largest possible region of
      // the input image.
      m_Slabs.clear();
      m_Slabs.push_back(this->GetInput()->GetLargestPossibleRegion()) ;
    }

  this->AdjustSlabRegions(m_Slabs, this->GetOutput()->GetRequestedRegion()) ;
  itkDebugMacro(<< "After adjustment, there are " << static_cast<unsigned long>( m_Slabs.size() ) 
                << " slabs.");

  SlabRegionVectorIteratorType iter = m_Slabs.begin();
    
  int count = 0 ;
  while (iter != m_Slabs.end())
    {
    if (this->GetDebug() && m_UsingSlabIdentification)
      {
      itkDebugMacro(<< "## Slab :" << count);
      }

    // correct inter-slice intensity inhomogeniety
    // using 0th degree Legendre polynomial

    if (m_UsingInterSliceIntensityCorrection)
      {
      itkDebugMacro(<< "  Correcting inter-slice intensity..." );
      this->CorrectInterSliceIntensityInhomogeneity(*iter) ;
      itkDebugMacro(<< "  Inter-slice intensity corrected.");
      }

    // correct 3D bias
    if (m_UsingBiasFieldCorrection)
      {
      itkDebugMacro(<< "  Correcting bias..." );

      BiasFieldType bias = 
        this->EstimateBiasField(*iter, m_BiasFieldDegree, 
                                m_VolumeCorrectionMaximumIteration) ;

      const BiasFieldType::CoefficientArrayType& tempCoefficients = 
        bias.GetCoefficients() ;
      m_EstimatedBiasFieldCoefficients.resize( tempCoefficients.size() ) ;
      for (unsigned int k = 0 ; k < tempCoefficients.size() ; k++)
        {
        m_EstimatedBiasFieldCoefficients[k] = tempCoefficients[k] ;
        }

      this->CorrectImage(bias, *iter) ;
      itkDebugMacro(<< "  Bias corrected." );
      }
    iter++ ;
    count++ ;
    }
    
  if (this->IsBiasFieldMultiplicative()) 
    {
      // for multiple calls it is necessary to restore the tissue classes and the Initial Radius 
      // (i.e. everything that has been log-transformed in the initialization)
      const unsigned int size = m_TissueClassMeans.Size();
      for( unsigned int i = 0 ; i < size; i++ ) 
   {      
     m_TissueClassMeans[i] = exp(m_TissueClassMeans[i]) - 1.0 ;
     m_TissueClassSigmas[i] = exp(m_TissueClassSigmas[i]) * (1.0 + m_TissueClassMeans[i]) - m_TissueClassMeans[i];
   }
      m_OptimizerInitialRadius = exp(m_OptimizerInitialRadius) - 1.0 ;
    }

  if (m_GeneratingOutput)
    {
    itkDebugMacro( << "Generating the output image...");

    if (this->IsBiasFieldMultiplicative()) 
      {
      ExpImage(m_InternalInput.GetPointer(), m_InternalInput.GetPointer()) ;
      }
        
    OutputImageType* output = this->GetOutput() ;
    output->SetRegions(this->GetInput()->GetLargestPossibleRegion()) ;
    output->Allocate() ;
    CopyAndConvertImage
      (m_InternalInput.GetPointer(), output, output->GetRequestedRegion()) ;

    itkDebugMacro( << "The output image generated." );
    }
}

// energy function related members
template<class TInputImage, class TOutputImage, class TMaskImage>
void 
MRIBiasFieldCorrectionFilter<TInputImage, TOutputImage, TMaskImage>
::SetTissueClassStatistics( const Array<double> & means, 
                            const Array<double> & sigmas) 
  throw (ExceptionObject)
{
  size_t meanSize = means.Size() ;
  size_t sigmaSize = sigmas.Size() ;
    
  if ( meanSize == 0 )
    { 
    itkExceptionMacro(<< "arrays of Means is empty");
    }

  if ( sigmaSize == 0 )
    { 
    itkExceptionMacro(<< "arrays of Sigmas is empty");
    }

  if (meanSize != sigmaSize )
    { 
    itkExceptionMacro(<< "arrays of Means and Sigmas have different lengths");
    }
      

  m_TissueClassMeans  = means ;
  m_TissueClassSigmas = sigmas ;
}

// protected members
template<class TInputImage, class TOutputImage, class TMaskImage>
bool 
MRIBiasFieldCorrectionFilter<TInputImage, TOutputImage, TMaskImage>
::CheckMaskImage(ImageMaskType* mask)
{
  InputImageRegionType region =
    this->GetInput()->GetBufferedRegion() ;

  ImageMaskRegionType m_region =
    mask->GetBufferedRegion() ;
  if (region.GetSize() != m_region.GetSize())
    {
    return false ;
    }
  return true ;
}


template<class TInputImage, class TOutputImage, class TMaskImage>
void 
MRIBiasFieldCorrectionFilter<TInputImage, TOutputImage, TMaskImage>
::Log1PImage(InternalImageType* source,
             InternalImageType* target)
{
  InternalImageRegionType region ;
  region = source->GetRequestedRegion() ;
    
  ImageRegionIterator<InternalImageType> s_iter(source, region) ;
  ImageRegionIterator<InternalImageType> t_iter(target, region) ;
    
  InternalImagePixelType pixel ;
  s_iter.GoToBegin() ;
  t_iter.GoToBegin() ;
  while (!s_iter.IsAtEnd())
    {
    pixel = s_iter.Get() ;
        
    if (pixel < 0)
      t_iter.Set( 0.0 ) ;
    else
      t_iter.Set( log( pixel + 1 ) ) ;
        
    ++s_iter ;
    ++t_iter ;
    }
}

template<class TInputImage, class TOutputImage, class TMaskImage>
void 
MRIBiasFieldCorrectionFilter<TInputImage, TOutputImage, TMaskImage>
::ExpImage(InternalImageType* source,
           InternalImageType* target)
{
  InternalImageRegionType region ;
  region = source->GetLargestPossibleRegion() ;

  ImageRegionIterator<InternalImageType> s_iter(source, region) ;
  ImageRegionIterator<InternalImageType> t_iter(target, region) ;

  s_iter.GoToBegin() ;
  t_iter.GoToBegin() ;

  double temp ;
  while (!s_iter.IsAtEnd())
    {
    temp = s_iter.Get() ;
    //t_iter.Set( m_EnergyFunction->GetEnergy0(temp)) ;
    temp = exp(temp) - 1;
    t_iter.Set( (InternalImagePixelType) temp  ) ;
    
    ++s_iter ;
    ++t_iter ;
    }
}

template<class TInputImage, class TOutputImage, class TMaskImage>
void 
MRIBiasFieldCorrectionFilter<TInputImage, TOutputImage, TMaskImage>
::GetBiasFieldSize(InputImageRegionType region,
                   BiasFieldType::DomainSizeType& biasSize)
{
  InputImageSizeType size = region.GetSize() ;
  unsigned int dim;
  int biasDim = 0 ;

  for(dim = 0 ; dim < ImageDimension ; dim++)
    {
    if (size[dim] > 1)
      {
      biasDim++ ;
      }
    }

  biasSize.resize(biasDim) ;

  biasDim = 0 ;
  for(dim = 0 ; dim < ImageDimension ; dim++)
    {
    if (size[dim] > 1)
      {
      biasSize[biasDim] = size[dim] ;
      biasDim++ ;
      }
    }
}

template<class TInputImage, class TOutputImage, class TMaskImage>
void 
MRIBiasFieldCorrectionFilter<TInputImage, TOutputImage, TMaskImage>
::AdjustSlabRegions(SlabRegionVectorType& slabs, 
                    OutputImageRegionType requestedRegion) 
{
  OutputImageIndexType indexFirst = requestedRegion.GetIndex() ;
  OutputImageSizeType size = requestedRegion.GetSize() ;
  OutputImageIndexType indexLast = indexFirst ;
  for (unsigned long i = 0 ; i < ImageDimension ; i++)
    indexLast[i] = indexFirst[i] + static_cast<long>(size[i]) - 1 ;

  long coordFirst = indexFirst[m_SlicingDirection] ;
  long coordLast = indexLast[m_SlicingDirection] ;
  long coordFirst2 ;
  long coordLast2 ;
  long tempCoordFirst ;
  long tempCoordLast ;

  OutputImageRegionType tempRegion ;
  OutputImageSizeType tempSize = size ;
  OutputImageIndexType tempIndex = indexFirst ;

  SlabRegionVectorIteratorType iter = slabs.begin() ;
  while (iter != slabs.end())
    {
    coordFirst2 = (*iter).GetIndex()[m_SlicingDirection] ;
    coordLast2 = coordFirst2 + 
      static_cast<long>((*iter).GetSize()[m_SlicingDirection]) - 1 ;

    if (coordFirst > coordFirst2)
      tempCoordFirst = coordFirst ;
    else
      tempCoordFirst = coordFirst2 ;

    if (coordLast < coordLast2)
      tempCoordLast = coordLast ;
    else
      tempCoordLast = coordLast2 ;
          
    if (tempCoordFirst <= tempCoordLast)
      {
      tempIndex[m_SlicingDirection] = tempCoordFirst ;
      tempSize[m_SlicingDirection] = tempCoordLast - tempCoordFirst + 1 ;
      tempRegion.SetIndex(tempIndex) ;
      tempRegion.SetSize(tempSize) ;
      *iter = tempRegion ;
      }
    else
      {
      // no ovelapping, so remove the slab from the vector
      slabs.erase(iter) ;
      }
    iter++ ;
    }
}

} // end namespace itk

#endif
