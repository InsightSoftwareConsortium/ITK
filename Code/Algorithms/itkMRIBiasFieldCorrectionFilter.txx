/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMRIBiasFieldCorrectionFilter.txx
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
#ifndef __itkMRIBiasFieldCorrectionFilter_txx
#define __itkMRIBiasFieldCorrectionFilter_txx

#include "itkMRIBiasFieldCorrectionFilter.h"

namespace itk
{

  // =========== MRIBiasEnergyFunction members =================

  template<class TImage, class TImageMask, class TBiasField>
  MRIBiasEnergyFunction<TImage, TImageMask, TBiasField>
  ::MRIBiasEnergyFunction(std::vector<double> classMeans, 
                          std::vector<double> classSigmas)
  {
    m_Image = 0 ;
    m_Mask = 0 ;
    m_BiasField = 0 ;

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
  void
  MRIBiasEnergyFunction<TImage, TImageMask, TBiasField>
  ::SetImage(ImagePointer image)
  {
    m_Image = image ;
  }


  template<class TImage, class TImageMask, class TBiasField>
  void
  MRIBiasEnergyFunction<TImage, TImageMask, TBiasField>
  ::SetMask(MaskPointer mask)
  {
    m_Mask = mask ;
  }

  template<class TImage, class TImageMask, class TBiasField>
  void
  MRIBiasEnergyFunction<TImage, TImageMask, TBiasField>
  ::SetRegion(ImageRegionType region)
  {
    m_Region = region ;
  }

  template<class TImage, class TImageMask, class TBiasField>
  void
  MRIBiasEnergyFunction<TImage, TImageMask, TBiasField>
  ::SetBiasField(BiasFieldType* biasField)
  {
    m_BiasField = biasField ;
  }


  template<class TImage, class TImageMask, class TBiasField>
  MRIBiasEnergyFunction<TImage, TImageMask, TBiasField>::MeasureType
  MRIBiasEnergyFunction<TImage, TImageMask, TBiasField>
  ::GetValue(ParametersType parameters, MeasureType& ret) 
  {

    if (m_Image == 0 || m_InternalEnergyFunction == 0 || m_BiasField == 0)
      exit(0) ;

    double  total = 0.0;
  
    ImageRegionIterator<ImageType> iIter(m_Image, m_Region) ;

    m_BiasField->SetCoefficients(parameters) ;
    //dump(m_BiasField->GetCoefficients()) ;
    typename TBiasField::SimpleForwardIterator bIter(m_BiasField) ;
    bIter.Begin() ;

    if (!m_Mask)
      {
        while (!iIter.IsAtEnd())
          {
            total += 
             (*m_InternalEnergyFunction)(iIter.Get() - bIter.Get());
            ++iIter ;
            ++bIter ;
          }
      }
    else
      {
        itk::ImageRegionIterator<MaskType> 
          mIter(m_Mask, m_Region) ;
        while (!iIter.IsAtEnd())
          {
            if (mIter.Get() > 0.0) 
              {
                total += 
                  (*m_InternalEnergyFunction)(iIter.Get() - bIter.Get());
              }
            ++iIter ;
            ++bIter;
            ++mIter ;
          }
      }

    return ret = total ;
  }


  // =========== MRIBiasFieldCorrectionFilter members ==================

  template<class TInputImage, class TOutputImage>
  MRIBiasFieldCorrectionFilter<TInputImage, TOutputImage>
  ::MRIBiasFieldCorrectionFilter()
  {
    m_InputMask = 0 ;
    m_OutputMask = 0 ;

    m_BiasMultiplicative = true ;
    m_BiasFieldDegree = 3 ;
    m_OptimizerInitialRadius = 1 ;
    m_OptimizerMaximumIteration = 100 ;
    m_OptimizerGrowFactor = 0 ;
    m_OptimizerShrinkFactor = 0 ;
    
    m_EnergyFunction = 0 ;
    m_Optimizer = OptimizerType::New() ;
    
    if (ImageDimension == 3)
        m_UsingInterSliceIntensityCorrection = true ;
    else
        m_UsingInterSliceIntensityCorrection = false ;
    
    m_UsingSlabIdentification = false ;
    m_UsingBiasFieldCorrection = true ;
    m_GeneratingOutput = true ;

    m_VerboseMode = false ;
  }

  template<class TInputImage, class TOutputImage>
  MRIBiasFieldCorrectionFilter<TInputImage, TOutputImage>
  ::~MRIBiasFieldCorrectionFilter()
  {
    delete m_EnergyFunction ;
    m_EnergyFunction = 0 ;
  }
  
  template<class TInputImage, class TOutputImage>
  void
  MRIBiasFieldCorrectionFilter<TInputImage, TOutputImage>
  ::SetInputMask(ImageMaskPointer inputMask)
  {
    if (this->CheckMaskImage(inputMask))
      m_InputMask = inputMask ;
    else
      throw ExceptionObject(__FILE__, __LINE__) ;
  }

  template<class TInputImage, class TOutputImage>
  void 
  MRIBiasFieldCorrectionFilter<TInputImage, TOutputImage>
  ::SetOutputMask(ImageMaskPointer outputMask)
  {
    if (this->CheckMaskImage(outputMask))
      m_OutputMask = outputMask ;
    else
      throw ExceptionObject(__FILE__, __LINE__) ;
  }

  template<class TInputImage, class TOutputImage>
  void 
  MRIBiasFieldCorrectionFilter<TInputImage, TOutputImage>
  ::Initialize() throw (ExceptionObject)
  {
    // if the bias is multiplicative, we will use logarithm
    // the following section applies logarithm to key parameters and 
    // image data
    if (this->IsBiasFieldMultiplicative())
      { 
        for (int i = 0 ; i < m_TissueClassMeans.size() ; i++) 
          {
            m_TissueClassSigmas[i] = log(1.0 + m_TissueClassSigmas[i] / 
                                         (m_TissueClassMeans[i] + 1.0)) ;
            m_TissueClassMeans[i] = log(m_TissueClassMeans[i] + 1.0) ;
          }

        
        m_OptimizerInitialRadius = log(m_OptimizerInitialRadius) ;
        
        this->Log1PImage(m_InternalInput, m_InternalInput) ;
      }



    // initialize the energy function
    if (m_TissueClassMeans.size() < 1) 
      throw ExceptionObject(__FILE__, __LINE__) ;

    if (!m_EnergyFunction)
      {
        m_EnergyFunction = new EnergyFunction(m_TissueClassMeans, 
                                              m_TissueClassSigmas) ;
      }


    m_EnergyFunction->SetImage(m_InternalInput) ;

    if (m_InputMask)
      m_EnergyFunction->SetMask(m_InputMask) ;
    
    // initialize the 1+1 optimizer
    m_Optimizer->SetVerboseMode(m_VerboseMode) ;
    m_Optimizer->SetCostFunction(m_EnergyFunction) ;
    
    if (m_OptimizerGrowFactor > 0)
      {
        if (m_OptimizerShrinkFactor > 0)
          m_Optimizer->Initialize(m_OptimizerInitialRadius, 
                                  m_OptimizerGrowFactor,
                                  m_OptimizerShrinkFactor) ;
        else
          m_Optimizer->Initialize(m_OptimizerInitialRadius,
                                  m_OptimizerGrowFactor) ;
      }
    else
      {
        if (m_OptimizerShrinkFactor > 0)
          m_Optimizer->Initialize(m_OptimizerInitialRadius, 
                                  m_OptimizerShrinkFactor) ;
        else
          m_Optimizer->Initialize(m_OptimizerInitialRadius) ;
      }

    m_Optimizer->SetMaximumIteration(m_OptimizerMaximumIteration) ;
  }


  template<class TInputImage, class TOutputImage>
  void 
  MRIBiasFieldCorrectionFilter<TInputImage, TOutputImage>
  ::EstimateBiasField(BiasField* bias,
                      InputImageRegionType region)
  {
    m_EnergyFunction->SetBiasField(bias) ;
    m_EnergyFunction->SetRegion(region) ;
    m_Optimizer->SetSpaceDimension(bias->GetNoOfCoefficients()) ;
    m_Optimizer->SetInitialPosition(bias->GetCoefficients()) ;
    m_Optimizer->Run() ;
    bias->SetCoefficients(m_Optimizer->GetCurrentPosition()) ;
  }


  template<class TInputImage, class TOutputImage>
  void 
  MRIBiasFieldCorrectionFilter<TInputImage, TOutputImage>
  ::CorrectImage(BiasField* bias,
                InputImageRegionType region)
  {
    typedef InternalImagePixelType Pixel ;

    ImageRegionIterator<InternalImageType> iIter(m_InternalInput, region) ;
  
    BiasField::SimpleForwardIterator bIter(bias) ;

    //    ImageRegionIterator<OutputImageType> oIter(m_I, region) ;

    bIter.Begin() ;
    //    oIter.GoToBegin() ;
    iIter.GoToBegin() ;

    ImageMaskPointer outputMask = this->GetOutputMask() ;
    if (outputMask)
      {
        ImageRegionIterator<ImageMaskType> mIter(outputMask, region) ;
        mIter.GoToBegin() ;
        while (!iIter.IsAtEnd())
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
            // ++oIter ;
            ++bIter ;
            ++iIter ;
          }
      }
    else
      {
        while (!iIter.IsAtEnd())
          {
            double diff = iIter.Get() - bIter.Get() ;
            iIter.Set( (Pixel) diff) ;
            //++oIter ;
            ++bIter ;
            ++iIter ;
          }
      }
  }

  
  template<class TInputImage, class TOutputImage>
  void 
  MRIBiasFieldCorrectionFilter<TInputImage, TOutputImage>
  ::CorrectInterSliceIntensityInhomogeneity(InputImageRegionType region)
  {
    int lastSlice = region.GetIndex()[m_SlicingDirection] + 
      region.GetSize()[m_SlicingDirection] ;
    InputImageRegionType sliceRegion ;
    InputImageIndexType index = region.GetIndex() ;
    InputImageSizeType size = region.GetSize() ;
    size[m_SlicingDirection] = 1 ;
    sliceRegion.SetSize(size) ;
    BiasField::DomainSizeType biasSize ;
    while (index[m_SlicingDirection] < lastSlice)
      {
        if (m_VerboseMode)
          std::cout << "    -- slice : " << index[m_SlicingDirection] 
                    << std::endl ;

        this->GetBiasFieldSize(sliceRegion, biasSize) ;
        BiasField* bias = new BiasField(biasSize.size(), 0, biasSize) ;
        sliceRegion.SetIndex(index) ;
        this->EstimateBiasField(bias, sliceRegion) ;
        this->CorrectImage(bias, sliceRegion) ;
        index[m_SlicingDirection] += 1 ;
        delete bias ;
      }
  }


  template<class TInputImage, class TOutputImage>
  void 
  MRIBiasFieldCorrectionFilter<TInputImage, TOutputImage>
  ::GenerateData()
  {
    m_InternalInput = InternalImageType::New() ;

    // copy the input image to the internal image and cast the type to
    // float (InternalImageType::PixelType)
    CopyAndConvertImage<InputImageType, InternalImageType>
      (this->GetInput(), m_InternalInput, 
       this->GetOutput()->GetRequestedRegion()) ;

    if (m_VerboseMode)
      std::cout << "Initializing filter..." << std::endl ;
    this->Initialize() ;
    if (m_VerboseMode)
      std::cout << "Filter initialized." << std::endl ;

    if (m_UsingSlabIdentification)
      {
        if (m_VerboseMode)
          std::cout << "Searching slabs..." << std::endl ;
        // find slabs
        MRASlabIdentifier<InputImageType>::Pointer identifier = 
          MRASlabIdentifier<InputImageType>::New() ;
        identifier->SetImage(this->GetInput()) ;
        identifier->SetNumberOfMinimumsPerSlice(100) ;
        identifier->GenerateSlabRegions() ;
        m_Slabs = identifier->GetSlabRegionVector() ;

        if (m_VerboseMode)
          std::cout << m_Slabs.size() << " slabs found." << std::endl ;
      }
    else
      {
        // creates a single region which is the largest possible region of
        // the input image.
        m_Slabs.push_back(this->GetInput()->GetLargestPossibleRegion()) ;
      }

    this->AdjustSlabRegions(m_Slabs, this->GetOutput()->GetRequestedRegion()) ;
    if (m_VerboseMode)
      std::cout << "After adjustment, ther are " << m_Slabs.size() 
                << " slabs." << std::endl ;

    SlabRegionVectorIteratorType iter = m_Slabs.begin();
    
    BiasField::DomainSizeType biasSize ;

    int count = 0 ;
    while (iter != m_Slabs.end())
      {
        if (m_VerboseMode && m_UsingSlabIdentification)
          std::cout << "## Slab :" << count << std::endl ;

        // correct inter-slice intensity inhomogeniety
        // using 0th degree Legendre polynomial

        if (m_UsingInterSliceIntensityCorrection)
          {
            // turn off optimizer's verbose mode 
            m_Optimizer->SetVerboseMode(false) ;

            if (m_VerboseMode)
              std::cout << "  Correcting inter-slice intensity..." 
                        << std::endl ;
            this->CorrectInterSliceIntensityInhomogeneity(*iter) ;
            if (m_VerboseMode)
              std::cout << "  Inter-slice intensity corrected." << std::endl ;
            
            // restore optimizer's verbose mode setting
            m_Optimizer->SetVerboseMode(m_VerboseMode) ;
          }

        // correct 3D bias
        if (m_UsingBiasFieldCorrection)
          {
            if (m_VerboseMode)
              std::cout << "  Correcting bias..." << std::endl ;

            this->GetBiasFieldSize(*iter, biasSize) ;
            BiasField* bias = 
              new BiasField(biasSize.size(), m_BiasFieldDegree, biasSize) ;
            if (bias->GetNoOfCoefficients() 
                == m_BiasFieldCoefficients.size())
              {
                bias->SetCoefficients(m_BiasFieldCoefficients) ;
              }
            
            this->EstimateBiasField(bias, *iter) ;
            m_EstimatedBiasFieldCoefficients = bias->GetCoefficients() ;
            m_BiasFieldDimension = bias->GetDimension() ;
            m_NoOfBiasFieldCoefficients = bias->GetNoOfCoefficients() ;
            m_BiasFieldDomainSize = bias->GetDomainSize() ;
            this->CorrectImage(bias, *iter) ;
            delete bias ;
            if (m_VerboseMode)
              std::cout << "  Bias corrected." << std::endl ;
          }
        iter++ ;
        count++ ;
      }
    
    OutputImagePointer output = this->GetOutput() ;
    if (m_GeneratingOutput)
      {
        if (m_VerboseMode)
          std::cout << "Generating the output image..." << std::endl ;

        if (this->IsBiasFieldMultiplicative()) 
          ExpImage(m_InternalInput, m_InternalInput) ;
        
        CopyAndConvertImage<InternalImageType, OutputImageType>
          (m_InternalInput, output, output->GetRequestedRegion()) ;

        if (m_VerboseMode)
          std::cout << "The output image generated." << std::endl ;
      }
  }

    
    
  // energy function related members

  template<class TInputImage, class TOutputImage>
  void 
  MRIBiasFieldCorrectionFilter<TInputImage, TOutputImage>
  ::SetTissueClassStatistics(std::vector<double> means, 
                             std::vector<double> sigmas) 
    throw (ExceptionObject)
  {
    size_t meanSize = means.size() ;
    size_t sigmaSize = sigmas.size() ;
    
    if (meanSize == 0 || sigmaSize == 0)
      throw ExceptionObject(__FILE__, __LINE__) ;

    if (meanSize != sigmaSize )
      throw ExceptionObject(__FILE__, __LINE__) ;

    m_TissueClassMeans = means ;
    m_TissueClassSigmas = sigmas ;
  }

  // protected members

  template<class TInputImage, class TOutputImage>
  bool 
  MRIBiasFieldCorrectionFilter<TInputImage, TOutputImage>
  ::CheckMaskImage(ImageMaskPointer mask)
  {
    if (this->GetNumberOfOutputs() != 1)
      throw ExceptionObject(__FILE__, __LINE__) ;

    InputImageRegionType region =
      this->GetInput()->GetBufferedRegion() ;

    ImageMaskRegionType m_region =
      mask->GetBufferedRegion() ;
    if (region.GetSize() != 
        m_region.GetSize())
      return false ;
    
    return true ;
  }


  template<class TInputImage, class TOutputImage>
  void 
  MRIBiasFieldCorrectionFilter<TInputImage, TOutputImage>
  ::Log1PImage(InternalImagePointer source,
             InternalImagePointer target)
  {
    InternalImageRegionType region ;
    region = source->GetRequestedRegion() ;
    
    ImageRegionIterator<InternalImageType> s_iter(source, region) ;
    ImageRegionIterator<InternalImageType> t_iter(target, region) ;
    
    InternalImagePixelType pixel ;

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

  template<class TInputImage, class TOutputImage>
  void 
  MRIBiasFieldCorrectionFilter<TInputImage, TOutputImage>
  ::ExpImage(InternalImagePointer source,
             InternalImagePointer target)
  {
    InternalImageRegionType region ;
    region = source->GetRequestedRegion() ;
    
    ImageRegionIterator<InternalImageType> s_iter(source, region) ;
    ImageRegionIterator<InternalImageType> t_iter(target, region) ;
    
    InternalImagePixelType pixel ;

    while (!s_iter.IsAtEnd())
      {
        pixel = s_iter.Get() ;
        
        t_iter.Set(exp(pixel) - 1) ;
        
        ++s_iter ;
        ++t_iter ;
      }
  }

  template<class TInputImage, class TOutputImage>
  void 
  MRIBiasFieldCorrectionFilter<TInputImage, TOutputImage>
  ::GetBiasFieldSize(InputImageRegionType region,
                     BiasField::DomainSizeType& biasSize)
  {
    InputImageSizeType size = region.GetSize() ;
    long dim = 0 ;
    biasSize.clear() ;
    for(dim = 0 ; dim < ImageDimension ; dim++)
      {
        if (size[dim] > 1)
          biasSize.push_back(size[dim]) ;
      }
  }

  template<class TInputImage, class TOutputImage>
  void 
  MRIBiasFieldCorrectionFilter<TInputImage, TOutputImage>
  ::AdjustSlabRegions(SlabRegionVectorType& slabs, 
                      OutputImageRegionType requestedRegion) 
  {
    OutputImageIndexType indexFirst = requestedRegion.GetIndex() ;
    OutputImageSizeType size = requestedRegion.GetSize() ;
    OutputImageIndexType indexLast = indexFirst ;
    for (unsigned long i = 0 ; i < ImageDimension ; i++)
      indexLast[i] = indexFirst[i] + size[i] - 1 ;

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
          (*iter).GetSize()[m_SlicingDirection] - 1 ;

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
