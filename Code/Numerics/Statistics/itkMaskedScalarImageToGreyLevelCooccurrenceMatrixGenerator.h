/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMaskedScalarImageToGreyLevelCooccurrenceMatrixGenerator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMaskedScalarImageToGreyLevelCooccurrenceMatrixGenerator_h
#define __itkMaskedScalarImageToGreyLevelCooccurrenceMatrixGenerator_h

#include "itkScalarImageToGreyLevelCooccurrenceMatrixGenerator.h"
#include "itkMacro.h"

namespace itk {
  namespace Statistics {

/** \class MaskedScalarImageToGreyLevelCooccurrenceMatrixGenerator 
*  \brief This class computes a grey-level co-occurence matrix (histogram) from
* a given image and mask. GLCM's are used for image texture description.
*
* For details about the general method, see the documentation for the 
* ScalarImageToGreyLevelCooccurrenceMatrixGenerator class.
*
* This class differs in that a mask may be set. The GLCM will only be populated
* with co-occurence measurements where both the pixel in question and the
* offset pixel both fall in the masked region. The mask must be the same size
* as the input image.
*
* If no mask is set, the behavior is exactly the same as the 
* ScalarImageToGreyLevelCooccurrenceMatrixGenerator class.
*
* \sa ScalarImageToGreyLevelCooccurrenceMatrixGenerator
* \sa GreyLevelCooccurrenceMatrixTextureCoefficientsCalculator
* \sa ScalarImageTextureCalculator
*
* Author: Zachary Pincus
*/
    
template< class TImageType,
          class THistogramFrequencyContainer = DenseFrequencyContainer >
class MaskedScalarImageToGreyLevelCooccurrenceMatrixGenerator : 
    public ScalarImageToGreyLevelCooccurrenceMatrixGenerator< TImageType, 
      THistogramFrequencyContainer >
  {
  public:
    /** Standard typedefs */
    typedef MaskedScalarImageToGreyLevelCooccurrenceMatrixGenerator Self;
    typedef ScalarImageToGreyLevelCooccurrenceMatrixGenerator< TImageType, 
      THistogramFrequencyContainer > Superclass;
    typedef SmartPointer<Self> Pointer;
    typedef SmartPointer<const Self> ConstPointer;
    
    /** Run-time type information (and related methods). */
    itkTypeMacro(MaskedScalarImageToGreyLevelCooccurrenceMatrixGenerator, Object);
    
    /** standard New() method support */
    itkNewMacro(Self) ;
    
    typedef TImageType                                      ImageType;
    typedef typename ImageType::Pointer                     ImagePointer;
    typedef typename ImageType::ConstPointer                ImageConstPointer;
    typedef typename ImageType::PixelType                   PixelType;
    typedef typename ImageType::RegionType                  RegionType;
    typedef typename ImageType::SizeType                    RadiusType;
    typedef typename ImageType::OffsetType                  OffsetType;
    typedef VectorContainer<unsigned char, OffsetType>      OffsetVector;
    typedef typename OffsetVector::Pointer                  OffsetVectorPointer;
    typedef typename OffsetVector::ConstPointer             OffsetVectorConstPointer;
    
    typedef typename NumericTraits<PixelType>::RealType     MeasurementType;
    
    typedef Histogram< MeasurementType, 2, THistogramFrequencyContainer >
                                                            HistogramType;
    typedef typename HistogramType::Pointer                 HistogramPointer;
    typedef typename HistogramType::ConstPointer            HistogramConstPointer;
    typedef typename HistogramType::MeasurementVectorType   MeasurementVectorType;
    

       
    /** Connects the mask image for which the histogram is going to be computed */
    itkSetConstObjectMacro( ImageMask, ImageType );
    itkGetConstObjectMacro( ImageMask, ImageType );

    /** Set the pixel value of the mask that should be considered "inside" the 
      object. Defaults to one. */
    itkSetMacro( InsidePixelValue, PixelType );
    itkGetMacro( InsidePixelValue, PixelType );

  protected:
    MaskedScalarImageToGreyLevelCooccurrenceMatrixGenerator();
    virtual ~MaskedScalarImageToGreyLevelCooccurrenceMatrixGenerator() {};
    void PrintSelf(std::ostream& os, Indent indent) const;
    void FillHistogram( RadiusType radius, RegionType region );

   private:    
    ImageConstPointer m_ImageMask;
    PixelType    m_InsidePixelValue;

  };
    
    
  } // end of namespace Statistics 
} // end of namespace itk 

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMaskedScalarImageToGreyLevelCooccurrenceMatrixGenerator.txx"
#endif

#endif
