/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkIterativeInverseDeformationFieldImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkIterativeInverseDeformationFieldImageFilter_h
#define __itkIterativeInverseDeformationFieldImageFilter_h


#include "itkImageToImageFilter.h"

#include "itkWarpVectorImageFilter.h"
#include "itkVectorLinearInterpolateImageFunction.h"
#include "itkImageRegionIterator.h"
#include "itkTimeProbe.h"



namespace itk
{
 
/** \class IterativeInverseDeformationFieldImageFilter
 * \brief Computes the inverse of a deformation field.
 *
 * IterativeInverseDeformationFieldImageFilter takes a deformation field as input and
 * computes the deformation field that is its inverse. If the input deformation
 * field was mapping coordinates from a space A into a space B, the output of
 * this filter will map coordinates from the space B into the space A.
 * 
 * The algorithm implemented in this filter uses an iterative method for
 * progresively refining the values of the inverse field. Starting from the
 * direct field, at every pixel the direct mapping of this point is found, and
 * a the nevative of the current deformation is stored in the inverse field at
 * the nearest pixel. Then, subsequent iterations verify if any of the neigbor pixels
 * provide a better return to the current pixel, in which case its value is taken for
 * updating the vector in the inverse field.
 *
 * This method was discussed in the users-list during February 2004.
 *
 * \author  Corinne Mattmann
 *
 */

  template < class TInputImage, class TOutputImage >
  class ITK_EXPORT IterativeInverseDeformationFieldImageFilter :
  public ImageToImageFilter<TInputImage,TOutputImage> 
  {
  public:
    /** Standard class typedefs. */
    typedef IterativeInverseDeformationFieldImageFilter  Self;
    typedef ImageToImageFilter<TInputImage,TOutputImage>   Superclass;
    typedef SmartPointer<Self>        Pointer;
    typedef SmartPointer<const Self>  ConstPointer;

    /** Method for creation through the object factory. */
    itkNewMacro(Self);

    /** Run-time type information (and related methods). */
    itkTypeMacro(IterativeInverseDeformationFieldImageFilter, ImageToImageFilter);

    /** Some typedefs. */
    typedef TInputImage InputImageType;
    typedef typename InputImageType::ConstPointer    InputImageConstPointer;
    typedef typename InputImageType::Pointer         InputImagePointer;
    typedef typename InputImageType::PointType       InputImagePointType; 
    typedef typename InputImageType::RegionType      InputImageRegionType; 
    typedef typename InputImageType::SpacingType     InputImageSpacingType; 
    typedef TOutputImage OutputImageType;
    typedef typename OutputImageType::Pointer        OutputImagePointer;
    typedef typename OutputImageType::PixelType      OutputImagePixelType;
    typedef typename OutputImageType::PointType      OutputImagePointType;
    typedef typename OutputImageType::IndexType      OutputImageIndexType;
    typedef typename OutputImagePixelType::ValueType OutputImageValueType;

    typedef TimeProbe TimeType;

    typedef ImageRegionConstIterator<InputImageType> InputConstIterator;
    typedef ImageRegionIterator<InputImageType> InputIterator;
    typedef ImageRegionIterator<OutputImageType> OutputIterator;

    typedef WarpVectorImageFilter<TOutputImage,TInputImage,TOutputImage> VectorWarperType;

    typedef VectorLinearInterpolateImageFunction<TInputImage,double> FieldInterpolatorType;
    typedef typename FieldInterpolatorType::Pointer     FieldInterpolatorPointer;
    typedef typename FieldInterpolatorType::OutputType  FieldInterpolatorOutputType;

    itkSetMacro(NumberOfIterations, unsigned int);
    itkGetMacro(NumberOfIterations, unsigned int);

    // If the error (in mm) between forward and backward mapping is smaller than the StopValue,
    // the algorithm stops.
    // This value can be used to speed up the calculation.
    itkSetMacro(StopValue, double);
    itkGetMacro(StopValue, double);

    char* GetReport() {return this->m_Report;}

#ifdef ITK_USE_CONCEPT_CHECKING
    /** Begin concept checking */
    itkConceptMacro(OutputHasNumericTraitsCheck,
                    (Concept::HasNumericTraits<OutputImageValueType>));
    /** End concept checking */
#endif

  protected:
    IterativeInverseDeformationFieldImageFilter();
    ~IterativeInverseDeformationFieldImageFilter() {}

    void PrintSelf(std::ostream& os, Indent indent) const;
    void MakeReport();

    void GenerateData( );

    unsigned int m_NumberOfIterations;
    double m_StopValue;
    double m_Time;

  private:
    IterativeInverseDeformationFieldImageFilter(const Self&); //purposely not implemented
    void operator=(const Self&); //purposely not implemented
  };

} // end namespace itk

#include "itkIterativeInverseDeformationFieldImageFilter.txx"

#endif

