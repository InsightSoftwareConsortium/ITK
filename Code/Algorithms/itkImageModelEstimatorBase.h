/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageModelEstimatorBase.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkImageModelEstimatorBase_h
#define _itkImageModelEstimatorBase_h

#include "itkLightProcessObject.h"

namespace itk
{

/** \class ImageModelEstimatorBase
 * \brief Base class for model estimation from images used for classification.
 * 
 * itkImageModelEstimatorBase is the base class for the ImageModelEstimator 
 * objects. It provides the basic function definitions that are inherent to
 *  a ImageModelEstimator objects.
 *
 * This is the SuperClass for the ImageModelEstimator framework. This is an
 * abstract class defining an interface for all such objects 
 * available through the ImageModelEstimator framework in the ITK toolkit.
 *
 * The basic functionality of the ImageModelEstimator framework base class is to    
 * generate the models used in classification applications. It requires input 
 * images and a training image to be provided by the user.
 * This object supports data handling of multiband images. The object
 * accepts the input image in vector format only, where each pixel is a 
 * vector and each element of the vector corresponds to an entry from
 * 1 particular band of a multiband dataset. A single band image is treated 
 * as a vector image with a single element for every vector. The classified
 * image is treated as a single band scalar image. 
 *
 * EstimateModels() is a pure virtual function making this an abstract class. 
 * The template parameter is the type of a membership function the 
 * ImageModelEstimator populates.
 *
 * A membership function represents a specific knowledge about
 * a class. In other words, it should tell us how "likely" is that a
 * measurement vector (pattern) belong to the class. 
 *
 * As the method name indicates, you can have more than one membership 
 * function. One for each classes. The order you put the membership 
 * calculator becomes the class label for the class that is represented
 * by the membership calculator. 
 *

 *
 * \ingroup ClassificationFilters 
 */
template <class TInputImage,
          class TMembershipFunction>
class ITK_EXPORT ImageModelEstimatorBase: public LightProcessObject
{
public:
  /** Standard class typedefs. */
  typedef ImageModelEstimatorBase   Self;
  typedef LightProcessObject Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageModelEstimatorBase,LightProcessObject);

  /** Set the number of classes. */
  itkSetMacro(NumberOfModels, unsigned int);

  /** Get the number of classes. */
  itkGetConstMacro(NumberOfModels, unsigned int);

  /** Type definitions for the membership function . */
  typedef typename TMembershipFunction::Pointer MembershipFunctionPointer ;

  typedef std::vector< MembershipFunctionPointer > 
  MembershipFunctionPointerVector;

  /** Type definitions for the training image. */
  typedef          TInputImage          InputImageType;
  typedef typename TInputImage::Pointer InputImagePointer;

  /** Type definitions for the training image. */
  //typedef typename TTrainingImage::Pointer TrainingImagePointer;

  /** Set the input image. */
  itkSetObjectMacro(InputImage,InputImageType);

  /** Get the input image. */
  itkGetObjectMacro(InputImage,InputImageType);

  /** Set the classified image. */
  void SetMembershipFunctions(MembershipFunctionPointerVector 
                              membershipFunctions)
  {
    m_MembershipFunctions = membershipFunctions;
  }
  
  /** Method to get mean */
  const MembershipFunctionPointerVector GetMembershipFunctions() const
  {
    return m_MembershipFunctions;
  }

  /** Method to number of membership functions */
  unsigned int GetNumberOfMembershipFunctions() 
  {
    return static_cast<unsigned int>( m_MembershipFunctions.size() );
  }

  /** Method to reset the membership fucntion mean */
  void DeleteAllMembershipFunctions() 
  {
    m_MembershipFunctions.resize(0);
  }

  /** Stores a MembershipCalculator of a class in its internal vector */
  unsigned int AddMembershipFunction(MembershipFunctionPointer function);

  /** Define a virtual function to perform model generation from the input data
   */
  void Update() ;

protected:
  ImageModelEstimatorBase();
  ~ImageModelEstimatorBase();
  void PrintSelf(std::ostream& os, Indent indent) const;

  virtual void GenerateData();

private:

  ImageModelEstimatorBase(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  unsigned int                    m_NumberOfModels;

  /** Container to hold the membership functions */
  MembershipFunctionPointerVector m_MembershipFunctions;

  /**Container for holding the training image */
  InputImagePointer               m_InputImage;

  /** The core virtual function to perform modelling of the input data */
  virtual void EstimateModels() = 0;
}; // class ImageModelEstimator


} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageModelEstimatorBase.txx"
#endif



#endif
















