/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAffineGeometryFrame.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkAffineGeometryFrame_h
#define __itkAffineGeometryFrame_h

#include <itkObject.h>
#include <itkMacro.h>
#include <itkScalableAffineTransform.h>
#include <itkBoundingBox.h>

namespace itk 
{

 /** \class AffineGeometryFrame
  * \brief Describes the geometry of a data object  
  */
template <class TScalarType = double, unsigned int NDimensions = 3>
class AffineGeometryFrame : public itk::Object
{
public:
  typedef AffineGeometryFrame       Self;
  typedef itk::Object               Superclass;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  typedef ScalableAffineTransform<TScalarType, NDimensions>    TransformType;
  typedef BoundingBox<unsigned long, NDimensions, TScalarType> BoundingBoxType;
  typedef typename BoundingBoxType::BoundsArrayType            BoundsArrayType;
  typedef typename BoundingBoxType::Pointer                 BoundingBoxPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Method for creation through the object factory. */
  itkTypeMacro(AffineGeometryFrame, Object);

  /** Get the bounding box */
  itkGetConstObjectMacro(BoundingBox, BoundingBoxType);
  
  const BoundsArrayType GetBounds() const
    {
    assert(m_BoundingBox.IsNotNull());
    return m_BoundingBox->GetBounds();
    }

  /** Set the bounding box Only possible via the BoundsArray 
   * to make clear that a copy of the bounding-box is stored, 
   * not a reference to it.*/
  virtual void SetBounds(const BoundsArrayType& bounds);

  /** Get the extent of the bounding box */
  TScalarType GetExtent(unsigned int direction) const
    {
    assert(direction<NDimensions);
    assert(m_BoundingBox.IsNotNull());
    BoundsArrayType bounds = m_BoundingBox->GetBounds();
    return bounds[direction*2+1]-bounds[direction*2];
    }

  /** Set/Get the IndexToObjectTransform */
  itkGetConstObjectMacro(IndexToObjectTransform, TransformType);
  itkGetObjectMacro(IndexToObjectTransform, TransformType);
  itkSetObjectMacro(IndexToObjectTransform, TransformType);
  
  /** Set/Get the ObjectToNodeTransform */
  itkGetConstObjectMacro(ObjectToNodeTransform, TransformType);
  itkGetObjectMacro(ObjectToNodeTransform, TransformType);
  itkSetObjectMacro(ObjectToNodeTransform, TransformType);

  /** Set/Get the IndexToWorldTransform */
  itkGetConstObjectMacro(IndexToWorldTransform, TransformType);
  itkGetObjectMacro(IndexToWorldTransform, TransformType);
  itkSetObjectMacro(IndexToWorldTransform, TransformType);


  /** Get the IndexToNodeTransform 
   *  This Transform cannot be set, and is just computed internally */
  itkGetConstObjectMacro(IndexToNodeTransform, TransformType);
  
  /** Initialize the geometry frame */
  virtual void Initialize();

  /** Clone the geometry frame */
  virtual Pointer Clone() const;

protected:

  AffineGeometryFrame();
  virtual ~AffineGeometryFrame();

  /** used in clone to initialize the newly created geometry */
  virtual void InitializeGeometry(Self * newGeometry) const;
  void SetBoundsArray(const BoundsArrayType& bounds, 
                      BoundingBoxPointer& boundingBox);
  mutable BoundingBoxPointer m_BoundingBox;

  /** Transform from unit coordinates to object coordinates */
  typename TransformType::Pointer m_IndexToObjectTransform;
  typename TransformType::Pointer m_ObjectToNodeTransform;
  typename TransformType::Pointer m_IndexToNodeTransform;
  typename TransformType::Pointer m_IndexToWorldTransform;
  
private:
  AffineGeometryFrame(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

};

} // namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkAffineGeometryFrame.txx"
#endif


#endif /* __itkAffineGeometryFrame_h */
