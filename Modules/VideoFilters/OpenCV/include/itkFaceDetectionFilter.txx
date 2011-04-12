/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFaceDetectionFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkFaceDetectionFilter_txx
#define __itkFaceDetectionFilter_txx

#include "itkFaceDetectionFilter.h"
#include "itkPasteImageFilter.h"
#include "itkExceptionObject.h"

namespace itk
{

template< typename TInputImage >
FaceDetectionFilter< TInputImage >
::FaceDetectionFilter()
{
  this->m_TrainerFileName = "";
  this->m_Color = 255;
  this->m_FacesTotal = 0;
  this->m_LineThickness = 2;
  this->m_DrawRectangles = true;
  this->m_GenerateROI = false;
  this->m_FacesAsROI = new std::list< itk::ImageRegion<2>* >;
}

/*
 * PrintSelf
 */
template< typename TInputImage >
void
FaceDetectionFilter< TInputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Trainer File name :" <<this->m_TrainerFileName<< std::endl;
  os << indent << "Color :" <<this->m_Color<< std::endl;
  os << indent << "Faces Total :" <<this->m_FacesTotal<< std::endl;
  os << indent << "Line Thickness :" <<this->m_LineThickness<< std::endl;
  os << indent << "Draw rectangles mode :" <<this->m_DrawRectangles<<std::endl;
  os << indent << "Generate ROI mode :" <<this->m_GenerateROI<< std::endl;
  os << indent << "Number of element in the list FacesAsROI :" 
    <<this->m_FacesAsROI->size()<< std::endl;
  
} // end PrintSelf

template< typename TInputImage >
std::list< itk::ImageRegion<2>* >*  FaceDetectionFilter < TInputImage >
::GetFacesAsROI()
{
  return this->m_FacesAsROI;
}

template< typename TInputImage >
void FaceDetectionFilter< TInputImage >
::GenerateData()
{
  // Get the input and output pointers
  typename TInputImage::Pointer inputPtr = const_cast<InputImageType*>(this->GetInput());
  //To make sure the data is here :
  inputPtr->Update();
  
  //compute the pixel depth
  int depth = sizeof(ImagePixelType);
  
  //Get the image in region
  itk::ImageRegion<2> region = inputPtr->GetLargestPossibleRegion();
  itk::Size<2> itkSize = region.GetSize();
  
  CvSize CVSize;
  CVSize.width = itkSize[0];
  CVSize.height = itkSize[1];

  //Create the header
  IplImage* CVImage = cvCreateImageHeader(CVSize,depth,1);
  // and Retrieve the data so we don't reload the data
  //We instead use the same buffer ( same buffer -> same image )
  cvSetData(CVImage,const_cast<ImagePixelType*>(inputPtr->GetBufferPointer()),CVSize.width);

  //apply the algorithm
  this->DetectFaces(CVImage);
  
  typename TInputImage::Pointer outputPtr = this->GetOutput();

  //Paste the input onto the output
  typename PasteImageFilter<InputImageType>::Pointer paste = PasteImageFilter<InputImageType>::New();  
  paste->SetDestinationImage(outputPtr);
  paste->SetSourceImage(inputPtr);
  paste->SetDestinationIndex(region.GetIndex());
  paste->SetSourceRegion(region);
  paste->Update(); 
  this->GraftOutput(paste->GetOutput());

}

template< typename TInputImage >
void FaceDetectionFilter< TInputImage >
::DetectFaces(IplImage *img)
{
  assert( img ); 

  CvMemStorage* storage = 0;
  CvHaarClassifierCascade* cascade = 0;
  
  // Load the HaarClassifierCascade
  cascade= (CvHaarClassifierCascade*)cvLoad( this->m_TrainerFileName.c_str() ); 
  if ( cascade == NULL )
    {
    itk::ExceptionObject e;
    std::string description = "Error while loading the Haar Classifier."
      "Make sure the file : ";
    description += this->m_TrainerFileName;
    description += " exists.";
    e.SetDescription(description.c_str());
    e.SetLocation("itkFaceDetectionFilter, line 118") ; 
    }
  
  // Allocate the memory storage
  storage = cvCreateMemStorage(0);
  assert( storage );

  // Find whether the cascade is loaded, to find the faces. If yes, then:
  if( cascade )
    {
    // There can be more than one face in an image. So create a growable sequence of faces.
    // Detect the objects and store them in the sequence
    CvSeq* faces = cvHaarDetectObjects( img, cascade, storage,
                                        1.1, 2, CV_HAAR_DO_CANNY_PRUNING,
                                        cvSize(40, 40) );
    //Set the total number of detected face.
    this->m_FacesTotal = faces->total; 
    //Execute the option if needed
    if ( this->m_GenerateROI )
      {
      this->GenerateROI(faces);
      }
    if ( this->m_DrawRectangles )
      {
      this->DrawRectangles(img,faces);
      }
    }
  else
    {
    itk::ExceptionObject exception;
    exception.SetDescription("The trainer in the face detection filter is empty");
    exception.SetLocation("FaceDetectionFilter");
    throw exception;
    }

  cvReleaseHaarClassifierCascade( &cascade );
  cvReleaseMemStorage( &storage );
}

template< typename TInputImage >
void FaceDetectionFilter< TInputImage >
::DrawRectangles( IplImage *img, CvSeq *faces)
{
  assert(faces);

  // Create two points to represent the face locations
  CvPoint pt1, pt2;
  int i;

  // Loop the number of faces found.
  for( i = 0; i < this->m_FacesTotal; i++ )
    {
    // Create a new rectangle for drawing the face
    CvRect* r = (CvRect*)cvGetSeqElem( faces, i );
    assert(r);
    // Find the dimensions of the face,and scale it if necessary
    pt1.x = r->x;
    pt2.x = (r->x+r->width);
    pt1.y = r->y;
    pt2.y = (r->y+r->height);
    // Draw the rectangle in the input image
    cvRectangle( img, pt1, pt2,
      CV_RGB(this->m_Color,this->m_Color,this->m_Color),
      this->m_LineThickness, 8, 0 );
    }
}

template< typename TInputImage >
void FaceDetectionFilter< TInputImage >
::GenerateROI(CvSeq *faces)
{
  assert(faces);
  int i;
  
  // Loop the number of faces found.
  for( i = 0; i < this->m_FacesTotal; i++ )
    {
    //Getting the rectangle
    CvRect* r = (CvRect*)cvGetSeqElem( faces, i );
    assert(r);
    //create a new region
    itk::ImageRegion<2> *region = new itk::ImageRegion<2>();
    //Set teh region
    itk::Index<2> index;
    index[0] = r->x;
    index[1] = r->y;
    region->SetIndex(index);
    itk::Size<2> size;
    size[0] = r->width;
    size[1] = r->height;
    region->SetSize(size);
    //and eventually we store it in the list.
    this->m_FacesAsROI->push_front(region);
    }
}

} // end namespace itk

#endif
