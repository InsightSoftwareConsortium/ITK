/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFaceDetectionFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkFaceDetectionFilter_h
#define __itkFaceDetectionFilter_h

#include "cv.h"
#include "highgui.h"
#include "itkImageToImageFilter.h"
#include "list"

namespace itk
{
/** \class FaceDetectionFilter
 * \brief Detect the face in an image using OpenCV libs
 *
 * Face detection filter is used to detect faces with an openCV module
 * This filter only works with 2D  grayscale images. The output image has the 
 * same type as the input image.
 * 
 * In order to detect the faces, the filter has to be trained. 
 * Consequently, you always need to specify the trainer file.
 * (it shoud be a .xml file).
 * By default, the filter generate white rectangles around the faces it detects
 * You can also change the option with the SetGenerateROI and SetDrawRectangles
 * method.
 * If you only want the ROI, then the filter output image is the same as the input
 * If you want both option, then you would find lines from the drawn recatngles
 * in your ROI if some ROIs are overlaping.
 * If both options are disabled, then nothing happens.
 *
 * For the ROI, you may want also to use the filter RegionOfInterestImageFilter to 
 * get the faces from your image. 
 *
 * \sa PasteImageFilter
 * \sa RegionOfInterestImageFilter
 *
 * \ingroup Video-Filters-OpenCV
 */



template< class TInputImage >
class ITK_EXPORT FaceDetectionFilter : public ImageToImageFilter< TInputImage,TInputImage >
{
public:
  /** Standard class typedefs. */
  typedef FaceDetectionFilter                                   Self;
  typedef ImageToImageFilter< TInputImage, TInputImage >       Superclass;
  typedef SmartPointer< Self >                                  Pointer;
  typedef SmartPointer< const Self >                            ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(FaceDetectionFilter, ImageToImageFilter);

  /** Some typedefs for the input and output types. */
  typedef TInputImage  InputImageType;

  typedef typename InputImageType::PixelType ImagePixelType;

  /** Set/Get the file that exercise the filter. **/
  itkSetMacro(TrainerFileName,std::string);
  itkGetMacro(TrainerFileName,std::string);
 
  /** Set/Get the option of drawing rectangles around the region detected **/
  /** ATTENTION : If this option is enabled with the generateROI option, you**/  
  /** may see lines from the rectangles drawn in the region interests. It **/
  /** also means that some ROIs are overlaping **/
  itkSetMacro(DrawRectangles,bool);
  itkGetConstMacro(DrawRectangles,bool);

  /** Set/Get the RGB color when drawing rectangles (White by default) **/
  itkSetMacro(Color,int);
  itkGetConstMacro(Color,int);

  /** Set/Get the line thickness when drawing rectangles (1 by default) **/
  /** If negatif, the rectangles is filled with the line color **/
  itkSetMacro(LineThickness,int);
  itkGetConstMacro(LineThickness,int);

  /** Set/Get the option of generating a public list of ROI. This list can **/
  /** be access with getFacesAsROI() **/
  itkSetMacro(GenerateROI,bool);
  itkGetConstMacro(GenerateROI,bool);


  /** Get Method for accessing the Faces as ROI. Significant only if the **/
  /** the filter is runned with the option GenerateROI on **/
  std::list< itk::ImageRegion<2>* >* GetFacesAsROI ();

  /** Get the total of faces found **/
  itkGetConstMacro(FacesTotal,int);

protected: 
  
  void PrintSelf(std::ostream & os, Indent indent) const;

  void GenerateData();

  FaceDetectionFilter();
  ~FaceDetectionFilter(){};


private:
  FaceDetectionFilter(const Self &); //purposely not implemented
  void operator=(const Self &);  //purposely not implemented

  void DetectFaces( IplImage *image);
  void DrawRectangles (IplImage *image, CvSeq *faces);
  void GenerateROI( CvSeq *faces );

  std::list< itk::ImageRegion<2>* >* m_FacesAsROI;
  int m_LineThickness;
  int m_Color;
  bool m_DrawRectangles;
  bool m_GenerateROI;
  std::string m_TrainerFileName;
  int m_FacesTotal;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFaceDetectionFilter.txx"
#endif

#endif
