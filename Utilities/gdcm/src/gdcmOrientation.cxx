/*=========================================================================
                                                                                
  Program:   gdcm
  Module:    gdcmOrientation.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$
                                                                                
  Copyright (c) CREATIS (Centre de Recherche et d'Applications en Traitement de
  l'Image). All rights reserved. See Doc/License.txt or
  http://www.creatis.insa-lyon.fr/Public/Gdcm/License.html for details.
                                                                                
     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.
                                                                                
=========================================================================*/

#include "gdcmOrientation.h"
#include "gdcmFile.h"
#include "gdcmDebug.h"
#include <math.h> // for sqrt

namespace gdcm 
{
//--------------------------------------------------------------------
//  THERALYS Algorithm to determine the most similar basic orientation
//
//  Transliterated from original Python code.
//  Kept as close as possible to the original code
//  in order to speed up any further modif of Python code :-(
//-----------------------------------------------------------------------

/**
 * \brief  THERALYS' Algorithm to determine the most similar basic orientation
 *           (Axial, Coronal, Sagital) of the image
 * \note Should be run on the first gdcm::File of a 'coherent' Serie
 * @return orientation code
 *   #   0 : Not Applicable (neither 0020,0037 Image Orientation Patient 
 *   #                       nor     0020,0032 Image Position           found)
 *   #   1 : Axial
 *   #  -1 : Axial invert
 *   #   2 : Coronal
 *   #  -2 : Coronal invert
 *   #   3 : Sagital
 *   #  -3 : Sagital invert
 *   #   4 : Heart Axial
 *   #  -4 : Heart Axial invert
 *   #   5 : Heart Coronal
 *   #  -5 : Heart Coronal invert
 *   #   6 : Heart Sagital
 *   #  -6 : Heart Sagital invert
 */

static const char  *OrientationTypeStrings[] = { 
  "Not Applicable",
  "Axial",
  "Coronal",
  "Sagital",
  "Heart Axial",
  "Heart Coronal",
  "Heart Sagital",
  "Axial invert",
  "Coronal invert",
  "Sagital invert",
  "Heart Axial invert",
  "Heart Coronal invert",
  "Heart Sagital invert",
  NULL
};

/// \brief returns human readable interpretation of the most 
///        similar basic orientation (Axial, Coronal, Sagital, ...) of the image
const char* Orientation::GetOrientationTypeString(OrientationType const o)
{
  int k = (int)o;
  if (k < 0) 
       k = -k + 6;

  return OrientationTypeStrings[k];
}

/// \brief returns of the most similar basic orientation
///        (Axial, Coronal, Sagital, ...) of the image
OrientationType Orientation::GetOrientationType( File *f )
{
   float iop[6];
   bool succ = f->GetImageOrientationPatient( iop );
   if ( !succ )
   {
      gdcmErrorMacro( "No Image Orientation (0020,0037)/(0020,0032) found in the file, cannot proceed." )
      return NotApplicable;
   }
   vector3D ori1;
   vector3D ori2;

   ori1.x = iop[0]; ori1.y = iop[1]; ori1.z = iop[2]; 
   ori2.x = iop[3]; ori2.y = iop[4]; ori2.z = iop[5];

   // two perpendicular vectors describe one plane
   double dicPlane[6][2][3] =
   { {  { 1,   0,    0   },{ 0,      1,     0     }  }, // Axial
     {  { 1,   0,    0   },{ 0,      0,    -1     }  }, // Coronal
     {  { 0,   1,    0   },{ 0,      0,    -1     }  }, // Sagittal
     {  { 0.8, 0.5,  0.0 },{-0.1,    0.1 , -0.95  }  }, // Axial - HEART
     {  { 0.8, 0.5,  0.0 },{-0.6674, 0.687, 0.1794}  }, // Coronal - HEART
     {  {-0.1, 0.1, -0.95},{-0.6674, 0.687, 0.1794}  }  // Sagittal - HEART
   };

   vector3D refA;
   vector3D refB;
   int i = 0;
   Res res;   // [ <result> , <memory of the last succes calcule> ]
   res.first = 0;
   res.second = 99999;

   for (int numDicPlane=0; numDicPlane<6; numDicPlane++)
   {
       ++i;
       // refA=plane[0]
       refA.x = dicPlane[numDicPlane][0][0]; 
       refA.y = dicPlane[numDicPlane][0][1]; 
       refA.z = dicPlane[numDicPlane][0][2];
       // refB=plane[1]
       refB.x = dicPlane[numDicPlane][1][0]; 
       refB.y = dicPlane[numDicPlane][1][1]; 
       refB.z = dicPlane[numDicPlane][1][2];
       res=VerfCriterion(  i, CalculLikelyhood2Vec(refA,refB,ori1,ori2), res );
       res=VerfCriterion( -i, CalculLikelyhood2Vec(refB,refA,ori1,ori2), res );
   }
   // res thought looks like is a float value, but is indeed an int
   // try casting it to int first then enum value to please VS7:
   int int_res = (int)res.first;
   gdcmAssertMacro( int_res <= 6 && int_res >= -6);
   return (OrientationType)int_res;
}

Res 
Orientation::VerfCriterion(int typeCriterion, double criterionNew, Res const &in)
{
   Res res;
   double type = in.first;
   double criterion = in.second;
   if (/*criterionNew < 0.1 && */criterionNew < criterion)
   {
      type      = typeCriterion;
      criterion = criterionNew;
   }
   res.first  = type;
   res.second = criterion;
   return res;
} 

inline double square_dist(vector3D const &v1, vector3D const &v2)
{
  double res;
  res = (v1.x - v2.x)*(v1.x - v2.x) +
        (v1.y - v2.y)*(v1.y - v2.y) +
        (v1.z - v2.z)*(v1.z - v2.z);
  return res;
}

//------------------------- Purpose : -----------------------------------
//- This function determines the orientation similarity of two planes.
//  Each plane is described by two vectors.
//------------------------- Parameters : --------------------------------
//- <refA>  : - type : vector 3D (double)
//- <refB>  : - type : vector 3D (double)
//            - Description of the first plane
//- <ori1>  : - type : vector 3D (double)
//- <ori2>  : - type : vector 3D (double)
//            - Description of the second plane
//------------------------- Return : ------------------------------------
// double :   0 if the planes are perpendicular. While the difference of
//            the orientation between the planes are big more enlarge is
//            the criterion.
//------------------------- Other : -------------------------------------
// The calculus is based with vectors normalice
double
Orientation::CalculLikelyhood2Vec(vector3D const &refA, vector3D const &refB, 
                                  vector3D const &ori1, vector3D const &ori2 )
{

   vector3D ori3 = ProductVectorial(ori1,ori2);
   vector3D refC = ProductVectorial(refA,refB);
   double res = square_dist(refC, ori3);

   return sqrt(res);
}

//------------------------- Purpose : -----------------------------------
//- Calculus of the poduct vectorial between two vectors 3D
//------------------------- Parameters : --------------------------------
//- <vec1>  : - type : vector 3D (double)
//- <vec2>  : - type : vector 3D (double)
//------------------------- Return : ------------------------------------
// (vec) :    - Vector 3D
//------------------------- Other : -------------------------------------
vector3D
Orientation::ProductVectorial(vector3D const &vec1, vector3D const &vec2)
{
   vector3D vec3;
   vec3.x =    vec1.y*vec2.z - vec1.z*vec2.y;
   vec3.y = -( vec1.x*vec2.z - vec1.z*vec2.x);
   vec3.z =    vec1.x*vec2.y - vec1.y*vec2.x;

   return vec3;
}



// ---------------------------------------------------------------------------
// Here is the original Python code, kindly supplied by THERALYS
//
// C++ code doesn't give good results
// --> FIXME

/*

def TypeOrientation(self,file0):
"""
# ------------------------- Purpose : -----------------------------------
# - This function compare the orientation of the given image and the
#   basics orientations (Axial, Cornal, Sagital)
# ------------------------- Parameters : --------------------------------
# - <file0> : - type : string
#             - The name of the first image file of the serie
# ------------------------- Return : ------------------------------------
#   1 :   Axial
#  -1 :   Axial invert
#   2 :   Coronal
#  -2 :   Coronal invert
#   3 :   Sagital
#  -3 :   Sagital invert
#   4 :   Heart Axial
#  -4 :   Heart Axial invert
#   5 :   Heart Coronal
#  -5 :   Heart Coronal invert
#   6 :   Heart Sagital
#  -6 :   Heart Sagital invert
#
   # ------------------------- Other : -------------------------------------
# This method finds the most similar basic orientation.
"""
try:
   toRead = gdcm.File(file0)
   ValDict = GetValuesDict(toRead)
   try:
      imageOrientation=ValDict["Image Orientation (Patient)"]
   except KeyError:
      imageOrientation=ValDict["Image Orientation"]

   ori1=[float(split(imageOrientation,"\\")[0]),\
      float(split(imageOrientation,"\\")[1]),\
      float(split(imageOrientation,"\\")[2])]
   ori2=[float(split(imageOrientation,"\\")[3]),\
      float(split(imageOrientation,"\\")[4]),\
      float(split(imageOrientation,"\\")[5])]

## two vectors perpendicular describe one plane
   dicPlane=[ [  [1,0,0],[0,1,0]   ],  ## Axial
            [  [1,0,0],[0,0,-1]  ],  ## Coronal
            [  [0,1,0],[0,0,-1]  ],  ## Sagittal
            [  [ 0.8 , 0.5 ,  0.0 ],[-0.1 , 0.1 , -0.95]        ],## Axial - HEART
            [  [ 0.8 , 0.5 ,  0.0 ],[-0.6674 , 0.687 , 0.1794]  ],## Coronal - HEART
            [  [-0.1 , 0.1 , -0.95],[-0.6674 , 0.687 , 0.1794]  ] ] ## Sagittal - HEART

   i=0
   res=[0,99999]  ## [ <result> , <memory of the last succes calcule> ]
   for plane in dicPlane:
      i=i+1
      refA=plane[0]
      refB=plane[1]
      res=self.VerfCriterion(  i , self.CalculLikelyhood2Vec(refA,refB,ori1,ori2) , res )
      res=self.VerfCriterion( -i , self.CalculLikelyhood2Vec(refB,refA,ori1,ori2) , res )
   return res[0]

   except KeyError:
   return 0


   def VerfCriterion(self,typeCriterion,criterionNew,res):
      type = res[0]
      criterion = res[1]
#     if criterionNew<0.1 and criterionNew<criterion:
      if criterionNew<criterion:
         criterion=criterionNew
         type=typeCriterion
      return [ type , criterion ]


   def CalculLikelyhood2Vec(self,refA,refB,ori1,ori2):
"""
   # ------------------------- Purpose : -----------------------------------
   # - This function determine the orientation similarity of two planes.
   #   Each plane is described by two vector.
   # ------------------------- Parameters : --------------------------------
   # - <refA>  : - type : vector 3D (float)
   # - <refB>  : - type : vector 3D (float)
   #             - Description of the first plane
   # - <ori1>  : - type : vector 3D (float)
   # - <ori2>  : - type : vector 3D (float)
   #             - Description of the second plane
   # ------------------------- Return : ------------------------------------
   #  float :   0 if the planes are perpendicular. 
   # While the difference of the orientation between the planes 
   # are big more enlarge is
   # the criterion.
   # ------------------------- Other : -------------------------------------
   #  The calculus is based with vectors normalice
   """

      ori3=self.ProductVectorial(ori1,ori2)
      refC=self.ProductVectorial(refA,refB)
      res=math.pow(refC[0]-ori3[0],2) + math.pow(refC[1]-ori3[1],2) + math.pow(refC[2]-ori3[2],2)
      return math.sqrt(res)

   def ProductVectorial(self,vec1,vec2):
      """
      # ------------------------- Purpose : -----------------------------------
      # - Calculus of the poduct vectorial between two vectors 3D
      # ------------------------- Parameters : --------------------------------
      # - <vec1>  : - type : vector 3D (float)
      # - <vec2>  : - type : vector 3D (float)
      # ------------------------- Return : ------------------------------------
      #  (vec) :    - Vector 3D
      # ------------------------- Other : -------------------------------------
      """
      vec3=[0,0,0]
      vec3[0]=vec1[1]*vec2[2] - vec1[2]*vec2[1]
      vec3[1]=-( vec1[0]*vec2[2] - vec1[2]*vec2[0])
      vec3[2]=vec1[0]*vec2[1] - vec1[1]*vec2[0]
      return vec3

   def GetValuesDict(image):
      """
      Returns a dictionnary containing values associated with Field Names
      dict["Dicom Field Name"]="Dicom field value"
      """
      val=image.GetFirstEntry()
      dic={}
      while(val):
         if isinstance(val,gdcm.ValEntryPtr):
            dic[val.GetName()]=val.GetValue()
         val=image.GetNextEntry()
      return dic

*/


// ------------------------------------------------------------------------
/*
2.2.2 Orientation of DICOM images


http://www.dclunie.com/medical-image-faq/html/part2.html#DICOMOrientation
says :

A question that is frequently asked in comp.protocols.dicom is how to determine
 which side of an image is which (e.g. left, right) and so on. 
 The short answer is that for projection radiographs this is specified
 explicitly using the "Patient Orientation" attribute, and for cross-sectional
 images it needs to be derived from the "Image Orientation (Patient)" direction
 cosines. In the standard these are explained as follows:

    * "C.7.6.1.1.1 Patient Orientation. 
                The Patient Orientation (0020,0020) relative to the image
                plane shall be specified by two values that designate the 
                anatomical direction of the positive row axis (left to right)
                and the positive column axis (top to bottom). 
                The first entry is the direction of the rows, given by the 
                direction of the last pixel in the first row from the first 
                pixel in that row. 
                The second entry is the direction of the columns, given by 
                the direction of the last pixel in the first column from the
                first pixel in that column. 
                Anatomical direction shall be designated by the capital 
                letters: A (anterior), P (posterior), R (right),L (left), 
                H (head), F (foot). 
                Each value of the orientation attribute shall contain at 
                least one of these characters. 
                If refinements in the orientation descriptions are to be 
                specified, then they shall be designated by one or two 
                additional letters in each value. 
                Within each value, the letters shall be ordered with the 
                principal orientation designated in the first character."
 
    * "C.7.6.2.1.1 Image Position And Image Orientation. 
                The "Image Position (Patient)" (0020,0032) specifies the x, y, 
                and z coordinates of the upper left hand corner of the image; 
                it is the center of the first voxel transmitted. 
                The "Image Orientation (Patient)" (0020,0037) specifies the 
                direction cosines of the first row and the first column 
                with respect to the patient. 
                These Attributes shall be provided as a pair. 
                Row value for the x, y, and z axes respectively followed by 
                the Column value for the x, y, and z axes respectively. 
                The direction of the axes is defined fully by the patient's 
                orientation. 
                The x-axis is increasing to the left hand side of the patient.
                The y-axis is increasing to the posterior side of the patient.
                The z-axis is increasing toward the head of the patient. 
                The patient based coordinate system is a right handed system,
                i.e. the vector cross product of a unit vector along the 
                positive x-axis and a unit vector along the positive y-axis
                is equal to a unit vector along the positive z-axis." 

Some simple code to take one of the direction cosines (vectors) from the 
Image Orientation (Patient) attribute and generate strings equivalent to one 
of the values of Patient Orientation looks like this (noting that if the vector
is not aligned exactly with one of the major axes, the resulting string will 
have multiple letters in as described under "refinements" in C.7.6.1.1.1): 

*/

/**
 * \brief Computes the Patient Orientation relative to the image plane
 *          from the 'Image Orientation (Patient)'
 *          - or from 0020 0035Image Orientation (RET) -
 *          - The first entry is the direction of the rows, given by the 
 *          direction of the last pixel in the first row from the first 
 *          pixel in that row. 
 *          - The second entry is the direction of the columns, given by 
 *          the direction of the last pixel in the first column from the
 *          first pixel in that column. 
 *          Anatomical direction is designated by the capital 
 *          letters: A (anterior), P (posterior), R (right),L (left), 
 *          H (head), F (foot).
 *          - Refinements in the orientation descriptions are designated 
 *          by one or two additional letters in each value.
 *          Use it when "Patient Orientation" (0020,0020) is not found 
 * @return orientation string as "rowsOrientation\columnsOrientation"
 */
std::string Orientation::GetOrientation ( File *f )
{
   float iop[6];
   if ( !f->GetImageOrientationPatient( iop ) )
   return GDCM_UNFOUND;

   std::string orientation;
   orientation = GetSingleOrientation ( iop ) 
               + "\\" 
               + GetSingleOrientation ( iop + 3 );
   return orientation;
}


std::string Orientation::GetSingleOrientation ( float *iop)
{
   std::string orientation;

   char orientationX = iop[0] < 0 ? 'R' : 'L';
   char orientationY = iop[1] < 0 ? 'A' : 'P';
   char orientationZ = iop[2] < 0 ? 'F' : 'H';

   double absX = iop[0];
   if (absX < 0) absX = -absX;
      double absY = iop[1];
   if (absY < 0) absY = -absY;
      double absZ = iop[2];
   if (absZ < 0) absZ = -absZ;

   for (int i=0; i<3; ++i) 
   {
      if (absX>.0001 && absX>absY && absX>absZ) 
      {
         orientation = orientation + orientationX;
         absX=0;
       }
       else if (absY>.0001 && absY>absX && absY>absZ) 
       {
          orientation = orientation + orientationY;
          absY=0;
       }
       else if (absZ>.0001 && absZ>absX && absZ>absY) 
       {
           orientation = orientation + orientationZ;
           absZ=0;
       }
       else 
          break;
     }
   return orientation;
} 


/*-------------------------------------------------------------------

Some more stuff, from XMedcon

---> Personal remark from JPRx :
--> patient_position (0x0018,0x5100) can be "HFS ", "FFS ", "HFP ", "FFP " 
--> or, not so common, 
// HFDR = Head First-Decubitus Right
// HFDL = Head First-Decubitus Left
// FFDR = Feet First-Decubitus Right
// FFDL = Feet First-Decubitus Left
--> the cosines may have any value -1.< <+1., for MR images !

enum patient_slice_orientation_type
  {
    patient_slice_orientation_unknown = 0,
    supine_headfirst_transaxial,
    supine_headfirst_sagittal,
    supine_headfirst_coronal,
    supine_feetfirst_transaxial,
    supine_feetfirst_sagittal,
    supine_feetfirst_coronal,
    prone_headfirst_transaxial,
    prone_headfirst_sagittal,
    prone_headfirst_coronal,
    prone_feetfirst_transaxial,
    prone_feetfirst_sagittal,
    prone_feetfirst_coronal
  };

void GetImageOrientationPatient(gdcm::File &h,F32 image_orientation_patient[6])
{
  h.GetImageOrientationPatient(image_orientation_patient);
}

#if 0
//
// this is all completely cribbed from the xmedcon library, since
// we're trying to do what it does, mostly.
patient_slice_orientation_type
GetPatSliceOrient(gdcm::File &h)
{
  F32 image_orientation_patient[6];

  // protected, do it the hard way
  //  h.GetImageOrientationPatient(image_orientation_patient);
  GetImageOrientationPatient(h,image_orientation_patient);

  enum { headfirst, feetfirst } patient_orientation;
  enum { supine, prone } patient_rotation;
  enum { transaxial, sagittal, coronal } slice_orientation;

  std::string patient_position = h.GetEntryByNumber(0x0018,0x5100);
  if(patient_position == "gdcm::Unfound")
    {
    patient_position = "HF";
    }
  if(patient_position.find("HF") != std::string::npos)
    {
    patient_orientation = headfirst;
    }
  else if(patient_position.find("FF") != std::string::npos)
    {
    patient_orientation = feetfirst;
    }

  if(patient_position.find("S") != std::string::npos)
    {
    patient_rotation = supine;
    }
  else if(patient_position.find("P") != std::string::npos)
    {
    patient_rotation = prone;
    }

  if((image_orientation_patient[0] == 1 || image_orientation_patient[0] == -1) &&
     (image_orientation_patient[4] == +1 || image_orientation_patient[4] == -1))
    {
    slice_orientation = transaxial;
    }
  else if((image_orientation_patient[1] == 1 || image_orientation_patient[1] == -1) &&
          (image_orientation_patient[5] == +1 || image_orientation_patient[5] == -1))
    {
    slice_orientation = sagittal;
    }
  else if((image_orientation_patient[0] == 1 || image_orientation_patient[0] == -1) &&
          (image_orientation_patient[5] == +1 || image_orientation_patient[5] == -1))
    {
    slice_orientation = coronal;
    }
  //
  // combine results
  patient_slice_orientation_type patient_slice_orientation = 
    patient_slice_orientation_unknown;
  switch (patient_rotation) 
    {
    case supine:
      switch (patient_orientation) 
        {
        case headfirst:
          switch (slice_orientation) 
            {
            case transaxial:
              patient_slice_orientation = supine_headfirst_transaxial;
              break;
            case sagittal:
              patient_slice_orientation = supine_headfirst_sagittal;
              break;
            case coronal:
              patient_slice_orientation = supine_headfirst_coronal;
              break;
            }
          break;
        case feetfirst:
          switch (slice_orientation) 
            {
            case transaxial:
              patient_slice_orientation = supine_feetfirst_transaxial;
              break;
            case sagittal:
              patient_slice_orientation = supine_feetfirst_sagittal;
              break;
            case coronal:
              patient_slice_orientation = supine_feetfirst_coronal;
              break;
            }
          break;
        }
      break;
    case prone:
      switch (patient_orientation) 
        {
        case headfirst:
          switch (slice_orientation) 
            {
            case transaxial:
              patient_slice_orientation = prone_headfirst_transaxial;
              break;
            case sagittal:
              patient_slice_orientation = prone_headfirst_sagittal;
              break;
            case coronal:
              patient_slice_orientation = prone_headfirst_coronal;
              break;
            }
          break;
        case feetfirst:
          switch (slice_orientation) 
            {
            case transaxial:
              patient_slice_orientation = prone_feetfirst_transaxial;
              break;
            case sagittal:
              patient_slice_orientation = prone_feetfirst_sagittal;
              break;
            case coronal:
              patient_slice_orientation = prone_feetfirst_coronal;
              break;
            }
          break;
        }
      break;
    }
  if(patient_slice_orientation != patient_slice_orientation_unknown)
    return patient_slice_orientation;
  //
  // this is what xmedcon does
  if ((image_orientation_patient[0] == +1)   &&
      (image_orientation_patient[4] == +1))   
    patient_slice_orientation = supine_headfirst_transaxial;
  else if ((image_orientation_patient[0] == -1)   &&
           (image_orientation_patient[4] == +1))   
    patient_slice_orientation = supine_feetfirst_transaxial;
  else if ((image_orientation_patient[0] == -1)   &&
           (image_orientation_patient[4] == -1))   
    patient_slice_orientation = prone_headfirst_transaxial;
  else if ((image_orientation_patient[0] == +1)   &&
           (image_orientation_patient[4] == -1))   
    patient_slice_orientation = prone_feetfirst_transaxial;

  else if ((image_orientation_patient[1] == +1)   &&
           (image_orientation_patient[5] == -1))   
    patient_slice_orientation = supine_headfirst_sagittal;
  else if ((image_orientation_patient[1] == +1)   &&
           (image_orientation_patient[5] == +1))   
    patient_slice_orientation = supine_feetfirst_sagittal;
  else if ((image_orientation_patient[1] == -1)   &&
           (image_orientation_patient[5] == -1))   
    patient_slice_orientation = prone_headfirst_sagittal;
  else if ((image_orientation_patient[1] == -1)   &&
           (image_orientation_patient[5] == +1))   
    patient_slice_orientation = prone_feetfirst_sagittal;

  else if ((image_orientation_patient[0] == +1)   &&
           (image_orientation_patient[5] == -1))   
    patient_slice_orientation = supine_headfirst_coronal;
  else if ((image_orientation_patient[0] == -1)   &&
           (image_orientation_patient[5] == +1))   
    patient_slice_orientation = supine_feetfirst_coronal;
  else if ((image_orientation_patient[0] == -1)   &&
           (image_orientation_patient[5] == -1))   
    patient_slice_orientation = prone_headfirst_coronal;
  else if ((image_orientation_patient[0] == +1)   &&
           (image_orientation_patient[5] == +1))   
    patient_slice_orientation = prone_feetfirst_coronal;
  return patient_slice_orientation;
}
#else

-------------------------------------------------------------------------*/

} // end namespace gdcm
