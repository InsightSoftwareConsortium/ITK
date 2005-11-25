/*=========================================================================
                                                                                
  Program:   gdcm
  Module:    gdcmOrientation.h
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

#ifndef GDCMORIENTATION_H
#define GDCMORIENTATION_H

#include "gdcmBase.h"
#include <map>

namespace gdcm 
{
typedef struct
{
   double x;
   double y;
   double z;
} vector3D;

typedef std::pair<double, double> Res;
class File;

//-----------------------------------------------------------------------------
/**
 * \brief Orientation class for dealing with DICOM image orientation
 * A gentle reminder for non-medical user:
 * PatientPosition (0x0010,0x5100) tells us the way the patient was introduced in the imager
 *  - HFS : Head First Supine
 *  - FFS : Feet First Supine
 *  - HFP : Head First Prone
 *  - FFP : Feet First Prone
 * Note: HFP and FFP are not very common values, since the position must be pretty unconfortable for the Patient -the patient is lying on his belly; but, if he has handcuffs there is no other way ...-
 *
 * ImageOrientationPatient (0x0020,0x0037) gives 6 cosines (2 for each plane)
 * Patient Orientation (as found in the optional 0x0020,0x0020, or computed by
 * std::string Orientation::GetOrientation ( File *f ), tells us about the direction of X and Y axes.
 * 
 * The values can be
 *  - A/P anterior/posterior
 *  - L/R left/right
 *  - H/F head/feet
 * One can see it as "values within a 'Patient referential".
 *
 * Example #1:
 * Imagine the patient, in "HFS" position.
 * Full body sagital images are requested.
 * All the cosines will be -1, 0, or +1;
 * "Patient Orientation" (deduced) will be "A/F".
 * Positive X axis is oriented 'towards patient's nose
 * Positive Y axis  is oriented 'towards patient's feet
 *
 * Example #2:
 * Imagine now that patient has a stiffneck and his head is *turned* 30 degrees towards the left.
 * Head sagital images are requested.
 * One of the cosines will be almost 0.5
 * Deduced "Patient Orientation" will be "AL\F"
 * (main X axis orientation is towards patient's nose, and a little bit towards the left)
 * but the image looks *perfectly* sagital (for the head, not for the patient) !
 *
 * Imagine the patient's stiffneck causes head to be *bended* 30 degrees towards the left AND *turned* left.
 * Sagital images are requested...
 * You'll probabely have 3 letters for X axis and  Y axis, and the image remains *perfectly* sagital !
 * The values are given within the 'Patient referential', *not* within the 'Organ referential' ...
 */
typedef enum {
   NotApplicable = 0,
   Axial = 1,
   AxialInvert = -1,
   Coronal = 2,
   CoronalInvert = -2,
   Sagital = 3,
   SagitalInvert = -3,
   HeartAxial = 4,
   HeartAxialInvert = -4,
   HeartCoronal = 5,
   HeartCoronalInvert = -5,
   HeartSagital = 6,
   HeartSagitalInvert = -6
} OrientationType;

class GDCM_EXPORT Orientation : public Base
{
public:
  Orientation() {}
  ~Orientation() {}

  OrientationType GetOrientationType( File *file );
  std::string GetOrientation ( File *file );  
  
  static const char* GetOrientationTypeString(OrientationType const o);

private:
   Res VerfCriterion(int typeCriterion, double criterionNew, Res const &res);
   double CalculLikelyhood2Vec(vector3D const &refA, vector3D const &refB, 
                               vector3D const &ori1, vector3D const &ori2);
   vector3D ProductVectorial(vector3D const &vec1, vector3D const &vec2);
   std::string GetSingleOrientation ( float *iop);
};
} // end namespace gdcm
//-----------------------------------------------------------------------------
#endif
