#include <nifti1_io.h>


int main (int argc, char *argv[])
{
    nifti_image my_image;
    my_image.ndim=3;
    my_image.nx=7;
    my_image.ny=11;
    my_image.nz=13;
    my_image.datatype=DT_UINT16 ;
    my_image.dx=1.0F;                    /*!< grid spacings      */
    my_image.dy=1.0F;                    /*!< grid spacings      */
    my_image.dz=1.0F;                    /*!< grid spacings      */
    my_image.xyz_units=NIFTI_UNITS_MICRON ;              /*!< dx,dy,dz units: NIFTI_UNITS_* code  */
    my_image.time_units=NIFTI_UNITS_USEC ;              /*!< dt       units: NIFTI_UNITS_* code  */
    my_image.nifti_type=1 ;
    my_image.intent_code=NIFTI_INTENT_ESTIMATE ;           /*!< statistic type (or something)       */
    strncpy(my_image.intent_name,"PHANTOM",16) ;       /*!< optional description of intent data */
    strncpy(my_image.descrip,"This is a very long dialog here to use up more than 80 characters of space to test to see if the code is robust enough to deal appropriatly with very long and obnoxious lines.",80);
    my_image.fname="xxx.nii" ;                 /*!< header filename (.hdr or .nii)         */
    my_image.iname="xxx.nii" ;                 /*!< image filename  (.img or .nii)         */
//    my_image.byteorder=LSB_FIRST;             /*!< byte order on disk (MSB_ or LSB_FIRST) */
    my_image.data=calloc(my_image.nx*my_image.ny*my_image.nz,sizeof(unsigned short)) ;                  /*!< pointer to data: nbyper*nvox bytes     */
    nifti_image_write   ( &my_image ) ;
    return 0;
}
