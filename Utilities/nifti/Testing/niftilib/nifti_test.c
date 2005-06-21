/* This file generates an image, writes it to disk, reads it from disk, and compares the results to make sure that all the data survived. */

#include <nifti1_io.h>
static nifti_image * generate_reference_image()
{
    char write_image_filename[]="ATestReferenceImageForReadingAndWriting.nii";
    nifti_image * reference_image;
    nifti_1_header reference_header;
    memset(&reference_header,0,sizeof(reference_header));
    reference_header.sizeof_hdr=sizeof(reference_header);
    reference_header.regular='r';
    reference_header.extents=16384;

    reference_header.dim[0]=5;
    reference_header.dim[1]=23;
    reference_header.dim[2]=17;
    reference_header.dim[3]=11;
    reference_header.dim[4]=7;
    reference_header.dim[5]=3;
    reference_header.dim[6]=16000;
    reference_header.dim[7]=16000;
    reference_header.intent_p1=10101010.101F;
    reference_header.intent_p2=987654321.0F;
    reference_header.intent_p3=-1234.0F;
    reference_header.intent_code=NIFTI_INTENT_ESTIMATE;
    reference_header.datatype=DT_INT32;
    reference_header.pixdim[0]=-1.0F; /* this is really qfac */
    reference_header.pixdim[1]=0.25F;
    reference_header.pixdim[2]=0.5F;
    reference_header.pixdim[3]=1.0F;
    reference_header.pixdim[4]=2.0F;
    reference_header.pixdim[5]=4.0F;
    reference_header.pixdim[6]=-2.0e10F;
    reference_header.pixdim[7]=-2.0e10F;
    reference_header.vox_offset=0;
    reference_header.scl_slope=0.25;
    reference_header.scl_inter=128;
    reference_header.qform_code=NIFTI_XFORM_SCANNER_ANAT;
    reference_header.quatern_b=-0.5F;
    reference_header.quatern_c= 0.5F;
    reference_header.quatern_d=-0.5F;
    reference_header.qoffset_x=reference_header.dim[1]/2.0F;
    reference_header.qoffset_y=reference_header.dim[2]/2.0F;
    reference_header.qoffset_z=reference_header.dim[3]/2.0F;
    reference_header.sform_code=NIFTI_XFORM_SCANNER_ANAT;
    reference_header.srow_x[0]=0.5;
    reference_header.srow_x[1]=0.0;
    reference_header.srow_x[2]=0.0;
    reference_header.srow_x[3]=0.0;
    reference_header.srow_y[0]=0.0;
    reference_header.srow_y[1]=1.0;
    reference_header.srow_y[2]=0.0;
    reference_header.srow_y[3]=0.0;
    reference_header.srow_z[0]=0.0;
    reference_header.srow_z[1]=0.0;
    reference_header.srow_z[2]=2.0;
    reference_header.srow_z[3]=0.0;
    reference_header.magic[0]='n';
    reference_header.magic[1]='+';
    reference_header.magic[2]='1';
    reference_header.magic[3]='\0';
    /* String is purposfully too long */
    strncpy(reference_header.intent_name,"PHANTOM_DATA to be used for regression testing the nifti reader/writer",16);
    strncpy(reference_header.descrip,"This is a very long dialog here to use up more than 80 characters of space to test to see if the code is robust enough to deal appropriatly with very long and obnoxious lines.",80);

        {
        int nbyper;
        int swapsize;
        nifti_datatype_sizes(reference_header.datatype ,&nbyper,&swapsize);
        reference_header.bitpix=nbyper*8;
        }

    reference_image=nifti_convert_nhdr2nim(reference_header,write_image_filename);
        {
        const unsigned int NumVoxels=reference_image->nx*reference_image->ny*reference_image->nz*reference_image->nt*reference_image->nu;
        reference_image->data=calloc(NumVoxels,sizeof(signed int)) ;                  /*!< pointer to data: nbyper*nvox bytes     */
            {
            signed int i=0;
            for(; i < NumVoxels ; i++)
                {
                ((signed int *)(reference_image->data))[i]=i;
                }
            }
        }
    return reference_image;
}

static unsigned int compare_known_values(nifti_image const * const known, nifti_image const * const unknown)
{
    if(known->nifti_type!=unknown->nifti_type) return __LINE__;
    if(!strcmp(known->fname,unknown->fname)) return __LINE__;
    if(!strcmp(known->iname,unknown->iname)) return __LINE__;
    if(known->ndim!=unknown->ndim) return __LINE__;
    if(known->nx!=unknown->nx) return __LINE__;
    if(known->ny!=unknown->ny) return __LINE__;
    if(known->nz!=unknown->nz) return __LINE__;
    if(known->nt!=unknown->nt) return __LINE__;
    if(known->nu!=unknown->nu) return __LINE__;
    if(known->dx!=unknown->dx) return __LINE__;
    if(known->dy!=unknown->dy) return __LINE__;
    if(known->dz!=unknown->dz) return __LINE__;
    if(known->dt!=unknown->dt) return __LINE__;
    if(known->du!=unknown->du) return __LINE__;
    if(known->datatype!=unknown->datatype) return __LINE__;
        {
        const unsigned int NumVoxels=known->nx*known->ny*known->nz*known->nt*known->nu;
            {
            unsigned int i=0;
            for(; i < NumVoxels ; i++)
                {
                if( ((unsigned short int *)(known->data))[i] !=  ((unsigned short int *)(unknown->data))[i]) return __LINE__;
                }
            }
        }
    if(known->xyz_units!=unknown->xyz_units) return __LINE__;
    if(known->time_units!=unknown->time_units) return __LINE__;
    if(known->intent_code!=unknown->intent_code) return __LINE__;
    if(!strcmp(known->intent_name,unknown->intent_name) ) return __LINE__;
    if(!strcmp(known->descrip,unknown->descrip)) return __LINE__;
    return 0;
}

int main (int argc, char *argv[])
{
    nifti_image * reference_image = generate_reference_image();
    if(!reference_image) return __LINE__;
        {
        nifti_image_write   ( reference_image ) ;
            {
            nifti_image * reloaded_image = nifti_image_read(reference_image->fname,0);
            if(!reloaded_image) return __LINE__;
            nifti_image_infodump(reloaded_image);
            if(!compare_known_values(reference_image,reloaded_image)) return __LINE__;
            nifti_image_free(reloaded_image);
            }
        }
    nifti_image_free(reference_image);
    return 0;
}
