#include <nifti1_io.h>

enum NIFTITEST_BOOL {
    NIFTITEST_TRUE=1,
    NIFTITEST_FALSE=0
};

void PrintTest(const int line,const char * message,const int FailureOccured, const enum NIFTITEST_BOOL isFatal,int *ErrorAccum)
{
    if(FailureOccured==NIFTITEST_TRUE)  /* This line can be commented out for a more verbose output */
        {
        char const * const PREFIX= (FailureOccured)?"==========ERROR":"..........SUCCESS";
        char const * const ISFATALPREFIX= (isFatal && FailureOccured)?" FATAL":"";
        printf("%s%s (LINE %d): %s\n",PREFIX,ISFATALPREFIX,line,message);
        fflush(stdout);
        *ErrorAccum+=FailureOccured;
        if(isFatal==NIFTITEST_TRUE && FailureOccured==NIFTITEST_TRUE)
            {
            printf("\n\nTOTAL ERRORS=%d\n",*ErrorAccum);
            exit( *ErrorAccum);
            }
        }
    return;
}

nifti_image * generate_reference_image( const char * write_image_filename , int * const Errors)
{
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
    reference_header.dim[6]=1; //This MUST be 1 anything else is invalid due to code that usees huristics to fix other possible problems;
    reference_header.dim[7]=1; //This MUST be 1 anything else is invalid due to code that usees huristics to fix other possible problems;
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

    nifti_image * reference_image=nifti_convert_nhdr2nim(reference_header,write_image_filename);
        {
        const unsigned int NumVoxels=reference_image->nx*reference_image->ny*reference_image->nz*reference_image->nt*reference_image->nu;
        reference_image->data=(signed int *)calloc(NumVoxels,sizeof(signed int)) ; /*!< pointer to data: nbyper*nvox bytes     */
        PrintTest(__LINE__,"Checking memory allocation",reference_image->data ==0 ,NIFTITEST_TRUE,Errors);
            {
            signed int i=0;
            for(; i < NumVoxels ; i++)
                {
                ((signed int *)(reference_image->data))[i]=i;
                }
            }
        }
        PrintTest(__LINE__,"Setting filenames",nifti_set_filenames( reference_image,write_image_filename, 0, 0 ) != 0, NIFTITEST_TRUE,Errors);
        PrintTest(__LINE__,"Setting type from names",nifti_set_type_from_names( reference_image ) != 0, NIFTITEST_TRUE,Errors);
/*   PrintTest(__LINE__,"Checking type and names",nifti_type_and_names_match( reference_image , 1 ) != 1, NIFTITEST_TRUE,Errors); */
        PrintTest(__LINE__,"Check reference_image data is non null",(reference_image->data==0),NIFTITEST_TRUE,Errors);
    return reference_image;
}


void compare_reference_image_values(nifti_image const * const reference_image, nifti_image const * const reloaded_image, int * const Errors)
{
    PrintTest(__LINE__,"Checking nifti_type",(reference_image->nifti_type!=reloaded_image->nifti_type),NIFTITEST_FALSE,Errors);
    PrintTest(__LINE__,"Checking fname",(strcmp(reference_image->fname,reloaded_image->fname)),NIFTITEST_FALSE,Errors);
    PrintTest(__LINE__,"Checking iname",(strcmp(reference_image->iname,reloaded_image->iname)),NIFTITEST_FALSE,Errors);
    PrintTest(__LINE__,"Checking ndim",(reference_image->ndim!=reloaded_image->ndim),NIFTITEST_FALSE,Errors);
    PrintTest(__LINE__,"Checking nx",(reference_image->nx!=reloaded_image->nx),NIFTITEST_FALSE,Errors);
    PrintTest(__LINE__,"Checking ny",(reference_image->ny!=reloaded_image->ny),NIFTITEST_FALSE,Errors);
    PrintTest(__LINE__,"Checking nz",(reference_image->nz!=reloaded_image->nz),NIFTITEST_FALSE,Errors);
    PrintTest(__LINE__,"Checking nt",(reference_image->nt!=reloaded_image->nt),NIFTITEST_FALSE,Errors);
    PrintTest(__LINE__,"Checking nu",(reference_image->nu!=reloaded_image->nu),NIFTITEST_FALSE,Errors);
    PrintTest(__LINE__,"Checking dx",(reference_image->dx!=reloaded_image->dx),NIFTITEST_FALSE,Errors);
    PrintTest(__LINE__,"Checking dy",(reference_image->dy!=reloaded_image->dy),NIFTITEST_FALSE,Errors);
    PrintTest(__LINE__,"Checking dz",(reference_image->dz!=reloaded_image->dz),NIFTITEST_FALSE,Errors);
    PrintTest(__LINE__,"Checking dt",(reference_image->dt!=reloaded_image->dt),NIFTITEST_FALSE,Errors);
    PrintTest(__LINE__,"Checking du",(reference_image->du!=reloaded_image->du),NIFTITEST_FALSE,Errors);
    PrintTest(__LINE__,"Checking datatype",(reference_image->datatype!=reloaded_image->datatype),NIFTITEST_FALSE,Errors);
        {
        const unsigned int NumVoxels=reference_image->nx*reference_image->ny*reference_image->nz*reference_image->nt*reference_image->nu;
        PrintTest(__LINE__,"Check loaded data is non null",(reloaded_image->data==0),NIFTITEST_TRUE,Errors);
        PrintTest(__LINE__,"Check reference_image data is non null",(reference_image->data==0),NIFTITEST_TRUE,Errors);
            {
            unsigned int CurrVoxel=0;
            for(; CurrVoxel < NumVoxels ; CurrVoxel++)
                {
                /*printf("%d ",CurrVoxel); fflush(stdout);*/
                if( ((int *)(reference_image->data))[CurrVoxel] !=  ((int *)(reloaded_image->data))[CurrVoxel]) 
                    {
                       PrintTest(__LINE__,"Incorrect Pixel Value Found",0,NIFTITEST_FALSE,Errors);
                    }
                }
            }
        }
    PrintTest(__LINE__,"Checking xyz_units",(reference_image->xyz_units!=reloaded_image->xyz_units),NIFTITEST_FALSE,Errors);
    PrintTest(__LINE__,"Checking time_units",(reference_image->time_units!=reloaded_image->time_units),NIFTITEST_FALSE,Errors);
    PrintTest(__LINE__,"Checking intent_code",(reference_image->intent_code!=reloaded_image->intent_code),NIFTITEST_FALSE,Errors);
    PrintTest(__LINE__,"Checking intent_name",(strncmp(reference_image->intent_name,reloaded_image->intent_name,16) )!=0,NIFTITEST_FALSE,Errors);
    PrintTest(__LINE__,"Checking description",(strncmp(reference_image->descrip,reloaded_image->descrip,80))!=0,NIFTITEST_FALSE,Errors);
    return ;
}

int main (int argc, char *argv[])
{
    char TEMP_STR[256];
    int Errors=0;
        {
        PrintTest(__LINE__,"NOT REALLY AN ERROR, JUST TESTING THE ERROR TEST REPORTING MECHANISM",1,NIFTITEST_FALSE,&Errors);
        PrintTest(__LINE__,"NOT REALLY AN ERROR, JUST TESTING THE ERROR COUNTING MECHANISM",Errors==1,NIFTITEST_FALSE,&Errors);
        Errors=0;
        }
        {
        const char write_image_filename[6][64]={
            "ATestReferenceImageForReadingAndWriting.nii",
            "ATestReferenceImageForReadingAndWriting.hdr",
            "ATestReferenceImageForReadingAndWriting.img",
            "ATestReferenceImageForReadingAndWriting.nii.gz",
            "ATestReferenceImageForReadingAndWriting.hdr.gz",
            "ATestReferenceImageForReadingAndWriting.img.gz"
        };
        printf("======= Testing All Nifti Valid Names ======\n");
        fflush(stdout);
        unsigned int filenameindex;
        for(filenameindex=0;filenameindex<6; filenameindex++)
            {
            printf("======= Testing with filename: %s ======\n",write_image_filename[filenameindex]);
            fflush(stdout);
            nifti_image * reference_image = generate_reference_image(write_image_filename[filenameindex],&Errors);
            PrintTest(__LINE__,"Create reference image",reference_image==0,NIFTITEST_TRUE,&Errors);
                nifti_image_write   ( reference_image ) ;
                {
                nifti_image * reloaded_image = nifti_image_read(reference_image->fname,1);
                PrintTest(__LINE__,"Reload of image ",reloaded_image==0,NIFTITEST_TRUE,&Errors);
                nifti_image_infodump(reloaded_image);
                compare_reference_image_values(reference_image,reloaded_image,&Errors);
                nifti_image_free(reloaded_image);
                }
            nifti_image_free(reference_image);
            }
        }
        {
        enum { NUM_FILE_NAMES=8 };
        const char * FILE_NAMES[NUM_FILE_NAMES]={
            "myimage",
            "myimage.tif",
            "myimage.tif.gz",
            "myimage.nii",
            "myimage.img.gz",
            ".nii",
            ".myhiddenimage",
            ".myhiddenimage.nii"
        };
        const char * KNOWN_FILE_BASENAMES[NUM_FILE_NAMES]={
            "myimage",
            "myimage.tif",
            "myimage.tif.gz",
            "myimage",
            "myimage",
            "",
            ".myhiddenimage",
            ".myhiddenimage"
        };
        const int KNOWN_nifti_validfilename[NUM_FILE_NAMES]={
            1,
            1,
            1,
            1,
            1,
            0,
            1,
            1
        };
        const int KNOWN_nifti_is_complete_filename[NUM_FILE_NAMES]={
            0,
            0,
            0,
            1,
            1,
            0,
            0,
            1
        };
        unsigned int fni;
        for(fni=0;fni<NUM_FILE_NAMES;fni++)
            {
            printf("\nTesting \"%s\" filename\n",FILE_NAMES[fni]);
                {
                int KnownValid=nifti_validfilename(FILE_NAMES[fni]);
                snprintf(TEMP_STR,256,"nifti_validfilename(\"%s\")=%d",FILE_NAMES[fni],KnownValid);
                PrintTest(__LINE__,TEMP_STR,KnownValid != KNOWN_nifti_validfilename[fni],NIFTITEST_FALSE,&Errors);
                }
                {
                int KnownValid=nifti_is_complete_filename(FILE_NAMES[fni]);
                snprintf(TEMP_STR,256,"nifti_is_complete_filename(\"%s\")=%d",FILE_NAMES[fni],KnownValid);
                PrintTest(__LINE__,TEMP_STR,KnownValid != KNOWN_nifti_is_complete_filename[fni],NIFTITEST_FALSE,&Errors);
                }

                {
                char * basename=nifti_makebasename(FILE_NAMES[fni]);
                snprintf(TEMP_STR,256,"nifti_makebasename(\"%s\")=\"%s\"",FILE_NAMES[fni],basename);
                PrintTest(__LINE__,TEMP_STR,strcmp(basename,KNOWN_FILE_BASENAMES[fni]) != 0,NIFTITEST_FALSE,&Errors);
                free(basename);
                }
            }
        }
    printf("\n\nTOTAL ERRORS=%d\n",Errors);
    return Errors;
}
