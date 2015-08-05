#if MINC2
/* Functions for enabling/disabling error messages from the library.
 */

#ifdef __cplusplus
extern "C" {    
#endif /* __cplusplus */

MNCAPI int MI2typelen(int);

MNCAPI int MI2varname(int fd, int varid, char *varnm);

MNCAPI int MI2varid(int fd, const char *varnm);

MNCAPI int MI2attinq(int fd, int varid, const char *attnm, nc_type *type_ptr,
           int *length_ptr);

MNCAPI int MI2attname(int fd, int varid, int attid, char *name);

MNCAPI int MI2inquire(int fd, int *ndims_ptr, int *nvars_ptr, int *natts_ptr,
            int *unlimdim_ptr);

MNCAPI int MI2varinq(int fd, int varid, char *varnm_ptr, nc_type *type_ptr,
           int *ndims_ptr, int *dims_ptr, int *natts_ptr);

MNCAPI int MI2dimid(int fd, const char *dimnm);

MNCAPI int MI2diminq(int fd, int dimid, char *dimnm_ptr, long *len_ptr);

MNCAPI int MI2dimdef(int fd, const char *dimnm, long length);

MNCAPI int MI2attget(int fd, int varid, const char *attnm, void *value);

MNCAPI int MI2attput(int fd, int varid, const char *attnm, nc_type val_typ, 
           int val_len, const void *val_ptr);

MNCAPI int MI2endef(int fd);

MNCAPI int MI2vardef(int fd, const char *varnm, nc_type vartype, int ndims,
           const int *dimids);

MNCAPI int MI2varget(int fd, int varid, const long *start_ptr, 
           const long *count_ptr, void *val_ptr);

MNCAPI int MI2varput(int fd, int varid, const long *start_ptr,
           const long *count_ptr, const void *val_ptr);

MNCAPI int MI2varput1(int fd, int varid, const long *mindex_ptr,
            const void *val_ptr);

MNCAPI int MI2attdel(int fd, int varid, const char *attnm);

MNCAPI int MI2dimrename(int fd, int dimid, const char *new_name);

MNCAPI int MI2varputg(int fd, int varid, const long *startp, 
            const long *countp, const long *stridep, 
            const long *imapp, const void *valp);

MNCAPI int MI2attcopy(int infd, int invarid, const char *name, int outfd, 
            int outvarid);

MNCAPI int MI2redef(int fd);
MNCAPI int MI2sync(int fd);
MNCAPI int MI2setfill(int fd, int fillmode);

#ifndef _MI2_FORCE_NETCDF_
#define nctypelen MI2typelen
#define ncvarname MI2varname
#define ncvarid MI2varid
#define ncdimid MI2dimid
#define ncvarinq MI2varinq
#define ncdiminq MI2diminq
#define ncdimdef MI2dimdef
#define ncattdel MI2attdel
#define ncvardef MI2vardef
#define ncvarput1 MI2varput1
#define ncvarput MI2varput
#define ncvarget MI2varget
#define ncattinq MI2attinq
#define ncvarputg MI2varputg
#define nccreate micreate
#define ncopen miopen
#define ncclose miclose
#define ncattput MI2attput
#define ncinquire MI2inquire
#define ncattname MI2attname
#define ncdimrename MI2dimrename
#define ncattcopy MI2attcopy
#define ncendef MI2endef
#define ncattget MI2attget
#define ncredef MI2redef
#define ncsync MI2sync
#define ncsetfill MI2setfill

#ifndef NC_NOFILL
#define NC_NOFILL 0x100
#endif

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* _MI2_FORCE_NETCDF_ not defined */
#endif /* MINC2 */

