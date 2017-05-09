/* hdf_convenience.h
 */

/* DUMMY rootvariable ID */
#define MI_ROOTVARIABLE_ID (NC_MAX_VARS + 1) /* Impossible value */

/* HDF functions for compatibility layer. */
extern int hdf_varname(int fd, int varid, char *varnm);
extern int hdf_varid(int fd, const char *varnm);

extern int hdf_attget(int fd, int varid, const char *attnm, void *value);
extern int hdf_attput(int fd, int varid, const char *attnm, nc_type val_typ, 
		      int val_len, const void *val_ptr);
extern int hdf_attdel(int fd, int varid, const char *attnm);
extern int hdf_attinq(int fd, int varid, const char *attnm, nc_type *type_ptr,
		      int *length_ptr);
extern int hdf_attname(int fd, int varid, int attnum, char *name);
extern int hdf_inquire(int fd, int *ndims_ptr, int *nvars_ptr, int *natts_ptr,
		       int *unlimdim_ptr);
extern int hdf_varinq(int fd, int varid, char *varnm_ptr, nc_type *type_ptr,
		      int *ndims_ptr, int *dims_ptr, int *natts_ptr);
extern int hdf_dimid(int fd, const char *dimnm);
extern int hdf_diminq(int fd, int dimid, char *dimnm_ptr, long *len_ptr);
extern int hdf_dimdef(int fd, const char *dimnm, long length);

extern void hdf_enddef(int fd);
extern int hdf_vardef(int fd, const char *varnm, nc_type vartype, int ndims,
		      const int *dimids);
extern int hdf_varget(int fd, int varid, const long *start_ptr, 
		      const long *count_ptr, void *val_ptr);
extern int hdf_vargetg(int fd, int varid, const long *startp, 
                       const long *countp, const long *stridep,
                       const long *imapp, void *valp);
extern int hdf_varput(int fd, int varid, const long *start_ptr,
		      const long *count_ptr, const void *val_ptr);
extern int hdf_varput1(int fd, int varid, const long *mindex_ptr,
		       const void *val_ptr);
extern int hdf_varputg(int fd, int varid, const long *startp, 
		       const long *countp, const long *stridep, 
		       const long *imapp, const void *valp);

extern int hdf_varsize(int fd, int varid, long *size_ptr);

extern int hdf_dimrename(int fd, int dimid, const char *new_name);

extern herr_t hdf_copy_attr(hid_t in_id, const char *attr_name, void *op_data);

extern int hdf_open(const char *path, int mode);
extern int hdf_create(const char *path, int mode, struct mi2opts *opts_ptr);
extern int hdf_close(int fd);
extern int hdf_access(const char *path);
extern int hdf_flush(int fd);

