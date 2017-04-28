#ifndef VOL_IO_PROTOTYPES_H
#define VOL_IO_PROTOTYPES_H

#include "basic.h"
#include "minc2_structs.h"

/*transforms*/

VIOAPI  VIO_STR  get_default_transform_file_suffix( void );

VIOAPI  VIO_Status  output_transform(
    FILE                *file,
    const char          *filename,
    int                 *volume_count_ptr,
    const char          *comments,
    VIO_General_transform   *transform );

VIOAPI  VIO_Status  input_transform(
    FILE                *file,
    const char          *filename,
    VIO_General_transform   *transform );

VIOAPI  VIO_Status  output_transform_file(
    const char           *filename,
    const char           *comments,
    VIO_General_transform   *transform );

VIOAPI  VIO_Status  input_transform_file(
    const char              *filename,
    VIO_General_transform   *transform );

VIOAPI  void  create_linear_transform(
    VIO_General_transform   *transform,
    VIO_Transform           *linear_transform );

VIOAPI  void  create_thin_plate_transform_real(
    VIO_General_transform    *transform,
    int                  n_dimensions,
    int                  n_points,
    VIO_Real                 **points,
    VIO_Real                 **displacements );

VIOAPI  void  create_thin_plate_transform(
    VIO_General_transform    *transform,
    int                  n_dimensions,
    int                  n_points,
    float                **points,
    float                **displacements );

VIOAPI  void  create_grid_transform(
    VIO_General_transform    *transform,
    VIO_Volume               displacement_volume,
    VIO_STR                  displacement_volume_file
                                   );

VIOAPI  void  create_grid_transform_no_copy(
    VIO_General_transform    *transform,
    VIO_Volume               displacement_volume,
    VIO_STR                  displacement_volume_file
                                           );

VIOAPI  void  create_user_transform(
    VIO_General_transform         *transform,
    void                      *user_data,
    size_t                    size_user_data,
    VIO_User_transform_function   transform_function,
    VIO_User_transform_function   inverse_transform_function );

VIOAPI  VIO_Transform_types  get_transform_type(
    VIO_General_transform   *transform );

VIOAPI  int  get_n_concated_transforms(
    VIO_General_transform   *transform );

VIOAPI  VIO_General_transform  *get_nth_general_transform(
    VIO_General_transform   *transform,
    int                 n );

VIOAPI  VIO_Transform  *get_linear_transform_ptr(
    VIO_General_transform   *transform );

VIOAPI  VIO_Transform  *get_inverse_linear_transform_ptr(
    VIO_General_transform   *transform );

VIOAPI  VIO_Status  general_transform_point_with_input_steps(
    VIO_General_transform   *transform,
    VIO_Real                x,
    VIO_Real                y,
    VIO_Real                z,
    VIO_Real                *input_volume_steps,
    VIO_Real                *x_transformed,
    VIO_Real                *y_transformed,
    VIO_Real                *z_transformed );

VIOAPI  VIO_Status  general_inverse_transform_point_with_input_steps(
    VIO_General_transform   *transform,
    VIO_Real                x,
    VIO_Real                y,
    VIO_Real                z,
    VIO_Real                *input_volume_steps,
    VIO_Real                *x_transformed,
    VIO_Real                *y_transformed,
    VIO_Real                *z_transformed );

VIOAPI  VIO_Status  general_transform_point(
    VIO_General_transform   *transform,
    VIO_Real                x,
    VIO_Real                y,
    VIO_Real                z,
    VIO_Real                *x_transformed,
    VIO_Real                *y_transformed,
    VIO_Real                *z_transformed );

VIOAPI  VIO_Status  general_inverse_transform_point(
    VIO_General_transform   *transform,
    VIO_Real                x,
    VIO_Real                y,
    VIO_Real                z,
    VIO_Real                *x_transformed,
    VIO_Real                *y_transformed,
    VIO_Real                *z_transformed );

VIOAPI  void  copy_general_transform(
    VIO_General_transform   *transform,
    VIO_General_transform   *copy );

VIOAPI  void  invert_general_transform(
    VIO_General_transform   *transform );

VIOAPI  void  create_inverse_general_transform(
    VIO_General_transform   *transform,
    VIO_General_transform   *inverse );

VIOAPI  void  concat_general_transforms(
    VIO_General_transform   *first,
    VIO_General_transform   *second,
    VIO_General_transform   *result );

VIOAPI  void  delete_general_transform(
    VIO_General_transform   *transform );

VIOAPI  VIO_Status  mni_get_nonwhite_character(
    FILE   *file,
    char   *ch );

VIOAPI  VIO_Status  mni_skip_expected_character(
    FILE   *file,
    char   expected_ch );

VIOAPI  VIO_Status  mni_input_line(
    FILE     *file,
    VIO_STR   *string );

VIOAPI  VIO_Status  mni_input_string(
    FILE     *file,
    VIO_STR   *string,
    char     termination_char1,
    char     termination_char2 );

VIOAPI  VIO_Status  mni_input_keyword_and_equal_sign(
    FILE         *file,
    const char   keyword[],
    VIO_BOOL     print_error_message );

VIOAPI  VIO_Status  mni_input_real(
    FILE    *file,
    VIO_Real    *d );

VIOAPI  VIO_Status  mni_input_reals(
    FILE    *file,
    int     *n,
    VIO_Real    *reals[] );

VIOAPI  VIO_Status  mni_input_int(
    FILE    *file,
    int     *i );

VIOAPI  void  output_comments(
    FILE       *file,
    const char *comments );

VIOAPI  VIO_STR  get_default_tag_file_suffix( void );

VIOAPI  VIO_Status  initialize_tag_file_output(
    FILE      *file,
    VIO_STR    comments,
    int       n_volumes );

VIOAPI  VIO_Status  output_one_tag(
    FILE      *file,
    int       n_volumes,
    VIO_Real      tag_volume1[],
    VIO_Real      tag_volume2[],
    VIO_Real      *weight,
    int       *structure_id,
    int       *patient_id,
    VIO_STR    label );

VIOAPI  void  terminate_tag_file_output(
    FILE    *file );

VIOAPI  VIO_Status  output_tag_points(
    FILE      *file,
    VIO_STR    comments,
    int       n_volumes,
    int       n_tag_points,
    VIO_Real      **tags_volume1,
    VIO_Real      **tags_volume2,
    VIO_Real      weights[],
    int       structure_ids[],
    int       patient_ids[],
    VIO_STR    *labels );

VIOAPI  void  free_tag_points(
    int       n_volumes,
    int       n_tag_points,
    VIO_Real      **tags_volume1,
    VIO_Real      **tags_volume2,
    VIO_Real      weights[],
    int       structure_ids[],
    int       patient_ids[],
    char      **labels );

VIOAPI  VIO_Status  initialize_tag_file_input(
    FILE      *file,
    int       *n_volumes_ptr );

VIOAPI  VIO_Status  output_tag_file(
    VIO_STR    filename,
    VIO_STR    comments,
    int       n_volumes,
    int       n_tag_points,
    VIO_Real      **tags_volume1,
    VIO_Real      **tags_volume2,
    VIO_Real      weights[],
    int       structure_ids[],
    int       patient_ids[],
    VIO_STR    labels[] );

VIOAPI  VIO_Status  input_tag_file(
    VIO_STR    filename,
    int       *n_volumes,
    int       *n_tag_points,
    VIO_Real      ***tags_volume1,
    VIO_Real      ***tags_volume2,
    VIO_Real      **weights,
    int       **structure_ids,
    int       **patient_ids,
    VIO_STR    *labels[] );

VIOAPI  VIO_BOOL input_one_tag(
    FILE      *file,
    int       n_volumes,
    VIO_Real      tag_volume1[],
    VIO_Real      tag_volume2[],
    VIO_Real      *weight,
    int       *structure_id,
    int       *patient_id,
    VIO_STR    *label,
    VIO_Status    *status );

VIOAPI  VIO_Status  input_tag_points(
    FILE      *file,
    int       *n_volumes_ptr,
    int       *n_tag_points,
    VIO_Real      ***tags_volume1,
    VIO_Real      ***tags_volume2,
    VIO_Real      **weights,
    int       **structure_ids,
    int       **patient_ids,
    VIO_STR    *labels[] );

VIOAPI  void  evaluate_thin_plate_spline(
    int     n_dims,
    int     n_values,
    int     n_points,
    VIO_Real    **points,
    VIO_Real    **weights,
    VIO_Real    pos[],
    VIO_Real    values[],
    VIO_Real    **derivs );

VIOAPI  VIO_Status  thin_plate_spline_transform(
    int     n_dims,
    int     n_points,
    VIO_Real    **points,
    VIO_Real    **weights,
    VIO_Real    x,
    VIO_Real    y,
    VIO_Real    z,
    VIO_Real    *x_transformed,
    VIO_Real    *y_transformed,
    VIO_Real    *z_transformed );

VIOAPI  VIO_Status  thin_plate_spline_inverse_transform(
    int     n_dims,
    int     n_points,
    VIO_Real    **points,
    VIO_Real    **weights,
    VIO_Real    x,
    VIO_Real    y,
    VIO_Real    z,
    VIO_Real    *x_transformed,
    VIO_Real    *y_transformed,
    VIO_Real    *z_transformed );

VIOAPI  VIO_Real  thin_plate_spline_U(
    VIO_Real   pos[],
    VIO_Real   landmark[],
    int    n_dims );

VIOAPI  VIO_Colour  make_rgba_Colour(
    int    r,
    int    g,
    int    b,
    int    a );

VIOAPI  int  get_Colour_r(
    VIO_Colour   colour );

VIOAPI  int  get_Colour_g(
    VIO_Colour   colour );

VIOAPI  int  get_Colour_b(
    VIO_Colour   colour );

VIOAPI  int  get_Colour_a(
    VIO_Colour   colour );

VIOAPI  VIO_Colour  make_Colour(
    int   r,
    int   g,
    int   b );

VIOAPI  VIO_Real  get_Colour_r_0_1(
    VIO_Colour   colour );

VIOAPI  VIO_Real  get_Colour_g_0_1(
    VIO_Colour   colour );

VIOAPI  VIO_Real  get_Colour_b_0_1(
    VIO_Colour   colour );

VIOAPI  VIO_Real  get_Colour_a_0_1(
    VIO_Colour   colour );

VIOAPI  VIO_Colour  make_Colour_0_1(
    VIO_Real   r,
    VIO_Real   g,
    VIO_Real   b );

VIOAPI  VIO_Colour  make_rgba_Colour_0_1(
    VIO_Real   r,
    VIO_Real   g,
    VIO_Real   b,
    VIO_Real   a );

VIOAPI  VIO_BOOL solve_linear_system(
    int   n,
    VIO_Real  **coefs,
    VIO_Real  values[],
    VIO_Real  solution[] );

VIOAPI  VIO_BOOL invert_square_matrix(
    int   n,
    VIO_Real  **matrix,
    VIO_Real  **inverse );

VIOAPI  VIO_BOOL newton_root_find(
    int    n_dimensions,
    void   (*function) ( void *, VIO_Real [],  VIO_Real [], VIO_Real ** ),
    void   *function_data,
    VIO_Real   initial_guess[],
    VIO_Real   desired_values[],
    VIO_Real   solution[],
    VIO_Real   function_tolerance,
    VIO_Real   delta_tolerance,
    int    max_iterations );

VIOAPI  void  create_orthogonal_vector(
    VIO_Vector  *v,
    VIO_Vector  *ortho );

VIOAPI  void  create_two_orthogonal_vectors(
    VIO_Vector   *v,
    VIO_Vector   *v1,
    VIO_Vector   *v2 );

VIOAPI  VIO_BOOL  compute_transform_inverse(
    VIO_Transform  *transform,
    VIO_Transform  *inverse );

VIOAPI  void  get_linear_spline_coefs(
    VIO_Real  **coefs );

VIOAPI  void  get_quadratic_spline_coefs(
    VIO_Real  **coefs );

VIOAPI  void  get_cubic_spline_coefs(
    VIO_Real  **coefs );

VIOAPI  VIO_Real  cubic_interpolate(
    VIO_Real   u,
    VIO_Real   v0,
    VIO_Real   v1,
    VIO_Real   v2,
    VIO_Real   v3 );

VIOAPI  void  evaluate_univariate_interpolating_spline(
    VIO_Real    u,
    int     degree,
    VIO_Real    coefs[],
    int     n_derivs,
    VIO_Real    derivs[] );

VIOAPI  void  evaluate_bivariate_interpolating_spline(
    VIO_Real    u,
    VIO_Real    v,
    int     degree,
    VIO_Real    coefs[],
    int     n_derivs,
    VIO_Real    derivs[] );

VIOAPI  void  evaluate_trivariate_interpolating_spline(
    VIO_Real    u,
    VIO_Real    v,
    VIO_Real    w,
    int     degree,
    VIO_Real    coefs[],
    int     n_derivs,
    VIO_Real    derivs[] );

VIOAPI  void  evaluate_interpolating_spline(
    int     n_dims,
    VIO_Real    parameters[],
    int     degree,
    int     n_values,
    VIO_Real    coefs[],
    int     n_derivs,
    VIO_Real    derivs[] );

VIOAPI  void  spline_tensor_product(
    int     n_dims,
    VIO_Real    positions[],
    int     degrees[],
    VIO_Real    *bases[],
    int     n_values,
    VIO_Real    coefs[],
    int     n_derivs[],
    VIO_Real    results[] );

VIOAPI  void  make_identity_transform( VIO_Transform   *transform );

VIOAPI  VIO_BOOL close_to_identity(
    VIO_Transform   *transform );

VIOAPI  void  get_transform_origin(
    VIO_Transform   *transform,
    VIO_Point       *origin );

VIOAPI  void  set_transform_origin(
    VIO_Transform   *transform,
    VIO_Point       *origin );

VIOAPI  void  get_transform_origin_real(
    VIO_Transform   *transform,
    VIO_Real        origin[] );

VIOAPI  void  get_transform_x_axis(
    VIO_Transform   *transform,
    VIO_Vector      *x_axis );

VIOAPI  void  get_transform_x_axis_real(
    VIO_Transform   *transform,
    VIO_Real        x_axis[] );

VIOAPI  void  set_transform_x_axis(
    VIO_Transform   *transform,
    VIO_Vector      *x_axis );

VIOAPI  void  set_transform_x_axis_real(
    VIO_Transform   *transform,
    VIO_Real        x_axis[] );

VIOAPI  void  get_transform_y_axis(
    VIO_Transform   *transform,
    VIO_Vector      *y_axis );

VIOAPI  void  get_transform_y_axis_real(
    VIO_Transform   *transform,
    VIO_Real        y_axis[] );

VIOAPI  void  set_transform_y_axis(
    VIO_Transform   *transform,
    VIO_Vector      *y_axis );

VIOAPI  void  set_transform_y_axis_real(
    VIO_Transform   *transform,
    VIO_Real        y_axis[] );

VIOAPI  void  get_transform_z_axis(
    VIO_Transform   *transform,
    VIO_Vector      *z_axis );

VIOAPI  void  get_transform_z_axis_real(
    VIO_Transform   *transform,
    VIO_Real        z_axis[] );

VIOAPI  void  set_transform_z_axis(
    VIO_Transform   *transform,
    VIO_Vector      *z_axis );

VIOAPI  void  set_transform_z_axis_real(
    VIO_Transform   *transform,
    VIO_Real        z_axis[] );

VIOAPI  void   make_change_to_bases_transform(
    VIO_Point      *origin,
    VIO_Vector     *x_axis,
    VIO_Vector     *y_axis,
    VIO_Vector     *z_axis,
    VIO_Transform  *transform );

VIOAPI  void   make_change_from_bases_transform(
    VIO_Point      *origin,
    VIO_Vector     *x_axis,
    VIO_Vector     *y_axis,
    VIO_Vector     *z_axis,
    VIO_Transform  *transform );

VIOAPI  void   concat_transforms(
    VIO_Transform   *result,
    VIO_Transform   *t1,
    VIO_Transform   *t2 );

VIOAPI  VIO_Status  transform_point(
    VIO_Transform  *transform,
    VIO_Real       x,
    VIO_Real       y,
    VIO_Real       z,
    VIO_Real       *x_trans,
    VIO_Real       *y_trans,
    VIO_Real       *z_trans );

VIOAPI  VIO_Status  transform_vector(
    VIO_Transform  *transform,
    VIO_Real       x,
    VIO_Real       y,
    VIO_Real       z,
    VIO_Real       *x_trans,
    VIO_Real       *y_trans,
    VIO_Real       *z_trans );

VIOAPI  void  *alloc_memory_in_bytes(
    size_t       n_bytes
    _ALLOC_SOURCE_LINE_ARG_DEF );

VIOAPI  void  *alloc_memory_1d(
    size_t       n_elements,
    size_t       type_size
    _ALLOC_SOURCE_LINE_ARG_DEF );

VIOAPI  void  *alloc_memory_2d(
    size_t       n1,
    size_t       n2,
    size_t       type_size
    _ALLOC_SOURCE_LINE_ARG_DEF );

VIOAPI  void  *alloc_memory_3d(
    size_t       n1,
    size_t       n2,
    size_t       n3,
    size_t       type_size
    _ALLOC_SOURCE_LINE_ARG_DEF );

VIOAPI  void  *alloc_memory_4d(
    size_t       n1,
    size_t       n2,
    size_t       n3,
    size_t       n4,
    size_t       type_size
    _ALLOC_SOURCE_LINE_ARG_DEF );

VIOAPI  void  *alloc_memory_5d(
    size_t       n1,
    size_t       n2,
    size_t       n3,
    size_t       n4,
    size_t       n5,
    size_t       type_size
    _ALLOC_SOURCE_LINE_ARG_DEF );

VIOAPI  void  realloc_memory(
    void      **ptr,
    size_t    n_elements,
    size_t    type_size
    _ALLOC_SOURCE_LINE_ARG_DEF );

VIOAPI  void  free_memory_1d(
    void   **ptr
    _ALLOC_SOURCE_LINE_ARG_DEF );

VIOAPI  void  free_memory_2d(
    void   ***ptr
    _ALLOC_SOURCE_LINE_ARG_DEF );

VIOAPI  void  free_memory_3d(
    void   ****ptr
    _ALLOC_SOURCE_LINE_ARG_DEF );

VIOAPI  void  free_memory_4d(
    void   *****ptr
    _ALLOC_SOURCE_LINE_ARG_DEF );

VIOAPI  void  free_memory_5d(
    void   ******ptr
    _ALLOC_SOURCE_LINE_ARG_DEF );

#if 0
VIOAPI  size_t  get_total_memory_alloced( void );

VIOAPI  VIO_BOOL alloc_checking_enabled( void );

VIOAPI  void  set_alloc_checking( VIO_BOOL state );

VIOAPI  void  record_ptr_alloc_check(
    void      *ptr,
    size_t    n_bytes,
    VIO_STR    source_file,
    int       line_number );

VIOAPI  void  change_ptr_alloc_check(
    void      *old_ptr,
    void      *new_ptr,
    size_t    n_bytes,
    VIO_STR    source_file,
    int       line_number );

VIOAPI  VIO_BOOL unrecord_ptr_alloc_check(
    void     *ptr,
    VIO_STR   source_file,
    int      line_number );

VIOAPI  void  output_alloc_to_file(
    VIO_STR   filename );

VIOAPI  void  print_alloc_source_line(
    VIO_STR  filename,
    int     line_number );
#endif

VIOAPI  void  set_array_size(
    void      **array,
    size_t    type_size,
    size_t    previous_n_elems,
    size_t    new_n_elems,
    size_t    chunk_size
    _ALLOC_SOURCE_LINE_ARG_DEF );

VIOAPI  VIO_BOOL real_is_double( void );

VIOAPI  VIO_BOOL file_exists(
    VIO_STR        filename );

VIOAPI  VIO_BOOL file_directory_exists(
    VIO_STR        filename );

VIOAPI  VIO_BOOL check_clobber_file(
    VIO_STR   filename );

VIOAPI  VIO_BOOL check_clobber_file_default_suffix(
    VIO_STR   filename,
    VIO_STR   default_suffix );

VIOAPI  VIO_Status  make_backup_file(
    VIO_STR   filename,
    VIO_STR   *backup_filename );

VIOAPI  void  cleanup_backup_file(
    VIO_STR   filename,
    VIO_STR   backup_filename,
    VIO_Status   status_of_write );

VIOAPI  void  remove_file(
    VIO_STR  filename );

VIOAPI  VIO_Status  copy_file(
    VIO_STR  src,
    VIO_STR  dest );

VIOAPI  VIO_Status  move_file(
    VIO_STR  src,
    VIO_STR  dest );

VIOAPI  VIO_STR  expand_filename(
    const char  *filename );

VIOAPI  VIO_BOOL filename_extension_matches(
    VIO_STR   filename,
    VIO_STR   extension );

VIOAPI  VIO_STR  remove_directories_from_filename(
    VIO_STR  filename );

VIOAPI  VIO_BOOL file_exists_as_compressed(
    VIO_STR       filename,
    VIO_STR       *compressed_filename );

VIOAPI  VIO_STR  get_temporary_filename( void );

VIOAPI  VIO_Status  open_file(
    VIO_STR            filename,
    VIO_IO_types       io_type,
    VIO_File_formats   file_format,
    FILE               **file );

VIOAPI  VIO_Status  open_file_with_default_suffix(
    const char         *filename,
    VIO_STR            default_suffix,
    VIO_IO_types       io_type,
    VIO_File_formats   file_format,
    FILE               **file );

VIOAPI  VIO_Status  set_file_position(
    FILE     *file,
    long     byte_position );

VIOAPI  VIO_Status  close_file(
    FILE     *file );

VIOAPI  VIO_STR  extract_directory(
    const char    *filename );

VIOAPI  VIO_STR  get_absolute_filename(
    VIO_STR    filename,
    VIO_STR    directory );

VIOAPI  VIO_Status  flush_file(
    FILE     *file );

VIOAPI  VIO_Status  input_character(
    FILE  *file,
    char   *ch );

VIOAPI  VIO_Status  unget_character(
    FILE  *file,
    char  ch );

VIOAPI  VIO_Status  input_nonwhite_character(
    FILE   *file,
    char   *ch );

VIOAPI  VIO_Status  output_character(
    FILE   *file,
    char   ch );

VIOAPI  VIO_Status   skip_input_until(
    FILE   *file,
    char   search_char );

VIOAPI  VIO_Status  output_string(
    FILE    *file,
    VIO_STR  str );

VIOAPI  VIO_Status  input_string(
    FILE    *file,
    VIO_STR  *str,
    char    termination_char );

VIOAPI  VIO_Status  input_quoted_string(
    FILE            *file,
    VIO_STR          *str );

VIOAPI  VIO_Status  input_possibly_quoted_string(
    FILE            *file,
    VIO_STR          *str );

VIOAPI  VIO_Status  output_quoted_string(
    FILE            *file,
    VIO_STR          str );

VIOAPI  VIO_Status  input_binary_data(
    FILE            *file,
    void            *data,
    size_t          element_size,
    int             n );

VIOAPI  VIO_Status  output_binary_data(
    FILE            *file,
    void            *data,
    size_t          element_size,
    int             n );

VIOAPI  VIO_Status  input_newline(
    FILE            *file );

VIOAPI  VIO_Status  output_newline(
    FILE            *file );

VIOAPI  VIO_Status  input_line(
    FILE    *file,
    VIO_STR  *line );

VIOAPI  VIO_Status  input_boolean(
    FILE            *file,
    VIO_BOOL        *b );

VIOAPI  VIO_Status  output_boolean(
    FILE            *file,
    VIO_BOOL        b );

VIOAPI  VIO_Status  input_short(
    FILE            *file,
    short           *s );

VIOAPI  VIO_Status  output_short(
    FILE            *file,
    short           s );

VIOAPI  VIO_Status  input_unsigned_short(
    FILE            *file,
    unsigned short  *s );

VIOAPI  VIO_Status  output_unsigned_short(
    FILE            *file,
    unsigned short  s );

VIOAPI  VIO_Status  input_int(
    FILE  *file,
    int   *i );

VIOAPI  VIO_Status  output_int(
    FILE            *file,
    int             i );

VIOAPI  VIO_Status  input_real(
    FILE            *file,
    VIO_Real            *r );

VIOAPI  VIO_Status  output_real(
    FILE            *file,
    VIO_Real            r );

VIOAPI  VIO_Status  input_float(
    FILE            *file,
    float           *f );

VIOAPI  VIO_Status  output_float(
    FILE            *file,
    float           f );

VIOAPI  VIO_Status  input_double(
    FILE            *file,
    double          *d );

VIOAPI  VIO_Status  output_double(
    FILE            *file,
    double          d );

VIOAPI  VIO_Status  io_binary_data(
    FILE            *file,
    VIO_IO_types    io_flag,
    void            *data,
    size_t          element_size,
    int             n );

VIOAPI  VIO_Status  io_newline(
    FILE            *file,
    VIO_IO_types     io_flag,
    VIO_File_formats format );

VIOAPI  VIO_Status  io_quoted_string(
    FILE            *file,
    VIO_IO_types     io_flag,
    VIO_File_formats format,
    VIO_STR          *str );

VIOAPI  VIO_Status  io_boolean(
    FILE            *file,
    VIO_IO_types     io_flag,
    VIO_File_formats format,
    VIO_BOOL        *b );

VIOAPI  VIO_Status  io_short(
    FILE            *file,
    VIO_IO_types     io_flag,
    VIO_File_formats format,
    short           *short_int );

VIOAPI  VIO_Status  io_unsigned_short(
    FILE            *file,
    VIO_IO_types     io_flag,
    VIO_File_formats format,
    unsigned short  *unsigned_short );

VIOAPI  VIO_Status  io_unsigned_char(
    FILE            *file,
    VIO_IO_types     io_flag,
    VIO_File_formats format,
    unsigned  char  *c );

VIOAPI  VIO_Status  io_int(
    FILE            *file,
    VIO_IO_types     io_flag,
    VIO_File_formats format,
    int             *i );

VIOAPI  VIO_Status  io_real(
    FILE            *file,
    VIO_IO_types     io_flag,
    VIO_File_formats format,
    VIO_Real         *r );

VIOAPI  VIO_Status  io_float(
    FILE            *file,
    VIO_IO_types     io_flag,
    VIO_File_formats format,
    float           *f );

VIOAPI  VIO_Status  io_double(
    FILE            *file,
    VIO_IO_types     io_flag,
    VIO_File_formats format,
    double          *d );

VIOAPI  VIO_Status  io_ints(
    FILE            *file,
    VIO_IO_types     io_flag,
    VIO_File_formats format,
    int             n,
    int             *ints[] );

VIOAPI  VIO_Status  io_unsigned_chars(
    FILE            *file,
    VIO_IO_types     io_flag,
    VIO_File_formats format,
    int             n,
    unsigned char   *unsigned_chars[] );

VIOAPI  void  set_print_function( void  (*function) ( VIO_STR ) );

VIOAPI  void  push_print_function( void );

VIOAPI  void  pop_print_function( void );

VIOAPI  void  print( VIO_STR format, ... );

VIOAPI  void  set_print_error_function( void  (*function) ( char [] ) );

VIOAPI  void  push_print_error_function( void );

VIOAPI  void  pop_print_error_function( void );

VIOAPI  void  print_error( char format[], ... ) VIO_FORMAT_FUNCTION(printf, 1, 2);

VIOAPI  void   handle_internal_error( char  str[] );

VIOAPI  void  abort_if_allowed( void );

VIOAPI  void  initialize_progress_report(
    VIO_progress_struct *progress,
    VIO_BOOL          one_line_only,
    int               n_steps,
    VIO_STR            title );

VIOAPI  void  update_progress_report(
    VIO_progress_struct   *progress,
    int               current_step );

VIOAPI  void  terminate_progress_report(
    VIO_progress_struct   *progress );

VIOAPI  VIO_STR  alloc_string(
    size_t   length );

VIOAPI  VIO_STR  create_string(
    const char   *initial );

VIOAPI  void  delete_string(
    VIO_STR   string );

VIOAPI  VIO_STR  concat_strings(
    VIO_STR   str1,
    VIO_STR   str2 );

VIOAPI  void  replace_string(
    VIO_STR   *string,
    VIO_STR   new_string );

VIOAPI  void  concat_char_to_string(
    VIO_STR   *string,
    char     ch );

VIOAPI  void  concat_to_string(
    VIO_STR   *string,
    VIO_STR   str2 );

VIOAPI  int  string_length(
    const char   *string );

VIOAPI  VIO_BOOL equal_strings(
    const char   *str1,
    const char   *str2 );

VIOAPI  VIO_BOOL is_lower_case(
    char  ch );

VIOAPI  VIO_BOOL is_upper_case(
    char  ch );

VIOAPI  char  get_lower_case(
    char   ch );

VIOAPI  char  get_upper_case(
    char   ch );

VIOAPI  VIO_BOOL string_ends_in(
    VIO_STR   string,
    VIO_STR   ending );

VIOAPI    VIO_STR   strip_outer_blanks(
    VIO_STR  str );

VIOAPI  int  find_character(
    VIO_STR    string,
    char      ch );

VIOAPI  void  make_string_upper_case(
    VIO_STR    string );

VIOAPI  VIO_BOOL blank_string(
    VIO_STR   string );

VIOAPI  VIO_Real  current_cpu_seconds( void );

VIOAPI  VIO_Real  current_realtime_seconds( void );

VIOAPI  VIO_STR  format_time(
    VIO_STR   format,
    VIO_Real     seconds );

VIOAPI  void  print_time(
    VIO_STR   format,
    VIO_Real     seconds );

VIOAPI  VIO_STR  get_clock_time( void );

VIOAPI  void  sleep_program( VIO_Real seconds );

VIOAPI  VIO_STR  get_date( void );

/*The rest of transform functions*/

VIOAPI  VIO_Real  convert_voxel_to_value(
    VIO_Volume   volume,
    VIO_Real     voxel );

VIOAPI  VIO_Real  convert_value_to_voxel(
    VIO_Volume   volume,
    VIO_Real     value );

VIOAPI  VIO_Real  get_volume_voxel_value(
    VIO_Volume   volume,
    int      v0,
    int      v1,
    int      v2,
    int      v3,
    int      v4 );

VIOAPI  VIO_Real  get_volume_real_value(
    VIO_Volume   volume,
    int      v0,
    int      v1,
    int      v2,
    int      v3,
    int      v4 );

VIOAPI  void  set_volume_voxel_value(
    VIO_Volume   volume,
    int      v0,
    int      v1,
    int      v2,
    int      v3,
    int      v4,
    VIO_Real     voxel );

VIOAPI  void  set_volume_real_value(
    VIO_Volume   volume,
    int      v0,
    int      v1,
    int      v2,
    int      v3,
    int      v4,
    VIO_Real     value );

VIOAPI  void  set_volume_interpolation_tolerance(
    VIO_Real   tolerance );

VIOAPI  int   evaluate_volume(
    VIO_Volume         volume,
    VIO_Real           voxel[],
    VIO_BOOL       interpolating_dimensions[],
    int            degrees_continuity,
    VIO_BOOL       use_linear_at_edge,
    VIO_Real           outside_value,
    VIO_Real           values[],
    VIO_Real           **first_deriv,
    VIO_Real           ***second_deriv );

VIOAPI  void   evaluate_volume_in_world(
    VIO_Volume         volume,
    VIO_Real           x,
    VIO_Real           y,
    VIO_Real           z,
    int            degrees_continuity,
    VIO_BOOL       use_linear_at_edge,
    VIO_Real           outside_value,
    VIO_Real           values[],
    VIO_Real           deriv_x[],
    VIO_Real           deriv_y[],
    VIO_Real           deriv_z[],
    VIO_Real           deriv_xx[],
    VIO_Real           deriv_xy[],
    VIO_Real           deriv_xz[],
    VIO_Real           deriv_yy[],
    VIO_Real           deriv_yz[],
    VIO_Real           deriv_zz[] );

VIOAPI  void  convert_voxels_to_values(
    VIO_Volume   volume,
    int      n_voxels,
    VIO_Real     voxels[],
    VIO_Real     values[] );

VIOAPI  void  get_volume_value_hyperslab(
    VIO_Volume   volume,
    int      v0,
    int      v1,
    int      v2,
    int      v3,
    int      v4,
    int      n0,
    int      n1,
    int      n2,
    int      n3,
    int      n4,
    VIO_Real     values[] );

VIOAPI  void  get_volume_value_hyperslab_5d(
    VIO_Volume   volume,
    int      v0,
    int      v1,
    int      v2,
    int      v3,
    int      v4,
    int      n0,
    int      n1,
    int      n2,
    int      n3,
    int      n4,
    VIO_Real     values[] );

VIOAPI  void  get_volume_value_hyperslab_4d(
    VIO_Volume   volume,
    int      v0,
    int      v1,
    int      v2,
    int      v3,
    int      n0,
    int      n1,
    int      n2,
    int      n3,
    VIO_Real     values[] );

VIOAPI  void  get_volume_value_hyperslab_3d(
    VIO_Volume   volume,
    int      v0,
    int      v1,
    int      v2,
    int      n0,
    int      n1,
    int      n2,
    VIO_Real     values[] );

VIOAPI  void  get_volume_value_hyperslab_2d(
    VIO_Volume   volume,
    int      v0,
    int      v1,
    int      n0,
    int      n1,
    VIO_Real     values[] );

VIOAPI  void  get_volume_value_hyperslab_1d(
    VIO_Volume   volume,
    int      v0,
    int      n0,
    VIO_Real     values[] );

VIOAPI  void  get_voxel_values_5d(
    VIO_Data_types data_type,
    void        *void_ptr,
    int         steps[],
    int         counts[],
    VIO_Real        values[] );

VIOAPI  void  get_voxel_values_4d(
    VIO_Data_types  data_type,
    void        *void_ptr,
    int         steps[],
    int         counts[],
    VIO_Real        values[] );

VIOAPI  void  get_voxel_values_3d(
    VIO_Data_types  data_type,
    void        *void_ptr,
    int         steps[],
    int         counts[],
    VIO_Real        values[] );

VIOAPI  void  get_voxel_values_2d(
    VIO_Data_types  data_type,
    void        *void_ptr,
    int         steps[],
    int         counts[],
    VIO_Real        values[] );

VIOAPI  void  get_voxel_values_1d(
    VIO_Data_types  data_type,
    void        *void_ptr,
    int         step0,
    int         n0,
    VIO_Real        values[] );

VIOAPI  void  get_volume_voxel_hyperslab_5d(
    VIO_Volume   volume,
    int      v0,
    int      v1,
    int      v2,
    int      v3,
    int      v4,
    int      n0,
    int      n1,
    int      n2,
    int      n3,
    int      n4,
    VIO_Real     values[] );

VIOAPI  void  get_volume_voxel_hyperslab_4d(
    VIO_Volume   volume,
    int      v0,
    int      v1,
    int      v2,
    int      v3,
    int      n0,
    int      n1,
    int      n2,
    int      n3,
    VIO_Real     values[] );

VIOAPI  void  get_volume_voxel_hyperslab_3d(
    VIO_Volume   volume,
    int      v0,
    int      v1,
    int      v2,
    int      n0,
    int      n1,
    int      n2,
    VIO_Real     values[] );

VIOAPI  void  get_volume_voxel_hyperslab_2d(
    VIO_Volume   volume,
    int      v0,
    int      v1,
    int      n0,
    int      n1,
    VIO_Real     values[] );

VIOAPI  void  get_volume_voxel_hyperslab_1d(
    VIO_Volume   volume,
    int      v0,
    int      n0,
    VIO_Real     values[] );

VIOAPI  void  get_volume_voxel_hyperslab(
    VIO_Volume   volume,
    int      v0,
    int      v1,
    int      v2,
    int      v3,
    int      v4,
    int      n0,
    int      n1,
    int      n2,
    int      n3,
    int      n4,
    VIO_Real     voxels[] );

VIOAPI  VIO_Status  initialize_free_format_input(
    VIO_STR               filename,
    VIO_Volume               volume,
    volume_input_struct  *volume_input );

VIOAPI  void  delete_free_format_input(
    volume_input_struct   *volume_input );

VIOAPI  VIO_BOOL input_more_free_format_file(
    VIO_Volume            volume,
    volume_input_struct   *volume_input,
    VIO_Real              *fraction_done );

VIOAPI  int   get_minc_file_n_dimensions(
    VIO_STR   filename );

VIOAPI  int   get_minc2_file_n_dimensions(
    VIO_STR   filename );

VIOAPI  Minc_file  initialize_minc_input_from_minc_id(
    int                  minc_id,
    VIO_Volume           volume,
    minc_input_options   *options );

VIOAPI  Minc_file  initialize_minc_input(
    VIO_STR               filename,
    VIO_Volume            volume,
    minc_input_options    *options );

VIOAPI  Minc_file  initialize_minc2_input(
    VIO_STR               filename,
    VIO_Volume            volume,
    minc_input_options    *options );

VIOAPI  int  get_n_input_volumes(
    Minc_file  file );

VIOAPI  VIO_Status  close_minc_input(
    Minc_file   file );

VIOAPI  VIO_Status  close_minc2_input(
    Minc_file   file );

VIOAPI  VIO_Status  input_minc_hyperslab(
    Minc_file        file,
    VIO_Data_types   data_type,
    int              n_array_dims,
    int              array_sizes[],
    void             *array_data_ptr,
    int              to_array[],
    int              start[],
    int              count[] );

VIOAPI  VIO_BOOL input_more_minc_file(
    Minc_file   file,
    VIO_Real        *fraction_done );

VIOAPI  VIO_BOOL input_more_minc2_file(
                  Minc_file   file,
                  VIO_Real        *fraction_done );

VIOAPI  VIO_BOOL advance_input_volume(
    Minc_file   file );

VIOAPI  void  reset_input_volume(
    Minc_file   file );

VIOAPI  int  get_minc_file_id(
    Minc_file  file );

VIOAPI  void  set_default_minc_input_options(
    minc_input_options  *options );

VIOAPI  void  set_minc_input_promote_invalid_to_zero_flag(
    minc_input_options  *options,
    VIO_BOOL            flag );

VIOAPI  void  set_minc_input_promote_invalid_to_min_flag(
    minc_input_options  *options,
    VIO_BOOL            flag );

VIOAPI  void  set_minc_input_vector_to_scalar_flag(
    minc_input_options  *options,
    VIO_BOOL            flag );

VIOAPI  void  set_minc_input_vector_to_colour_flag(
    minc_input_options  *options,
    VIO_BOOL            flag );

VIOAPI  void  set_minc_input_colour_dimension_size(
    minc_input_options  *options,
    int                 size );

VIOAPI  void  set_minc_input_colour_max_dimension_size(
    minc_input_options  *options,
    int                 size );

VIOAPI  void  set_minc_input_colour_indices(
    minc_input_options  *options,
    int                 indices[4] );

VIOAPI  void  set_minc_input_user_real_range(
    minc_input_options  *options,
    double              minimum,
    double              maximum );

VIOAPI  VIO_Status  start_volume_input(
    VIO_STR              filename,
    int                  n_dimensions,
    VIO_STR              dim_names[],
    nc_type              volume_nc_data_type,
    VIO_BOOL             volume_signed_flag,
    VIO_Real             volume_voxel_min,
    VIO_Real             volume_voxel_max,
    VIO_BOOL             create_volume_flag,
    VIO_Volume           *volume,
    minc_input_options   *options,
    volume_input_struct  *input_info );

VIOAPI  void  delete_volume_input(
    volume_input_struct   *input_info );

VIOAPI  VIO_BOOL input_more_of_volume(
    VIO_Volume            volume,
    volume_input_struct   *input_info,
    VIO_Real              *fraction_done );

VIOAPI  void  cancel_volume_input(
    VIO_Volume            volume,
    volume_input_struct   *input_info );

VIOAPI  VIO_Status  input_volume(
    VIO_STR              filename,
    int                  n_dimensions,
    VIO_STR              dim_names[],
    nc_type              volume_nc_data_type,
    VIO_BOOL             volume_signed_flag,
    VIO_Real             volume_voxel_min,
    VIO_Real             volume_voxel_max,
    VIO_BOOL             create_volume_flag,
    VIO_Volume           *volume,
    minc_input_options   *options );

VIOAPI  Minc_file   get_volume_input_minc_file(
    volume_input_struct   *volume_input );

VIOAPI   void   create_empty_multidim_array(
    VIO_multidim_array  *array,
    int             n_dimensions,
    VIO_Data_types  data_type );

VIOAPI  VIO_Data_types  get_multidim_data_type(
    VIO_multidim_array       *array );

VIOAPI  void  set_multidim_data_type(
    VIO_multidim_array   *array,
    VIO_Data_types       data_type );

VIOAPI  int  get_type_size(
    VIO_Data_types   type );

VIOAPI  void  get_type_range(
    VIO_Data_types   type,
    VIO_Real         *min_value,
    VIO_Real         *max_value );

VIOAPI VIO_BOOL set_multidim_n_dimensions(
    VIO_multidim_array *array,
    int                n_dimensions);

VIOAPI  void  set_multidim_sizes(
    VIO_multidim_array   *array,
    int              sizes[] );

VIOAPI  void  get_multidim_sizes(
    VIO_multidim_array   *array,
    int              sizes[] );

VIOAPI  VIO_BOOL multidim_array_is_alloced(
    VIO_multidim_array   *array );

VIOAPI  void  alloc_multidim_array(
    VIO_multidim_array   *array );

VIOAPI   void   create_multidim_array(
    VIO_multidim_array  *array,
    int             n_dimensions,
    int             sizes[],
    VIO_Data_types  data_type );

VIOAPI  void  delete_multidim_array(
    VIO_multidim_array   *array );

VIOAPI  int  get_multidim_n_dimensions(
    VIO_multidim_array   *array );

VIOAPI  void  copy_multidim_data_reordered(
    int                 type_size,
    void                *void_dest_ptr,
    int                 n_dest_dims,
    int                 dest_sizes[],
    void                *void_src_ptr,
    int                 n_src_dims,
    int                 src_sizes[],
    int                 counts[],
    int                 to_dest_index[],
    VIO_BOOL            use_src_order );

VIOAPI  void  copy_multidim_reordered(
    VIO_multidim_array  *dest,
    int                 dest_ind[],
    VIO_multidim_array  *src,
    int                 src_ind[],
    int                 counts[],
    int                 to_dest_index[] );

VIOAPI  Minc_file  initialize_minc_output(
    VIO_STR                filename,
    int                    n_dimensions,
    VIO_STR                dim_names[],
    int                    sizes[],
    nc_type                file_nc_data_type,
    VIO_BOOL               file_signed_flag,
    VIO_Real               file_voxel_min,
    VIO_Real               file_voxel_max,
    VIO_General_transform  *voxel_to_world_transform,
    VIO_Volume             volume_to_attach,
    minc_output_options    *options );

VIOAPI  Minc_file  initialize_minc2_output(
    VIO_STR                filename,
    int                    n_dimensions,
    VIO_STR                dim_names[],
    int                    sizes[],
    nc_type                file_nc_data_type,
    VIO_BOOL               file_signed_flag,
    VIO_Real               file_voxel_min,
    VIO_Real               file_voxel_max,
    VIO_General_transform  *voxel_to_world_transform,
    VIO_Volume             volume_to_attach,
    minc_output_options    *options );

VIOAPI  VIO_Status  copy_auxiliary_data_from_minc_file(
    Minc_file    file,
    VIO_STR      filename,
    VIO_STR      history_string );

VIOAPI  VIO_Status  copy_auxiliary_data_from_minc2_file(
    Minc_file    file,
    VIO_STR      filename,
    VIO_STR      history_string );

VIOAPI  VIO_Status  copy_auxiliary_data_from_open_minc_file(
    Minc_file    file,
    int          src_cdfid,
    VIO_STR      history_string );

VIOAPI  VIO_Status  copy_auxiliary_data_from_open_minc2_file(
    Minc_file    file,
    mihandle_t   minc_id,
    VIO_STR      history_string );

VIOAPI  VIO_Status  add_minc_history(
    Minc_file   file,
    VIO_STR     history_string );

VIOAPI  VIO_Status  add_minc2_history(
    Minc_file   file,
    VIO_STR     history_string );

VIOAPI  VIO_Status  set_minc_output_random_order(
    Minc_file   file );

VIOAPI  VIO_Status  set_minc2_output_random_order(
    Minc_file   file );

VIOAPI  VIO_Status  output_minc_hyperslab(
    Minc_file           file,
    VIO_Data_types      data_type,
    int                 n_array_dims,
    int                 array_sizes[],
    void                *array_data_ptr,
    int                 to_array[],
    int                 file_start[],
    int                 file_count[] );

VIOAPI  VIO_Status  output_volume_to_minc_file_position(
    Minc_file     file,
    VIO_Volume    volume,
    int           volume_count[],
    long          file_start[] );

VIOAPI  VIO_Status  output_minc_volume(
    Minc_file   file );

VIOAPI  VIO_Status  close_minc_output(
    Minc_file   file );

VIOAPI  VIO_Status  output_minc2_volume(
    Minc_file   file );

VIOAPI  VIO_Status  close_minc2_output(
    Minc_file   file );

VIOAPI  void  set_default_minc_output_options(
    minc_output_options  *options           );

VIOAPI  void  copy_minc_output_options(
    minc_output_options  *src,
    minc_output_options  *dest );

VIOAPI  void  delete_minc_output_options(
    minc_output_options  *options           );

VIOAPI  void  set_minc_output_dimensions_order(
    minc_output_options  *options,
    int                  n_dimensions,
    VIO_STR              dimension_names[] );

VIOAPI  void  set_minc_output_real_range(
    minc_output_options  *options,
    VIO_Real                 real_min,
    VIO_Real                 real_max );

VIOAPI  void  set_minc_output_use_volume_starts_and_steps_flag(
    minc_output_options  *options,
    VIO_BOOL             flag );

VIOAPI  VIO_Status   get_file_dimension_names(
    VIO_STR   filename,
    int       *n_dims,
    VIO_STR   *dim_names[] );

VIOAPI  VIO_STR  *create_output_dim_names(
    VIO_Volume            volume,
    VIO_STR               original_filename,
    minc_output_options   *options,
    int                   file_sizes[] );

VIOAPI  VIO_Status   copy_volume_auxiliary_and_history(
    Minc_file   minc_file,
    VIO_STR      filename,
    VIO_STR      original_filename,
    VIO_STR      history );

VIOAPI  VIO_Status  output_modified_volume(
    VIO_STR               filename,
    nc_type               file_nc_data_type,
    VIO_BOOL              file_signed_flag,
    VIO_Real              file_voxel_min,
    VIO_Real              file_voxel_max,
    VIO_Volume            volume,
    VIO_STR               original_filename,
    VIO_STR               history,
    minc_output_options   *options );

VIOAPI  VIO_Status  output_volume(
    VIO_STR               filename,
    nc_type               file_nc_data_type,
    VIO_BOOL              file_signed_flag,
    VIO_Real              file_voxel_min,
    VIO_Real              file_voxel_max,
    VIO_Volume            volume,
    VIO_STR               history,
    minc_output_options   *options );

VIOAPI  void  convert_values_to_voxels(
    VIO_Volume   volume,
    int          n_voxels,
    VIO_Real     values[],
    VIO_Real     voxels[] );

VIOAPI  void  set_volume_value_hyperslab(
    VIO_Volume   volume,
    int      v0,
    int      v1,
    int      v2,
    int      v3,
    int      v4,
    int      n0,
    int      n1,
    int      n2,
    int      n3,
    int      n4,
    VIO_Real     values[] );

VIOAPI  void  set_volume_value_hyperslab_5d(
    VIO_Volume   volume,
    int      v0,
    int      v1,
    int      v2,
    int      v3,
    int      v4,
    int      n0,
    int      n1,
    int      n2,
    int      n3,
    int      n4,
    VIO_Real     values[] );

VIOAPI  void  set_volume_value_hyperslab_4d(
    VIO_Volume   volume,
    int      v0,
    int      v1,
    int      v2,
    int      v3,
    int      n0,
    int      n1,
    int      n2,
    int      n3,
    VIO_Real     values[] );

VIOAPI  void  set_volume_value_hyperslab_3d(
    VIO_Volume   volume,
    int      v0,
    int      v1,
    int      v2,
    int      n0,
    int      n1,
    int      n2,
    VIO_Real     values[] );

VIOAPI  void  set_volume_value_hyperslab_2d(
    VIO_Volume   volume,
    int      v0,
    int      v1,
    int      n0,
    int      n1,
    VIO_Real     values[] );

VIOAPI  void  set_volume_value_hyperslab_1d(
    VIO_Volume   volume,
    int      v0,
    int      n0,
    VIO_Real     values[] );

VIOAPI  void  set_volume_voxel_hyperslab_5d(
    VIO_Volume   volume,
    int      v0,
    int      v1,
    int      v2,
    int      v3,
    int      v4,
    int      n0,
    int      n1,
    int      n2,
    int      n3,
    int      n4,
    VIO_Real     values[] );

VIOAPI  void  set_volume_voxel_hyperslab_4d(
    VIO_Volume   volume,
    int      v0,
    int      v1,
    int      v2,
    int      v3,
    int      n0,
    int      n1,
    int      n2,
    int      n3,
    VIO_Real     values[] );

VIOAPI  void  set_volume_voxel_hyperslab_3d(
    VIO_Volume   volume,
    int      v0,
    int      v1,
    int      v2,
    int      n0,
    int      n1,
    int      n2,
    VIO_Real     values[] );

VIOAPI  void  set_volume_voxel_hyperslab_2d(
    VIO_Volume   volume,
    int      v0,
    int      v1,
    int      n0,
    int      n1,
    VIO_Real     values[] );

VIOAPI  void  set_volume_voxel_hyperslab_1d(
    VIO_Volume   volume,
    int      v0,
    int      n0,
    VIO_Real     values[] );

VIOAPI  void  set_volume_voxel_hyperslab(
    VIO_Volume   volume,
    int      v0,
    int      v1,
    int      v2,
    int      v3,
    int      v4,
    int      n0,
    int      n1,
    int      n2,
    int      n3,
    int      n4,
    VIO_Real     voxels[] );

VIOAPI  void  set_n_bytes_cache_threshold(
    int  threshold );

VIOAPI  int  get_n_bytes_cache_threshold( void );

VIOAPI  void  set_default_max_bytes_in_cache(
    int   max_bytes );

VIOAPI  int  get_default_max_bytes_in_cache( void );

VIOAPI  void  set_default_cache_block_sizes(
    int                      block_sizes[] );

VIOAPI  void  set_cache_block_sizes_hint(
    VIO_Cache_block_size_hints  hint );

VIOAPI  void  initialize_volume_cache(
    VIO_volume_cache_struct   *cache,
    VIO_Volume                volume );

VIOAPI  VIO_BOOL volume_cache_is_alloced(
    VIO_volume_cache_struct   *cache );

VIOAPI  void  flush_volume_cache(
    VIO_Volume                volume );

VIOAPI  void  delete_volume_cache(
    VIO_volume_cache_struct   *cache,
    VIO_Volume                volume );

VIOAPI  void  set_volume_cache_block_sizes(
    VIO_Volume    volume,
    int           block_sizes[] );

VIOAPI  void  set_volume_cache_size(
    VIO_Volume    volume,
    int           max_memory_bytes );

VIOAPI  void  set_cache_output_volume_parameters(
    VIO_Volume                  volume,
    VIO_STR                     filename,
    nc_type                     file_nc_data_type,
    VIO_BOOL                    file_signed_flag,
    VIO_Real                    file_voxel_min,
    VIO_Real                    file_voxel_max,
    VIO_STR                     original_filename,
    VIO_STR                     history,
    minc_output_options         *options )
;

VIOAPI  void  open_cache_volume_input_file(
    VIO_volume_cache_struct   *cache,
    VIO_Volume                volume,
    VIO_STR                filename,
    minc_input_options    *options );

VIOAPI  void  cache_volume_range_has_changed(
    VIO_Volume   volume );

VIOAPI  void  set_cache_volume_file_offset(
    VIO_volume_cache_struct *cache,
    VIO_Volume            volume,
    long                  file_offset[] );

VIOAPI  VIO_Real  get_cached_volume_voxel(
    VIO_Volume   volume,
    int      x,
    int      y,
    int      z,
    int      t,
    int      v );

VIOAPI  void  set_cached_volume_voxel(
    VIO_Volume   volume,
    int      x,
    int      y,
    int      z,
    int      t,
    int      v,
    VIO_Real     value );

VIOAPI  VIO_BOOL cached_volume_has_been_modified(
    VIO_volume_cache_struct  *cache );

VIOAPI  VIO_BOOL volume_is_cached(
    VIO_Volume  volume );

VIOAPI  void   set_volume_cache_debugging(
    VIO_Volume   volume,
    int      output_every );

VIOAPI  VIO_STR  *get_default_dim_names(
    int    n_dimensions );

VIOAPI  VIO_BOOL convert_dim_name_to_spatial_axis(
    VIO_STR  name,
    int     *axis );

VIOAPI mitype_t vio_type_to_minc2_type(
    VIO_Data_types  vio_data_type);

VIOAPI VIO_Data_types  minc2_type_to_vio_type(
    mitype_t    minc_data_type);

VIOAPI mitype_t nc_type_to_minc2_type(
    nc_type  nc_data_type,
    VIO_BOOL signed_flag );

VIOAPI VIO_Data_types  minc2_type_to_vio_type(
    mitype_t    minc_data_type);


VIOAPI   VIO_Volume   create_volume(
    int         n_dimensions,
    VIO_STR     dimension_names[],
    nc_type     nc_data_type,
    VIO_BOOL    signed_flag,
    VIO_Real    voxel_min,
    VIO_Real    voxel_max );

VIOAPI  void  set_volume_type(
    VIO_Volume   volume,
    nc_type      nc_data_type,
    VIO_BOOL     signed_flag,
    VIO_Real     voxel_min,
    VIO_Real     voxel_max );

VIOAPI  void  set_volume_type2(
    VIO_Volume   volume,
    mitype_t     minc_data_type,
    VIO_Real     voxel_min,
    VIO_Real     voxel_max );

VIOAPI  void  set_volume_labels(
    VIO_Volume   volume,
    VIO_BOOL     is_labels );

VIOAPI  VIO_BOOL  get_volume_labels(
    VIO_Volume   volume );

VIOAPI  nc_type  get_volume_nc_data_type(
    VIO_Volume   volume,
    VIO_BOOL     *signed_flag );

VIOAPI  mitype_t  get_volume_minc2_data_type(
    VIO_Volume   volume);

VIOAPI  VIO_Data_types  get_volume_data_type(
    VIO_Volume       volume );

VIOAPI  void  set_rgb_volume_flag(
    VIO_Volume   volume,
    VIO_BOOL     flag );

VIOAPI  VIO_BOOL is_an_rgb_volume(
    VIO_Volume   volume );

VIOAPI  void  alloc_volume_data(
    VIO_Volume   volume );

VIOAPI  VIO_BOOL volume_is_alloced(
    VIO_Volume   volume );

VIOAPI  void  free_volume_data(
    VIO_Volume   volume );

VIOAPI  void  delete_volume(
    VIO_Volume   volume );

VIOAPI  int  get_volume_n_dimensions(
    VIO_Volume volume );

VIOAPI  VIO_BOOL  set_volume_n_dimensions(
    VIO_Volume volume, int n_dimensions);

VIOAPI  void  get_volume_sizes(
    VIO_Volume volume,
    int      sizes[] );

VIOAPI  void  set_volume_sizes(
    VIO_Volume   volume,
    int          sizes[] );

VIOAPI  size_t  get_volume_total_n_voxels(
    VIO_Volume    volume );

VIOAPI  void  compute_world_transform(
    int                 spatial_axes[VIO_N_DIMENSIONS],
    VIO_Real            separations[],
    VIO_Real            direction_cosines[][VIO_N_DIMENSIONS],
    VIO_Real            starts[],
    VIO_General_transform   *world_transform );

VIOAPI  void  convert_transform_to_starts_and_steps(
    VIO_General_transform  *transform,
    int                n_volume_dimensions,
    VIO_Real           step_signs[],
    int                spatial_axes[],
    VIO_Real           starts[],
    VIO_Real           steps[],
    VIO_Real           dir_cosines[][VIO_N_DIMENSIONS] );

VIOAPI  void  set_voxel_to_world_transform(
    VIO_Volume             volume,
    VIO_General_transform  *transform );

VIOAPI  VIO_General_transform  *get_voxel_to_world_transform(
    VIO_Volume   volume );

VIOAPI  VIO_STR  *get_volume_dimension_names(
    VIO_Volume   volume );

VIOAPI  void  delete_dimension_names(
    VIO_Volume   volume,
    VIO_STR   dimension_names[] );

VIOAPI  VIO_STR  get_volume_space_type(
    VIO_Volume   volume );

VIOAPI  void  set_volume_space_type(
    VIO_Volume   volume,
    VIO_STR   name );

VIOAPI  void  get_volume_separations(
    VIO_Volume   volume,
    VIO_Real     separations[] );

VIOAPI  void  set_volume_separations(
    VIO_Volume   volume,
    VIO_Real     separations[] );

VIOAPI  void  set_volume_starts(
    VIO_Volume  volume,
    VIO_Real    starts[] );

VIOAPI  void  get_volume_starts(
    VIO_Volume  volume,
    VIO_Real    starts[] );

VIOAPI  void  set_volume_direction_unit_cosine(
    VIO_Volume   volume,
    int      axis,
    VIO_Real     dir[] );

VIOAPI  void  set_volume_direction_cosine(
    VIO_Volume   volume,
    int      axis,
    VIO_Real     dir[] );

VIOAPI  void  get_volume_direction_cosine(
    VIO_Volume   volume,
    int      axis,
    VIO_Real     dir[] );

/* These next 5 functions may be called by other modules in the library, 
 * but they are not truly public. That is why they are not declared VIOAPI.
 */
VIO_BOOL is_volume_dimension_irregular(VIO_Volume, int);
long get_volume_irregular_starts(VIO_Volume, int, long, VIO_Real *);
long get_volume_irregular_widths(VIO_Volume, int, long, VIO_Real *);
long set_volume_irregular_starts(VIO_Volume, int, long, VIO_Real *);
long set_volume_irregular_widths(VIO_Volume, int, long, VIO_Real *);

VIOAPI  VIO_Real nonspatial_voxel_to_world(VIO_Volume, int, int);
VIOAPI  long nonspatial_world_to_voxel(VIO_Volume, int, VIO_Real);

VIOAPI  void  set_volume_translation(
    VIO_Volume  volume,
    VIO_Real    voxel[],
    VIO_Real    world_space_voxel_maps_to[] );

VIOAPI  void  get_volume_translation(
    VIO_Volume  volume,
    VIO_Real    voxel[],
    VIO_Real    world_space_voxel_maps_to[] );

VIOAPI  void  reorder_voxel_to_xyz(
    VIO_Volume   volume,
    VIO_Real     voxel[],
    VIO_Real     xyz[] );

VIOAPI  void  reorder_xyz_to_voxel(
    VIO_Volume   volume,
    VIO_Real     xyz[],
    VIO_Real     voxel[] );

VIOAPI  void  convert_voxel_to_world(
    VIO_Volume   volume,
    VIO_Real     voxel[],
    VIO_Real     *x_world,
    VIO_Real     *y_world,
    VIO_Real     *z_world );

VIOAPI  void  convert_3D_voxel_to_world(
    VIO_Volume   volume,
    VIO_Real     voxel1,
    VIO_Real     voxel2,
    VIO_Real     voxel3,
    VIO_Real     *x_world,
    VIO_Real     *y_world,
    VIO_Real     *z_world );

VIOAPI  void  convert_voxel_normal_vector_to_world(
    VIO_Volume          volume,
    VIO_Real            voxel_vector[],
    VIO_Real            *x_world,
    VIO_Real            *y_world,
    VIO_Real            *z_world );

VIOAPI  void  convert_voxel_vector_to_world(
    VIO_Volume          volume,
    VIO_Real            voxel_vector[],
    VIO_Real            *x_world,
    VIO_Real            *y_world,
    VIO_Real            *z_world );

VIOAPI  void  convert_world_vector_to_voxel(
    VIO_Volume          volume,
    VIO_Real            x_world,
    VIO_Real            y_world,
    VIO_Real            z_world,
    VIO_Real            voxel_vector[] );

VIOAPI  void  convert_world_to_voxel(
    VIO_Volume   volume,
    VIO_Real     x_world,
    VIO_Real     y_world,
    VIO_Real     z_world,
    VIO_Real     voxel[] );

VIOAPI  void  convert_3D_world_to_voxel(
    VIO_Volume   volume,
    VIO_Real     x_world,
    VIO_Real     y_world,
    VIO_Real     z_world,
    VIO_Real     *voxel1,
    VIO_Real     *voxel2,
    VIO_Real     *voxel3 );

VIOAPI  VIO_Real  get_volume_voxel_min(
    VIO_Volume   volume );

VIOAPI  VIO_Real  get_volume_voxel_max(
    VIO_Volume   volume );

VIOAPI  void  get_volume_voxel_range(
    VIO_Volume     volume,
    VIO_Real       *voxel_min,
    VIO_Real       *voxel_max );

VIOAPI  void  set_volume_voxel_range(
    VIO_Volume   volume,
    VIO_Real     voxel_min,
    VIO_Real     voxel_max );

VIOAPI  void  get_volume_real_range(
    VIO_Volume     volume,
    VIO_Real       *min_value,
    VIO_Real       *max_value );

VIOAPI  VIO_Real  get_volume_real_min(
    VIO_Volume     volume );

VIOAPI  VIO_Real  get_volume_real_max(
    VIO_Volume     volume );

VIOAPI  void  set_volume_real_range(
    VIO_Volume   volume,
    VIO_Real     real_min,
    VIO_Real     real_max );

VIOAPI  VIO_Volume   copy_volume_definition_no_alloc(
    VIO_Volume   volume,
    nc_type  nc_data_type,
    VIO_BOOL signed_flag,
    VIO_Real voxel_min,
    VIO_Real voxel_max );

VIOAPI  VIO_Volume   copy_volume_definition(
    VIO_Volume   volume,
    nc_type  nc_data_type,
    VIO_BOOL signed_flag,
    VIO_Real voxel_min,
    VIO_Real voxel_max );

VIOAPI  VIO_Volume  copy_volume(
    VIO_Volume   volume );

VIOAPI  VIO_Volume  copy_volume_new_type(
    VIO_Volume volume, nc_type nc_data_type, VIO_BOOL signed_flag );

VIOAPI  VIO_Status  grid_transform_point(
    VIO_General_transform   *transform,
    VIO_Real                x,
    VIO_Real                y,
    VIO_Real                z,
    VIO_Real                *x_transformed,
    VIO_Real                *y_transformed,
    VIO_Real                *z_transformed );

VIOAPI  VIO_Status  grid_inverse_transform_point_with_input_steps(
    VIO_General_transform   *transform,
    VIO_Real                x,
    VIO_Real                y,
    VIO_Real                z,
    VIO_Real                *input_volume_steps,
    VIO_Real                *x_transformed,
    VIO_Real                *y_transformed,
    VIO_Real                *z_transformed );
    
VIOAPI  VIO_Status  grid_inverse_transform_point(
    VIO_General_transform   *transform,
    VIO_Real                x,
    VIO_Real                y,
    VIO_Real                z,
    VIO_Real                *x_transformed,
    VIO_Real                *y_transformed,
    VIO_Real                *z_transformed );

#endif /*VOL_IO_PROTOTYPES_H*/
