These notes are with respect to the AFNI GIFTI C API.

- This API is built on top of expat, and is released into the public domain.

- The main functions are:

    gifti_read_image, gifti_write_image, gifti_free_image

- The exported functions in gifti_io.h are intended for users of this
  API, particularly those prototypes listed first.
- The gifti_xml functions are not intended for external use.

- Taking pointers from the gifti_image structure should be okay (providing
  they are then set to NULL), but it may be less confusing to copy data out.
- To save memory, it is suggested to steal the darray[k]->data pointers, and
  set them to NULL.

----------------------------------------------------------------------
Speeds: (last checked in version 0.7)

    Since I/O speed was a question when deciding whether to use XML for
    GIFTI, read times (in seconds) from a mac G5 are provided.

    (no b64 error checking)         (with b64 error counting and skipping)

    ascii     base64    base64gz    base64    base64gz  surface name
    -----     ------    --------    ------    --------  ------------
     0.05       0.00     0.00        0.01     0.00      aoantonym
     0.37       0.08     0.12        0.10     0.13      pial
     7.20       0.95     0.75        1.14     0.79      time_series
----------------------------------------------------------------------

todo:
  - on alloc failure, propogate error up and return NULL
  - accessor functions
  - indexed reading
  - test doxygen comments

  - #ifdef  __cplusplus
  - select node list
  - display all valid attributes?  DTD?  valid attribute values given the name?
            known metadata?

  - gifti_remove_gim_metadata()
  - gifti_remove_DA_metadata()
  - gifti_remove_all_metadata()
  - gifti_MD_delete(name)
  - post XML read: validate gifti structure
        - check for UNKNOWN attributes
                - done?? "invalid darray XX"
        - check that MD names are unique
