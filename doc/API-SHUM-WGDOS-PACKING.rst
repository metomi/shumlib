API Reference: shum_wgdos_packing
---------------------------------

Fortran Functions/Subroutines
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

``get_shum_wgdos_packing_version``
''''''''''''''''''''''''''''''''''

All Shumlib libraries expose a module and function named in this format; it 
allows access to the Shumlib version number used when compiling the library.

    **Available via module**
        ``f_shum_wgdos_packing_version_mod``

    **Syntax**
        ``version = get_shum_wgdos_packing_version()``

    **Returns**
        ``version (INTEGER)``
            Shumlib version number, in format ``YYYYMMX`` (the 4-digit year
            followed by 2-digit month and an additional digit signifying the
            release count within that specified month).

``f_shum_read_wgdos_header``
''''''''''''''''''''''''''''

This function extracts several values from the WGDOS file header of a packed
data array. The main reason for this is to query the packed data for appropriate
sizes to use for allocating the return arrays of the unpacking routine below.

    **Available via module**
        ``f_shum_wgdos_packing_mod``

    **Syntax**
        ``status = f_shum_read_wgdos_header(packed_field, num_words, accuracy, cols, rows, message)``

    **Inputs**
        ``packed_field (32-bit INTEGER)``
            The packed field data, a 1D array.
    
    **Outputs**
        ``num_words (INTEGER)``
            Total number of (32-bit) words in packed array.
        ``accuracy (INTEGER)``
            Packing accuracy (power of 2).
        ``cols (INTEGER)``
            Number of columns (row length) in unpacked field.
        ``rows (INTEGER)``
            Number of rows in unpacked field.

    **Input & Output**
        ``message (CHARACTER(LEN=*))``
            Error message buffer.

    **Return Value**
        ``status (INTEGER)``
            Exit status; ``0`` means success, anything else means an error has
            occurred and in that case the ``message`` argument will contain 
            information about the problem.

    **Notes**
        The ``INTEGER`` arguments here (except where explicitly specified above)
        must be either *all* 32-bit or *all* 64-bit (but *not* a mixture of the
        two).  Their "true" values in the file header are actually 32-bit
        (``num_words`` and ``accuracy``) and 16-bit (for ``cols`` and ``rows``).


``f_shum_wgdos_pack``
'''''''''''''''''''''

This function accepts an unpacked field and packs it to a given accuracy using
the WGDOS algorithm, masking out any points containing a defined "missing data"
value in the process.

    **Available via module**
        ``f_shum_wgdos_packing_mod``

    **Syntax**
        ``status = f_shum_wgdos_pack(field, [stride,] accuracy, rmdi, packed_field, [n_packed_words,] message)``

    **Inputs**
        ``field (64-bit REAL)``
            The unpacked field data (which may be either a 1D or 2D array).
        ``stride (INTEGER)``
            If ``field`` is 1D this must be provided to indicate the stride (or 
            row length) for the packing to use.
        ``accuracy (INTEGER)``
            Packing accuracy (power of 2).
        ``rmdi (REAL)``
            Missing data indicator value; any values in the field equal to this
            value will be considered (and packed as) missing values.

    **Outputs**
        ``packed_field (32-bit INTEGER)``
            The WGDOS packed field; a 1D array which may either be
            ``ALLOCATABLE`` (but not allocated) *or* any array (``ALLOCATABLE``
            or otherwise) which will be filled by the packed data. An
            unallocated array will be allocated to the exact size of the packed
            field by the function, otherwise the array must be large enough to
            hold the packed data.
        ``n_packed_words (INTEGER)``
            This must be provided unless ``packed_field`` is an unallocated
            ``ALLOCATABLE``. It will receive the number of elements of
            ``packed_field`` containing the packed data (i.e. 
            ``packed_field(1:n_packed_words)`` which may be less than the full
            extent of the array).

    **Input & Output**
        ``message (CHARACTER(LEN=*))``
            Error message buffer.

    **Return Value**
        ``status (INTEGER)``
            Exit status; ``0`` means success, anything else means an error has
            occurred and in that case the ``message`` argument will contain 
            information about the problem.

    **Notes**
        The arguments here (except where explicitly specified above) may be
        either *all* 32-bit or *all* 64-bit (but *not* a mixture of the two).


``f_shum_wgdos_unpack``
'''''''''''''''''''''''

This function accepts an array of WGDOS packed data and unpacks it, populating
any missing points with a given value and returning the unpacked array.

    **Available via module**
        ``f_shum_wgdos_packing_mod``

    **Syntax**
        ``status = f_shum_wgdos_unpack(packed_field, rmdi, field, [stride,] message)``

    **Inputs**
        ``packed_field (32-bit INTEGER)``
            WGDOS packed field data; a 1D array.
        ``rmdi (REAL)``
            Missing data indicator value; any values in the field which are
            tagged as containing missing values will be replaced by this value.

    **Outputs**
        ``field (64-bit REAL)``
            The unpacked field data (which may be either a 1D or 2D array).
        ``stride (INTEGER)``
            If ``field`` is 1D this must be provided to indicate the stride (or 
            row length) for the unpacking to use.

    **Input & Output**
        ``message (CHARACTER(LEN=*))``
            Error message buffer.

    **Return Value**
        ``status (INTEGER)``
            Exit status; ``0`` means success, anything else means an error has
            occurred and in that case the ``message`` argument will contain 
            information about the problem.

    **Notes**
        The arguments here (except where explicitly specified above) may be
        either *all* 32-bit or *all* 64-bit (but *not* a mixture of the two).

C Functions
%%%%%%%%%%%

``get_shum_wgdos_packing_version``
''''''''''''''''''''''''''''''''''

All Shumlib libraries expose a function named in this format; it allows access
to the Shumlib version number used when compiling the library.

    **Required header/s**
        ``c_shum_wgdos_packing_version.h``

    **Syntax**
        ``version = get_shum_wgdos_packing_version()``

    **Returns**
        ``version (int)``
            Shumlib version number, in format ``YYYYMMX`` (the 4-digit year
            followed by 2-digit month and an additional digit signifying the
            release count within that specified month).

``c_shum_read_wgdos_header``
''''''''''''''''''''''''''''

This function extracts several values from the WGDOS file header of a packed
data array. The main reason for this is to query the packed data for appropriate
sizes to use for allocating the return arrays of the unpacking routine below.

    **Required header/s**
        ``c_shum_wgdos_packing.h``

    **Syntax**
        ``c_shum_read_wgdos_header(bytes_in, num_words, accuracy, cols, rows, message, message_len)``

    **Arguments**
        ``bytes_in (char*)``
            The packed field data as an array of bytes.
        ``num_words (int64_t*)``
            Total number of (32-bit) words in packed array.
        ``accuracy (int64_t*)``
            Packing accuracy (power of 2).
        ``cols (int64_t*)``
            Number of columns (row length) in unpacked field.
        ``rows (int64_t*)``
            Number of rows in unpacked field.
        ``message (char*)``
            Error message buffer.
        ``message_len (int64_t*)``
            Length of error message buffer.

    **Return Value**
        ``(int64_t)``
            Exit status; ``0`` means success, anything else means an error has
            occurred and in that case the ``message`` argument will contain 
            information about the problem.


``c_shum_wgdos_pack``
'''''''''''''''''''''

This function accepts an unpacked field and packs it to a given accuracy using
the WGDOS algorithm, masking out any points containing a defined "missing data"
value in the process.

    **Required header/s**
        ``c_shum_wgdos_packing.h``

    **Syntax**
        ``c_shum_wgdos_pack(field, cols, rows, accuracy, rmdi, packed_field, len_comp, num_words, message, message_len)``

    **Arguments**
        ``field (double*)``
            The unpacked field data (may be 1D or 2D).
        ``cols (int64_t*)``
            Number of columns (row length) in unpacked field.
        ``rows (int64_t*)``
            Number of rows in unpacked field.
        ``accuracy (int64_t*)``
            Packing accuracy (power of 2).
        ``rmdi (double*)``
            Missing data indicator value; any values in the field equal to this
            value will be considered (and packed as) missing values.
        ``packed_field (int32_t*)``
            For returning the WGDOS packed field.
        ``len_comp (int64_t*)``
            Length of ``packed_field``.
        ``num_words (int64_t*)``
            The length into ``packed_field`` that contains the packed data.
        ``message (char*)``
            Error message buffer.
        ``message_len (int64_t*)``
            Length of error message buffer.

    **Return Value**
        ``status (int64_t)``
            Exit status; ``0`` means success, anything else means an error has
            occurred and in that case the ``message`` argument will contain 
            information about the problem.


``c_shum_wgdos_unpack``
'''''''''''''''''''''''

This function accepts an array of WGDOS packed data and unpacks it, populating
any missing points with a given value and returning the unpacked array.

    **Require header/s**
        ``c_shum_wgdos_packing.h``

    **Syntax**
        ``c_shum_wgdos_unpack(packed_field, len_comp, cols, rows, rmdi, field, message, message_len)``

    **Arguments**
        ``packed_field (int32_t*)``
            The WGDOS packed field.
        ``len_comp (int64_t*)``
            Length of ``packed_field``.
        ``cols (int64_t*)``
            Number of columns (row length) in unpacked field.
        ``rows (int64_t*)``
            Number of rows in unpacked field.
        ``rmdi (double*)``
            Missing data indicator value; any values in the field equal to this
            value will be considered (and packed as) missing values.
        ``field (double*)``
            The unpacked field data (may be 1D or 2D).
        ``message (char*)``
            Error message buffer.
        ``message_len (int64_t*)``
            Length of error message buffer.

    **Return Value**
        ``status (int64_t)``
            Exit status; ``0`` means success, anything else means an error has
            occurred and in that case the ``message`` argument will contain 
            information about the problem.

