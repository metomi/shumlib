API Reference: shum_fieldsfile
------------------------------

Fortran Functions/Subroutines
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

``get_shum_fieldsfile_version``
''''''''''''''''''''''''''''''''''

All Shumlib libraries expose a module and function named in this format; it 
allows access to the Shumlib version number used when compiling the library.

    **Available via module**
        ``f_shum_fieldsfile_version_mod``

    **Syntax**
        ``version = get_shum_fieldsfile_version()``

    **Returns**
        ``version (64-bit INTEGER)``
            Shumlib version number, in format ``YYYYMMX`` (the 4-digit year
            followed by 2-digit month and an additional digit signifying the
            release count within that specified month).

``f_shum_open_file``
''''''''''''''''''''

This function is used to open an existing FieldsFile (or variant thereof) - see
the routine ``f_shum_create_file`` for creating a new file.  

    **Available via module**
        ``f_shum_fieldsfile_mod``

    **Syntax**
        ``status = f_shum_open_file(filename, ff_id, message)``

    **Inputs**
        ``filename (CHARACTER)``
            The path to the filename (which must already exist).

    **Outputs**
        ``ff_id (64-bit INTEGER)``
            A value which acts as an identifier for the file, the value 
            itself is arbitrary but must be passed to all other operations
            which need to reference the file opened here.

    **Input & Output**
        ``message (CHARACTER(LEN=*))``
            Error message buffer.

    **Return Value**
        ``status (64-bit INTEGER)``
            Exit status; ``0`` means success, anything else means an error has
            occurred and in that case the ``message`` argument will contain 
            information about the problem.

``f_shum_create_file``
''''''''''''''''''''''

This function is used to create a new FieldsFile (or variant thereof) - see
the routine ``f_shum_open_file`` for opening an existing file.

    **Available via module**
        ``f_shum_fieldsfile_mod``

    **Syntax**
        ``status = f_shum_create_file(filename, n_lookups, ff_id, message)``

    **Inputs**
        ``filename (CHARACTER)``
            The path to the filename (which will be created).
        ``n_lookups (64-bit INTEGER)``
            The space reserved for the lookup table in this file will be set
            to be large enough to hold this number of lookup entries.  This
            cannot be changed once set for the lifetime of the file.
        ``overwrite (optional LOGICAL)``
            Determines whether the presence of an existing file with the given
            name should be treated as an error or not. If not provided the
            default behaviour is to allow overwriting an existing file.

    **Outputs**
        ``ff_id (64-bit INTEGER)``
            A value which acts as an identifier for the file, the value 
            itself is arbitrary but must be passed to all other operations
            which need to reference the file opened here.

    **Input & Output**
        ``message (CHARACTER(LEN=*))``
            Error message buffer.

    **Return Value**
        ``status (64-bit INTEGER)``
            Exit status; ``0`` means success, anything else means an error has
            occurred and in that case the ``message`` argument will contain 
            information about the problem.

``f_shum_close_file``
'''''''''''''''''''''

This function closes access to a previously opened file.

    **Available via module**
        ``f_shum_fieldsfile_mod``

    **Syntax**
        ``status = f_shum_close_file(ff_id, message)``

    **Inputs**
        ``ff_id (64-bit INTEGER)``
            The identifier for the file; this must be a value returned by an
            earlier call to ``f_shum_open_file`` or ``f_shum_create_file``.

    **Input & Output**
        ``message (CHARACTER(LEN=*))``
            Error message buffer.

    **Return Value**
        ``status (64-bit INTEGER)``
            Exit status; ``0`` means success, anything else means an error has
            occurred and in that case the ``message`` argument will contain 
            information about the problem.

``f_shum_read_fixed_length_header``
'''''''''''''''''''''''''''''''''''

This function reads in and returns the fixed length header from the file.

    **Available via module**
        ``f_shum_fieldsfile_mod``

    **Syntax**
        ``status = f_shum_read_fixed_length_header(ff_id, fixed_length_header, message)``

    **Inputs**
        ``ff_id (64-bit INTEGER)``
            The identifier for the file; this must be a value returned by an
            earlier call to ``f_shum_open_file`` or ``f_shum_create_file``.

    **Outputs**
        ``fixed_length_header (64-bit INTEGER)``
            The fixed length header (always a 1D array with exactly
            256 elements).

    **Input & Output**
        ``message (CHARACTER(LEN=*))``
            Error message buffer.

    **Return Value**
        ``status (64-bit INTEGER)``
            Exit status; ``0`` means success, anything else means an error has
            occurred and in that case the ``message`` argument will contain 
            information about the problem.

``f_shum_read_integer_constants``
'''''''''''''''''''''''''''''''''

This function reads in and returns the integer constants from the file.

    **Available via module**
        ``f_shum_fieldsfile_mod``

    **Syntax**
        ``status = f_shum_read_integer_constants(ff_id, integer_constants, message)``

    **Inputs**
        ``ff_id (64-bit INTEGER)``
            The identifier for the file; this must be a value returned by an
            earlier call to ``f_shum_open_file`` or ``f_shum_create_file``.

    **Input & Output**
        ``integer_constants (64-bit INTEGER)``
            The integer constants, a 1D ``ALLOCATABLE`` array which will become
            ``ALLOCATED`` to the correct size following the call (if it was 
            already ``ALLOCATED`` it will first be ``DEALLOCATED``).
        ``message (CHARACTER(LEN=*))``
            Error message buffer.

    **Return Value**
        ``status (64-bit INTEGER)``
            Exit status; ``0`` means success, anything above ``0`` means an 
            error has occurred, and a value of ``-1`` means this component was
            not present in the file.  In both unsuccessful cases the ``message`` 
            argument will contain further information.

``f_shum_read_real_constants``
''''''''''''''''''''''''''''''

This function reads in and returns the real constants from the file.

    **Available via module**
        ``f_shum_fieldsfile_mod``

    **Syntax**
        ``status = f_shum_read_real_constants(ff_id, real_constants, message)``

    **Inputs**
        ``ff_id (64-bit INTEGER)``
            The identifier for the file; this must be a value returned by an
            earlier call to ``f_shum_open_file`` or ``f_shum_create_file``.

    **Input & Output**
        ``real_constants (64-bit REAL)``
            The real constants, a 1D ``ALLOCATABLE`` array which will become
            ``ALLOCATED`` to the correct size following the call (if it was 
            already ``ALLOCATED`` it will first be ``DEALLOCATED``).
        ``message (CHARACTER(LEN=*))``
            Error message buffer.

    **Return Value**
        ``status (64-bit INTEGER)``
            Exit status; ``0`` means success, anything above ``0`` means an 
            error has occurred, and a value of ``-1`` means this component was
            not present in the file.  In both unsuccessful cases the ``message`` 
            argument will contain further information.

``f_shum_read_level_dependent_constants``
'''''''''''''''''''''''''''''''''''''''''

This function reads in and returns the level dependent constants from the file.

    **Available via module**
        ``f_shum_fieldsfile_mod``

    **Syntax**
        ``status = f_shum_read_level_dependent_constants(ff_id, level_dependent_constants, message)``

    **Inputs**
        ``ff_id (64-bit INTEGER)``
            The identifier for the file; this must be a value returned by an
            earlier call to ``f_shum_open_file`` or ``f_shum_create_file``.

    **Input & Output**
        ``level_dependent_constants (64-bit REAL)``
            The level dependent constants, a 2D ``ALLOCATABLE`` array which 
            will become ``ALLOCATED`` to the correct size following the call 
            (if it was already ``ALLOCATED`` it will first be ``DEALLOCATED``).
        ``message (CHARACTER(LEN=*))``
            Error message buffer.

    **Return Value**
        ``status (64-bit INTEGER)``
            Exit status; ``0`` means success, anything above ``0`` means an 
            error has occurred, and a value of ``-1`` means this component was
            not present in the file.  In both unsuccessful cases the ``message`` 
            argument will contain further information.

``f_shum_read_row_dependent_constants``
'''''''''''''''''''''''''''''''''''''''

This function reads in and returns the row dependent constants from the file.

    **Available via module**
        ``f_shum_fieldsfile_mod``

    **Syntax**
        ``status = f_shum_read_row_dependent_constants(ff_id, row_dependent_constants, message)``

    **Inputs**
        ``ff_id (64-bit INTEGER)``
            The identifier for the file; this must be a value returned by an
            earlier call to ``f_shum_open_file`` or ``f_shum_create_file``.

    **Input & Output**
        ``row_dependent_constants (64-bit REAL)``
            The row dependent constants, a 2D ``ALLOCATABLE`` array which 
            will become ``ALLOCATED`` to the correct size following the call 
            (if it was already ``ALLOCATED`` it will first be ``DEALLOCATED``).
        ``message (CHARACTER(LEN=*))``
            Error message buffer.

    **Return Value**
        ``status (64-bit INTEGER)``
            Exit status; ``0`` means success, anything above ``0`` means an 
            error has occurred, and a value of ``-1`` means this component was
            not present in the file.  In both unsuccessful cases the ``message`` 
            argument will contain further information.

``f_shum_read_column_dependent_constants``
''''''''''''''''''''''''''''''''''''''''''

This function reads in and returns the column dependent constants from the file.

    **Available via module**
        ``f_shum_fieldsfile_mod``

    **Syntax**
        ``status = f_shum_read_column_dependent_constants(ff_id, column_dependent_constants, message)``

    **Inputs**
        ``ff_id (64-bit INTEGER)``
            The identifier for the file; this must be a value returned by an
            earlier call to ``f_shum_open_file`` or ``f_shum_create_file``.

    **Input & Output**
        ``column_dependent_constants (64-bit REAL)``
            The column dependent constants, a 2D ``ALLOCATABLE`` array which 
            will become ``ALLOCATED`` to the correct size following the call 
            (if it was already ``ALLOCATED`` it will first be ``DEALLOCATED``).
        ``message (CHARACTER(LEN=*))``
            Error message buffer.

    **Return Value**
        ``status (64-bit INTEGER)``
            Exit status; ``0`` means success, anything above ``0`` means an 
            error has occurred, and a value of ``-1`` means this component was
            not present in the file.  In both unsuccessful cases the ``message`` 
            argument will contain further information.

``f_shum_read_additional_parameters``
'''''''''''''''''''''''''''''''''''''

This function reads in and returns the additional parameters from the file.

    **Available via module**
        ``f_shum_fieldsfile_mod``

    **Syntax**
        ``status = f_shum_read_additional_parameters(ff_id, additional_parameters, message)``

    **Inputs**
        ``ff_id (64-bit INTEGER)``
            The identifier for the file; this must be a value returned by an
            earlier call to ``f_shum_open_file`` or ``f_shum_create_file``.

    **Input & Output**
        ``column_dependent_constants (64-bit REAL)``
            The additional parameters, a 2D ``ALLOCATABLE`` array which 
            will become ``ALLOCATED`` to the correct size following the call 
            (if it was already ``ALLOCATED`` it will first be ``DEALLOCATED``).
        ``message (CHARACTER(LEN=*))``
            Error message buffer.

    **Return Value**
        ``status (64-bit INTEGER)``
            Exit status; ``0`` means success, anything above ``0`` means an 
            error has occurred, and a value of ``-1`` means this component was
            not present in the file.  In both unsuccessful cases the ``message`` 
            argument will contain further information.

``f_shum_read_extra_constants``
'''''''''''''''''''''''''''''''

This function reads in and returns the extra constants from the file.

    **Available via module**
        ``f_shum_fieldsfile_mod``

    **Syntax**
        ``status = f_shum_read_extra_constants(ff_id, extra_constants, message)``

    **Inputs**
        ``ff_id (64-bit INTEGER)``
            The identifier for the file; this must be a value returned by an
            earlier call to ``f_shum_open_file`` or ``f_shum_create_file``.

    **Input & Output**
        ``extra_constants (64-bit REAL)``
            The extra constants, a 1D ``ALLOCATABLE`` array which 
            will become ``ALLOCATED`` to the correct size following the call 
            (if it was already ``ALLOCATED`` it will first be ``DEALLOCATED``).
        ``message (CHARACTER(LEN=*))``
            Error message buffer.

    **Return Value**
        ``status (64-bit INTEGER)``
            Exit status; ``0`` means success, anything above ``0`` means an 
            error has occurred, and a value of ``-1`` means this component was
            not present in the file.  In both unsuccessful cases the ``message`` 
            argument will contain further information.

``f_shum_read_temp_histfile``
'''''''''''''''''''''''''''''

This function reads in and returns the temporary historyfile from the file.

    **Available via module**
        ``f_shum_fieldsfile_mod``

    **Syntax**
        ``status = f_shum_read_temp_histfile(ff_id, temp_histfile, message)``

    **Inputs**
        ``ff_id (64-bit INTEGER)``
            The identifier for the file; this must be a value returned by an
            earlier call to ``f_shum_open_file`` or ``f_shum_create_file``.

    **Input & Output**
        ``temp_histfile (64-bit REAL)``
            The temp_histfile, a 1D ``ALLOCATABLE`` array which 
            will become ``ALLOCATED`` to the correct size following the call 
            (if it was already ``ALLOCATED`` it will first be ``DEALLOCATED``).
        ``message (CHARACTER(LEN=*))``
            Error message buffer.

    **Return Value**
        ``status (64-bit INTEGER)``
            Exit status; ``0`` means success, anything above ``0`` means an 
            error has occurred, and a value of ``-1`` means this component was
            not present in the file.  In both unsuccessful cases the ``message`` 
            argument will contain further information.

``f_shum_read_compressed_index``
''''''''''''''''''''''''''''''''

This function reads in and returns the compressed indices from the file.

    **Available via module**
        ``f_shum_fieldsfile_mod``

    **Syntax**
        ``status = f_shum_read_compressed_index(ff_id, compressed_index, index, message)``

    **Inputs**
        ``ff_id (64-bit INTEGER)``
            The identifier for the file; this must be a value returned by an
            earlier call to ``f_shum_open_file`` or ``f_shum_create_file``.
        ``index (64-bit INTEGER)``
            Indicates which of the 3 compressed index headers should be returned
            (can take a value of 1, 2 or 3).

    **Input & Output**
        ``compressed_index (64-bit REAL)``
            The compressed index (one of 3 depending on the value of ``index``), 
            a 1D ``ALLOCATABLE`` array which will become ``ALLOCATED`` to the 
            correct size following the call (if it was already ``ALLOCATED`` 
            it will first be ``DEALLOCATED``).
        ``message (CHARACTER(LEN=*))``
            Error message buffer.

    **Return Value**
        ``status (64-bit INTEGER)``
            Exit status; ``0`` means success, anything above ``0`` means an 
            error has occurred, and a value of ``-1`` means this component was
            not present in the file.  In both unsuccessful cases the ``message`` 
            argument will contain further information.

``f_shum_read_lookup``
''''''''''''''''''''''

    **Available via module**
        ``f_shum_fieldsfile_mod``

    **Syntax**
        ``status = f_shum_read_lookup(ff_id, lookup, message)``

    **Inputs**
        ``ff_id (64-bit INTEGER)``
            The identifier for the file; this must be a value returned by an
            earlier call to ``f_shum_open_file`` or ``f_shum_create_file``.

    **Input & Output**
        ``lookup (64-bit REAL)``
            The lookup table, a 2D ``ALLOCATABLE`` array which will become 
            ``ALLOCATED`` to the correct size following the call (if it was 
            already ``ALLOCATED`` it will first be ``DEALLOCATED``).
        ``message (CHARACTER(LEN=*))``
            Error message buffer.

    **Return Value**
        ``status (64-bit INTEGER)``
            Exit status; ``0`` means success, anything above ``0`` means an 
            error has occurred, and a value of ``-1`` means this component was
            not present in the file.  In both unsuccessful cases the ``message`` 
            argument will contain further information.

``f_shum_read_field_data``
''''''''''''''''''''''''''

    **Available via module**
        ``f_shum_fieldsfile_mod``

    **Syntax**
        ``status = f_shum_read_field_data(ff_id, index, field_data, message, ignore_dtype)``

    **Inputs**
        ``ff_id (64-bit INTEGER)``
            The identifier for the file; this must be a value returned by an
            earlier call to ``f_shum_open_file`` or ``f_shum_create_file``.
        ``index (64-bit INTEGER)``
            Indicates the index into the lookup table containing the field data
            that should be returned.

    **Input & Output**
        ``field_data``
            The field data, a 1D ``ALLOCATABLE`` array which will become 
            ``ALLOCATED`` to the correct size following the call (if it was
            already ``ALLOCATED`` it will first be ``DEALLOCATED``). The type
            of ``field_data`` may be either ``INTEGER`` or ``REAL`` and can be
            either 32-bit or 64-bit.  Which combination of these is correct
            depends on the data and packing types of the field, which you can 
            determine by examining the lookup table yourself.  Passing a 
            ``field_data`` array that does not match the type and precision 
            indicated by the lookup will result in an error (*unless* the
            optional ``ignore_dtype`` flag is passed (see below)).
        ``message (CHARACTER(LEN=*))``
            Error message buffer.
        ``ignore_dtype (optional, LOGICAL)``
            If provided and set to true (default if not provided is false) the
            type and precision of the ``field_data`` variable will be used 
            regardless of what the lookup specifies. This means that for example
            a field which should be 64-bit ``REAL`` data can be read into a 
            32-bit ``INTEGER`` array (as raw bytes; to retrieve the true 
            ``REAL`` values later each pair of values would need to be combined
            and then changed into a ``REAL`` representation using ``TRANSFER``)
            
    **Return Value**
        ``status (64-bit INTEGER)``
            Exit status; ``0`` means success, anything above ``0`` means an 
            error has occurred, and a value of ``-1`` means this component was
            not present in the file.  In both unsuccessful cases the ``message`` 
            argument will contain further information.


``f_shum_write_fixed_length_header``
''''''''''''''''''''''''''''''''''''

This function writes a fixed length header array to a file.

    **Available via module**
        ``f_shum_fieldsfile_mod``

    **Syntax**
        ``status = f_shum_write_fixed_length_header(ff_id, fixed_length_header, message)``

    **Inputs**
        ``ff_id (64-bit INTEGER)``
            The identifier for the file; this must be a value returned by an
            earlier call to ``f_shum_open_file`` or ``f_shum_create_file``.
        ``fixed_length_header (64-bit INTEGER)``
            The fixed length header (always a 1D array with exactly
            256 elements).

    **Input & Output**
        ``message (CHARACTER(LEN=*))``
            Error message buffer.

    **Return Value**
        ``status (64-bit INTEGER)``
            Exit status; ``0`` means success, anything else means an error has
            occurred and in that case the ``message`` argument will contain 
            information about the problem.

    **Notes**
        Unlike many of the other routines, no writing to disk actually takes 
        place upon calling this command.  The fixed length header is committed
        to disk when the file is closed.  Also note that any positional
        elements of the fixed length header passed here are discarded (as the
        API will ensure positional elements always reflect the written 
        structure of the file).


``f_shum_write_integer_constants``
''''''''''''''''''''''''''''''''''

This function writes an integer constants array to a file.

    **Available via module**
        ``f_shum_fieldsfile_mod``

    **Syntax**
        ``status = f_shum_write_integer_constants(ff_id, integer_constants, message)``

    **Inputs**
        ``ff_id (64-bit INTEGER)``
            The identifier for the file; this must be a value returned by an
            earlier call to ``f_shum_open_file`` or ``f_shum_create_file``.
        ``integer_constants (64-bit INTEGER)``
            The integer constants (a 1D array of any length is allowed, but see
            the file format definition for details of the expected lengths for
            different fieldsfile variants).

    **Input & Output**
        ``message (CHARACTER(LEN=*))``
            Error message buffer.

    **Return Value**
        ``status (64-bit INTEGER)``
            Exit status; ``0`` means success, anything else means an error has
            occurred and in that case the ``message`` argument will contain 
            information about the problem.

``f_shum_write_real_constants``
'''''''''''''''''''''''''''''''

This function writes a real constants array to a file.

    **Available via module**
        ``f_shum_fieldsfile_mod``

    **Syntax**
        ``status = f_shum_write_real_constants(ff_id, real_constants, message)``

    **Inputs**
        ``ff_id (64-bit INTEGER)``
            The identifier for the file; this must be a value returned by an
            earlier call to ``f_shum_open_file`` or ``f_shum_create_file``.
        ``real_constants (64-bit REAL)``
            The real constants (a 1D array of any length is allowed, but see
            the file format definition for details of the expected lengths for
            different fieldsfile variants).

    **Input & Output**
        ``message (CHARACTER(LEN=*))``
            Error message buffer.

    **Return Value**
        ``status (64-bit INTEGER)``
            Exit status; ``0`` means success, anything else means an error has
            occurred and in that case the ``message`` argument will contain 
            information about the problem.

``f_shum_write_level_dependent_constants``
''''''''''''''''''''''''''''''''''''''''''

This function writes a level dependent constants array to a file.

    **Available via module**
        ``f_shum_fieldsfile_mod``

    **Syntax**
        ``status = f_shum_write_level_dependent_constants(ff_id, level_dependent_constants, message)``

    **Inputs**
        ``ff_id (64-bit INTEGER)``
            The identifier for the file; this must be a value returned by an
            earlier call to ``f_shum_open_file`` or ``f_shum_create_file``.
        ``level_dependent_constants (64-bit REAL)``
            The level dependent constants (a 2D array of any size is allowed, 
            but see the file format definition for details of the expected 
            dimensions for different fieldsfile variants).

    **Input & Output**
        ``message (CHARACTER(LEN=*))``
            Error message buffer.

    **Return Value**
        ``status (64-bit INTEGER)``
            Exit status; ``0`` means success, anything else means an error has
            occurred and in that case the ``message`` argument will contain 
            information about the problem.

``f_shum_write_row_dependent_constants``
''''''''''''''''''''''''''''''''''''''''

This function writes a row dependent constants array to a file.

    **Available via module**
        ``f_shum_fieldsfile_mod``

    **Syntax**
        ``status = f_shum_write_row_dependent_constants(ff_id, row_dependent_constants, message)``

    **Inputs**
        ``ff_id (64-bit INTEGER)``
            The identifier for the file; this must be a value returned by an
            earlier call to ``f_shum_open_file`` or ``f_shum_create_file``.
        ``row_dependent_constants (64-bit REAL)``
            The row dependent constants (a 2D array of any size is allowed, 
            but see the file format definition for details of the expected 
            dimensions for different fieldsfile variants).

    **Input & Output**
        ``message (CHARACTER(LEN=*))``
            Error message buffer.

    **Return Value**
        ``status (64-bit INTEGER)``
            Exit status; ``0`` means success, anything else means an error has
            occurred and in that case the ``message`` argument will contain 
            information about the problem.

``f_shum_write_column_dependent_constants``
'''''''''''''''''''''''''''''''''''''''''''

This function writes a column dependent constants array to a file.

    **Available via module**
        ``f_shum_fieldsfile_mod``

    **Syntax**
        ``status = f_shum_write_column_dependent_constants(ff_id, column_dependent_constants, message)``

    **Inputs**
        ``ff_id (64-bit INTEGER)``
            The identifier for the file; this must be a value returned by an
            earlier call to ``f_shum_open_file`` or ``f_shum_create_file``.
        ``column_dependent_constants (64-bit REAL)``
            The column dependent constants (a 2D array of any size is allowed, 
            but see the file format definition for details of the expected 
            dimensions for different fieldsfile variants).

    **Input & Output**
        ``message (CHARACTER(LEN=*))``
            Error message buffer.

    **Return Value**
        ``status (64-bit INTEGER)``
            Exit status; ``0`` means success, anything else means an error has
            occurred and in that case the ``message`` argument will contain 
            information about the problem.

``f_shum_write_additional_parameters``
''''''''''''''''''''''''''''''''''''''

This function writes an additional parameters array to a file.

    **Available via module**
        ``f_shum_fieldsfile_mod``

    **Syntax**
        ``status = f_shum_write_additional_parameters(ff_id, additional_parameters, message)``

    **Inputs**
        ``ff_id (64-bit INTEGER)``
            The identifier for the file; this must be a value returned by an
            earlier call to ``f_shum_open_file`` or ``f_shum_create_file``.
        ``additional_parameters (64-bit REAL)``
            The additional parameters (a 2D array of any size is allowed, 
            but see the file format definition for details of the expected 
            dimensions for different fieldsfile variants).

    **Input & Output**
        ``message (CHARACTER(LEN=*))``
            Error message buffer.

    **Return Value**
        ``status (64-bit INTEGER)``
            Exit status; ``0`` means success, anything else means an error has
            occurred and in that case the ``message`` argument will contain 
            information about the problem.

``f_shum_write_extra_constants``
''''''''''''''''''''''''''''''''

This function writes an extra constants array to a file.

    **Available via module**
        ``f_shum_fieldsfile_mod``

    **Syntax**
        ``status = f_shum_write_extra_constants(ff_id, extra_constants, message)``

    **Inputs**
        ``ff_id (64-bit INTEGER)``
            The identifier for the file; this must be a value returned by an
            earlier call to ``f_shum_open_file`` or ``f_shum_create_file``.
        ``extra_constants (64-bit REAL)``
            The extra constants (a 1D array of any length is allowed, 
            but see the file format definition for details of the expected 
            lengths for different fieldsfile variants).

    **Input & Output**
        ``message (CHARACTER(LEN=*))``
            Error message buffer.

    **Return Value**
        ``status (64-bit INTEGER)``
            Exit status; ``0`` means success, anything else means an error has
            occurred and in that case the ``message`` argument will contain 
            information about the problem.

``f_shum_write_temp_histfile``
''''''''''''''''''''''''''''''

This function writes a temp histfile array to a file.

    **Available via module**
        ``f_shum_fieldsfile_mod``

    **Syntax**
        ``status = f_shum_write_temp_histfile(ff_id, extra_constants, message)``

    **Inputs**
        ``ff_id (64-bit INTEGER)``
            The identifier for the file; this must be a value returned by an
            earlier call to ``f_shum_open_file`` or ``f_shum_create_file``.
        ``temp_histfile (64-bit REAL)``
            The temp histfile (a 1D array of any length is allowed, 
            but see the file format definition for details of the expected 
            lengths for different fieldsfile variants).

    **Input & Output**
        ``message (CHARACTER(LEN=*))``
            Error message buffer.

    **Return Value**
        ``status (64-bit INTEGER)``
            Exit status; ``0`` means success, anything else means an error has
            occurred and in that case the ``message`` argument will contain 
            information about the problem.

``f_shum_write_compressed_index``
'''''''''''''''''''''''''''''''''

This function writes one of 3 compressed index arrays to a file.

    **Available via module**
        ``f_shum_fieldsfile_mod``

    **Syntax**
        ``status = f_shum_write_compressed_index(ff_id, compressed_index, index, message)``

    **Inputs**
        ``ff_id (64-bit INTEGER)``
            The identifier for the file; this must be a value returned by an
            earlier call to ``f_shum_open_file`` or ``f_shum_create_file``.
        ``compressed_index (64-bit REAL)``
            The compressed index (a 1D array of any length is allowed, 
            but see the file format definition for details of the expected 
            lengths for different fieldsfile variants).
        ``index (64-bit INTEGER)``
            Indicates which of the 3 compressed index headers should be written
            (can take a value of 1, 2 or 3).

    **Input & Output**
        ``message (CHARACTER(LEN=*))``
            Error message buffer.

    **Return Value**
        ``status (64-bit INTEGER)``
            Exit status; ``0`` means success, anything else means an error has
            occurred and in that case the ``message`` argument will contain 
            information about the problem.

``f_shum_write_lookup``
'''''''''''''''''''''''

This function writes the lookup table array to a file, and is intended to be
used when using the "direct" approach to writing the file (where the lookups
are written and processed prior to writing the data).

    **Available via module**
        ``f_shum_fieldsfile_mod``

    **Syntax**
        ``status = f_shum_write_lookup(ff_id, lookup, start_index, message)``

    **Inputs**
        ``ff_id (64-bit INTEGER)``
            The identifier for the file; this must be a value returned by an
            earlier call to ``f_shum_open_file`` or ``f_shum_create_file``.
        ``lookup (64-bit INTEGER)``
            The lookup (a 2D array which must have a first dimension of 
            ``f_shum_lookup_dim1_len``, and a second dimension corresponding
            to the number of fields which must not exceed the number passed 
            to ``f_shum_create_file``).
        ``start_index (64-bit INTEGER)``
            The position in the file's lookup table where the first element
            of the given lookup should be written (can be used to write the
            lookup table with multiple calls; e.g. to write lookups starting
            from the 5th lookup ``start_index`` would be set to 5).

    **Input & Output**
        ``message (CHARACTER(LEN=*))``
            Error message buffer.

    **Return Value**
        ``status (64-bit INTEGER)``
            Exit status; ``0`` means success, anything else means an error has
            occurred and in that case the ``message`` argument will contain 
            information about the problem.

    **Notes** 
        Unlike many of the other routines, no writing to disk actually
        takes place upon calling this command.  The lookup is committed to disk
        when the file is closed.  Also note that any positional elements of the
        lookup passed here are discarded (as the API will ensure positional
        elements always reflect the written structure of the file).


``f_shum_precalc_data_positions``
'''''''''''''''''''''''''''''''''

For use when writing the file using the "direct" approach - this function scans
the lookups which have been written using ``f_shum_write_lookup`` and calculates
appropriate sizes for the positional elements (i.e. where each field's data
will begin and how much space it will take up).  Doing this allows the fields
to be written in any order.

    **Available via module**
        ``f_shum_fieldsfile_mod``

    **Syntax**
        ``status = f_shum_precalc_data_positions(ff_id, max_points, message, n_land_points, n_sea_points)``

    **Inputs**
        ``ff_id (64-bit INTEGER)``
            The identifier for the file; this must be a value returned by an
            earlier call to ``f_shum_open_file`` or ``f_shum_create_file``.
        ``max_points (64-bit INTEGER)``
            The number of 64-bit (8-byte) words which will be setup in the 
            lookup table for each field.
        ``n_land_points (optional 64-bit INTEGER)``
            If provided, gives an alternative ``max_points`` value to use for
            fields designated as land-only (according to their ``LBPACK`` value).
        ``n_sea_points (optional 64-bit INTEGER)``
            If provided, gives an alternative ``max_points`` value to use for
            fields designated as sea-only (according to their ``LBPACK`` value).

    **Input & Output**
        ``message (CHARACTER(LEN=*))``
            Error message buffer.

    **Return Value**
        ``status (64-bit INTEGER)``
            Exit status; ``0`` means success, anything else means an error has
            occurred and in that case the ``message`` argument will contain 
            information about the problem.


``f_shum_write_field_data``
'''''''''''''''''''''''''''

This function writes the data for a single field.  There are two different ways
to manage the writing of field data; "direct" or "sequential".  The syntax used 
for this command is dependent on which of the writing methods are in use for 
the given file.  A file being written via the "direct" method will have had 
previous calls to ``f_shum_write_lookup`` and ``f_shum_precalc_data_positions``,
whereas a file using the "sequential" method will not.

    **Available via module**
        ``f_shum_fieldsfile_mod``

    **Syntax (Direct)**
        ``status = f_shum_write_field_data(ff_id, index, field_data, message, ignore_dtype)``
    **Syntax (Sequential)**
        ``status = f_shum_write_field_data(ff_id, lookup, field_data, message, ignore_dtype)``

    **Inputs**
        ``ff_id (64-bit INTEGER)``
            The identifier for the file; this must be a value returned by an
            earlier call to ``f_shum_open_file`` or ``f_shum_create_file``.
        ``index (64-bit INTEGER)``
            Indicates the index into the lookup table corresponding to the 
            field data being passed (and where it should be written to).
        ``lookup (64-bit INTEGER)``
            The lookup of the field corresponding to the data being passed.
        ``field_data``
            The field data, a 1D array which may be either ``INTEGER`` or 
            ``REAL`` and can be either 32-bit or 64-bit.  Which combination 
            of these is correct depends on the data and packing types of the 
            field, which should be specified in the lookup table. Passing a 
            ``field_data`` array that does not match the type and precision 
            indicated by the lookup will result in an error (*unless* the
            optional ``ignore_dtype`` flag is also passed (see below)).
        ``ignore_dtype (optional, LOGICAL)``
            If provided and set to true (default if not provided is false) the
            type and precision of the ``field_data`` variable will be used 
            regardless of what the lookup specifies. This means that for example
            a field which should be 64-bit ``REAL`` data can be written as a 
            32-bit ``INTEGER`` array (as raw bytes; to retrieve the true 
            ``REAL`` values later each pair of values would need to be combined
            and then changed into a ``REAL`` representation using ``TRANSFER``)

    **Outputs**
        ``message (CHARACTER(LEN=*))``
            Error message buffer.

    **Return Value**
        ``status (64-bit INTEGER)``
            Exit status; ``0`` means success, anything else means an error has
            occurred and in that case the ``message`` argument will contain 
            information about the problem.


``f_shum_read_stashmaster``
'''''''''''''''''''''''''''

This function reads in a UM STASHmaster file from disk and populates a given
structure with the results.

    **Available via module**
        ``f_shum_fieldsfile_mod``

    **Syntax**
        ``status = f_shum_read_stashmaster(filename, STASHmaster, message)``

    **Inputs**
        ``filename (CHARACTER)``
            The filename of the file containing the STASHmaster.
        ``STASHmaster (TYPE(shum_STASHmaster), 1D array length 99999)``
            A Pointer array of type ``shum_STASHmaster`` (also provided by
            this module).  It must have 99999 elements and after the call to
            this function any indices (stash codes) which have an entry in
            the given STASHmaster file will have been populated, e.g. if the
            variable ``STASHm`` contains this argument, the grid code for 
            STASH code 16004 would be ``STASHm(16004) % record % grid``.

    **Outputs**
        ``message (CHARACTER(LEN=*))``
            Error message buffer.

    **Return Value**
        ``status (64-bit INTEGER)``
            Exit status; ``0`` means success, anything else means an error has
            occurred and in that case the ``message`` argument will contain 
            information about the problem.

``f_shum_add_new_stash_record``
'''''''''''''''''''''''''''''''

Adds a single STASH record to an existing STASHmaster structure (this is used
by the above routine).

    **Available via module**
        ``f_shum_fieldsfile_mod``

    **Syntax**
        ``status = f_shum_add_new_stash_record(STASHmaster, model, section, item, name, space, point, time, grid, levelt, levelf, levell, pseudt, pseudf, pseudl, levcom, option, version_mask, halo, datat, dumpp, packing_codes, rotate, ppfc, user, lbvc, blev, tlev, rblevv, cfll, cfff, message)``

    **Inputs**
        Note: For the meaning of the STASH variables please consult the UM
        Documentation Paper C04 (currently only available via a UM licence).

        ``STASHmaster (TYPE(shum_STASHmaster), 1D array length 99999)``
            A Pointer array of type ``shum_STASHmaster`` (also provided by
            this module).  It must have 99999 elements and after the call to
            this function the index at the computed stash codes will have 
            been populated.

        ``model (64-bit INTEGER)``

        ``section (64-bit INTEGER)``

        ``item (64-bit INTEGER)``

        ``name (CHARACTER(LEN=36))``

        ``space (64-bit INTEGER)``

        ``point (64-bit INTEGER)``
        
        ``time (64-bit INTEGER)``

        ``grid (64-bit INTEGER)``

        ``levelt (64-bit INTEGER)``

        ``levelf (64-bit INTEGER)``

        ``levell (64-bit INTEGER)``

        ``pseudt (64-bit INTEGER)``

        ``pseudf (64-bit INTEGER)``

        ``pseudl (64-bit INTEGER)``

        ``levcom (64-bit INTEGER)``

        ``option (1d array of 8-bit INTEGERs (LEN=30))``

        ``version_mask (1d array of 8-bit INTEGERs (LEN=20))``

        ``halo (64-bit INTEGER)``

        ``datat (64-bit INTEGER)``

        ``dumpp (64-bit INTEGER)``

        ``packing_codes (1d array of 64-bit INTEGERs (LEN=10))``

        ``rotate (64-bit INTEGER)``

        ``ppfc (64-bit INTEGER)``

        ``user (64-bit INTEGER)``

        ``lbvc (64-bit INTEGER)``

        ``blev (64-bit INTEGER)``

        ``tlev (64-bit INTEGER)``

        ``rblevv (64-bit INTEGER)``

        ``cfll (64-bit INTEGER)``

        ``cfff (64-bit INTEGER)``

    **Outputs**
        ``message (CHARACTER(LEN=*))``
            Error message buffer.

    **Return Value**
        ``status (64-bit INTEGER)``
            Exit status; ``0`` means success, anything else means an error has
            occurred and in that case the ``message`` argument will contain 
            information about the problem.


C Functions
%%%%%%%%%%%

``get_shum_fieldsfile_version``
''''''''''''''''''''''''''''''''''

All Shumlib libraries expose a function named in this format; it allows access
to the Shumlib version number used when compiling the library.

    **Required header/s**
        None - always defined provided any other part of library is included.

    **Syntax**
        ``version = get_shum_fieldsfile_version()``

    **Returns**
        ``version (int)``
            Shumlib version number, in format ``YYYYMMX`` (the 4-digit year
            followed by 2-digit month and an additional digit signifying the
            release count within that specified month).
