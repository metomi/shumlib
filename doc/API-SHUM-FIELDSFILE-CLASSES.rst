API Reference: UM File and Field classes
----------------------------------------

This API is **experimental**; it provides a higher-level way of reading and
writing fieldsfiles over the functions provided in ``shum_fieldsfile``. There
are three classes provided, ``shum_file_type`` which represents a fieldsfile (or
UM start dump), and  ``shum_field_type``, which represents a UM field.

The third class is ``shum_ff_status_type``, which is returned by methods in the
other two classes:

Public Variables: ``shum_ff_status_type``
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

As well as providing the variables detailed below, this module also has
overloaded operators for ``==``, ``>``, ``<``, ``>=``, ``<=`` and ``/=`` to
allow the ``icode`` inside the status object to be compared with a raw integer, 
or with other status objects, e.g. ``IF (status /= 0_int64)`` is equivalent 
to ``IF (status%icode /= 0_int64)``.


``icode (shum_ff_status_type)``

An integer return code from the method. This will be zero for success; a 
non-zero value indicates an issue. In cases where there is a distinction
between a warning and a fatal error, warnings have a negative icode and 
errors positive icode.


``message (CHARACTER(LEN=1024)``
If the icode of this object is non-zero, this should contain a message
describing what the issue was.



Public Variables: ``shum_file_type``
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

``fields (shum_field_type array)``

This is an ALLOCATABLE array of ``shum_field_type`` objects, corresponding to the
fields present in the file.


``num_fields (64-bit INTEGER)``

This contains the total number of fields currently in the file.


Fortran Methods (Functions): ``shum_file_type``
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

``open_file``
'''''''''''''

This function opens a fieldsfile (or UM start dump). By default, an existing
file is opened read-only.

    **Available via class**
        ``shum_file_type``

    **Syntax**
        ``icode = um_file%open_file(filename, num_lookups, overwrite)``

    **Inputs**
        ``filename (CHARACTER)``
            The path to the filename.

        ``num_lookup (optional 64-bit INTEGER)``
            The number of empty lookup entries to create. If not set, this
            defaults to 4096, as per the FieldsFile standard.

        ``overwrite (optional LOGICAL)``
            If set to true, this method will overwrite existing files rather
            than open them as read-only.

    **Return Value**
        ``status (shum_ff_status_type)``
            Exit status; ``0`` means success, anything else means an error has
            occurred and in that case the ``message`` variable in the object
            will contain information about the problem.



``read_header``
'''''''''''''''

This function reads the header from a previously-opened file. It also populates
the file with field objects corresponding to each field (without loading the
actual data).

    **Available via class**
        ``shum_file_type``

    **Syntax**
        ``icode = um_file%read_header()``

    **Return Value**
        ``status (shum_ff_status_type)``
            Exit status; ``0`` means success, anything else means an error has
            occurred and in that case the ``message`` variable in the object
            will contain information about the problem.


``read_field``
''''''''''''''

This function reads in the data for a specified field in the file. The data
will be available using ``um_file%fields(field_number)%get_data``.

    **Available via class**
        ``shum_file_type``

    **Syntax**
        ``icode = um_file%read_field(field_number)``

    **Inputs**
        ``field_number (64-bit INTEGER)``
            The number of the field in the file the method will try and read
            data from.

    **Return Value**
        ``status (shum_ff_status_type)``
            Exit status; ``0`` means success, anything else means an error has
            occurred and in that case the ``message`` variable in the object
            will contain information about the problem.


``write_header``
''''''''''''''''

This function writes the header information in the ``shum_file_type`` object to
disk. Note that actually writing certain header components may be deferred
until the file is closed.

    **Available via class**
        ``shum_file_type``

    **Syntax**
        ``icode = um_file%write_header()``

    **Return Value**
        ``status (shum_ff_status_type)``
            Exit status; ``0`` means success, anything else means an error has
            occurred and in that case the ``message`` variable in the object
            will contain information about the problem.


``write_field``
'''''''''''''''

This function writes the specified data to disk. Packing is controlled by the
**LBPACK** lookup value. Note that to write land- or sea-compressed data, the
land-sea mask must already exist in the file.

    **Available via class**
        ``shum_file_type``

    **Syntax**
        ``icode = um_file%write_field(field_number)``

    **Inputs**
        ``field_number (64-bit INTEGER)``
            The number of the field in the file the method will write the data
            for.

    **Return Value**
        ``status (shum_ff_status_type)``
            Exit status; ``0`` means success, anything else means an error has
            occurred and in that case the ``message`` variable in the object
            will contain information about the problem.


``close_file``
''''''''''''''

This function closes access to a previously opened file.

    **Available via class**
        ``shum_file_type``

    **Syntax**
        ``icode = um_file%close_file()``

    **Return Value**
        ``status (shum_ff_status_type)``
            Exit status; ``0`` means success, anything else means an error has
            occurred and in that case the ``message`` variable in the object
            will contain information about the problem.


``set_fixed_length_header``
'''''''''''''''''''''''''''

This method sets the fixed-length header in the file object.

    **Available via class**
        ``shum_file_type``

    **Syntax**
        ``icode = um_file%set_fixed_length_header(fixed_length_header)``

    **Inputs**
        ``fixed_length_header (64-bit INTEGER)``
            The fixed length header (always a 1D array with exactly
            256 elements).

    **Return Value**
        ``status (shum_ff_status_type)``
            Exit status; ``0`` means success, anything else means an error has
            occurred and in that case the ``message`` variable in the object
            will contain information about the problem.


``get_fixed_length_header``
'''''''''''''''''''''''''''

This method gets the fixed-length header in the file object.

    **Available via class**
        ``shum_file_type``

    **Syntax**
        ``icode = um_file%get_fixed_length_header(fixed_length_header)``

    **Input & Output**
        ``fixed_length_header (64-bit INTEGER)``
            The fixed length header (always a 1D array with exactly
            256 elements).

    **Return Value**
        ``status (shum_ff_status_type)``
            Exit status; ``0`` means success, anything else means an error has
            occurred and in that case the ``message`` variable in the object
            will contain information about the problem.


``set_fixed_length_header_by_index``
''''''''''''''''''''''''''''''''''''

This method sets a value in the fixed-length header in the file object.

    **Available via class**
        ``shum_file_type``

    **Syntax**
        ``icode = um_file%set_fixed_length_header_by_index(num_index, value_to_set)``

    **Input**
        ``num_index (64-bit INTEGER)``
            The index of the position of the fixed length header to set.
        ``value_to_set (64-bit INTEGER)``
            The value to set in the fixed length header.

    **Return Value**
        ``status (shum_ff_status_type)``
            Exit status; ``0`` means success, anything else means an error has
            occurred and in that case the ``message`` variable in the object
            will contain information about the problem.


``get_fixed_length_header_by_index``
''''''''''''''''''''''''''''''''''''

This method gets a value from the fixed-length header in the file object.

    **Available via class**
        ``shum_file_type``

    **Syntax**
        ``icode = um_file%get_fixed_length_header_by_index(num_index, value_to_get)``

    **Input**
        ``num_index (64-bit INTEGER)``
            The index of the position of the fixed length header to retrieve.

    **Output**
        ``value_to_get (64-bit INTEGER)``
            The value in that position in the fixed length header.

    **Return Value**
        ``status (shum_ff_status_type)``
            Exit status; ``0`` means success, anything else means an error has
            occurred and in that case the ``message`` variable in the object
            will contain information about the problem.


``set_integer_constants``
'''''''''''''''''''''''''

This method sets the integer constants in the file object.

    **Available via class**
        ``shum_file_type``

    **Syntax**
        ``icode = um_file%set_integer_constants(integer_constants)``

    **Inputs**
        ``integer_constants (64-bit INTEGER)``
            The integer constants, which should be a 1D ``ALLOCATABLE`` array.

    **Return Value**
        ``status (shum_ff_status_type)``
            Exit status; ``0`` means success, anything else means an error has
            occurred and in that case the ``message`` variable in the object
            will contain information about the problem.


``get_integer_constants``
'''''''''''''''''''''''''

This method gets the integer constants in the file object.

    **Available via class**
        ``shum_file_type``

    **Syntax**
        ``icode = um_file%get_integer_constants(integer_constants)``

    **Input & Output**
        ``integer_constants (64-bit INTEGER)``
            The integer constants, which should be a 1D ``ALLOCATABLE`` array.

    **Return Value**
        ``status (shum_ff_status_type)``
            Exit status; ``0`` means success, anything else means an error has
            occurred and in that case the ``message`` variable in the object
            will contain information about the problem.


``set_integer_constants_by_index``
''''''''''''''''''''''''''''''''''

This method sets one of the integer constants in the file object.

    **Available via class**
        ``shum_file_type``

    **Syntax**
        ``icode = um_file%set_integer_constants_by_index(num_index, value_to_set)``

    **Input**
        ``num_index (64-bit INTEGER)``
            The index of the position of the integer constants to set.
        ``value_to_set (64-bit INTEGER)``
            The value to set in the integer constants.

    **Return Value**
        ``status (shum_ff_status_type)``
            Exit status; ``0`` means success, anything else means an error has
            occurred and in that case the ``message`` variable in the object
            will contain information about the problem.


``get_integer_constants_by_index``
''''''''''''''''''''''''''''''''''

This method gets one of the integer constants in the file object.

    **Available via class**
        ``shum_file_type``

    **Syntax**
        ``icode = um_file%get_integer_constants_by_index(num_index, value_to_get)``

    **Input**
        ``num_index (64-bit INTEGER)``
            The index of the position of the integer constants to retrieve.

    **Output**
        ``value_to_get (64-bit INTEGER)``
            The value in that position in the integer constants.


    **Return Value**
        ``status (shum_ff_status_type)``
            Exit status; ``0`` means success, anything else means an error has
            occurred and in that case the ``message`` variable in the object
            will contain information about the problem.


``set_real_constants``
''''''''''''''''''''''

This method sets the real constants in the file object.

    **Available via class**
        ``shum_file_type``

    **Syntax**
        ``icode = um_file%set_real_constants(real_constants)``

    **Inputs**
        ``real_constants (64-bit REAL)``
            The real constants, which should be a 1D ``ALLOCATABLE`` array.

    **Return Value**
        ``status (shum_ff_status_type)``
            Exit status; ``0`` means success, anything else means an error has
            occurred and in that case the ``message`` variable in the object
            will contain information about the problem.


``get_real_constants``
''''''''''''''''''''''

This method gets the real constants header in the file object.

    **Available via class**
        ``shum_file_type``

    **Syntax**
        ``icode = um_file%get_real_constants(real_constants)``

    **Input & Output**
        ``real_constants (64-bit REAL)``
            The real constants, which should be a 1D ``ALLOCATABLE`` array.

    **Return Value**
        ``status (shum_ff_status_type)``
            Exit status; ``0`` means success, anything else means an error has
            occurred and in that case the ``message`` variable in the object
            will contain information about the problem.



``set_real_constants_by_index``
''''''''''''''''''''''''''''''''''

This method sets one of the real constants in the file object.

    **Available via class**
        ``shum_file_type``

    **Syntax**
        ``icode = um_file%set_real_constants_by_index(num_index, value_to_set)``

    **Input**
        ``num_index (64-bit INTEGER)``
            The index of the position of the real constants to set.
        ``value_to_set (64-bit REAL)``
            The value to set in the real constants.

    **Return Value**
        ``status (shum_ff_status_type)``
            Exit status; ``0`` means success, anything else means an error has
            occurred and in that case the ``message`` variable in the object
            will contain information about the problem.


``get_real_constants_by_index``
''''''''''''''''''''''''''''''''''

This method gets one of the real constants in the file object.

    **Available via class**
        ``shum_file_type``

    **Syntax**
        ``icode = um_file%get_real_constants_by_index(num_index, value_to_get)``

    **Input**
        ``num_index (64-bit INTEGER)``
            The index of the position of the real constants to retrieve.

    **Output**
        ``value_to_get (64-bit REAL)``
            The value in that position in the real constants.


    **Return Value**
        ``status (shum_ff_status_type)``
            Exit status; ``0`` means success, anything else means an error has
            occurred and in that case the ``message`` variable in the object
            will contain information about the problem.


``set_level_dependent_constants``
'''''''''''''''''''''''''''''''''

This method sets the level-dependent constants in the file object.

    **Available via class**
        ``shum_file_type``

    **Syntax**
        ``icode = um_file%set_level_dependent_constants(level_dependent_constants)``

    **Inputs**
        ``level_dependent_constants (64-bit REAL)``
            The level-dependent constants, which should be a 2D ``ALLOCATABLE`` array.

    **Return Value**
        ``status (shum_ff_status_type)``
            Exit status; ``0`` means success, anything else means an error has
            occurred and in that case the ``message`` variable in the object
            will contain information about the problem.


``get_level_dependent_constants``
'''''''''''''''''''''''''''''''''

This method gets the level-dependent constants in the file object.

    **Available via class**
        ``shum_file_type``

    **Syntax**
        ``icode = um_file%get_level_dependent_constants(level_dependent_constants)``

    **Input & Output**
        ``level_dependent_constants (64-bit REAL)``
            The level-dependent constants, which should be a 2D ``ALLOCATABLE`` array.

    **Return Value**
        ``status (shum_ff_status_type)``
            Exit status; ``0`` means success, anything else means an error has
            occurred and in that case the ``message`` variable in the object
            will contain information about the problem.



``set_row_dependent_constants``
'''''''''''''''''''''''''''''''

This method sets the row-dependent constants in the file object.

    **Available via class**
        ``shum_file_type``

    **Syntax**
        ``icode = um_file%set_row_dependent_constants(row_dependent_constants)``

    **Inputs**
        ``row_dependent_constants (64-bit REAL)``
            The row-dependent constants, which should be a 2D ``ALLOCATABLE`` array.

    **Return Value**
        ``status (shum_ff_status_type)``
            Exit status; ``0`` means success, anything else means an error has
            occurred and in that case the ``message`` variable in the object
            will contain information about the problem.


``get_row_dependent_constants``
'''''''''''''''''''''''''''''''

This method gets the row-dependent constants in the file object.

    **Available via class**
        ``shum_file_type``

    **Syntax**
        ``icode = um_file%get_row_dependent_constants(row_dependent_constants)``

    **Input & Output**
        ``row_dependent_constants (64-bit REAL)``
            The row-dependent constants, which should be a 2D ``ALLOCATABLE`` array.

    **Return Value**
        ``status (shum_ff_status_type)``
            Exit status; ``0`` means success, anything above ``0`` means an
            error has occurred, and a value of ``-1`` means this component was
            not present in the file. In all cases the ``message`` variable
            in the object will contain further information.


``set_column_dependent_constants``
''''''''''''''''''''''''''''''''''

This method sets the column-dependent constants in the file object.

    **Available via class**
        ``shum_file_type``

    **Syntax**
        ``icode = um_file%set_column_dependent_constants(column_dependent_constants)``

    **Inputs**
        ``column_dependent_constants (64-bit REAL)``
            The column-dependent constants, which should be a 2D ``ALLOCATABLE`` array.

    **Return Value**
        ``status (shum_ff_status_type)``
            Exit status; ``0`` means success, anything else means an error has
            occurred and in that case the ``message`` variable in the object
            will contain information about the problem.


``get_column_dependent_constants``
''''''''''''''''''''''''''''''''''

This method gets the column-dependent constants in the file object.

    **Available via class**
        ``shum_file_type``

    **Syntax**
        ``icode = um_file%get_column_dependent_constants(column_dependent_constants)``

    **Input & Output**
        ``column_dependent_constants (64-bit REAL)``
            The column-dependent constants, which should be a 2D ``ALLOCATABLE`` array.

    **Return Value**
        ``status (shum_ff_status_type)``
            Exit status; ``0`` means success, anything above ``0`` means an
            error has occurred, and a value of ``-1`` means this component was
            not present in the file. In all cases the ``message`` variable 
            in the object will contain further information.



``set_additional_parameters``
'''''''''''''''''''''''''''''

This method sets the additional parameters in the file object.

    **Available via class**
        ``shum_file_type``

    **Syntax**
        ``icode = um_file%set_additional_parameters(additional_parameters)``

    **Inputs**
        ``additional_parameters (64-bit REAL)``
            The additional parameters, which should be a 2D ``ALLOCATABLE`` array.

    **Return Value**
        ``status (shum_ff_status_type)``
            Exit status; ``0`` means success, anything else means an error has
            occurred and in that case the ``message`` variable in the object
            will contain information about the problem.


``get_additional_parameters``
'''''''''''''''''''''''''''''

This method gets the additional parameters in the file object.

    **Available via class**
        ``shum_file_type``

    **Syntax**
        ``icode = um_file%get_additional_parameters(additional_parameters)``

    **Input & Output**
        ``additional_parameters (64-bit REAL)``
            The additional parameters, which should be a 2D ``ALLOCATABLE`` array.

    **Return Value**
        ``status (shum_ff_status_type)``
            Exit status; ``0`` means success, anything above ``0`` means an
            error has occurred, and a value of ``-1`` means this component was
            not present in the file. In all cases the ``message`` variable in
            the object will contain further information.


``set_extra_constants``
'''''''''''''''''''''''

This method sets the extra constants in the file object.

    **Available via class**
        ``shum_file_type``

    **Syntax**
        ``icode = um_file%set_extra_constants(extra_constants)``

    **Inputs**
        ``extra_constants (64-bit REAL)``
            The extra constants, which should be a 1D ``ALLOCATABLE`` array.

    **Return Value**
        ``status (shum_ff_status_type)``
            Exit status; ``0`` means success, anything else means an error has
            occurred and in that case the ``message`` variable in the object
            will contain information about the problem.


``get_extra_constants``
'''''''''''''''''''''''

This method gets the extra constants in the file object.

    **Available via class**
        ``shum_file_type``

    **Syntax**
        ``icode = um_file%get_extra_constants(extra_constants)``

    **Input & Output**
        ``extra_constants (64-bit REAL)``
            The extra constants, which should be a 1D ``ALLOCATABLE`` array.

    **Return Value**
        ``status (shum_ff_status_type)``
            Exit status; ``0`` means success, anything above ``0`` means an
            error has occurred, and a value of ``-1`` means this component was
            not present in the file. In all cases the ``message`` variable in
            the object will contain further information.


``set_temp_histfile``
'''''''''''''''''''''

This method sets the temporary history file in the file object.

    **Available via class**
        ``shum_file_type``

    **Syntax**
        ``icode = um_file%set_temp_histfile(temp_histfile)``

    **Inputs**
        ``temp_histfile (64-bit REAL)``
            The temporary history file, which should be a 1D ``ALLOCATABLE`` array.

    **Return Value**
        ``status (shum_ff_status_type)``
            Exit status; ``0`` means success, anything else means an error has
            occurred and in that case the ``message`` variable in the object
            will contain information about the problem.


``get_temp_histfile``
'''''''''''''''''''''

This method gets the temporary history file in the file object.

    **Available via class**
        ``shum_file_type``

    **Syntax**
        ``icode = um_file%get_temp_histfile(temp_histfile)``

    **Input & Output**
        ``temp_histfile (64-bit REAL)``
            The temporary history file, which should be a 1D ``ALLOCATABLE`` array.

    **Return Value**
        ``status (shum_ff_status_type)``
            Exit status; ``0`` means success, anything above ``0`` means an
            error has occurred, and a value of ``-1`` means this component was
            not present in the file. In all cases the ``message`` variable in
            the object will contain further information.


``set_compressed_index``
''''''''''''''''''''''''

This method sets one of the compressed indices in the file object.

    **Available via class**
        ``shum_file_type``

    **Syntax**
        ``icode = um_file%set_compressed_index(num_index, compressed_index)``

    **Inputs**
        ``num_index (64-bit INTEGER)``
            Indicates which of the 3 compressed index headers should be returned
            (can take a value of 1, 2 or 3).

        ``compressed_index (64-bit REAL)``
            The compressed index, which should be a 1D ``ALLOCATABLE`` array.

    **Return Value**
        ``status (shum_ff_status_type)``
            Exit status; ``0`` means success, anything else means an error has
            occurred and in that case the ``message`` variable in the object
            will contain information about the problem.


``get_compressed_index``
''''''''''''''''''''''''

This method gets one of the compressed indices in the file object.

    **Available via class**
        ``shum_file_type``

    **Syntax**
        ``icode = um_file%get_compressed_index(num_index, compressed_index)``

    **Inputs**
        ``num_index (64-bit INTEGER)``
            Indicates which of the 3 compressed index headers should be returned
            (can take a value of 1, 2 or 3).

    **Input & Output**
        ``compressed_index (64-bit REAL)``
            The compressed index, which should be a 1D ``ALLOCATABLE`` array.

    **Return Value**
        ``status (shum_ff_status_type)``
            Exit status; ``0`` means success, anything above ``0`` means an
            error has occurred, and a value of ``-1`` means this component was
            not present in the file. In all cases the ``message`` variable in
            the object will contain further information.



``get_field``
'''''''''''''

This method retrieves a field object from the file object. Note that you can
call methods on a field object without detaching it from the file by calling
(for example) ``um_file%fields(field_number)%get_stashcode(stashcode)``.

    **Available via class**
        ``shum_file_type``

    **Syntax**
        ``icode = um_file%get_field(field_number, field)``

    **Inputs**
        ``field_number (64-bit INTEGER)``
            The number of the field in the file which to retrieve.

    **Input & Output**
        ``field (shum_field_type object)``
            The field object whose position in file corresponds to the
            ``field_number``.

    **Return Value**
        ``status (shum_ff_status_type)``
            Exit status; ``0`` means success, anything else means an error has
            occurred and in that case the ``message`` variable in the object
            will contain information about the problem.


``find_field_indices_in_file``
''''''''''''''''''''''''''''''

This method searches the fields in a file given a set of criteria, and returns
an array of indices defining the locations of the fields matching the specified
criteria. The returned list could be used to index the fields within the file,
for example:

  ``icode = um_file%read_field(found_field_indices(1_int64))``

  ``icode = um_file%get_field(found_field_indices(1_int64), local_field)``

where ``found_field_indices`` is a list of indices matching the criteria and 
``local_field`` is a field of type ``shum_field_type``. In this case the field
corresponding to the first index of ``found_field_indices`` is retrieved.

    **Available via class**
        ``shum_file_type``

    **Syntax**
        ``icode = um_file%find_field_indices_in_file(found_field_indices, max_returned_fields, stashcode, lbproc, fctime, level_code)``

    **Input & Output**
        ``found_field_indices (one dimensional 64-bit ALLOCATABLE INTEGER array)``
            A list of fields in the file that matches the specified criteria.

    **Inputs**
        ``max_returned_fields (optional 64-bit INTEGER)``
            This limits the number of fields returned by the method. Setting
            this to ``N`` returns only the first ``N`` matching fields.


        ``stashcode (optional 64-bit INTEGER)``
            If present, the found fields must match this STASH code. This
            corresponds to the LBUSER(4) value in the lookup.

        ``lbproc (optional 64-bit INTEGER)``
            If present, the found fields must match this LBPROC value. This
            corresponds to the LBPROC value in the lookup.

        ``fctime (optional 64-bit REAL)``
            If present, the found fields must match this forecast time within a
            certain tolerance. This is the timespan in hours between the data
            and validity time. It is **NOT** the LBFT value, which is an
            integer.

        ``level_code (optional 64-bit INTEGER)``
            If present, the found fields must match this level code. This
            corresponds to the LBLEV value in the lookup.

    **Return Value**
        ``status (shum_ff_status_type)``
            Exit status; ``0`` means success, anything above ``0`` means an
            error has occurred, and a value of ``-1`` means no matching fields
            were present in the file. In all cases the ``message`` variable in
            the object will contain further information.

``find_fields_in_file``
'''''''''''''''''''''''

This method searches the fields in a file given a set of criteria, and returns
an array of those which match. Note that fields in the returned array will
have their data loaded, so they are henceforth independent of the UM file class.

    **Available via class**
        ``shum_file_type``

    **Syntax**
        ``icode = um_file%find_fields_in_file(found_fields, max_returned_fields, stashcode, lbproc, fctime, level_code)``

    **Input & Output**
        ``found_fields (shum_field_type object ALLOCATABLE array)``
            A list of fields in the file which matches the specified criteria.

    **Inputs**
        ``max_returned_fields (optional 64-bit INTEGER)``
            This limits the number of fields returned by the method. Setting
            this to ``N`` returns only the first ``N`` matching fields.


        ``stashcode (optional 64-bit INTEGER)``
            If present, the found fields must match this STASH code. This
            corresponds to the LBUSER(4) value in the lookup.

        ``lbproc (optional 64-bit INTEGER)``
            If present, the found fields must match this LBPROC value. This
            corresponds to the LBPROC value in the lookup.


        ``fctime (optional 64-bit REAL)``
            If present, the found fields must match this forecast time within a
            certain tolerance. This is the timespan in hours between the data
            and validity time. It is **NOT** the LBFT value, which is an
            integer.

        ``level_code (optional 64-bit INTEGER)``
            If present, the found fields must match this level code. This
            corresponds to the LBLEV value in the lookup.

    **Return Value**
        ``status (shum_ff_status_type)``
            Exit status; ``0`` means success, anything above ``0`` means an
            error has occurred, and a value of ``-1`` means no matching fields
            were present in the file. In all cases the ``message`` variable in
            the object will contain further information.

``find_forecast_time``
'''''''''''''''''''''''

This method searches the fields in a file for a given stashcode, and returns
an array of forecast times associated with that stashcode.

    **Available via class**
        ``shum_file_type``

    **Syntax**
        ``icode = um_file%find_forecast_time(found_fctime, stashcode)``

    **Input & Output**
        ``found_fctime (one dimensional 64-bit ALLOCATABLE REAL array)``
            A list of forecast times for the given stashcode.

    **Inputs**
        ``stashcode (64-bit INTEGER)``
            If present, the found fields must match this STASH code. This
            corresponds to the LBUSER(4) value in the lookup.

    **Return Value**
        ``status (shum_ff_status_type)``
            Exit status; ``0`` means success, anything above ``0`` means an
            error has occurred, and a value of ``-1`` means no matching fields
            were present in the file. In all cases the ``message`` variable in
            the object will contain further information.

``set_filename``
''''''''''''''''

This method sets the filename. Note that this would normally be set when
opening a file using the ``open_file`` method rather than being modified
later using this method.

    **Available via class**
        ``shum_file_type``

    **Syntax**
        ``icode = um_file%set_filename(filename)``

    **Inputs**
        ``filename (CHARACTER(LEN=*))``
            This contains the filename of the file.


    **Return Value**
        ``status (shum_ff_status_type)``
            Exit status; ``0`` means success, anything else means an error has
            occurred and in that case the ``message`` variable in the object
            will contain information about the problem.


``get_filename``
''''''''''''''''

This method gets the filename.


    **Available via class**
        ``shum_file_type``

    **Syntax**
        ``icode = um_file%get_filename(filename)``

    **Inputs**
        ``filename (CHARACTER(LEN=*))``
            This contains the filename of the file.


    **Return Value**
        ``status (shum_ff_status_type)``
            Exit status; ``0`` means success, anything else means an error has
            occurred and in that case the ``message`` variable in the object
            will contain information about the problem.



``copy_headers_from_file_object``
'''''''''''''''''''''''''''''''''

This method copies the headers (excluding lookup and fields) from a provided
input file object into the current file object. Thus a file can be used as a
`template` for another file.


    **Available via class**
        ``shum_file_type``

    **Syntax**
        ``icode = um_file%copy_headers_from_file_object(template_file_object)``

    **Inputs**
        ``template_file_object (shum_file_type object)``
            This contains the object to copy the headers from.

    **Return Value**
        ``status (shum_ff_status_type)``
            Exit status; ``0`` means success, anything else means an error has
            occurred and in that case the ``message`` variable in the object
            will contain information about the problem.


``add_field``
'''''''''''''

This method adds a field to a file. Once added, the ``num_fields`` variable in
the file object will contain the position in the file it was added in.


    **Available via class**
        ``shum_file_type``

    **Syntax**
        ``icode = um_file%add_field(new_field)``

    **Inputs**
        ``new_field (shum_field_type object)``
            The new field to add to the file.

    **Return Value**
        ``status (shum_ff_status_type)``
            Exit status; ``0`` means success, anything else means an error has
            occurred and in that case the ``message`` variable in the object
            will contain information about the problem.


``unload_field``
''''''''''''''''

This method unloads the data in a specified field from memory. Note that this
will refuse to unload the land-sea mask, to enable land- and sea-compressed
fields to be read/written.


    **Available via class**
        ``shum_file_type``

    **Syntax**
        ``icode = um_file%unload_field(field_number)``

    **Inputs**
        ``field_number (64-bit INTEGER)``
            The index of the field in the file to unload the data for.

    **Return Value**
        ``status (shum_ff_status_type)``
            Exit status; ``0`` means success, -1 means any data was already
            not loaded.



Fortran Methods (Functions):``shum_field_type``
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

``set_lookup``
''''''''''''''

This function sets the lookup of a field. It additionally generates the REAL
representation of the forecast time, as the value in the lookup (LBFT) is an
INTEGER. For fields on fixed-resolution grids, this also generates the
longitudes and latitudes of the field; variable-resolution grids set these with
``set_longitudes`` and ``set_latitudes`` instead (which should be done
automatically when ``read_header`` is called on an existing variable resolution
file).

    **Available via class**
        ``shum_field_type``

    **Syntax**
        ``icode = um_field%set_lookup(lookup_int, lookup_real)``

    **Inputs**
        ``lookup_int (64-bit INTEGER)``
            The integer part of the lookup. This must be a 1D array of exactly
            45 elements.
        ``lookup_real (64-bit REAL)``
            The real part of the lookup. This must be a 1D array of exactly
            19 elements.

    **Return Value**
        ``status (shum_ff_status_type)``
            Exit status; ``0`` means success, anything else means an error has
            occurred.


``get_lookup``
''''''''''''''

This function sets the lookup of a field.

    **Available via class**
        ``shum_field_type``

    **Syntax**
        ``icode = um_field%get_lookup(lookup_int, lookup_real)``

    **Input & Output**
        ``lookup_int (64-bit INTEGER)``
            The integer part of the lookup. This must be a 1D array of exactly
            45 elements.
        ``lookup_real (64-bit REAL)``
            The real part of the lookup. This must be a 1D array of exactly
            19 elements.

    **Return Value**
        ``status (shum_ff_status_type)``
            Exit status; ``0`` means success, anything else means an error has
            occurred.


``set_lookup_by_index``
'''''''''''''''''''''''''''

This function sets a specific lookup entry of a field. Lookup indices 1 to 45
should have INTEGER values, and 46 to 64 should have REAL values.

    **Available via class**
        ``shum_field_type``

    **Syntax**
        ``icode = um_field%set_int_lookup_by_index(num_index, value)``

    **Inputs**
        ``num_index (64-bit INTEGER)``
            The index of the lookup entry to get (between 1 and 64).

        ``value (64-bit INTEGER or 64-bit REAL)``
            The value of the lookup entry to set.

    **Return Value**
        ``status (shum_ff_status_type)``
            Exit status; ``0`` means success, anything else means an error has
            occurred.


``get_lookup_by_index``
'''''''''''''''''''''''''''

This function gets a specific lookup entry of a field. Lookup indices 1 to 45
should have INTEGER values, and 46 to 64 should have REAL values.

    **Available via class**
        ``shum_field_type``

    **Syntax**
        ``icode = um_field%get_int_lookup_by_index(num_index, value)``

    **Inputs**
        ``num_index (64-bit INTEGER)``
            The index of the lookup entry to get (between 1 and 64).

    **Input & Output**

        ``value (64-bit INTEGER or 64-bit REAL)``
            The value of the lookup entry to get.

    **Return Value**
        ``status (shum_ff_status_type)``
            Exit status; ``0`` means success, anything else means an error has
            occurred.


``get_stashcode``
'''''''''''''''''

This function gets the stash code of the field. This corresponds to LBUSER(4)
of the field.

    **Available via class**
        ``shum_field_type``

    **Syntax**
        ``icode = um_field%get_stashcode(stashcode)``

    **Input & Output**
        ``stashcode (64-bit INTEGER)``
            The value of the STASH code.

    **Return Value**
        ``status (shum_ff_status_type)``
            Exit status; ``0`` means success, anything else means an error has
            occurred.


``get_timestring``
''''''''''''''''''

This function gets a character string containing the validity time of the field.

    **Available via class**
        ``shum_field_type``

    **Syntax**
        ``icode = um_field%get_timestring(timestring)``

    **Input & Output**
        ``timestring (CHARACTER(LEN=16)``
            String containing the validity time of the data.

    **Return Value**
        ``status (shum_ff_status_type)``
            Exit status; ``0`` means success, anything else means an error has
            occurred.


``get_level_number``
''''''''''''''''''''

This function gets the level number of the field. This corresponds to LBLEV
of the field.

    **Available via class**
        ``shum_field_type``

    **Syntax**
        ``icode = um_field%get_level_number(level_number)``

    **Input & Output**
        ``level_number (64-bit INTEGER)``
            The value of the level code.

    **Return Value**
        ``status (shum_ff_status_type)``
            Exit status; ``0`` means success, anything else means an error has
            occurred.


``get_level_eta``
'''''''''''''''''

This function gets the level eta value of the field. This corresponds to BLEV
of the field.

    **Available via class**
        ``shum_field_type``

    **Syntax**
        ``icode = um_field%get_level_eta(level_eta)``

    **Input & Output**
        ``level_eta (64-bit REAL)``
            The value of the level eta.

    **Return Value**
        ``status (shum_ff_status_type)``
            Exit status; ``0`` means success, anything else means an error has
            occurred.


``get_real_fctime``
'''''''''''''''''''

This function gets the forecast time of the field (in hours). Note this does
**NOT** correspond to the LBFT value of the field; LBFT is an integer, so it
only deals with whole numbers of hours. This method gets the forecast time
as a real number to support fractional numbers of hours; the forecast time is
calculated as the difference between the validity time and the data time.

    **Available via class**
        ``shum_field_type``

    **Syntax**
        ``icode = um_field%get_real_fctime(fctime)``

    **Input & Output**
        ``fctime (64-bit REAL)``
            The different between the validity and data times of the field.

    **Return Value**
        ``status (shum_ff_status_type)``
            Exit status; ``0`` means success, anything else means an error has
            occurred.


``get_lbproc``
''''''''''''''

This function gets the processing code of the field. This corresponds to LBPROC
of the field.

    **Available via class**
        ``shum_field_type``

    **Syntax**
        ``icode = um_field%get_lbproc(proc)``

    **Input & Output**
        ``proc (64-bit INTEGER)``
            The value of the processing code.

    **Return Value**
        ``status (shum_ff_status_type)``
            Exit status; ``0`` means success, anything else means an error has
            occurred.


``set_stashmaster_properties``
''''''''''''''''''''''''''''''

This function is intended in the future to set the properties of the field
based on the STASHmaster file (e.g. the grid type).

    **Available via class**
        ``shum_field_type``

    **Syntax**
        ``icode = um_field%set_stashmaster_properties(grid)``

    **Input & Output**
        ``grid (64-bit INTEGER)``
            The grid type code from the STASHmaster.

    **Return Value**
        ``status (shum_ff_status_type)``
            Exit status; ``0`` means success, anything else means an error has
            occurred.


``set_longitudes``
''''''''''''''''''

This function sets the longitudes of the field.

    **Available via class**
        ``shum_field_type``

    **Syntax**
        ``icode = um_field%set_longitudes(longitudes)``

    **Inputs**
        ``longitudes (64-bit REAL)``
            An ALLOCATABLE array containing the values of the longitudes.

    **Return Value**
        ``status (shum_ff_status_type)``
            Exit status; ``0`` means success, anything else means an error has
            occurred.


``get_longitudes``
''''''''''''''''''

This function gets the longitudes of the field.


    **Available via class**
        ``shum_field_type``

    **Syntax**
        ``icode = um_field%get_longitudes(longitudes)``

    **Input & Output**
        ``longitudes (64-bit REAL)``
            An ALLOCATABLE array containing the values of the longitudes.

    **Return Value**
        ``status (shum_ff_status_type)``
            Exit status; ``0`` means success, anything else means an error has
            occurred.


``set_latitudes``
'''''''''''''''''

This function sets the latitudes of the field.

    **Available via class**
        ``shum_field_type``

    **Syntax**
        ``icode = um_field%set_latitudes(latitudes)``

    **Inputs**
        ``latitudes (64-bit REAL)``
            An ALLOCATABLE array containing the values of the latitudes.

    **Return Value**
        ``status (shum_ff_status_type)``
            Exit status; ``0`` means success, anything else means an error has
            occurred.


``get_latitudes``
'''''''''''''''''

This function gets the latitudes of the field.


    **Available via class**
        ``shum_field_type``

    **Syntax**
        ``icode = um_field%get_latitudes(latitudes)``

    **Input & Output**
        ``latitudes (64-bit REAL)``
            An ALLOCATABLE array containing the values of the latitudes.

    **Return Value**
        ``status (shum_ff_status_type)``
            Exit status; ``0`` means success, anything else means an error has
            occurred.


``get_coords``
''''''''''''''

This function gets the longitude and latitude of a particular grid point.

    **Available via class**
        ``shum_field_type``

    **Syntax**
        ``icode = um_field%get_coords(x, y, coords)``

    **Inputs**
        ``x (64-bit INTEGER)``
            The x-index point.

        ``y (64-bit INTEGER)``
            The y-index point.

    **Input & Output**
        ``coords (64-bit REAL)``
            An array with two elements containing the longitude and latitude
            of the specified grid point.

    **Return Value**
        ``status (shum_ff_status_type)``
            Exit status; ``0`` means success, anything else means an error has
            occurred.


``get_pole_location``
'''''''''''''''''''''

This function gets the longitude and latitude of a rotated pole.

    **Available via class**
        ``shum_field_type``

    **Syntax**
        ``icode = um_field%get_pole_location(pole_location)``

    **Input & Output**
        ``pole_location (64-bit REAL)``
            An array with two elements containing the longitude and latitude
            of a rotated pole.

    **Return Value**
        ``status (shum_ff_status_type)``
            Exit status; ``0`` means success, anything else means an error has
            occurred.


``set_data``
'''''''''''''

This function sets the data of a field.

    **Available via class**
        ``shum_field_type``

    **Syntax**
        ``icode = um_field%set_data(data)``

    **Inputs**
        ``data (64-bit REAL or 64-bit INTEGER)``
            An array containing the data. This should have dimensions
            corresponding to LBNPT by LBROW as set in the lookup.

    **Return Value**
        ``status (shum_ff_status_type)``
            Exit status; ``0`` means success, anything else means an error has
            occurred.

``get_data``
'''''''''''''

This function gets the data of a field.

    **Available via class**
        ``shum_field_type``

    **Syntax**
        ``icode = um_field%get_data(data)``

    **Input & Output**
        ``data (64-bit REAL or 64-bit INTEGER)``
            An array containing the data. This should have dimensions
            corresponding to LBNPT by LBROW as set in the lookup.

    **Return Value**
        ``status (shum_ff_status_type)``
            Exit status; ``0`` means success, anything else means an error has
            occurred.


``get_data_by_location``
'''''''''''''''''''''''''

This function gets the datum of a field at a particular grid position.

    **Available via class**
        ``shum_field_type``

    **Syntax**
        ``icode = um_field%get_data_by_location(data)``

    **Inputs**
        ``x (64-bit INTEGER)``
            The x-index point.

        ``y (64-bit INTEGER)``
            The y-index point.

    **Input & Output**
        ``data (64-bit REAL or 64-bit INTEGER)``
            Value of the field at the specified location.

    **Return Value**
        ``status (shum_ff_status_type)``
            Exit status; ``0`` means success, anything else means an error has
            occurred.


``unload_data``
'''''''''''''''

Removes the data of a field (but not the lookup or longitudes and latitudes)
from memory.

    **Available via class**
        ``shum_field_type``

    **Syntax**
        ``icode = um_field%unload_data()``

    **Return Value**
        ``status (shum_ff_status_type)``
            Exit status; ``0`` means success, -1 means any data was already
            not loaded.
