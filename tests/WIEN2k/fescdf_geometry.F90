module fescdf_geometry

    use iso_c_binding

    implicit none

    public :: &
        fescdf_geometry_t, &
        fescdf_geometry_new, &
        fescdf_geometry_read_metadata, &
        fescdf_geometry_write_metadata, &
        fescdf_geometry_free, &
        fescdf_geometry_set_file
    public :: &
        fescdf_geometry_get_number_of_physical_dimensions, &
        fescdf_geometry_get_absolute_or_reduced_coordinates, &
        fescdf_geometry_get_dimension_types, &
        fescdf_geometry_get_embedded_system, &
        fescdf_geometry_get_space_group, &
        fescdf_geometry_get_number_of_sites, &
        fescdf_geometry_get_number_of_species
        
    type :: fescdf_geometry_t
        private
        type(c_ptr) :: ptr = C_NULL_PTR
    end type fescdf_geometry_t

    interface
        subroutine escdf_geometry_new(geometry, path, mode) bind(c)
            import
            type(c_ptr) :: geometry
            character(kind=c_char) :: path(*)
            character(kind=c_char) :: mode(*)
        end subroutine escdf_geometry_new
    end interface
    interface
        subroutine escdf_geometry_read_metadata(geometry) bind(c)
            import
            type(c_ptr), value :: geometry
        end subroutine escdf_geometry_read_metadata
    end interface
    interface
        subroutine escdf_geometry_write_metadata(geometry) bind(c)
            import
            type(c_ptr), value :: geometry
        end subroutine escdf_geometry_write_metadata
    end interface
    interface
        subroutine escdf_geometry_free(geometry) bind(c)
            import
            type(c_ptr), value :: geometry
        end subroutine escdf_geometry_free
    end interface
    interface
        subroutine escdf_geometry_set_file(geometry, path, mode) bind(c)
            import
            type(c_ptr), value :: geometry
            character(kind=c_char) :: path(*)
            character(kind=c_char) :: mode(*)
        end subroutine escdf_geometry_set_file
    end interface

    interface
        subroutine escdf_geometry_get_number_of_physical_dimensions(geometry, meta) bind(c)
            import
            type(c_ptr), value :: geometry
            integer(c_int) :: meta
        end subroutine escdf_geometry_get_number_of_physical_dimensions
    end interface
    interface
        subroutine escdf_geometry_get_absolute_or_reduced_coordinates(geometry, meta) bind(c)
            import
            type(c_ptr), value :: geometry
            integer(c_int) :: meta
        end subroutine escdf_geometry_get_absolute_or_reduced_coordinates
    end interface
    interface
        subroutine escdf_geometry_get_dimension_types(geometry, meta) bind(c)
            import
            type(c_ptr), value :: geometry
            integer(c_int) :: meta
        end subroutine escdf_geometry_get_dimension_types
    end interface
    interface
        subroutine escdf_geometry_get_embedded_system(geometry, meta) bind(c)
            import
            type(c_ptr), value :: geometry
            character(kind=c_char) :: meta(*)
        end subroutine escdf_geometry_get_embedded_system
    end interface
    interface
        subroutine escdf_geometry_get_space_group(geometry, meta) bind(c)
            import
            type(c_ptr), value :: geometry
            integer(c_int) :: meta
        end subroutine escdf_geometry_get_space_group
    end interface
    interface
        subroutine escdf_geometry_get_number_of_sites(geometry, meta) bind(c)
            import
            type(c_ptr), value :: geometry
            integer(c_int) :: meta
        end subroutine escdf_geometry_get_number_of_sites
    end interface
    interface
        subroutine escdf_geometry_get_number_of_species(geometry, meta) bind(c)
            import
            type(c_ptr), value :: geometry
            integer(c_int) :: meta
        end subroutine escdf_geometry_get_number_of_species
    end interface

contains

    subroutine fescdf_geometry_new(geometry, path, mode)
        type(fescdf_geometry_t), intent(inout) :: geometry
        character(kind=c_char, len=*), intent(in) :: path
        character(kind=c_char, len=*), intent(in) :: mode

        call escdf_geometry_new(geometry%ptr, path, mode)

    end subroutine fescdf_geometry_new

    subroutine fescdf_geometry_read_metadata(geometry)
        type(fescdf_geometry_t), intent(inout) :: geometry

        call escdf_geometry_read_metadata(geometry%ptr)

    end subroutine fescdf_geometry_read_metadata

    subroutine fescdf_geometry_write_metadata(geometry)
        type(fescdf_geometry_t), intent(in) :: geometry

        call escdf_geometry_write_metadata(geometry%ptr)

    end subroutine fescdf_geometry_write_metadata

    subroutine fescdf_geometry_free(geometry)
        type(fescdf_geometry_t), intent(inout) :: geometry

        call escdf_geometry_free(geometry%ptr)
        geometry%ptr = C_NULL_PTR

    end subroutine fescdf_geometry_free

    subroutine fescdf_geometry_set_file(geometry, path, mode)
        type(fescdf_geometry_t), intent(inout) :: geometry
        character(kind=c_char, len=*), intent(in) :: path
        character(kind=c_char, len=*), intent(in) :: mode

        call escdf_geometry_set_file(geometry%ptr, path, mode)

    end subroutine fescdf_geometry_set_file

                    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    subroutine fescdf_geometry_get_number_of_physical_dimensions(geometry, meta)
        type(fescdf_geometry_t), intent(in) :: geometry
        integer, intent(out) :: meta

        call escdf_geometry_get_number_of_physical_dimensions(geometry%ptr, meta)

    end subroutine fescdf_geometry_get_number_of_physical_dimensions

    subroutine fescdf_geometry_get_absolute_or_reduced_coordinates(geometry, meta)
        type(fescdf_geometry_t), intent(in) :: geometry
        integer, pointer :: meta

        call escdf_geometry_get_absolute_or_reduced_coordinates(geometry%ptr, meta)

    end subroutine fescdf_geometry_get_absolute_or_reduced_coordinates

    subroutine fescdf_geometry_get_dimension_types(geometry, meta, dim_len)
        type(fescdf_geometry_t), intent(in) :: geometry
        integer, pointer :: meta(:)
        integer, intent(in) :: dim_len

        integer(c_int) :: tmp(dim_len)

        call escdf_geometry_get_dimension_types(geometry%ptr, tmp)

    end subroutine fescdf_geometry_get_dimension_types

    subroutine fescdf_geometry_get_embedded_system(geometry, meta)
        type(fescdf_geometry_t), intent(in) :: geometry
        character(len=3), pointer :: meta

        character(kind=c_char, len=4) :: tmp

        call escdf_geometry_get_embedded_system(geometry%ptr, meta)
        call c_to_f_string(tmp, meta)

    end subroutine fescdf_geometry_get_embedded_system

    subroutine fescdf_geometry_get_space_group(geometry, meta)
        type(fescdf_geometry_t), intent(in) :: geometry
        integer, pointer :: meta

        call escdf_geometry_get_space_group(geometry%ptr, meta)

    end subroutine fescdf_geometry_get_space_group

    subroutine fescdf_geometry_get_number_of_sites(geometry, meta)
        type(fescdf_geometry_t), intent(in) :: geometry
        integer, pointer :: meta

        call escdf_geometry_get_number_of_sites(geometry%ptr, meta)

    end subroutine fescdf_geometry_get_number_of_sites

    subroutine fescdf_geometry_get_number_of_species(geometry, meta)
        type(fescdf_geometry_t), intent(in) :: geometry
        integer, pointer :: meta

        call escdf_geometry_get_number_of_species(geometry%ptr, meta)

    end subroutine fescdf_geometry_get_number_of_species

                    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Helper functions to convert between C and Fortran strings
    ! Based on the routines by Joseph M. Krahn
    function f_to_c_string(f_string) result(c_string)
        character(len=*), intent(in) :: f_string
        character(kind=c_char,len=1) :: c_string(len_trim(f_string)+1)
            
        integer :: i, strlen

        strlen = len_trim(f_string)

        forall (i=1:strlen)
            c_string(i) = f_string(i:i)
        end forall
        c_string(strlen+1) = C_NULL_CHAR

    end function f_to_c_string

    subroutine c_to_f_string(c_string, f_string)
        character(kind=c_char,len=1), intent(in)  :: c_string(*)
        character(len=*),               intent(out) :: f_string

        integer :: i

        i = 1
        do while(c_string(i) /= C_NULL_CHAR .and. i <= len(f_string))
            f_string(i:i) = c_string(i)
            i = i + 1
        end do
        if (i < len(f_string)) f_string(i:) = ' '

    end subroutine c_to_f_string

    subroutine c_to_f_string_ptr(c_string, f_string)
        type(c_ptr),      intent(in)  :: c_string
        character(len=*), intent(out) :: f_string

        character(len=1, kind=c_char), pointer :: p_chars(:)
        integer :: i

        if (.not. c_associated(c_string)) then
            f_string = ' '
        else
            call c_f_pointer(c_string, p_chars, [huge(0)])
            i = 1
            do while(p_chars(i) /= C_NULL_CHAR .and. i <= len(f_string))
              f_string(i:i) = p_chars(i)
              i = i + 1
            end do
            if (i < len(f_string)) f_string(i:) = ' '
        end if

    end subroutine c_to_f_string_ptr

end module fescdf_geometry
