#!/usr/bin/env ruby

def generate_assertation(t, dim, has_range, equals = "1")
  #---- variable type ------
  t_def = {
    "logical" => "logical(kind=bool)",
    "string"  => "character (len = *)", 
    "int32"   => "integer(kind=int32)",
    "int64"   => "integer(kind=int64)",
    "real32"  => "real(kind=real32)",
    "real64"  => "real(kind=real64)",
  }

  del_def = {
    "real32"  => "real(kind=real32)",
    "real64"  => "real(kind=real64)",
  }

  t_eq = { "logical" => ".eqv.", }
  t_eq.default = "=="
  
  t_ne = { "logical" => ".neqv.", }
  t_ne.default = "/="

  #---- dimension ------
  if (equals)
    base_name = "assert_eq_"
    trouble = "has difference"
  else
    base_name = "assert_not_equals_"
    trouble = "has no difference"
  end


  if dim == "0d"
    name = base_name + t + "_"
    size = ""
    integers_def = ""
    ij = ""
    nm = ""
    loop_from = ""
    loop_to   = ""
    pre_message = ""
  elsif dim == "1d32"
    name = base_name + "1d32_" + t + "_"
    size = "n, "
    integers_def = "    integer(kind=int32), intent (in) :: n" + "\n" +
                   "    integer(kind=int32)              :: i"
    ij     = "(i)"
    ij_1st = "(1)"
    nm     = "(n)"
    loop_from = "    do i = 1, n"
    loop_to   = "    enddo"
    pre_message = "'1d array #{trouble}, ' // "
  elsif dim == "2d32"
    name = base_name + "2d32_" + t + "_"
    size = "n, m, "
    integers_def = "    integer(kind=int32), intent (in) :: n, m" + "\n" +
                   "    integer(kind=int32)              :: i, j"
    ij     = "(i, j)"
    ij_1st = "(1, 1)"
    nm     = "(n, m)"
    loop_from = "    do j = 1, m" + "\n" + 
                "      do i = 1, n"
    loop_to   = "      enddo" + "\n" + 
                "    enddo"
    pre_message = "'2d array #{trouble}, ' // "
  elsif dim == "1d64"
    name = base_name + "1d64_" + t + "_"
    size = "n, "
    integers_def = "    integer(kind=int64), intent (in) :: n" + "\n" +
                   "    integer(kind=int64)              :: i"
    ij     = "(i)"
    ij_1st = "(1)"
    nm     = "(n)"
    loop_from = "    do i = 1, n"
    loop_to   = "    enddo"
    pre_message = "'1d array #{trouble}, ' // "
  elsif dim == "2d64"
    name = base_name + "2d64_" + t + "_"
    size = "n, m, "
    integers_def = "    integer(kind=int64), intent (in) :: n, m" + "\n" +
                   "    integer(kind=int64)              :: i, j"
    ij     = "(i, j)"
    ij_1st = "(1, 1)"
    nm     = "(n, m)"
    loop_from = "    do j = 1, m" + "\n" +
                "      do i = 1, n"
    loop_to   = "      enddo" + "\n" +
                "    enddo"
    pre_message = "'2d array #{trouble}, ' // "
  else
    raise
  end

  #----- comparison and range -----
  delta = ""
  del_def_line = ""
  if t == "string"
    condition = "trim(strip(var1#{ij})) #{t_ne[t]} trim(strip(var2#{ij}))"
  elsif has_range == 0
    if t == "int" or t == "logical"
      condition = "var1#{ij} #{t_ne[t]} var2#{ij}"
    elsif t == "real32" or t == "real64"
      condition = "(var1#{ij} < var2#{ij}) .or. " +
                  "(var1#{ij} > var2#{ij})"
    else
      condition = "var1#{ij} #{t_ne[t]} var2#{ij}"
    end
  else
    name = name + "in_range_"
    delta = "delta, "
    del_def_line = "#{del_def[t]}, intent (in) :: delta"
    condition = "abs(var1#{ij} - var2#{ij}) > delta"
  end
  
  #----- returns ------
  if (equals)
    interface_eq  = "    module procedure " + name + "\n"
    interface_neq  = ""
  else
    interface_eq  = ""
    interface_neq = "    module procedure " + name + "\n"
  end

  f90str = <<-"END"
  !------ #{dim}_#{t} ------
  subroutine #{name}(var1, var2, #{size}#{delta}message)
#{integers_def}
    #{t_def[t]}, intent (in) :: var1#{nm}, var2#{nm}
    #{del_def_line}
    character(len = *), intent (in), optional :: message
  END

  if (not equals)
    f90str += <<-"END"
    logical :: same_so_far

    same_so_far = .true.
    END
  end

  f90str += (loop_from + "\n")

  if (equals)
    f90str += <<-"END"
        if (#{condition}) then
          call failed_assert_action(&
          & to_s(var1#{ij}), &
          & to_s(var2#{ij}), #{pre_message}message, if_is = .true.)
          return
        endif
    END
  else
    f90str += <<-"END"
        if (#{condition}) then
          same_so_far = .false.
        endif
    END
  end

  f90str += (loop_to + "\n")

  if (not equals)
    f90str += <<-"END"
    if (same_so_far) then
      call failed_assert_action(&
      & to_s(var1#{ij_1st}), &
      & to_s(var2#{ij_1st}), #{pre_message}message, if_is = .false.)
      return
    endif
    END
  end

    f90str += <<-"END"
    call add_success
  end subroutine #{name}

    END
  return interface_eq, interface_neq, f90str
end

def many_assert()
  types = %w/ logical string int32 int64 real32 real64 /
  dims = %w/ 0d 1d32 2d32 1d64 2d64 /
  
  interface_eq = ""
  interface_neq = ""
  f90str = ""
  
  [1, nil].each{|if_equals|
    types.each {|t|
      dims.each {|dim|
        range_loop = [0]
        if (t == "real32" or t == "real64")
          range_loop = [0, 1]
        end

        range_loop.each{|has_range|
          a_interface_eq, a_interface_neq, a_f90str = 
            generate_assertation(t, dim, has_range, if_equals)
  
          interface_eq  += a_interface_eq
          interface_neq += a_interface_neq
          f90str += a_f90str
        }
      }
    }
  }
  return [interface_eq, interface_neq, f90str]
end

gen_interface_eq, gen_interface_neq, gen_f90str = many_assert()

puts "-> fruit.f90"
File::open("fruit.f90", "w"){|gen|
  File::open("fruit_f90_source.txt"){|f|
    f.each {|line|
      line.chomp!
      if line =~ /^ *!#INTERFACE_GENERATED_COMES_HERE *$/
        gen.puts "  !====== begin of generated interface ======\n"
        gen.puts gen_interface_eq
        gen.puts "  !====== end of generated inteface ======\n"
      elsif line =~ /^ *!#NEQ_INTERFACE_GENERATED_COMES_HERE *$/
        gen.puts "  !====== begin of generated interface ======\n"
        gen.puts gen_interface_neq
        gen.puts "  !====== end of generated inteface ======\n"
      elsif line =~ /^ *!#CODE_GENERATED_COMES_HERE *$/
        gen.puts "  !====== begin of generated code ======\n"
        gen.puts gen_f90str
        gen.puts "  !====== end of generated code ======\n"
      else
        gen.puts line
      end
    }
  }

}

#eof
