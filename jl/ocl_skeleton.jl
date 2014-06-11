#------------------------------------------------------------------------------
# Author: Edward Tate <edward.tate@erlang-solutions.com>
#------------------------------------------------------------------------------
module OclSkeleton

#------------------------------------------------------------------------------
# Imports
#------------------------------------------------------------------------------
import OpenCL

const cl = OpenCL

#------------------------------------------------------------------------------
# Exports
#------------------------------------------------------------------------------
export run
export OclMap, OclReduce

#------------------------------------------------------------------------------
# Types
#------------------------------------------------------------------------------
abstract OclSkel

immutable OclMap <: OclSkel 
    global_size::Uint32
    local_size::Uint32
    kernel_path::String
    fun_path::String
    fun_name::String
end

immutable OclReduce <: OclSkel 
    global_size::Uint32
    local_size::Uint32
    kernel_path::String
    fun_path::String
    fun_name::String
end

immutable OclReduceSerial <: OclSkel 
    global_size::Uint32
    local_size::Uint32
    kernel_path::String
    fun_path::String
    fun_name::String
end

#------------------------------------------------------------------------------
# Ready OpenCL as soon as the module is loaded
#------------------------------------------------------------------------------
dev, ctx, queue = cl.create_compute_context()

#------------------------------------------------------------------------------
# Exported Functions
#------------------------------------------------------------------------------
function run (sk::OclMap, input::Array{Float64})
    src = build_kernel_src(sk.kernel_path, sk.fun_path, sk.fun_name)

    prg = cl.Program(ctx, source=src)

    try
        cl.build!(prg)
    catch
        println(prg[:build_log][dev])
    end

    map_k = cl.Kernel(prg, "map_f64")

    i_len = length(input)

    i_buff = cl.Buffer(Float64, ctx, (:r, :copy), hostbuf=input)
    o_buff = cl.Buffer(Float64, ctx, :w, i_len)

    cl.call(queue, map_k, sk.global_size, sk.local_size, i_buff, o_buff)

    cl.read(queue, o_buff)
end

function run (sk::OclReduce, input::Array{Float64})
    src = build_kernel_src(sk.kernel_path, sk.fun_path, sk.fun_name)

    prg = cl.Program(ctx, source=src)

    try
        cl.build!(prg)
    catch
        println(prg[:build_log][dev])
    end

    reduce_k = cl.Kernel(prg, "reduce_f64")

    i_len = length(input)
    o_len = i_len / sk.local_size

    i_buff = cl.Buffer(Float64, ctx, (:r, :copy), hostbuf=input)
    s_buff = cl.LocalMem(Float64, sk.local_size)
    o_buff = cl.Buffer(Float64, ctx, :w, i_len)

    cl.call(queue, reduce_k, sk.global_size, sk.local_size, 
            i_buff, s_buff, uint32(i_len), o_buff)

    cl.read(queue, o_buff)
end

function run (sk::OclReduceSerial, input::Array{Float64})
    src = build_kernel_src(sk.kernel_path, sk.fun_path, sk.fun_name)

    prg = cl.Program(ctx, source=src)

    try
        cl.build!(prg)
    catch
        println(prg[:build_log][dev])
    end

    reduce_k = cl.Kernel(prg, "reduce_f64")

    block = 1024
    i_len = length(input)

    i_buff = cl.Buffer(Float64, ctx, (:r, :copy), hostbuf=input)
    o_buff = cl.Buffer(Float64, ctx, :w, i_len)

    cl.call(queue, reduce_k, sk.global_size, sk.local_size, 
            i_buff, uint32(block), uint32(i_len), o_buff)

    cl.read(queue, o_buff)
end

#------------------------------------------------------------------------------
# Internal Functions
#------------------------------------------------------------------------------
function build_kernel_src (kernel_path, fun_path, fun_name)
    fp64 = "#ifdef cl_khr_fp64
    #pragma OPENCL EXTENSION cl_khr_fp64 : enable
#elif defined(cl_amd_fp64)
    #pragma OPENCL EXTENSION cl_amd_fp64 : enable
#else
    #error \"Double precision floating point not supported by OpenCL implementation.\"
#endif\n\n"
    kernel_src = open(readall, kernel_path)
    fun_src = open(readall, fun_path)
    fp64 * fun_src * "\n" * replace(kernel_src, "fun_name", fun_name)
end

end
