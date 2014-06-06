#------------------------------------------------------------------------------
# Author: Edward Tate <edward.tate@erlang-solutions.com>
#------------------------------------------------------------------------------
module OclPattern

#------------------------------------------------------------------------------
# Imports
#------------------------------------------------------------------------------
import OclSkeleton

const sk = OclSkeleton

#------------------------------------------------------------------------------
# Exports
#------------------------------------------------------------------------------
export ocl_square_diff
export ocl_sum, ocl_min, ocl_max
export test

#------------------------------------------------------------------------------
# OpenCL Kernels
#------------------------------------------------------------------------------
const map_f64_src = "../cl/map_f64.cl"
const reduce_f64_src_1 = "../cl/reduce_f64_1.cl"
const reduce_f64_src_2 = "../cl/reduce_f64_2.cl"
const reduce_f64_src_3 = "../cl/reduce_f64_3.cl"
const reduce_f64_src_4 = "../cl/reduce_f64_4.cl"

#------------------------------------------------------------------------------
# OpenCL Functions
#------------------------------------------------------------------------------

# Mapping Functions
const square_f64_src = "../cl/fun/square_f64.cl"

# Reduction Functions
const sum_f64_src = "../cl/fun/sum_f64.cl"
const min_f64_src = "../cl/fun/min_f64.cl"
const max_f64_src = "../cl/fun/max_f64.cl"

#------------------------------------------------------------------------------
# Exported Functions
#------------------------------------------------------------------------------
function ocl_square(gsize, lsize, input)
    map = sk.OclMap(gsize, lsize, map_f64_src, square_f64_src, "square_f64")
    sk.run(map, input)
end

function ocl_sum (gsize, lsize, input)
    red = sk.OclReduce(gsize, lsize, reduce_f64_src_1, sum_f64_src, "sum_f64")
    sk.run(red, input)
end

function ocl_min (gsize, lsize, input)
    red = sk.OclReduce(gsize, lsize, reduce_f64_src_2, min_f64_src, "min_f64")
    sk.run(red, input)
end

function ocl_max (gsize, lsize, input)
    red = sk.OclReduce(gsize, lsize, reduce_f64_src_3, max_f64_src, "max_f64")
    sk.run(red, input)
end

function ocl_max (gsize, lsize, input, block)
    red = sk.OclReduce(gsize, lsize, reduce_f64_src_4, max_f64_src, "max_f64")
    sk.run(red, input, block)
end

#------------------------------------------------------------------------------
# Test
#------------------------------------------------------------------------------
function test ()
    gsize = 1024
    lsize = 16

    input = rand(Float64, gsize)

    ocl_square(gsize, lsize, input)
    ocl_sum(gsize, lsize, input)
    ocl_min(gsize, lsize, input)
    ocl_max(gsize, lsize, input)
end

end
