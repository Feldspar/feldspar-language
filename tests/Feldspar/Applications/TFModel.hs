--
-- Copyright (c) 2020, ERICSSON AB
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
--
--     * Redistributions of source code must retain the above copyright notice,
--       this list of conditions and the following disclaimer.
--     * Redistributions in binary form must reproduce the above copyright
--       notice, this list of conditions and the following disclaimer in the
--       documentation and/or other materials provided with the distribution.
--     * Neither the name of the ERICSSON AB nor the names of its contributors
--       may be used to endorse or promote products derived from this software
--       without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
-- SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
-- CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
-- OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Feldspar.Applications.TFModel (tfModel) where

import Feldspar as F
import Feldspar.Vector as V
import qualified Prelude as P

tfModel :: (Pull DIM1 (Data Float)) -> (Pull DIM1 (Data Float)) -> (Pull DIM1 (Data Float)) -> (Pull DIM1 (Data Float)) -> (Pull DIM1 (Data Int32)) -> ((Data Float), (Data Float), (Data Float), (Data Int32) {-, (Pull DIM2 (Data Float), Pull DIM2 (Data Float), Pull DIM2 (Data Float))-} )
tfModel vIteratorGetNext__0 vIteratorGetNext__1 vIteratorGetNext__2 vIteratorGetNext__3 vIteratorGetNext__4 = let
    vdnn__input_from_feature_columns__input_layer__petal_length__ExpandDims__dim__0  :: ((Data Int32)) -- Arguments:
    vdnn__input_from_feature_columns__input_layer__petal_length__ExpandDims__dim__0  =  tfConst_Int32 ( -1 )

    vdnn__input_from_feature_columns__input_layer__petal_length__ExpandDims__0  :: ((Pull DIM2 (Data Float))) -- Arguments:  (Pull DIM1 (Data Float))  (Data Int32)
    vdnn__input_from_feature_columns__input_layer__petal_length__ExpandDims__0  =  tfExpandDims__LPull_DIM2_FloatR___LPull_DIM1_FloatR__Int32 vIteratorGetNext__0 vdnn__input_from_feature_columns__input_layer__petal_length__ExpandDims__dim__0

    vdnn__input_from_feature_columns__input_layer__petal_length__Shape__0  :: ((Pull DIM1 (Data Int32))) -- Arguments:  (Pull DIM2 (Data Float))
    vdnn__input_from_feature_columns__input_layer__petal_length__Shape__0  =  tfShape__LPull_DIM1_Int32R___LPull_DIM2_FloatR_ vdnn__input_from_feature_columns__input_layer__petal_length__ExpandDims__0

    vdnn__input_from_feature_columns__input_layer__petal_length__strided_slice__stack__0  :: ((Pull DIM1 (Data Int32))) -- Arguments:
    vdnn__input_from_feature_columns__input_layer__petal_length__strided_slice__stack__0  =  tfConst__LPull_DIM1_Int32R_ [0]

    vdnn__input_from_feature_columns__input_layer__petal_length__strided_slice__stack_1__0  :: ((Pull DIM1 (Data Int32))) -- Arguments:
    vdnn__input_from_feature_columns__input_layer__petal_length__strided_slice__stack_1__0  =  tfConst__LPull_DIM1_Int32R_ [1]

    vdnn__input_from_feature_columns__input_layer__petal_length__strided_slice__stack_2__0  :: ((Pull DIM1 (Data Int32))) -- Arguments:
    vdnn__input_from_feature_columns__input_layer__petal_length__strided_slice__stack_2__0  =  tfConst__LPull_DIM1_Int32R_ [1]

    vdnn__input_from_feature_columns__input_layer__petal_length__strided_slice__0  :: ((Data Int32)) -- Arguments:  (Pull DIM1 (Data Int32))  (Pull DIM1 (Data Int32))  (Pull DIM1 (Data Int32))  (Pull DIM1 (Data Int32))
    vdnn__input_from_feature_columns__input_layer__petal_length__strided_slice__0  =  tfStridedSlice_Int32__LPull_DIM1_Int32R___LPull_DIM1_Int32R___LPull_DIM1_Int32R___LPull_DIM1_Int32R_ vdnn__input_from_feature_columns__input_layer__petal_length__Shape__0 vdnn__input_from_feature_columns__input_layer__petal_length__strided_slice__stack__0 vdnn__input_from_feature_columns__input_layer__petal_length__strided_slice__stack_1__0 vdnn__input_from_feature_columns__input_layer__petal_length__strided_slice__stack_2__0

    vdnn__input_from_feature_columns__input_layer__petal_length__Reshape__shape__1__0  :: ((Data Int32)) -- Arguments:
    vdnn__input_from_feature_columns__input_layer__petal_length__Reshape__shape__1__0  =  tfConst_Int32 ( 1 )

    vdnn__input_from_feature_columns__input_layer__petal_length__Reshape__shape__0  :: ((Pull DIM1 (Data Int32))) -- Arguments:  (Data Int32)  (Data Int32)
    vdnn__input_from_feature_columns__input_layer__petal_length__Reshape__shape__0  =  tfPack__LPull_DIM1_Int32R__Int32_Int32 vdnn__input_from_feature_columns__input_layer__petal_length__strided_slice__0 vdnn__input_from_feature_columns__input_layer__petal_length__Reshape__shape__1__0

    vdnn__input_from_feature_columns__input_layer__petal_length__Reshape__0  :: ((Pull DIM2 (Data Float))) -- Arguments:  (Pull DIM2 (Data Float))  (Pull DIM1 (Data Int32))
    vdnn__input_from_feature_columns__input_layer__petal_length__Reshape__0  =  tfReshape__LPull_DIM2_FloatR___LPull_DIM2_FloatR___LPull_DIM1_Int32R_ vdnn__input_from_feature_columns__input_layer__petal_length__ExpandDims__0 vdnn__input_from_feature_columns__input_layer__petal_length__Reshape__shape__0

    vdnn__input_from_feature_columns__input_layer__petal_width__ExpandDims__dim__0  :: ((Data Int32)) -- Arguments:
    vdnn__input_from_feature_columns__input_layer__petal_width__ExpandDims__dim__0  =  tfConst_Int32 ( -1 )

    vdnn__input_from_feature_columns__input_layer__petal_width__ExpandDims__0  :: ((Pull DIM2 (Data Float))) -- Arguments:  (Pull DIM1 (Data Float))  (Data Int32)
    vdnn__input_from_feature_columns__input_layer__petal_width__ExpandDims__0  =  tfExpandDims__LPull_DIM2_FloatR___LPull_DIM1_FloatR__Int32 vIteratorGetNext__1 vdnn__input_from_feature_columns__input_layer__petal_width__ExpandDims__dim__0

    vdnn__input_from_feature_columns__input_layer__petal_width__Shape__0  :: ((Pull DIM1 (Data Int32))) -- Arguments:  (Pull DIM2 (Data Float))
    vdnn__input_from_feature_columns__input_layer__petal_width__Shape__0  =  tfShape__LPull_DIM1_Int32R___LPull_DIM2_FloatR_ vdnn__input_from_feature_columns__input_layer__petal_width__ExpandDims__0

    vdnn__input_from_feature_columns__input_layer__petal_width__strided_slice__stack__0  :: ((Pull DIM1 (Data Int32))) -- Arguments:
    vdnn__input_from_feature_columns__input_layer__petal_width__strided_slice__stack__0  =  tfConst__LPull_DIM1_Int32R_ [0]

    vdnn__input_from_feature_columns__input_layer__petal_width__strided_slice__stack_1__0  :: ((Pull DIM1 (Data Int32))) -- Arguments:
    vdnn__input_from_feature_columns__input_layer__petal_width__strided_slice__stack_1__0  =  tfConst__LPull_DIM1_Int32R_ [1]

    vdnn__input_from_feature_columns__input_layer__petal_width__strided_slice__stack_2__0  :: ((Pull DIM1 (Data Int32))) -- Arguments:
    vdnn__input_from_feature_columns__input_layer__petal_width__strided_slice__stack_2__0  =  tfConst__LPull_DIM1_Int32R_ [1]

    vdnn__input_from_feature_columns__input_layer__petal_width__strided_slice__0  :: ((Data Int32)) -- Arguments:  (Pull DIM1 (Data Int32))  (Pull DIM1 (Data Int32))  (Pull DIM1 (Data Int32))  (Pull DIM1 (Data Int32))
    vdnn__input_from_feature_columns__input_layer__petal_width__strided_slice__0  =  tfStridedSlice_Int32__LPull_DIM1_Int32R___LPull_DIM1_Int32R___LPull_DIM1_Int32R___LPull_DIM1_Int32R_ vdnn__input_from_feature_columns__input_layer__petal_width__Shape__0 vdnn__input_from_feature_columns__input_layer__petal_width__strided_slice__stack__0 vdnn__input_from_feature_columns__input_layer__petal_width__strided_slice__stack_1__0 vdnn__input_from_feature_columns__input_layer__petal_width__strided_slice__stack_2__0

    vdnn__input_from_feature_columns__input_layer__petal_width__Reshape__shape__1__0  :: ((Data Int32)) -- Arguments:
    vdnn__input_from_feature_columns__input_layer__petal_width__Reshape__shape__1__0  =  tfConst_Int32 ( 1 )

    vdnn__input_from_feature_columns__input_layer__petal_width__Reshape__shape__0  :: ((Pull DIM1 (Data Int32))) -- Arguments:  (Data Int32)  (Data Int32)
    vdnn__input_from_feature_columns__input_layer__petal_width__Reshape__shape__0  =  tfPack__LPull_DIM1_Int32R__Int32_Int32 vdnn__input_from_feature_columns__input_layer__petal_width__strided_slice__0 vdnn__input_from_feature_columns__input_layer__petal_width__Reshape__shape__1__0

    vdnn__input_from_feature_columns__input_layer__petal_width__Reshape__0  :: ((Pull DIM2 (Data Float))) -- Arguments:  (Pull DIM2 (Data Float))  (Pull DIM1 (Data Int32))
    vdnn__input_from_feature_columns__input_layer__petal_width__Reshape__0  =  tfReshape__LPull_DIM2_FloatR___LPull_DIM2_FloatR___LPull_DIM1_Int32R_ vdnn__input_from_feature_columns__input_layer__petal_width__ExpandDims__0 vdnn__input_from_feature_columns__input_layer__petal_width__Reshape__shape__0

    vdnn__input_from_feature_columns__input_layer__sepal_length__ExpandDims__dim__0  :: ((Data Int32)) -- Arguments:
    vdnn__input_from_feature_columns__input_layer__sepal_length__ExpandDims__dim__0  =  tfConst_Int32 ( -1 )

    vdnn__input_from_feature_columns__input_layer__sepal_length__ExpandDims__0  :: ((Pull DIM2 (Data Float))) -- Arguments:  (Pull DIM1 (Data Float))  (Data Int32)
    vdnn__input_from_feature_columns__input_layer__sepal_length__ExpandDims__0  =  tfExpandDims__LPull_DIM2_FloatR___LPull_DIM1_FloatR__Int32 vIteratorGetNext__2 vdnn__input_from_feature_columns__input_layer__sepal_length__ExpandDims__dim__0

    vdnn__input_from_feature_columns__input_layer__sepal_length__Shape__0  :: ((Pull DIM1 (Data Int32))) -- Arguments:  (Pull DIM2 (Data Float))
    vdnn__input_from_feature_columns__input_layer__sepal_length__Shape__0  =  tfShape__LPull_DIM1_Int32R___LPull_DIM2_FloatR_ vdnn__input_from_feature_columns__input_layer__sepal_length__ExpandDims__0

    vdnn__input_from_feature_columns__input_layer__sepal_length__strided_slice__stack__0  :: ((Pull DIM1 (Data Int32))) -- Arguments:
    vdnn__input_from_feature_columns__input_layer__sepal_length__strided_slice__stack__0  =  tfConst__LPull_DIM1_Int32R_ [0]

    vdnn__input_from_feature_columns__input_layer__sepal_length__strided_slice__stack_1__0  :: ((Pull DIM1 (Data Int32))) -- Arguments:
    vdnn__input_from_feature_columns__input_layer__sepal_length__strided_slice__stack_1__0  =  tfConst__LPull_DIM1_Int32R_ [1]

    vdnn__input_from_feature_columns__input_layer__sepal_length__strided_slice__stack_2__0  :: ((Pull DIM1 (Data Int32))) -- Arguments:
    vdnn__input_from_feature_columns__input_layer__sepal_length__strided_slice__stack_2__0  =  tfConst__LPull_DIM1_Int32R_ [1]

    vdnn__input_from_feature_columns__input_layer__sepal_length__strided_slice__0  :: ((Data Int32)) -- Arguments:  (Pull DIM1 (Data Int32))  (Pull DIM1 (Data Int32))  (Pull DIM1 (Data Int32))  (Pull DIM1 (Data Int32))
    vdnn__input_from_feature_columns__input_layer__sepal_length__strided_slice__0  =  tfStridedSlice_Int32__LPull_DIM1_Int32R___LPull_DIM1_Int32R___LPull_DIM1_Int32R___LPull_DIM1_Int32R_ vdnn__input_from_feature_columns__input_layer__sepal_length__Shape__0 vdnn__input_from_feature_columns__input_layer__sepal_length__strided_slice__stack__0 vdnn__input_from_feature_columns__input_layer__sepal_length__strided_slice__stack_1__0 vdnn__input_from_feature_columns__input_layer__sepal_length__strided_slice__stack_2__0

    vdnn__input_from_feature_columns__input_layer__sepal_length__Reshape__shape__1__0  :: ((Data Int32)) -- Arguments:
    vdnn__input_from_feature_columns__input_layer__sepal_length__Reshape__shape__1__0  =  tfConst_Int32 ( 1 )

    vdnn__input_from_feature_columns__input_layer__sepal_length__Reshape__shape__0  :: ((Pull DIM1 (Data Int32))) -- Arguments:  (Data Int32)  (Data Int32)
    vdnn__input_from_feature_columns__input_layer__sepal_length__Reshape__shape__0  =  tfPack__LPull_DIM1_Int32R__Int32_Int32 vdnn__input_from_feature_columns__input_layer__sepal_length__strided_slice__0 vdnn__input_from_feature_columns__input_layer__sepal_length__Reshape__shape__1__0

    vdnn__input_from_feature_columns__input_layer__sepal_length__Reshape__0  :: ((Pull DIM2 (Data Float))) -- Arguments:  (Pull DIM2 (Data Float))  (Pull DIM1 (Data Int32))
    vdnn__input_from_feature_columns__input_layer__sepal_length__Reshape__0  =  tfReshape__LPull_DIM2_FloatR___LPull_DIM2_FloatR___LPull_DIM1_Int32R_ vdnn__input_from_feature_columns__input_layer__sepal_length__ExpandDims__0 vdnn__input_from_feature_columns__input_layer__sepal_length__Reshape__shape__0

    vdnn__input_from_feature_columns__input_layer__sepal_width__ExpandDims__dim__0  :: ((Data Int32)) -- Arguments:
    vdnn__input_from_feature_columns__input_layer__sepal_width__ExpandDims__dim__0  =  tfConst_Int32 ( -1 )

    vdnn__input_from_feature_columns__input_layer__sepal_width__ExpandDims__0  :: ((Pull DIM2 (Data Float))) -- Arguments:  (Pull DIM1 (Data Float))  (Data Int32)
    vdnn__input_from_feature_columns__input_layer__sepal_width__ExpandDims__0  =  tfExpandDims__LPull_DIM2_FloatR___LPull_DIM1_FloatR__Int32 vIteratorGetNext__3 vdnn__input_from_feature_columns__input_layer__sepal_width__ExpandDims__dim__0

    vdnn__input_from_feature_columns__input_layer__sepal_width__Shape__0  :: ((Pull DIM1 (Data Int32))) -- Arguments:  (Pull DIM2 (Data Float))
    vdnn__input_from_feature_columns__input_layer__sepal_width__Shape__0  =  tfShape__LPull_DIM1_Int32R___LPull_DIM2_FloatR_ vdnn__input_from_feature_columns__input_layer__sepal_width__ExpandDims__0

    vdnn__input_from_feature_columns__input_layer__sepal_width__strided_slice__stack__0  :: ((Pull DIM1 (Data Int32))) -- Arguments:
    vdnn__input_from_feature_columns__input_layer__sepal_width__strided_slice__stack__0  =  tfConst__LPull_DIM1_Int32R_ [0]

    vdnn__input_from_feature_columns__input_layer__sepal_width__strided_slice__stack_1__0  :: ((Pull DIM1 (Data Int32))) -- Arguments:
    vdnn__input_from_feature_columns__input_layer__sepal_width__strided_slice__stack_1__0  =  tfConst__LPull_DIM1_Int32R_ [1]

    vdnn__input_from_feature_columns__input_layer__sepal_width__strided_slice__stack_2__0  :: ((Pull DIM1 (Data Int32))) -- Arguments:
    vdnn__input_from_feature_columns__input_layer__sepal_width__strided_slice__stack_2__0  =  tfConst__LPull_DIM1_Int32R_ [1]

    vdnn__input_from_feature_columns__input_layer__sepal_width__strided_slice__0  :: ((Data Int32)) -- Arguments:  (Pull DIM1 (Data Int32))  (Pull DIM1 (Data Int32))  (Pull DIM1 (Data Int32))  (Pull DIM1 (Data Int32))
    vdnn__input_from_feature_columns__input_layer__sepal_width__strided_slice__0  =  tfStridedSlice_Int32__LPull_DIM1_Int32R___LPull_DIM1_Int32R___LPull_DIM1_Int32R___LPull_DIM1_Int32R_ vdnn__input_from_feature_columns__input_layer__sepal_width__Shape__0 vdnn__input_from_feature_columns__input_layer__sepal_width__strided_slice__stack__0 vdnn__input_from_feature_columns__input_layer__sepal_width__strided_slice__stack_1__0 vdnn__input_from_feature_columns__input_layer__sepal_width__strided_slice__stack_2__0

    vdnn__input_from_feature_columns__input_layer__sepal_width__Reshape__shape__1__0  :: ((Data Int32)) -- Arguments:
    vdnn__input_from_feature_columns__input_layer__sepal_width__Reshape__shape__1__0  =  tfConst_Int32 ( 1 )

    vdnn__input_from_feature_columns__input_layer__sepal_width__Reshape__shape__0  :: ((Pull DIM1 (Data Int32))) -- Arguments:  (Data Int32)  (Data Int32)
    vdnn__input_from_feature_columns__input_layer__sepal_width__Reshape__shape__0  =  tfPack__LPull_DIM1_Int32R__Int32_Int32 vdnn__input_from_feature_columns__input_layer__sepal_width__strided_slice__0 vdnn__input_from_feature_columns__input_layer__sepal_width__Reshape__shape__1__0

    vdnn__input_from_feature_columns__input_layer__sepal_width__Reshape__0  :: ((Pull DIM2 (Data Float))) -- Arguments:  (Pull DIM2 (Data Float))  (Pull DIM1 (Data Int32))
    vdnn__input_from_feature_columns__input_layer__sepal_width__Reshape__0  =  tfReshape__LPull_DIM2_FloatR___LPull_DIM2_FloatR___LPull_DIM1_Int32R_ vdnn__input_from_feature_columns__input_layer__sepal_width__ExpandDims__0 vdnn__input_from_feature_columns__input_layer__sepal_width__Reshape__shape__0

    vdnn__input_from_feature_columns__input_layer__concat__axis__0  :: ((Data Int32)) -- Arguments:
    vdnn__input_from_feature_columns__input_layer__concat__axis__0  =  tfConst_Int32 ( 1 )

    vdnn__input_from_feature_columns__input_layer__concat__0  :: ((Pull DIM2 (Data Float))) -- Arguments:  (Pull DIM2 (Data Float))  (Pull DIM2 (Data Float))  (Pull DIM2 (Data Float))  (Pull DIM2 (Data Float))  (Data Int32)
    vdnn__input_from_feature_columns__input_layer__concat__0  =  tfConcatV2__LPull_DIM2_FloatR___LPull_DIM2_FloatR___LPull_DIM2_FloatR___LPull_DIM2_FloatR___LPull_DIM2_FloatR__Int32 vdnn__input_from_feature_columns__input_layer__petal_length__Reshape__0 vdnn__input_from_feature_columns__input_layer__petal_width__Reshape__0 vdnn__input_from_feature_columns__input_layer__sepal_length__Reshape__0 vdnn__input_from_feature_columns__input_layer__sepal_width__Reshape__0 vdnn__input_from_feature_columns__input_layer__concat__axis__0

    vdnn__hiddenlayer_0__kernel__part_0__0  :: ((Pull DIM2 (Data Float))) -- Arguments:
    vdnn__hiddenlayer_0__kernel__part_0__0  =  tfVariableV2__LPull_DIM2_FloatR_ [
        [-0.22915776, -0.1284789, 0.40061894, 0.88190246, 0.6411275, -0.011825498, -0.48292926, -0.14272195, 0.13295156, -0.41784248], 
        [-0.50960994, -0.36688396, 0.17651801, 0.8389609, 0.453085, -0.582387, -0.22155705, 0.24676925, 0.47222018, -0.16324177], 
        [0.4736216, 0.5858053, -0.5900148, -0.105154976, 0.58443266, -0.28998458, -0.13483047, -0.53848416, -0.5437269, 0.18252921], 
        [0.2369335, -0.22993013, 0.073349, -0.45011044, -0.5658791, 0.3757531, -0.48686343, -0.46701938, -0.3996953, 0.49442783]]

    vdnn__hiddenlayer_0__kernel__part_0__read__0  :: ((Pull DIM2 (Data Float))) -- Arguments:  (Pull DIM2 (Data Float))
    vdnn__hiddenlayer_0__kernel__part_0__read__0  =  tfIdentity__LPull_DIM2_FloatR___LPull_DIM2_FloatR_ vdnn__hiddenlayer_0__kernel__part_0__0

    vdnn__hiddenlayer_0__kernel__0  :: ((Pull DIM2 (Data Float))) -- Arguments:  (Pull DIM2 (Data Float))
    vdnn__hiddenlayer_0__kernel__0  =  tfIdentity__LPull_DIM2_FloatR___LPull_DIM2_FloatR_ vdnn__hiddenlayer_0__kernel__part_0__read__0

    vdnn__hiddenlayer_0__MatMul__0  :: ((Pull DIM2 (Data Float))) -- Arguments:  (Pull DIM2 (Data Float))  (Pull DIM2 (Data Float))
    vdnn__hiddenlayer_0__MatMul__0  =  tfMatMul__LPull_DIM2_FloatR___LPull_DIM2_FloatR___LPull_DIM2_FloatR_ vdnn__input_from_feature_columns__input_layer__concat__0 vdnn__hiddenlayer_0__kernel__0

    vdnn__hiddenlayer_0__bias__part_0__0  :: ((Pull DIM1 (Data Float))) -- Arguments:
    vdnn__hiddenlayer_0__bias__part_0__0  =  tfVariableV2__LPull_DIM1_FloatR_ [0.2922249, 0.09835548, -0.010420964, 0.054963745, -0.061770055, -0.09184818, 0.0, 0.0, 0.0, 0.17531496]

    vdnn__hiddenlayer_0__bias__part_0__read__0  :: ((Pull DIM1 (Data Float))) -- Arguments:  (Pull DIM1 (Data Float))
    vdnn__hiddenlayer_0__bias__part_0__read__0  =  tfIdentity__LPull_DIM1_FloatR___LPull_DIM1_FloatR_ vdnn__hiddenlayer_0__bias__part_0__0

    vdnn__hiddenlayer_0__bias__0  :: ((Pull DIM1 (Data Float))) -- Arguments:  (Pull DIM1 (Data Float))
    vdnn__hiddenlayer_0__bias__0  =  tfIdentity__LPull_DIM1_FloatR___LPull_DIM1_FloatR_ vdnn__hiddenlayer_0__bias__part_0__read__0

    vdnn__hiddenlayer_0__BiasAdd__0  :: ((Pull DIM2 (Data Float))) -- Arguments:  (Pull DIM2 (Data Float))  (Pull DIM1 (Data Float))
    vdnn__hiddenlayer_0__BiasAdd__0  =  tfBiasAdd__LPull_DIM2_FloatR___LPull_DIM2_FloatR___LPull_DIM1_FloatR_ vdnn__hiddenlayer_0__MatMul__0 vdnn__hiddenlayer_0__bias__0

    vdnn__hiddenlayer_0__Relu__0  :: ((Pull DIM2 (Data Float))) -- Arguments:  (Pull DIM2 (Data Float))
    vdnn__hiddenlayer_0__Relu__0  =  tfRelu__LPull_DIM2_FloatR___LPull_DIM2_FloatR_ vdnn__hiddenlayer_0__BiasAdd__0

    vdnn__hiddenlayer_1__kernel__part_0__0  :: ((Pull DIM2 (Data Float))) -- Arguments:
    vdnn__hiddenlayer_1__kernel__part_0__0  =  tfVariableV2__LPull_DIM2_FloatR_ [
        [0.91902715, -0.35829556, -0.09224357, -0.40747288, 0.7492209, 0.07088247, -0.23477161, 0.1152808, 0.31937864, -0.0580272, -0.25888464, 0.7409935, -0.06215009, 0.18443955, 0.24944644, 0.3284415, -0.22745858, 0.13451082, -0.19227692, 0.20338748], 
        [0.35662603, 0.2173161, 0.18877985, 0.011599037, 0.08809733, -0.25497255, -0.09583646, 0.10031079, -0.06803849, -0.2061738, 0.096870065, 0.43047985, -0.100177646, -0.39573732, 0.1660326, -0.3947237, -0.31452027, -0.21445248, -0.4254448, -0.015182592], 
        [-0.42269197, -0.4195275, -0.11207464, -0.13700771, 0.41393203, -0.20901743, 0.04732743, 0.09225946, -0.09392339, -0.33847886, 0.10363752, -0.17361541, 0.107595384, -0.18353471, 0.32443026, -0.083189875, 0.37931567, 0.079524875, 0.40435052, 0.3017344], 
        [-0.5622554, 0.7133307, 0.15080151, -0.3495558, 0.14575793, 0.30195674, 0.15121377, 0.33185408, 0.19357172, 0.6620173, 0.038580954, 0.18639979, 0.038617194, -0.40589866, -0.13531274, 0.011533737, -0.2762689, -0.13046473, -0.3581965, 0.18179104], 
        [0.0046178717, 0.10696611, 0.4096734, -0.32276788, 0.35429397, -0.23742612, -0.29443413, 0.13601224, -0.22504915, 0.44862476, -0.3532423, 0.27512944, -0.12803316, 0.034778032, 0.26412883, -0.17623931, 0.33495125, -0.4345055, -0.2594989, -0.23582277], 
        [-0.18266484, 0.3415615, 0.27812475, 0.3482477, -0.440592, -0.2926774, 0.28001595, -0.21678749, -0.023915023, -0.3279026, 0.2513073, 0.13292778, 0.2932229, -0.43424323, -0.17124985, -0.27325734, -0.021128714, -0.07839084, -0.10904494, 0.2434184], 
        [-0.42991394, -0.10096431, -0.009710252, -0.38018996, 0.13879794, -0.13311616, -0.109891415, -0.41724372, 0.11820352, 0.3472579, 0.41961545, -0.25845933, 0.3360781, 0.09124148, 0.17656809, -0.25353009, -0.09852135, 0.010751873, -0.1318874, 0.38414335], 
        [0.044273168, -0.18019396, 0.31458718, 0.4186619, 0.010449141, 0.25162756, -0.04306394, 0.31070393, -0.13772273, -0.3997048, 0.120829344, -0.32234043, 0.3216492, -0.27276868, 0.41848117, -0.1806595, -0.07468596, -0.14288163, -0.12519634, -0.019973993], 
        [-0.06213355, -0.3571053, -0.09315753, 0.1604957, -0.21493411, -0.3577929, 0.27601504, -0.17653474, 0.10841358, 0.26419187, -0.12400043, 0.18410826, -0.06698313, 0.24414581, 0.039065987, -0.20297644, -0.22071686, 0.122852445, 0.25033498, -0.11503911], 
        [0.36407644, 0.05116071, -0.5306618, 0.33854377, 0.6751507, -0.31676435, -0.08861971, -0.4757653, -0.41518882, -0.41636807, -0.3065623, -0.19379324, 0.029355407, 0.22312047, 0.65588176, -0.021840472, -0.31186697, -0.14150244, -0.24774861, -0.1970184]]

    vdnn__hiddenlayer_1__kernel__part_0__read__0  :: ((Pull DIM2 (Data Float))) -- Arguments:  (Pull DIM2 (Data Float))
    vdnn__hiddenlayer_1__kernel__part_0__read__0  =  tfIdentity__LPull_DIM2_FloatR___LPull_DIM2_FloatR_ vdnn__hiddenlayer_1__kernel__part_0__0

    vdnn__hiddenlayer_1__kernel__0  :: ((Pull DIM2 (Data Float))) -- Arguments:  (Pull DIM2 (Data Float))
    vdnn__hiddenlayer_1__kernel__0  =  tfIdentity__LPull_DIM2_FloatR___LPull_DIM2_FloatR_ vdnn__hiddenlayer_1__kernel__part_0__read__0

    vdnn__hiddenlayer_1__MatMul__0  :: ((Pull DIM2 (Data Float))) -- Arguments:  (Pull DIM2 (Data Float))  (Pull DIM2 (Data Float))
    vdnn__hiddenlayer_1__MatMul__0  =  tfMatMul__LPull_DIM2_FloatR___LPull_DIM2_FloatR___LPull_DIM2_FloatR_ vdnn__hiddenlayer_0__Relu__0 vdnn__hiddenlayer_1__kernel__0

    vdnn__hiddenlayer_1__bias__part_0__0  :: ((Pull DIM1 (Data Float))) -- Arguments:
    vdnn__hiddenlayer_1__bias__part_0__0  =  tfVariableV2__LPull_DIM1_FloatR_ [0.4430543, -0.23873109, -0.23747349, -0.005787201, 0.32123396, -0.026662799, 0.0, -0.12564348, -0.0019232112, 0.012768095, 0.0, 0.2855824, 0.0, -0.15287939, 0.2384614, -0.02637416, -0.027746255, 0.0, 0.0, -0.021932427]

    vdnn__hiddenlayer_1__bias__part_0__read__0  :: ((Pull DIM1 (Data Float))) -- Arguments:  (Pull DIM1 (Data Float))
    vdnn__hiddenlayer_1__bias__part_0__read__0  =  tfIdentity__LPull_DIM1_FloatR___LPull_DIM1_FloatR_ vdnn__hiddenlayer_1__bias__part_0__0

    vdnn__hiddenlayer_1__bias__0  :: ((Pull DIM1 (Data Float))) -- Arguments:  (Pull DIM1 (Data Float))
    vdnn__hiddenlayer_1__bias__0  =  tfIdentity__LPull_DIM1_FloatR___LPull_DIM1_FloatR_ vdnn__hiddenlayer_1__bias__part_0__read__0

    vdnn__hiddenlayer_1__BiasAdd__0  :: ((Pull DIM2 (Data Float))) -- Arguments:  (Pull DIM2 (Data Float))  (Pull DIM1 (Data Float))
    vdnn__hiddenlayer_1__BiasAdd__0  =  tfBiasAdd__LPull_DIM2_FloatR___LPull_DIM2_FloatR___LPull_DIM1_FloatR_ vdnn__hiddenlayer_1__MatMul__0 vdnn__hiddenlayer_1__bias__0

    vdnn__hiddenlayer_1__Relu__0  :: ((Pull DIM2 (Data Float))) -- Arguments:  (Pull DIM2 (Data Float))
    vdnn__hiddenlayer_1__Relu__0  =  tfRelu__LPull_DIM2_FloatR___LPull_DIM2_FloatR_ vdnn__hiddenlayer_1__BiasAdd__0

    vdnn__hiddenlayer_2__kernel__part_0__0  :: ((Pull DIM2 (Data Float))) -- Arguments:
    vdnn__hiddenlayer_2__kernel__part_0__0  =  tfVariableV2__LPull_DIM2_FloatR_ [
        [0.733978, -0.22337979, 0.05756271, -0.6250087, -0.108720556, -0.056013744, -0.15824348, 1.0348238, -0.11416438, 0.17187628], 
        [-0.40059328, 0.24101685, -0.118201464, 0.44585276, 0.15399876, -0.34718132, 0.3489313, -0.2740674, 0.18136132, -0.39949185], 
        [-0.4890623, 0.34056452, 0.24307007, 0.31925446, -0.18556026, 0.136404, 0.16395053, -0.10578544, -0.08262828, 0.3855685], 
        [-0.11366382, 0.26642495, 0.3533579, -0.13009264, 0.25430554, 0.30393267, -0.33465832, -0.20448783, -0.33136892, 0.074239016], 
        [0.45685878, -0.22882845, -0.25434595, 0.49234962, 0.15805432, 0.015673228, -0.02078916, 0.58093596, -0.33571833, -0.14481959], 
        [-0.32780758, 0.3061437, 0.38328183, -0.17125037, -0.08259513, -0.40341204, 0.34626484, -0.28618753, -0.35027325, -0.054013282], 
        [-0.03911683, 0.11623287, -0.18230042, -0.11083579, -0.42701057, -0.1704059, 0.27617127, -0.3758691, -0.13260809, 0.23518288], 
        [-0.17018762, -0.09394379, 0.43505174, 0.63032776, 0.041129526, -0.42323402, 0.19231428, -0.19358854, -0.038122028, 0.33970994], 
        [0.36439317, -0.20619626, 0.18864459, -0.28858137, -0.042990178, -0.3863318, 0.019200206, -0.22542068, 0.35247868, 0.3467402], 
        [-0.42888886, -0.47832388, 0.04193908, 0.87248045, 0.07521787, 0.40162098, -0.40372947, -0.39106426, 0.047203004, -0.20189966], 
        [0.04729873, 0.1831581, 0.08624935, -0.4373401, 0.078594804, 0.12810004, 0.3148359, -0.3250374, -0.14504185, 0.31794602], 
        [0.40538433, -0.45166394, -0.39693022, 0.5146139, -0.4722584, -0.029476827, -0.30965152, 0.009959998, -0.34476623, -0.2140796], 
        [0.30166793, 0.35991007, 0.16484255, -0.32360286, 0.16529721, 0.18460673, 0.11441356, -0.0757457, 0.2007947, 0.38534182], 
        [0.041597422, 0.024711803, -0.4436319, 0.061200038, 0.3162695, 0.26019573, 0.081287324, 0.03747905, -0.018881828, 0.04639857], 
        [0.49863812, 0.33036762, 0.13178337, 0.3115996, -0.2088872, -0.21293768, -0.19692189, 0.29911798, 0.1992141, 0.030161068], 
        [-0.11466662, 0.29340392, 0.1724779, 0.03414285, 0.3034265, 0.43148994, -0.2640946, 0.31874347, 0.180067, 0.2469759], 
        [-0.14266594, -0.21605411, -0.17657652, -0.17247485, 0.30320397, -0.34606943, -0.37666157, -0.16420524, 0.27821654, 0.08791734], 
        [0.25877953, -0.038422942, 0.28270572, -0.36984324, 0.4075216, -0.14031425, 0.3804996, -0.09306294, -0.1984529, 0.13743925], 
        [0.25177974, -0.011953086, -0.35328645, -0.13236478, -0.2763487, -0.036616832, -0.0005656481, -0.023326576, -0.37024075, -0.017632514], 
        [-0.19049832, 0.42474234, 0.12500519, 0.30992618, 0.35693204, -0.21147692, 0.24706304, -0.29064584, 0.2901163, -0.0909473]]

    vdnn__hiddenlayer_2__kernel__part_0__read__0  :: ((Pull DIM2 (Data Float))) -- Arguments:  (Pull DIM2 (Data Float))
    vdnn__hiddenlayer_2__kernel__part_0__read__0  =  tfIdentity__LPull_DIM2_FloatR___LPull_DIM2_FloatR_ vdnn__hiddenlayer_2__kernel__part_0__0

    vdnn__hiddenlayer_2__kernel__0  :: ((Pull DIM2 (Data Float))) -- Arguments:  (Pull DIM2 (Data Float))
    vdnn__hiddenlayer_2__kernel__0  =  tfIdentity__LPull_DIM2_FloatR___LPull_DIM2_FloatR_ vdnn__hiddenlayer_2__kernel__part_0__read__0

    vdnn__hiddenlayer_2__MatMul__0  :: ((Pull DIM2 (Data Float))) -- Arguments:  (Pull DIM2 (Data Float))  (Pull DIM2 (Data Float))
    vdnn__hiddenlayer_2__MatMul__0  =  tfMatMul__LPull_DIM2_FloatR___LPull_DIM2_FloatR___LPull_DIM2_FloatR_ vdnn__hiddenlayer_1__Relu__0 vdnn__hiddenlayer_2__kernel__0

    vdnn__hiddenlayer_2__bias__part_0__0  :: ((Pull DIM1 (Data Float))) -- Arguments:
    vdnn__hiddenlayer_2__bias__part_0__0  =  tfVariableV2__LPull_DIM1_FloatR_ [0.25748366, -0.05817512, 0.0, -0.04207438, -0.034728616, -0.08236575, -0.049633943, 0.40396103, 0.0, -0.049619246]

    vdnn__hiddenlayer_2__bias__part_0__read__0  :: ((Pull DIM1 (Data Float))) -- Arguments:  (Pull DIM1 (Data Float))
    vdnn__hiddenlayer_2__bias__part_0__read__0  =  tfIdentity__LPull_DIM1_FloatR___LPull_DIM1_FloatR_ vdnn__hiddenlayer_2__bias__part_0__0

    vdnn__hiddenlayer_2__bias__0  :: ((Pull DIM1 (Data Float))) -- Arguments:  (Pull DIM1 (Data Float))
    vdnn__hiddenlayer_2__bias__0  =  tfIdentity__LPull_DIM1_FloatR___LPull_DIM1_FloatR_ vdnn__hiddenlayer_2__bias__part_0__read__0

    vdnn__hiddenlayer_2__BiasAdd__0  :: ((Pull DIM2 (Data Float))) -- Arguments:  (Pull DIM2 (Data Float))  (Pull DIM1 (Data Float))
    vdnn__hiddenlayer_2__BiasAdd__0  =  tfBiasAdd__LPull_DIM2_FloatR___LPull_DIM2_FloatR___LPull_DIM1_FloatR_ vdnn__hiddenlayer_2__MatMul__0 vdnn__hiddenlayer_2__bias__0

    vdnn__hiddenlayer_2__Relu__0  :: ((Pull DIM2 (Data Float))) -- Arguments:  (Pull DIM2 (Data Float))
    vdnn__hiddenlayer_2__Relu__0  =  tfRelu__LPull_DIM2_FloatR___LPull_DIM2_FloatR_ vdnn__hiddenlayer_2__BiasAdd__0

    vdnn__logits__kernel__part_0__0  :: ((Pull DIM2 (Data Float))) -- Arguments:
    vdnn__logits__kernel__part_0__0  =  tfVariableV2__LPull_DIM2_FloatR_ [
        [0.9678828, -0.07429327, -1.4612858], 
        [-0.46043652, -0.087886825, -0.26906747], 
        [-0.08205062, 0.13083398, -0.107215166], 
        [-1.136796, -0.10522478, 0.24648522], 
        [0.36117786, 0.20214033, -0.1043409], 
        [-0.5552198, -0.59523, 0.29417604], 
        [0.06126838, 0.32809013, -0.2514663], 
        [0.12271329, 0.13569178, -1.3657666], 
        [0.059321046, -0.5515968, 0.6249279], 
        [0.47829178, 0.44494593, -0.50133526]]

    vdnn__logits__kernel__part_0__read__0  :: ((Pull DIM2 (Data Float))) -- Arguments:  (Pull DIM2 (Data Float))
    vdnn__logits__kernel__part_0__read__0  =  tfIdentity__LPull_DIM2_FloatR___LPull_DIM2_FloatR_ vdnn__logits__kernel__part_0__0

    vdnn__logits__kernel__0  :: ((Pull DIM2 (Data Float))) -- Arguments:  (Pull DIM2 (Data Float))
    vdnn__logits__kernel__0  =  tfIdentity__LPull_DIM2_FloatR___LPull_DIM2_FloatR_ vdnn__logits__kernel__part_0__read__0

    vdnn__logits__MatMul__0  :: ((Pull DIM2 (Data Float))) -- Arguments:  (Pull DIM2 (Data Float))  (Pull DIM2 (Data Float))
    vdnn__logits__MatMul__0  =  tfMatMul__LPull_DIM2_FloatR___LPull_DIM2_FloatR___LPull_DIM2_FloatR_ vdnn__hiddenlayer_2__Relu__0 vdnn__logits__kernel__0

    vdnn__logits__bias__part_0__0  :: ((Pull DIM1 (Data Float))) -- Arguments:
    vdnn__logits__bias__part_0__0  =  tfVariableV2__LPull_DIM1_FloatR_ [-0.044772338, 0.12347265, -0.09037269]

    vdnn__logits__bias__part_0__read__0  :: ((Pull DIM1 (Data Float))) -- Arguments:  (Pull DIM1 (Data Float))
    vdnn__logits__bias__part_0__read__0  =  tfIdentity__LPull_DIM1_FloatR___LPull_DIM1_FloatR_ vdnn__logits__bias__part_0__0

    vdnn__logits__bias__0  :: ((Pull DIM1 (Data Float))) -- Arguments:  (Pull DIM1 (Data Float))
    vdnn__logits__bias__0  =  tfIdentity__LPull_DIM1_FloatR___LPull_DIM1_FloatR_ vdnn__logits__bias__part_0__read__0

    vdnn__logits__BiasAdd__0  :: ((Pull DIM2 (Data Float))) -- Arguments:  (Pull DIM2 (Data Float))  (Pull DIM1 (Data Float))
    vdnn__logits__BiasAdd__0  =  memoize $ tfBiasAdd__LPull_DIM2_FloatR___LPull_DIM2_FloatR___LPull_DIM1_FloatR_ vdnn__logits__MatMul__0 vdnn__logits__bias__0

    vdnn__head__predictions__class_ids__dimension__0  :: ((Data Int32)) -- Arguments:
    vdnn__head__predictions__class_ids__dimension__0  =  tfConst_Int32 ( -1 )

    vdnn__head__predictions__class_ids__0  :: ((Pull DIM1 (Data Int32))) -- Arguments:  (Pull DIM2 (Data Float))  (Data Int32)
    vdnn__head__predictions__class_ids__0  =  tfArgMax__LPull_DIM1_Int32R___LPull_DIM2_FloatR__Int32 vdnn__logits__BiasAdd__0 vdnn__head__predictions__class_ids__dimension__0

    vdnn__head__predictions__ExpandDims__dim__0  :: ((Data Int32)) -- Arguments:
    vdnn__head__predictions__ExpandDims__dim__0  =  tfConst_Int32 ( -1 )

    vdnn__head__predictions__ExpandDims__0  :: ((Pull DIM2 (Data Int32))) -- Arguments:  (Pull DIM1 (Data Int32))  (Data Int32)
    vdnn__head__predictions__ExpandDims__0  =  tfExpandDims__LPull_DIM2_Int32R___LPull_DIM1_Int32R__Int32 vdnn__head__predictions__class_ids__0 vdnn__head__predictions__ExpandDims__dim__0

    vdnn__metrics__Cast__0  :: ((Pull DIM2 (Data Int32))) -- Arguments:  (Pull DIM2 (Data Int32))
    vdnn__metrics__Cast__0  =  tfCast__LPull_DIM2_Int32R___LPull_DIM2_Int32R_ vdnn__head__predictions__ExpandDims__0

    vdnn__head__labels__assert_rank_at_least__static_checks_determined_all_ok__0  :: Data () -- Arguments:
    vdnn__head__labels__assert_rank_at_least__static_checks_determined_all_ok__0  =  tfNoOp_

    vdnn__head__labels__ExpandDims__dim__0  :: ((Data Int32)) -- Arguments:
    vdnn__head__labels__ExpandDims__dim__0  =  tfConst_Int32 ( -1 )

    vdnn__head__labels__ExpandDims__0  :: ((Pull DIM2 (Data Int32))) -- Arguments:  (Pull DIM1 (Data Int32))  (Data Int32)
    vdnn__head__labels__ExpandDims__0  =  tfExpandDims__LPull_DIM2_Int32R___LPull_DIM1_Int32R__Int32 vIteratorGetNext__4 vdnn__head__labels__ExpandDims__dim__0

    vdnn__head__labels__0  :: ((Pull DIM2 (Data Int32))) -- Arguments:  (Pull DIM2 (Data Int32))
    vdnn__head__labels__0  =  tfIdentity__LPull_DIM2_Int32R___LPull_DIM2_Int32R_ vdnn__head__labels__ExpandDims__0 -- vdnn__head__labels__assert_rank_at_least__static_checks_determined_all_ok__0 vdnn__head__labels__assert_equal__Assert__Assert

    vdnn__head__assert_range__Identity__0  :: ((Pull DIM2 (Data Int32))) -- Arguments:  (Pull DIM2 (Data Int32))
    vdnn__head__assert_range__Identity__0  =  tfIdentity__LPull_DIM2_Int32R___LPull_DIM2_Int32R_ vdnn__head__labels__0 -- vdnn__head__assert_range__assert_less_equal__Assert__Assert vdnn__head__assert_range__assert_non_negative__assert_less_equal__Assert__Assert

    vdnn__metrics__Equal__0  :: ((Pull DIM2 (Data Bool))) -- Arguments:  (Pull DIM2 (Data Int32))  (Pull DIM2 (Data Int32))
    vdnn__metrics__Equal__0  =  tfEqual__LPull_DIM2_BoolR___LPull_DIM2_Int32R___LPull_DIM2_Int32R_ vdnn__metrics__Cast__0 vdnn__head__assert_range__Identity__0

    vdnn__metrics__ToFloat__0  :: ((Pull DIM2 (Data Float))) -- Arguments:  (Pull DIM2 (Data Bool))
    vdnn__metrics__ToFloat__0  =  tfCast__LPull_DIM2_FloatR___LPull_DIM2_BoolR_ vdnn__metrics__Equal__0

    vdnn__metrics__Const__0  :: ((Data Float)) -- Arguments:
    vdnn__metrics__Const__0  =  tfConst_Float ( 1.0 )

    vdnn__metrics__accuracy__broadcast_weights__assert_broadcastable__static_scalar_check_success__0  :: Data () -- Arguments:
    vdnn__metrics__accuracy__broadcast_weights__assert_broadcastable__static_scalar_check_success__0  =  tfNoOp_

    vdnn__metrics__accuracy__broadcast_weights__ones_like__Shape__0  :: ((Pull DIM1 (Data Int32))) -- Arguments:  (Pull DIM2 (Data Float))
    vdnn__metrics__accuracy__broadcast_weights__ones_like__Shape__0  =  tfShape__LPull_DIM1_Int32R___LPull_DIM2_FloatR_ vdnn__metrics__ToFloat__0 -- vdnn__metrics__accuracy__broadcast_weights__assert_broadcastable__static_scalar_check_success__0

    vdnn__metrics__accuracy__broadcast_weights__ones_like__Const__0  :: ((Data Float)) -- Arguments:
    vdnn__metrics__accuracy__broadcast_weights__ones_like__Const__0  =  tfConst_Float ( 1.0 ) -- vdnn__metrics__accuracy__broadcast_weights__assert_broadcastable__static_scalar_check_success__0

    vdnn__metrics__accuracy__broadcast_weights__ones_like__0  :: ((Pull DIM2 (Data Float))) -- Arguments:  (Pull DIM1 (Data Int32))  (Data Float)
    vdnn__metrics__accuracy__broadcast_weights__ones_like__0  =  tfFill__LPull_DIM2_FloatR___LPull_DIM1_Int32R__Float vdnn__metrics__accuracy__broadcast_weights__ones_like__Shape__0 vdnn__metrics__accuracy__broadcast_weights__ones_like__Const__0

    vdnn__metrics__accuracy__broadcast_weights__0  :: ((Pull DIM2 (Data Float))) -- Arguments:  (Data Float)  (Pull DIM2 (Data Float))
    vdnn__metrics__accuracy__broadcast_weights__0  =  tfMul__LPull_DIM2_FloatR__Float__LPull_DIM2_FloatR_ vdnn__metrics__Const__0 vdnn__metrics__accuracy__broadcast_weights__ones_like__0

    vdnn__metrics__accuracy__Mul__0  :: ((Pull DIM2 (Data Float))) -- Arguments:  (Pull DIM2 (Data Float))  (Pull DIM2 (Data Float))
    vdnn__metrics__accuracy__Mul__0  =  tfMul__LPull_DIM2_FloatR___LPull_DIM2_FloatR___LPull_DIM2_FloatR_ vdnn__metrics__ToFloat__0 vdnn__metrics__accuracy__broadcast_weights__0

    vdnn__metrics__accuracy__count__0  :: ((Data Float)) -- Arguments:
    vdnn__metrics__accuracy__count__0  =  tfVariableV2_Float ( 0.0 )

    vdnn__metrics__accuracy__Const__0  :: ((Pull DIM1 (Data Int32))) -- Arguments:
    vdnn__metrics__accuracy__Const__0  =  tfConst__LPull_DIM1_Int32R_ [0, 1]

    vdnn__metrics__accuracy__Sum__0  :: ((Data Float)) -- Arguments:  (Pull DIM2 (Data Float))  (Pull DIM1 (Data Int32))
    vdnn__metrics__accuracy__Sum__0  =  tfSum_Float__LPull_DIM2_FloatR___LPull_DIM1_Int32R_ vdnn__metrics__accuracy__broadcast_weights__0 vdnn__metrics__accuracy__Const__0

    vdnn__metrics__accuracy__AssignAdd_1__0  :: ((Data Float)) -- Arguments:  (Data Float)  (Data Float)
    vdnn__metrics__accuracy__AssignAdd_1__0  =  tfAssignAdd_Float_Float_Float vdnn__metrics__accuracy__count__0 vdnn__metrics__accuracy__Sum__0 -- vdnn__metrics__accuracy__Mul__0

    vdnn__metrics__accuracy__zeros_like_1__0  :: ((Data Float)) -- Arguments:
    vdnn__metrics__accuracy__zeros_like_1__0  =  tfConst_Float ( 0.0 )

    vdnn__metrics__accuracy__Greater_1__0  :: ((Data Bool)) -- Arguments:  (Data Float)  (Data Float)
    vdnn__metrics__accuracy__Greater_1__0  =  tfGreater_Bool_Float_Float vdnn__metrics__accuracy__AssignAdd_1__0 vdnn__metrics__accuracy__zeros_like_1__0

    vdnn__metrics__accuracy__total__0  :: ((Data Float)) -- Arguments:
    vdnn__metrics__accuracy__total__0  =  tfVariableV2_Float ( 0.0 )

    vdnn__metrics__accuracy__Const_1__0  :: ((Pull DIM1 (Data Int32))) -- Arguments:
    vdnn__metrics__accuracy__Const_1__0  =  tfConst__LPull_DIM1_Int32R_ [0, 1]

    vdnn__metrics__accuracy__Sum_1__0  :: ((Data Float)) -- Arguments:  (Pull DIM2 (Data Float))  (Pull DIM1 (Data Int32))
    vdnn__metrics__accuracy__Sum_1__0  =  tfSum_Float__LPull_DIM2_FloatR___LPull_DIM1_Int32R_ vdnn__metrics__accuracy__Mul__0 vdnn__metrics__accuracy__Const_1__0

    vdnn__metrics__accuracy__AssignAdd__0  :: ((Data Float)) -- Arguments:  (Data Float)  (Data Float)
    vdnn__metrics__accuracy__AssignAdd__0  =  tfAssignAdd_Float_Float_Float vdnn__metrics__accuracy__total__0 vdnn__metrics__accuracy__Sum_1__0

    vdnn__metrics__accuracy__truediv_1__0  :: ((Data Float)) -- Arguments:  (Data Float)  (Data Float)
    vdnn__metrics__accuracy__truediv_1__0  =  tfRealDiv_Float_Float_Float vdnn__metrics__accuracy__AssignAdd__0 vdnn__metrics__accuracy__AssignAdd_1__0

    vdnn__metrics__accuracy__update_op__0  :: ((Data Float)) -- Arguments:  (Data Bool)  (Data Float)  (Data Float)
    vdnn__metrics__accuracy__update_op__0  =  tfSelect_Float_Bool_Float_Float vdnn__metrics__accuracy__Greater_1__0 vdnn__metrics__accuracy__truediv_1__0 vdnn__metrics__accuracy__zeros_like_1__0

    vdnn__head__sparse_softmax_cross_entropy_loss__assert_broadcastable__static_scalar_check_success__0  :: Data () -- Arguments:
    vdnn__head__sparse_softmax_cross_entropy_loss__assert_broadcastable__static_scalar_check_success__0  =  tfNoOp_

    vdnn__head__sparse_softmax_cross_entropy_loss__remove_squeezable_dimensions__Squeeze__0  :: ((Pull DIM1 (Data Int32))) -- Arguments:  (Pull DIM2 (Data Int32))
    vdnn__head__sparse_softmax_cross_entropy_loss__remove_squeezable_dimensions__Squeeze__0  =  tfSqueeze__LPull_DIM1_Int32R___LPull_DIM2_Int32R_ vdnn__head__assert_range__Identity__0

    vdnn__head__sparse_softmax_cross_entropy_loss__xentropy__xentropy__0  :: ((Pull DIM1 (Data Float))) -- Arguments:  (Pull DIM2 (Data Float))  (Pull DIM1 (Data Int32))
    vdnn__head__sparse_softmax_cross_entropy_loss__xentropy__xentropy__0  =  tfSparseSoftmaxCrossEntropyWithLogits__LPull_DIM1_FloatR___LPull_DIM2_FloatR___LPull_DIM1_Int32R_ vdnn__logits__BiasAdd__0 vdnn__head__sparse_softmax_cross_entropy_loss__remove_squeezable_dimensions__Squeeze__0

    vdnn__head__sparse_softmax_cross_entropy_loss__Const__0  :: ((Data Float)) -- Arguments:
    vdnn__head__sparse_softmax_cross_entropy_loss__Const__0  =  tfConst_Float ( 1.0 )

    vdnn__head__sparse_softmax_cross_entropy_loss__Mul__0  :: ((Pull DIM1 (Data Float))) -- Arguments:  (Pull DIM1 (Data Float))  (Data Float)
    vdnn__head__sparse_softmax_cross_entropy_loss__Mul__0  =  tfMul__LPull_DIM1_FloatR___LPull_DIM1_FloatR__Float vdnn__head__sparse_softmax_cross_entropy_loss__xentropy__xentropy__0 vdnn__head__sparse_softmax_cross_entropy_loss__Const__0 -- vdnn__head__sparse_softmax_cross_entropy_loss__assert_broadcastable__static_scalar_check_success__0

    vdnn__head__ExpandDims__dim__0  :: ((Data Int32)) -- Arguments:
    vdnn__head__ExpandDims__dim__0  =  tfConst_Int32 ( -1 )

    vdnn__head__ExpandDims__0  :: ((Pull DIM2 (Data Float))) -- Arguments:  (Pull DIM1 (Data Float))  (Data Int32)
    vdnn__head__ExpandDims__0  =  tfExpandDims__LPull_DIM2_FloatR___LPull_DIM1_FloatR__Int32 vdnn__head__sparse_softmax_cross_entropy_loss__Mul__0 vdnn__head__ExpandDims__dim__0

    vdnn__metrics__average_loss__Const__0  :: ((Data Float)) -- Arguments:
    vdnn__metrics__average_loss__Const__0  =  tfConst_Float ( 1.0 )

    vdnn__metrics__average_loss__broadcast_weights__assert_broadcastable__static_scalar_check_success__0  :: Data () -- Arguments:
    vdnn__metrics__average_loss__broadcast_weights__assert_broadcastable__static_scalar_check_success__0  =  tfNoOp_

    vdnn__metrics__average_loss__broadcast_weights__ones_like__Shape__0  :: ((Pull DIM1 (Data Int32))) -- Arguments:  (Pull DIM2 (Data Float))
    vdnn__metrics__average_loss__broadcast_weights__ones_like__Shape__0  =  tfShape__LPull_DIM1_Int32R___LPull_DIM2_FloatR_ vdnn__head__ExpandDims__0 -- vdnn__metrics__average_loss__broadcast_weights__assert_broadcastable__static_scalar_check_success__0

    vdnn__metrics__average_loss__broadcast_weights__ones_like__Const__0  :: ((Data Float)) -- Arguments:
    vdnn__metrics__average_loss__broadcast_weights__ones_like__Const__0  =  tfConst_Float ( 1.0 ) -- vdnn__metrics__average_loss__broadcast_weights__assert_broadcastable__static_scalar_check_success__0

    vdnn__metrics__average_loss__broadcast_weights__ones_like__0  :: ((Pull DIM2 (Data Float))) -- Arguments:  (Pull DIM1 (Data Int32))  (Data Float)
    vdnn__metrics__average_loss__broadcast_weights__ones_like__0  =  tfFill__LPull_DIM2_FloatR___LPull_DIM1_Int32R__Float vdnn__metrics__average_loss__broadcast_weights__ones_like__Shape__0 vdnn__metrics__average_loss__broadcast_weights__ones_like__Const__0

    vdnn__metrics__average_loss__broadcast_weights__0  :: ((Pull DIM2 (Data Float))) -- Arguments:  (Data Float)  (Pull DIM2 (Data Float))
    vdnn__metrics__average_loss__broadcast_weights__0  =  tfMul__LPull_DIM2_FloatR__Float__LPull_DIM2_FloatR_ vdnn__metrics__average_loss__Const__0 vdnn__metrics__average_loss__broadcast_weights__ones_like__0

    vdnn__metrics__average_loss__Mul__0  :: ((Pull DIM2 (Data Float))) -- Arguments:  (Pull DIM2 (Data Float))  (Pull DIM2 (Data Float))
    vdnn__metrics__average_loss__Mul__0  =  tfMul__LPull_DIM2_FloatR___LPull_DIM2_FloatR___LPull_DIM2_FloatR_ vdnn__head__ExpandDims__0 vdnn__metrics__average_loss__broadcast_weights__0

    vdnn__metrics__average_loss__count__0  :: ((Data Float)) -- Arguments:
    vdnn__metrics__average_loss__count__0  =  tfVariableV2_Float ( 0.0 )

    vdnn__metrics__average_loss__Const_1__0  :: ((Pull DIM1 (Data Int32))) -- Arguments:
    vdnn__metrics__average_loss__Const_1__0  =  tfConst__LPull_DIM1_Int32R_ [0, 1]

    vdnn__metrics__average_loss__Sum__0  :: ((Data Float)) -- Arguments:  (Pull DIM2 (Data Float))  (Pull DIM1 (Data Int32))
    vdnn__metrics__average_loss__Sum__0  =  tfSum_Float__LPull_DIM2_FloatR___LPull_DIM1_Int32R_ vdnn__metrics__average_loss__broadcast_weights__0 vdnn__metrics__average_loss__Const_1__0

    vdnn__metrics__average_loss__AssignAdd_1__0  :: ((Data Float)) -- Arguments:  (Data Float)  (Data Float)
    vdnn__metrics__average_loss__AssignAdd_1__0  =  tfAssignAdd_Float_Float_Float vdnn__metrics__average_loss__count__0 vdnn__metrics__average_loss__Sum__0 -- vdnn__metrics__average_loss__Mul__0

    vdnn__metrics__average_loss__zeros_like_1__0  :: ((Data Float)) -- Arguments:
    vdnn__metrics__average_loss__zeros_like_1__0  =  tfConst_Float ( 0.0 )

    vdnn__metrics__average_loss__Greater_1__0  :: ((Data Bool)) -- Arguments:  (Data Float)  (Data Float)
    vdnn__metrics__average_loss__Greater_1__0  =  tfGreater_Bool_Float_Float vdnn__metrics__average_loss__AssignAdd_1__0 vdnn__metrics__average_loss__zeros_like_1__0

    vdnn__metrics__average_loss__total__0  :: ((Data Float)) -- Arguments:
    vdnn__metrics__average_loss__total__0  =  tfVariableV2_Float ( 0.0 )

    vdnn__metrics__average_loss__Const_2__0  :: ((Pull DIM1 (Data Int32))) -- Arguments:
    vdnn__metrics__average_loss__Const_2__0  =  tfConst__LPull_DIM1_Int32R_ [0, 1]

    vdnn__metrics__average_loss__Sum_1__0  :: ((Data Float)) -- Arguments:  (Pull DIM2 (Data Float))  (Pull DIM1 (Data Int32))
    vdnn__metrics__average_loss__Sum_1__0  =  tfSum_Float__LPull_DIM2_FloatR___LPull_DIM1_Int32R_ vdnn__metrics__average_loss__Mul__0 vdnn__metrics__average_loss__Const_2__0

    vdnn__metrics__average_loss__AssignAdd__0  :: ((Data Float)) -- Arguments:  (Data Float)  (Data Float)
    vdnn__metrics__average_loss__AssignAdd__0  =  tfAssignAdd_Float_Float_Float vdnn__metrics__average_loss__total__0 vdnn__metrics__average_loss__Sum_1__0

    vdnn__metrics__average_loss__truediv_1__0  :: ((Data Float)) -- Arguments:  (Data Float)  (Data Float)
    vdnn__metrics__average_loss__truediv_1__0  =  tfRealDiv_Float_Float_Float vdnn__metrics__average_loss__AssignAdd__0 vdnn__metrics__average_loss__AssignAdd_1__0

    vdnn__metrics__average_loss__update_op__0  :: ((Data Float)) -- Arguments:  (Data Bool)  (Data Float)  (Data Float)
    vdnn__metrics__average_loss__update_op__0  =  tfSelect_Float_Bool_Float_Float vdnn__metrics__average_loss__Greater_1__0 vdnn__metrics__average_loss__truediv_1__0 vdnn__metrics__average_loss__zeros_like_1__0

    vdnn__head__weighted_loss__assert_broadcastable__static_scalar_check_success__0  :: Data () -- Arguments:
    vdnn__head__weighted_loss__assert_broadcastable__static_scalar_check_success__0  =  tfNoOp_

    vdnn__head__weighted_loss__ToFloat__x__0  :: ((Data Float)) -- Arguments:
    vdnn__head__weighted_loss__ToFloat__x__0  =  tfConst_Float ( 1.0 ) -- vdnn__head__weighted_loss__assert_broadcastable__static_scalar_check_success__0

    vdnn__head__weighted_loss__Mul__0  :: ((Pull DIM2 (Data Float))) -- Arguments:  (Pull DIM2 (Data Float))  (Data Float)
    vdnn__head__weighted_loss__Mul__0  =  tfMul__LPull_DIM2_FloatR___LPull_DIM2_FloatR__Float vdnn__head__ExpandDims__0 vdnn__head__weighted_loss__ToFloat__x__0

    vdnn__head__weighted_loss__Const__0  :: ((Pull DIM1 (Data Int32))) -- Arguments:
    vdnn__head__weighted_loss__Const__0  =  tfConst__LPull_DIM1_Int32R_ [0, 1] -- vdnn__head__weighted_loss__assert_broadcastable__static_scalar_check_success__0

    vdnn__head__weighted_loss__Sum__0  :: ((Data Float)) -- Arguments:  (Pull DIM2 (Data Float))  (Pull DIM1 (Data Int32))
    vdnn__head__weighted_loss__Sum__0  =  tfSum_Float__LPull_DIM2_FloatR___LPull_DIM1_Int32R_ vdnn__head__weighted_loss__Mul__0 vdnn__head__weighted_loss__Const__0

    vmean__count__0  :: ((Data Float)) -- Arguments:
    vmean__count__0  =  tfVariableV2_Float ( 0.0 )

    vmean__Size__0  :: ((Data Int32)) -- Arguments:
    vmean__Size__0  =  tfConst_Int32 ( 1 )

    vmean__ToFloat__0  :: ((Data Float)) -- Arguments:  (Data Int32)
    vmean__ToFloat__0  =  tfCast_Float_Int32 vmean__Size__0

    vmean__AssignAdd_1__0  :: ((Data Float)) -- Arguments:  (Data Float)  (Data Float)
    vmean__AssignAdd_1__0  =  tfAssignAdd_Float_Float_Float vmean__count__0 vmean__ToFloat__0 -- vdnn__head__weighted_loss__Sum__0

    vmean__zeros_like_1__0  :: ((Data Float)) -- Arguments:
    vmean__zeros_like_1__0  =  tfConst_Float ( 0.0 )

    vmean__Greater_1__0  :: ((Data Bool)) -- Arguments:  (Data Float)  (Data Float)
    vmean__Greater_1__0  =  tfGreater_Bool_Float_Float vmean__AssignAdd_1__0 vmean__zeros_like_1__0

    vmean__total__0  :: ((Data Float)) -- Arguments:
    vmean__total__0  =  tfVariableV2_Float ( 0.0 )

    vmean__Const__0  :: ((Pull DIM1 (Data Int32))) -- Arguments:
    vmean__Const__0  =  tfConst__LPull_DIM1_Int32R_ []

    vmean__Sum__0  :: ((Data Float)) -- Arguments:  (Data Float)  (Pull DIM1 (Data Int32))
    vmean__Sum__0  =  tfSum_Float_Float__LPull_DIM1_Int32R_ vdnn__head__weighted_loss__Sum__0 vmean__Const__0

    vmean__AssignAdd__0  :: ((Data Float)) -- Arguments:  (Data Float)  (Data Float)
    vmean__AssignAdd__0  =  tfAssignAdd_Float_Float_Float vmean__total__0 vmean__Sum__0

    vmean__truediv_1__0  :: ((Data Float)) -- Arguments:  (Data Float)  (Data Float)
    vmean__truediv_1__0  =  tfRealDiv_Float_Float_Float vmean__AssignAdd__0 vmean__AssignAdd_1__0

    vmean__update_op__0  :: ((Data Float)) -- Arguments:  (Data Bool)  (Data Float)  (Data Float)
    vmean__update_op__0  =  tfSelect_Float_Bool_Float_Float vmean__Greater_1__0 vmean__truediv_1__0 vmean__zeros_like_1__0

    vgroup_deps__0  :: Data () -- Arguments:
    vgroup_deps__0  =  tfNoOp_ -- vdnn__metrics__accuracy__update_op__0 vdnn__metrics__average_loss__update_op__0 vmean__update_op__0

  in (vdnn__metrics__accuracy__update_op__0, vdnn__metrics__average_loss__update_op__0, vmean__update_op__0, 400 {-,  (vdnn__input_from_feature_columns__input_layer__petal_length__Reshape__0, vdnn__input_from_feature_columns__input_layer__concat__0, vdnn__hiddenlayer_0__MatMul__0) -} )

-- Map a function taking a vector argument over the rows of a matrix
mapOverRows :: (Pull DIM1 a -> b) -> Pull DIM2 a -> Pull DIM1 b
mapOverRows rowFun (Pull ixf (Z :. nRows :. rowLen)) = Pull ixf2 (Z :. nRows)
  where ixf2 (Z :. i) = rowFun (Pull ixf3 (Z :. rowLen))
             where ixf3 (Z :. j) = ixf (Z :. i :. j)

-- Map a function taking a vector argument to a vector result over the rows of a matrix and flatten to a 2D result
mapOverRowsF :: (Pull DIM1 a -> Pull DIM1 b) -> Pull DIM2 a -> Pull DIM2 b
mapOverRowsF rowFun (Pull ixf (Z :. nRows :. rowLen)) = Pull ixf2 (Z :. nRows :. rowLen)
  where ixf2 (Z :. i :. k) = rowFun (Pull ixf3 (Z :. rowLen)) ! (Z :. k)
             where ixf3 (Z :. j) = ixf (Z :. i :. j)

-- Map a function taking a vector argument over the columns of a matrix
mapOverCols :: (Pull DIM1 a -> b) -> Pull DIM2 a -> Pull DIM1 b
mapOverCols colFun (Pull ixf (Z :. nRows :. rowLen)) = Pull ixf2 (Z :. rowLen)
  where ixf2 (Z :. j) = colFun (Pull ixf3 (Z :. nRows))
             where ixf3 (Z :. i) = ixf (Z :. i :. j)

-- Concatenate two matrices horizontally
concatH :: Syntax a => Pull DIM2 a -> Pull DIM2 a -> Pull DIM2 a
concatH (Pull ixf1 (Z :. nRows1 :. rowLen1)) (Pull ixf2 (Z :. nRows2 :. rowLen2))
  = Pull (ixf ixf1 ixf2) (Z :. F.min nRows1 nRows2 :. (+) rowLen1 rowLen2)
  where ixf ixf1 ixf2 (Z :. i :. j)
          = (j F.< rowLen1 ? ixf1 (Z :. i :. j)) $ ixf2 (Z :. i :. (F.-) j rowLen1 :: Shape DIM2)

-- Concatenate two matrices vertically
concatV :: Syntax a => Pull DIM2 a -> Pull DIM2 a -> Pull DIM2 a
concatV (Pull ixf1 (Z :. nRows1 :. rowLen1)) (Pull ixf2 (Z :. nRows2 :. rowLen2))
  = Pull (ixf ixf1 ixf2) (Z :. (+) nRows1 nRows2 :. F.min rowLen1 rowLen2)
  where ixf ixf1 ixf2 (Z :. i :. j) = i F.< nRows1 ? ixf1 (Z :. i :. j) $ ixf2 (Z :. (F.-) i nRows1 :. j)

tfExpandDimsDIM2 :: Syntax a => Pull DIM1 a -> Data Int32 -> Pull DIM2 a
tfExpandDimsDIM2 (Pull ixf (Z :. n)) d = Pull ixfn (Z :. sf 1 n :. sf n 1)
  where ixfn (Z :. i :. j) = ixf (Z :. sf j i)
        sf :: Syntax a => a -> a -> a
        sf x y = d F.== (-2) ? x $
                 d F.== (-1) ? y $
                 d F.== 0    ? x $
                               y

dim2shape :: Pull DIM2 a -> Pull DIM1 (Data Int32)
dim2shape (Pull _ (Z :. m :. n)) = Pull (\ (Z :. i) -> F.i2n $ i F.== 0 ? m $ n) (Z :. 2)

-- Construct a matrix containing the value 'x'
tfFillDIM2 :: Pull DIM1 (Data Int32) -> a -> Pull DIM2 a
tfFillDIM2 sh x = Pull (const x) (Z :. (F.i2n $ sh ! (Z :. 0)) :. (F.i2n $ sh ! (Z :. 1)))

-- Make a Pull vector manifest
memoizePull :: Shapely sh => Pull sh (Data Float) -> Pull sh (Data Float)
memoizePull vec = arrToPull ext $ fromPull vec
  where ext = extent vec

-- A zipWith analogue that supports the Tensorflow (and numpy) broadcast (array promotion) rules
-- First some type hackery
data MaxShape sh1 sh2 where
type family BroadcastShape sh1 sh2
type instance BroadcastShape                    Z                    Z = Z
type instance BroadcastShape                    Z (sh  :. Data Length) = sh :. Data Length
type instance BroadcastShape (sh  :. Data Length)                    Z = sh :. Data Length
type instance BroadcastShape (sh1 :. Data Length) (sh2 :. Data Length) = BroadcastShape sh1 sh2 :. Data Length

-- Convert an index into the broadcast structure to an index into one of the components
bcAdj :: Shape sh1 -> Shape sh2 -> Shape (BroadcastShape sh1 sh2) -> Shape sh2
bcAdj (sh1 :. _) (sh2 :. n) (sh3 :. i) = bcAdj sh1 sh2 sh3 :. (n F.== 1 ? 0 $ i)
bcAdj          Z (sh2 :. n) (sh3 :. i) = sh3 :. i
bcAdj         sh          Z        sh3 = Z

-- Compute the shape of the result after broadcasting
bcShape :: Shape sh1 -> Shape sh2 -> Shape (BroadcastShape sh1 sh2)
bcShape (sh1 :. n1) (sh2 :. n2) = bcShape sh1 sh2 :. F.max n1 n2
bcShape Z           (sh  :. n)  = sh :. n
bcShape (sh  :. n)  Z           = sh :. n
bcShape Z           Z           = Z

-- And finally, the bcZipWith proper
bcZipWith f (Pull ixf1 sh1) (Pull ixf2 sh2) = Pull ixf sh
  where ixf ix = f (ixf1 $ bcAdj sh2 sh1 ix) (ixf2 $ bcAdj sh1 sh2 ix)
        sh = bcShape sh1 sh2

-- Special case for 1D vectors and no masks
tfStridedSliceDIM1 :: Pull DIM1 a -> Pull DIM1 (Data Int32) -> Pull DIM1 (Data Int32) -> Pull DIM1 (Data Int32)
                   -> Pull DIM1 a
tfStridedSliceDIM1 (Pull df (Z :. dn)) (Pull bf bs) (Pull ef es) (Pull sf ss) = Pull rf rs
  where rf (Z :. i) = df (Z :. (b + i*s))
        b = F.i2n $ bf (Z :. 0)
        e = F.i2n $ ef (Z :. 0)
        s = F.i2n $ sf (Z :. 0)
        rs = Z :. ((e-b) `F.div` s) -- This is probably not correct for strides other than 1

-- Special case for a 2D result, implements the special meaning of -1
tfReshapeDIM2 :: Pull sh a -> Pull DIM1 (Data Int32) -> Pull DIM2 a
tfReshapeDIM2 (Pull ixf sh) ds = Pull nixf nsh
  where s = size sh
        m = F.i2n $ ds ! (Z :. 0)
        n = F.i2n $ ds ! (Z :. 1)
        m1 = m F.== (-1) ? s `F.div` n $ m
        n1 = n F.== (-1) ? s `F.div` m $ n
        nsh = Z :. m1 :. n1
        nixf ix = ixf $ V.fromIndex sh $ V.toIndex nsh ix

-- Functions for computing cross entropy
softmaxRow :: Pull DIM1 (Data Float) -> Pull DIM1 (Data Float)
softmaxRow xs = V.map (F./ sumx) xexp
  where xmax = V.maximum xs
        xexp = V.map F.exp $ V.map (F.- xmax) xs
        sumx = V.fromZero $ V.sum xexp

softmaxDIM2 :: Pull DIM2 (Data Float) -> Pull DIM2 (Data Float)
softmaxDIM2 = {- V.flatten $ -} mapOverRowsF softmaxRow

crossEntropyDIM2 :: Pull DIM2 (Data Float) -> Pull DIM1 (Data Int32) -> Pull DIM1 (Data Float)
crossEntropyDIM2 xs ls = V.zipWith f (mapOverRows (\x -> x) normxs) ls
  where normxs = softmaxDIM2 xs
        f ys l = - F.log (ys ! (Z :. F.i2n l))

-- Squeeze the last dimension from an array
squeezeLast :: Pull (sh :. Data Length) a -> Pull sh a
squeezeLast (Pull ixf (sh :. n)) = Pull (\ ix -> ixf (ix :. 0)) sh

-- Find index of maximum value in vector
maxIdx :: Pull DIM1 (Data Float) -> Data Int32
maxIdx (Pull ixf (Z :. n)) = F.i2n $ forLoop (n-1) 0 $ \ i midx -> ixf (Z :. (i+1)) F.> ixf (Z :. midx) ? i+1 $ midx

-- 2D matrix transpose
transposeDIM2 :: Pull DIM2 a -> Pull DIM2 a
transposeDIM2 (Pull ixf sh) = Pull (ixf . swap) (swap sh)
  where swap :: Shape DIM2 -> Shape DIM2
        swap (Z :. m :. n) = Z :. n :. m

rowDIM2 :: Pull DIM2 a -> Data Index -> Pull DIM1 a
rowDIM2 xs i = indexed (Z :. cols) (\ (Z :. j) -> xs ! (Z :. i :. j))
  where (rows, cols) = case extent xs of Z :. r :. c -> (r,c)

colDIM2 :: Pull DIM2 a -> Data Index -> Pull DIM1 a
colDIM2 xs j = indexed (Z :. rows) (\ (Z :. i) -> xs ! (Z :. i :. j))
  where (rows, cols) = case extent xs of Z :. r :. c -> (r,c)

-- Matrix multiplication
mm :: Pull DIM2 (Data Float) -> Pull DIM2 (Data Float) -> Pull DIM2 (Data Float)
mm xs ys = indexed2 rowsX colsY ixf
  where (rowsX, colsX) = case extent xs of Z :. r :. c -> (r,c)
        (rowsY, colsY) = case extent ys of Z :. r :. c -> (r,c)
        ixf i j = V.fromZero $ V.sum $ V.zipWith (*) (rowDIM2 xs i) (colDIM2 ys j)

-- Matrix multiplication based on the 'column major' function in MultiDim
tfMatMul :: Pull DIM2 (Data Float) -> Pull DIM2 (Data Float) -> Pull DIM2 (Data Float)
tfMatMul = mm

-- We make our own zipWith
-- tfZipWithDIM1 f (Pull ixf1 (Z:. n1)) (Pull ixf2 (Z :. n2)) = Pull (\ ix -> ixf1 ix `f` ixf2 ix) (Z :. F.min n1 n2)

-- Now for something completely different

tfArgMax__LPull_DIM1_Int32R___LPull_DIM2_FloatR__Int32 :: Pull DIM2 (Data Float) -> Data Int32 -> Pull DIM1 (Data Int32)
tfArgMax__LPull_DIM1_Int32R___LPull_DIM2_FloatR__Int32 xs sel = sel == 0 ? mapOverCols maxIdx xs $ mapOverRows maxIdx xs

tfAssignAdd_Float_Float_Float :: Data Float -> Data Float -> Data Float
tfAssignAdd_Float_Float_Float = (+)

tfBiasAdd__LPull_DIM2_FloatR___LPull_DIM2_FloatR___LPull_DIM1_FloatR_ :: Pull DIM2 (Data Float) -> Pull DIM1 (Data Float) -> Pull DIM2 (Data Float)
tfBiasAdd__LPull_DIM2_FloatR___LPull_DIM2_FloatR___LPull_DIM1_FloatR_ xss bs = mapOverRowsF (zipWith (+) bs) xss

-- Type conversion functions
tfCast_Float_Int32 :: Data Int32 -> Data Float
tfCast_Float_Int32 = F.i2n -- We need more conversions in the simulator

tfCast__LPull_DIM2_FloatR___LPull_DIM2_BoolR_ :: Pull sh (Data Bool) -> Pull sh (Data Float)
tfCast__LPull_DIM2_FloatR___LPull_DIM2_BoolR_ = V.map (\ b -> b ? 1.0 $ 0.0)

tfCast__LPull_DIM2_Int32R___LPull_DIM2_Int32R_ xs = xs -- Works if we avoid using Int32 in practice, defining it as Int

tfConcatV2__LPull_DIM2_FloatR___LPull_DIM2_FloatR___LPull_DIM2_FloatR___LPull_DIM2_FloatR___LPull_DIM2_FloatR__Int32
   :: Pull DIM2 (Data Float) -> Pull DIM2 (Data Float) -> Pull DIM2 (Data Float) -> Pull DIM2 (Data Float) -> Data Int32 -> Pull DIM2 (Data Float)
tfConcatV2__LPull_DIM2_FloatR___LPull_DIM2_FloatR___LPull_DIM2_FloatR___LPull_DIM2_FloatR___LPull_DIM2_FloatR__Int32 xs ys zs ws d
   = d == 0 ? xs `concatV` ys `concatV` zs `concatV` ws $ xs `concatH` ys `concatH` zs `concatH` ws


tfConst_Float :: Float -> Data Float
tfConst_Float = value
tfConst_Int32 :: Int32 -> Data Int32
tfConst_Int32 = value

tfConst__LPull_DIM1_Int32R_ :: [Int32] -> Pull DIM1 (Data Int32)
tfConst__LPull_DIM1_Int32R_ ys = Pull (\ (Z :. ix) -> xs!ix) (Z :. getLength xs)
  where xs = value ys

tfEqual__LPull_DIM2_BoolR___LPull_DIM2_Int32R___LPull_DIM2_Int32R_ :: Pull DIM2 (Data Int32) -> Pull DIM2 (Data Int32) -> Pull DIM2 (Data Bool)
tfEqual__LPull_DIM2_BoolR___LPull_DIM2_Int32R___LPull_DIM2_Int32R_ = bcZipWith (==)

tfExpandDims__LPull_DIM2_FloatR___LPull_DIM1_FloatR__Int32 :: Pull DIM1 (Data Float) -> Data Int32 -> Pull DIM2 (Data Float)
tfExpandDims__LPull_DIM2_FloatR___LPull_DIM1_FloatR__Int32 = tfExpandDimsDIM2
tfExpandDims__LPull_DIM2_Int32R___LPull_DIM1_Int32R__Int32 :: Pull DIM1 (Data Int32) -> Data Int32 -> Pull DIM2 (Data Int32)
tfExpandDims__LPull_DIM2_Int32R___LPull_DIM1_Int32R__Int32 = tfExpandDimsDIM2

tfFill__LPull_DIM2_FloatR___LPull_DIM1_Int32R__Float :: Pull DIM1 (Data Int32) -> Data Float -> Pull DIM2 (Data Float)
tfFill__LPull_DIM2_FloatR___LPull_DIM1_Int32R__Float = tfFillDIM2


tfGreater_Bool_Float_Float :: Data Float -> Data Float -> Data Bool
tfGreater_Bool_Float_Float x y = x F.> y

tfIdentity__LPull_DIM1_FloatR___LPull_DIM1_FloatR_ :: Pull DIM1 (Data Float) -> Pull DIM1 (Data Float)
tfIdentity__LPull_DIM1_FloatR___LPull_DIM1_FloatR_ xs = xs
tfIdentity__LPull_DIM2_FloatR___LPull_DIM2_FloatR_ :: Pull DIM2 (Data Float) -> Pull DIM2 (Data Float)
tfIdentity__LPull_DIM2_FloatR___LPull_DIM2_FloatR_ xs = xs
tfIdentity__LPull_DIM2_Int32R___LPull_DIM2_Int32R_ :: Pull DIM2 (Data Int32) -> Pull DIM2 (Data Int32)
tfIdentity__LPull_DIM2_Int32R___LPull_DIM2_Int32R_ xs = xs

tfMatMul__LPull_DIM2_FloatR___LPull_DIM2_FloatR___LPull_DIM2_FloatR_ = tfMatMul

tfMul__LPull_DIM1_FloatR___LPull_DIM1_FloatR__Float :: Pull DIM1 (Data Float) -> Data Float -> Pull DIM1 (Data Float)
tfMul__LPull_DIM1_FloatR___LPull_DIM1_FloatR__Float xs x = V.map (F.* x) xs
tfMul__LPull_DIM2_FloatR__Float__LPull_DIM2_FloatR_ :: Data Float -> Pull DIM2 (Data Float) -> Pull DIM2 (Data Float)
tfMul__LPull_DIM2_FloatR__Float__LPull_DIM2_FloatR_ x = V.map (F.* x)
tfMul__LPull_DIM2_FloatR___LPull_DIM2_FloatR__Float :: Pull DIM2 (Data Float) -> Data Float -> Pull DIM2 (Data Float)
tfMul__LPull_DIM2_FloatR___LPull_DIM2_FloatR__Float xs x = V.map (F.* x) xs
tfMul__LPull_DIM2_FloatR___LPull_DIM2_FloatR___LPull_DIM2_FloatR_ :: Pull DIM2 (Data Float) -> Pull DIM2 (Data Float) -> Pull DIM2 (Data Float)
tfMul__LPull_DIM2_FloatR___LPull_DIM2_FloatR___LPull_DIM2_FloatR_ = bcZipWith (F.*)

tfNoOp_ :: Data ()
tfNoOp_ = value ()

-- tfOneShotIterator_FIXME:_resource

tfPack__LPull_DIM1_Int32R__Int32_Int32 :: Data Int32 -> Data Int32 -> Pull DIM1 (Data Int32)
tfPack__LPull_DIM1_Int32R__Int32_Int32 x y = V.indexed1 2 (\ i -> i F.== 0 ? x $ y)

tfRealDiv_Float_Float_Float :: Data Float -> Data Float -> Data Float
tfRealDiv_Float_Float_Float x y = x/y

tfRelu__LPull_DIM2_FloatR___LPull_DIM2_FloatR_ :: Pull DIM2 (Data Float) -> Pull DIM2 (Data Float)
tfRelu__LPull_DIM2_FloatR___LPull_DIM2_FloatR_ = V.map (\ x -> F.max x 0.0)

tfReshape__LPull_DIM2_FloatR___LPull_DIM2_FloatR___LPull_DIM1_Int32R_ :: Pull DIM2 (Data Float) -> Pull DIM1 (Data Int32) -> Pull DIM2 (Data Float)
tfReshape__LPull_DIM2_FloatR___LPull_DIM2_FloatR___LPull_DIM1_Int32R_ = tfReshapeDIM2

tfSelect_Float_Bool_Float_Float :: Data Bool -> Data Float -> Data Float -> Data Float
tfSelect_Float_Bool_Float_Float c t f = c ? t $ f

tfShape__LPull_DIM1_Int32R___LPull_DIM2_FloatR_ :: Pull DIM2 (Data Float) -> Pull DIM1 (Data Int32)
tfShape__LPull_DIM1_Int32R___LPull_DIM2_FloatR_ = dim2shape

tfSparseSoftmaxCrossEntropyWithLogits__LPull_DIM1_FloatR___LPull_DIM2_FloatR___LPull_DIM1_Int32R_
       :: Pull DIM2 (Data Float) -> Pull DIM1 (Data Int32) -> Pull DIM1 (Data Float)
tfSparseSoftmaxCrossEntropyWithLogits__LPull_DIM1_FloatR___LPull_DIM2_FloatR___LPull_DIM1_Int32R_
       = crossEntropyDIM2

tfSqueeze__LPull_DIM1_Int32R___LPull_DIM2_Int32R_ :: Pull DIM2 (Data Int32) -> Pull DIM1 (Data Int32)
tfSqueeze__LPull_DIM1_Int32R___LPull_DIM2_Int32R_ = squeezeLast

tfStridedSlice_Int32__LPull_DIM1_Int32R___LPull_DIM1_Int32R___LPull_DIM1_Int32R___LPull_DIM1_Int32R_
       :: Pull DIM1 (Data Int32) -> Pull DIM1 (Data Int32) -> Pull DIM1 (Data Int32) -> Pull DIM1 (Data Int32) -> Data Int32
tfStridedSlice_Int32__LPull_DIM1_Int32R___LPull_DIM1_Int32R___LPull_DIM1_Int32R___LPull_DIM1_Int32R_ d b e s = d ! (Z :. F.i2n (b ! (Z :. 0)))


tfSum_Float_Float__LPull_DIM1_Int32R_ :: Data Float -> Pull DIM1 (Data Int32) -> Data Float
tfSum_Float_Float__LPull_DIM1_Int32R_ x ds = x
tfSum_Float__LPull_DIM2_FloatR___LPull_DIM1_Int32R_ :: Pull DIM2 (Data Float) -> Pull DIM1 (Data Int32) -> Data Float
tfSum_Float__LPull_DIM2_FloatR___LPull_DIM1_Int32R_ xs ds = V.fromZero $ V.sum $ mapOverRows (V.fromZero . V.sum) xs

tfVariableV2_Float :: Float -> Data Float
tfVariableV2_Float = value

tfVariableV2__LPull_DIM1_FloatR_ :: [Float] -> Pull DIM1 (Data Float)
tfVariableV2__LPull_DIM1_FloatR_ xs = V.arrToPull (Z :. getLength arr) arr
  where arr = value xs

tfVariableV2__LPull_DIM2_FloatR_ :: [[Float]] -> Pull DIM2 (Data Float)
tfVariableV2__LPull_DIM2_FloatR_ xs = V.arrToPull sh $ value $ P.concat xs -- V.fromList $ P.concat xs
  where sh = Z :. m :. n
        m :: Data Length
        m = getLength arr -- value $ P.length xs
        n :: Data Length
        n = m F.== 0 ? 0 $ getLength (arr!0) -- F.i2n $ value $ if P.null xs then 0 else (P.length $ P.head xs)
        arr = value xs

-- | Memoize a Pull vector
memoize :: (Type a, Shapely sh) => Pull sh (Data a) -> Pull sh (Data a)
memoize vec = Pull ixf ext
  where ext = extent vec
        ixf ix = arr ! toIndex ext ix
        arr = fromPull vec
